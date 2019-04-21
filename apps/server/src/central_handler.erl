-module(central_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

-export([handle_message/3, auth_user/2]).

-record(state, {auth = false}).

init(Req, _) ->
  lager:info("Spawned a central handler."),
  Opts = #{compress => true},
  State = #state{},
  {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
  lager:info("Central connected, requesting auth."),
  self() ! {auth_request},
  {ok, State}.

websocket_handle({text, Frame}, State) ->
  Msg = jiffy:decode(Frame, [return_maps]),
  #{<<"message_type">> := MessageType} = Msg,
  handle_message(MessageType, Msg, State),
  {ok, State};
websocket_handle(ping, State) ->
  {reply, pong, State};
websocket_handle(_Frame, State) ->
  lager:info("Not handled. ~p", [_Frame]),
  {ok, State}.

websocket_info({auth_success, CentralId}, State) ->
  central_controller:register(self(), CentralId),
  Msg = jiffy:encode(#{message_type => <<"auth_success">>}),
  {reply, {text, Msg}, State#state{auth = true}};
websocket_info({auth_fail}, State) ->
  Msg = jiffy:encode(#{message_type => <<"auth_fail">>}),
  {reply, {text, Msg}, State#state{auth = false}};
websocket_info({auth_request}, State) ->
  Msg = jiffy:encode(#{message_type => <<"auth_req">>}),
  {reply, {text, Msg}, State#state{auth = false}};
%% External message to socket
websocket_info({conn_msg, Msg}, State) when is_map(Msg) ->
  %% We will add the user_id that is sending the message to the
  %% message and send to the central
  Payload = jiffy:encode(Msg),
  {reply, {text, Payload}, State};
websocket_info({msg, UserId, Msg}, State) when is_map(Msg) ->
  %% We will add the user_id that is sending the message to the
  %% message and send to the central
  UserMsg = #{message_type => <<"websocket">>,
              user_id => UserId,
              message => Msg},
  Payload = jiffy:encode(UserMsg),
  {reply, {text, Payload}, State};
websocket_info({http_msg, Uuid, Data}, State) when is_map(Data) ->
  UserMsg = #{message_type => <<"http">>,
              uuid => Uuid,
              data => Data},
  lager:info("Sending: ~p", [Uuid]),
  Payload = jiffy:encode(UserMsg),
  {reply, {text, Payload}, State};
websocket_info(Info, State) ->
  lager:debug("unhandled websocket info msg: ~p", [Info]),
  {ok, State}.

terminate(_Reason, _Req, _State) ->
  lager:info("Terminate"),
  ok.

%% ===================================================================
%% Internal
%% ===================================================================

auth_user(Token, CentralId) ->
  PrivKey= db:get_central_privkey(CentralId),
  jwt:decode(Token, PrivKey).

handle_message(<<"auth">>, Msg, #state{auth = false} = _State) ->
  #{<<"token">> := Token, <<"central_id">> := C} = Msg,
  case auth_user(Token, C) of
    {ok, Data} ->
      #{ <<"central_id">> := CentralId } = Data,
      lager:info("Data ~p", [Data]),
      self() ! {auth_success, CentralId};
    {error, _} ->
      self() ! {auth_fail}
  end;
handle_message(<<"http">>, Msg, #state{auth = true} = _State) ->
  #{<<"uuid">> := Uuid} = Msg,
  gen_server:call(central_controller, {http_ack_received, Uuid, Msg});
handle_message(<<"websocket">>, Msg, #state{auth = true} = _State) ->
  %% Get UserId and send to app_controller
  #{<<"user_id">> := UserId,
    <<"message">> := Message} = Msg,
  central_controller:app_send(UserId, Message);
handle_message(_, _Msg, #state{auth = false} = _State) ->
  self() ! {auth_request};
handle_message(_, _Msg, _State) ->
  lager:warning("unknown type").

