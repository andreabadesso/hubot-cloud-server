-module(app_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

-export([handle_message/3, auth_user/1]).

-record(state, {
          auth = false,
          user_id = undefined
         }).


init(Req, _) ->
  lager:info("Spawned an app handler."),
  Opts = #{compress => true},
  State = #state{},
  {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
  lager:info("App connected, requesting auth."),
  self() ! {auth_request},
  {ok, State}.

websocket_handle({text, Frame}, State) ->
  Msg = jiffy:decode(Frame, [return_maps]),
  #{<<"message_type">> := MessageType} = Msg,
  handle_message(MessageType, Msg, State),
  {ok, State};
websocket_handle(_Frame, State) ->
  lager:info("Not handled. ~p", [_Frame]),
  {ok, State}.

websocket_info({auth_success, CentralId, UserId}, State) ->
  app_controller:register(self(), CentralId, UserId),
  Msg = jiffy:encode(#{message_type => <<"auth_success">>}),
  {reply, {text, Msg}, State#state{auth = true, user_id = UserId}};
websocket_info({auth_fail}, State) ->
  Msg = jiffy:encode(#{message_type => <<"auth_fail">>}),
  {reply, {text, Msg}, State#state{auth = false}};
websocket_info({auth_request}, State) ->
  Msg = jiffy:encode(#{message_type => <<"auth_req">>}),
  {reply, {text, Msg}, State#state{auth = false}};
%% Send to client
websocket_info({msg, Msg}, State) when is_map(Msg) ->
  Payload = jiffy:encode(Msg),
  {reply, {text, Payload}, State};
%% Response from controller
websocket_info({ack, ok}, State) ->
  lager:debug("Message sent."),
  {ok, State};
websocket_info({ack, central_not_found}, State) ->
  %% Kill the socket
  lager:info("Central not connected, I will die."),
  {stop, State};
websocket_info(die, State) ->
  {stop, State};
websocket_info(Info, State) ->
  lager:debug("unhandled websocket info msg: ~p", [Info]),
  {ok, State}.

terminate(_Reason, _Req, State) ->
  lager:info("User connection terminated: ~p", [State#state.user_id]),
  ok.

%% ===================================================================
%% Internal
%% ===================================================================

auth_user(_Token) ->
  true.

handle_message(<<"auth">>, Msg, #state{auth = false} = _State) ->
  #{<<"token">> := Token,
    <<"central_id">> := CentralId,
    <<"user_id">> := UserId} = Msg,
  Auth = auth_user(Token),
  case Auth of
    true ->
      self() ! {auth_success, CentralId, UserId};
    false ->
      self() ! {auth_fail}
  end;
handle_message(_, Msg, #state{auth = true} = _State) ->
  app_controller:central_send(self(), Msg);
handle_message(_, _Msg, #state{auth = false} = _State) ->
  lager:info("Received message and is unauthenticated."),
  self() ! {auth_request};
handle_message(_Type, _Msg, _State) ->
  lager:warning("unknown type").

