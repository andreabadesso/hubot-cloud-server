-module(central_controller).

-behaviour(gen_server).

-export([start_link/0,
         register/2,
         unregister/1,
         send/3,
         http_send/2,
         app_send/2,
         is_connected/1,
         app_connect/2,
         app_disconnect/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(MAX_RETRY, 3).
-define(TMO_RETRY, 150).
-define(PING_INTERVAL, 55000).

-record(state, {
          central_list    = [],
          ack_list        = []
         }).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Pid, CentralId) ->
  gen_server:call(?MODULE, {register, Pid, CentralId}).

unregister(Pid) ->
  gen_server:call(?MODULE, {unregister, Pid}).

app_connect(UserId, CentralId) ->
  gen_server:call(?MODULE, {app_connect, UserId, CentralId}).

app_disconnect(UserId, CentralId) ->
  gen_server:call(?MODULE, {app_disconnect, UserId, CentralId}).

app_send(UserId, Msg) ->
  gen_server:call(?MODULE, {app_send, UserId, Msg}).

send(CentralId, UserId, Msg) ->
  gen_server:call(?MODULE, {send, CentralId, UserId, Msg}).

http_send(CentralId, Data) ->
  gen_server:call(?MODULE, {http_send, CentralId, Data}).

is_connected(CentralId) ->
  gen_server:call(?MODULE, {is_connected, CentralId}).


%% ===================================================================
%% gen_server
%% ===================================================================


%% 1) On register, add central_id to tuple
%% 2) On unregister, find connection with Pid and remove it.
%%
%% Apps get registered with:

init([]) ->
  lager:info("starting central controller"),
  State = #state{central_list = [], ack_list = []},
  {ok, State}.

%% Register the app on app_list
handle_call({register, Pid, CentralId}, _From, State) ->
  erlang:monitor(process, Pid),
  NewList = [{Pid, CentralId} | State#state.central_list],
  lager:info("add central socket ~p ~p", [Pid, CentralId]),
  {reply, ok, State#state{central_list = NewList}};
handle_call({app_connect, UserId, CentralId}, _From, State) ->
  case find(CentralId, 2, State#state.central_list) of
    {Pid, _} ->
      Pid ! {conn_msg, #{
                    message_type => <<"app_connect">>,
                    user_id => UserId
                   }},
      {reply, ok, State};
    none ->
      {reply, central_not_found, State}
  end;
handle_call({app_disconnect, UserId, CentralId}, _From, State) ->
  lager:info("App disconnected ~p (~p)", [UserId, CentralId]),
  case find(CentralId, 2, State#state.central_list) of
    {Pid, _} ->
      Pid ! {conn_msg, #{
                    message_type => <<"app_disconnect">>,
                    user_id => UserId
                   }},
      {reply, ok, State};
    none ->
      {reply, central_not_found, State}
  end;
handle_call({app_send, UserId, Msg}, {_Pid, _Ref}, State) ->
  app_controller:send(UserId, Msg),
  {reply, ok, State};
handle_call({http_send, CentralId, Data}, {FromPid, _Ref}, State) ->
  Uuid = list_to_binary(uuid:uuid_to_string(uuid:get_v4_urandom())),
  case find(CentralId, 2, State#state.central_list) of
    {Pid, _} ->
      Pid ! {http_msg, Uuid, Data},
      NewAckList = [{Uuid, FromPid} | State#state.ack_list],
      {reply, ok, State#state{ack_list = NewAckList}};
    none ->
      {reply, central_not_found, State}
  end;
handle_call({send, CentralId, UserId, Msg}, _From, State) ->
  case find(CentralId, 2, State#state.central_list) of
    {Pid, _} ->
      Pid ! {msg, UserId, Msg},
      {reply, ok, State};
    none ->
      {reply, central_not_found, State}
  end;
handle_call({is_connected, CentralId}, _From, State) ->
  case find(CentralId, 2, State#state.central_list) of
    {_, _} ->
      {reply, ok, State};
    none ->
      {reply, central_not_found, State}
  end;
handle_call({http_ack_received, Uuid, Data}, _From, State) ->
  case find(Uuid, 1, State#state.ack_list) of
    {_, Pid} ->
      Pid ! {http_ack, Data},
      {reply, ok, State};
    none ->
      lager:info("Uuid: ~p, List: ~p", [Uuid, State#state.ack_list]),
      lager:info("Received http ack but no one is listening."),
      {reply, ok, State}
  end;
%% Miss
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  erlang:demonitor(Ref),
  Element = find(Pid, 1, State#state.central_list),
  case Element of
    {_, CentralId} ->
      NewCentralList = lists:delete(Element, State#state.central_list),
      %% Tell app controller to disconnect all users connected
      %% to this central.
      lager:info("process ~p died for reason ~p", [Pid, Reason]),
      app_controller:central_dead(CentralId),
      {noreply, State#state{central_list = NewCentralList}};
    none ->
      lager:error("Process died but central was not registered."),
      {noreply, State}
  end;
handle_info(Info, State) ->
  lager:warning("Unhandled msg ~p", [Info]),
  {noreply, State}.

%%  gen_server:cast(?MODULE, {send, CentralId, Pid, Msg}).
handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% Internal
%% ===================================================================

find(K, Index, [H|T]) ->
  case Index of
    1 ->
      case H of
          {K, _} -> H;
          _ -> find(K, Index, T)
      end;
    2 ->
      case H of
          {_, K} -> H;
          _ -> find(K, Index, T)
      end
  end;
find(_, _, []) -> none.
