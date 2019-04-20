-module(app_controller).

-behaviour(gen_server).

-export([start_link/0,
         register/3,
         unregister/1,
         send/2,
         central_send/2,
         central_dead/1
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
          app_list
         }).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Pid, CentralId, UserId) ->
  gen_server:call(?MODULE, {register, Pid, CentralId, UserId}).

unregister(Pid) ->
  gen_server:call(?MODULE, {unregister, Pid}).


send(UserId, Msg) ->
  gen_server:call(?MODULE, {send, UserId, Msg}).

central_send(Pid, Msg) ->
  gen_server:call(?MODULE, {central_send, Pid, Msg}).

central_dead(CentralId) ->
  gen_server:call(?MODULE, {central_dead, CentralId}).

%% ===================================================================
%% gen_server
%% ===================================================================


init([]) ->
  lager:info("starting controller"),
  State = #state{app_list = []},
  {ok, State}.

%% Register the app on app_list
handle_call({register, Pid, CentralId, UserId}, _From, State) ->
  erlang:monitor(process, Pid),
  %% Check if requested central is registered
  CentralConnected = central_controller:is_connected(CentralId),
  case find(UserId, 2, State#state.app_list) of
    {OldPid, _, _} ->
      %% User is already connected, we should close the old
      %% socket and add the new one.
      lager:info("User is already connected, closing last connection"),
      OldPid ! die;
    none ->
      do_nothing
  end,
  case CentralConnected of
    ok ->
      NewList = [{Pid, UserId, CentralId} | State#state.app_list],
      lager:info("added app socket ~p ~p", [Pid, CentralId]),
      %% Register app socket on cloud client
      central_controller:app_connect(UserId, CentralId),
      {reply, ok, State#state{app_list = NewList}};
    central_not_found ->
      lager:info("Central is not connected, we should kill the app socket"),
      Pid ! {ack, central_not_found},
      {reply, ok, State}
  end;
handle_call({unregister, Pid}, _From, State) ->
  erlang:demonitor(Pid),
  App = find(Pid, 1, State#state.app_list),
  {_, UserId, CentralId} = App,
  central_controller:app_disconnect(UserId, CentralId),
  NewList = lists:delete(App, State#state.app_list),
  lager:info("removed app socket ~p (~p)", [Pid, App]),
  {reply, ok, State#state{app_list = NewList}};

handle_call({send, UserId, Msg}, _From, State) ->
  %% Find the central the socket is connected to.
  case find(UserId, 2, State#state.app_list) of
    {Pid, _, _} ->
      Pid ! {msg, Msg},
      {reply, ok, State};
    none ->
      {reply, client_not_found, State}
  end;
handle_call({central_dead, CentralId}, _From, State) ->
  Apps = find_all(CentralId, 3, State#state.app_list),
  [Pid ! die || {Pid, _UserId, _CentralId} <- Apps],
  lager:info("Killed apps: ~p", [Apps]),
  {reply, ok, State#state{app_list = []}};
handle_call({central_send, Pid, Msg}, _From, State) ->
  %% Find the central the socket is connected to.
  case find(Pid, 1, State#state.app_list) of
    {_, UserId, CentralId} ->
      Reply = central_controller:send(CentralId, UserId, Msg),
      Pid ! {ack, Reply},
      {reply, ok, State};
    none ->
      {reply, client_not_found, State}
  end;
%% Miss
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
  erlang:demonitor(Ref),
  case find(Pid, 1, State#state.app_list) of
    none ->
      lager:info("process ~p died for reason ~p", [Pid, Reason]),
      {noreply, State};
    Element ->
      {_, UserId, CentralId} = Element,
      central_controller:app_disconnect(UserId, CentralId),
      AppList = lists:delete(Element, State#state.app_list),
      lager:info("process ~p died for reason ~p", [Pid, Reason]),
      {noreply, State#state{app_list = AppList}}
  end;
handle_info(Info, State) ->
  lager:warning("Unhandled msg ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% Internal
%% ===================================================================

find_all(K, Index, List) ->
  find_all(K, Index, List, []).

find_all(K, Index, [H | T], Acc) ->
  case Index of
    1 ->
      case H of
          {K, _, _} -> find_all(K, Index, T, [H | Acc]);
          _ -> find_all(K, Index, T, Acc)
      end;
    2 ->
      case H of
          {_, K, _} -> find_all(K, Index, T, [H | Acc]);
          _ -> find(K, Index, T)
      end;
    3 ->
      case H of
          {_, _, K} -> find_all(K, Index, T, [H | Acc]);
          _ -> find(K, Index, T)
      end
  end;
find_all(K, Index, [], Acc) ->
  Acc.

find(K, Index, [H|T]) ->
  case Index of
    1 ->
      case H of
          {K, _, _} -> H;
          _ -> find(K, Index, T)
      end;
    2 ->
      case H of
          {_, K, _} -> H;
          _ -> find(K, Index, T)
      end;
    3 ->
      case H of
          {_, _, K} -> H;
          _ -> find(K, Index, T)
      end
  end;
find(_, _, []) -> none.
