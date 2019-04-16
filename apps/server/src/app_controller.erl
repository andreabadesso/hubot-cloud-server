-module(app_controller).

-behaviour(gen_server).

-export([start_link/0,
         register/3,
         unregister/1,
         send/2,
         central_send/2
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

%% ===================================================================
%% gen_server
%% ===================================================================


%% 1) On register, add central_id to tuple
%% 2) On unregister, find connection with Pid and remove it.
%%
%% Apps get registered with:

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
      {reply, ok, State#state{app_list = NewList}};
    central_not_found ->
      lager:info("Central is not connected, we should kill the app socket"),
      Pid ! {ack, central_not_found},
      {reply, ok, State}
  end;
handle_call({unregister, Pid}, _From, State) ->
  erlang:demonitor(Pid),
  App = find(Pid, 1, State#state.app_list),
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
  Element = find(Pid, 1, State#state.app_list),
  AppList = lists:delete(Element, State#state.app_list),
  lager:info("process ~p died for reason ~p", [Pid, Reason]),
  {noreply, State#state{app_list = AppList}};
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
