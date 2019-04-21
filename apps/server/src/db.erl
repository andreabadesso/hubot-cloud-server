-module(db).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get_central_privkey/1]).

-define(SERVER, ?MODULE).

-record(state, {conn = undefined}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

get_central_privkey(CentralId) ->
  gen_server:call(?MODULE, {get_central_privkey, CentralId}).

init([]) ->
  application:ensure_all_started(jwt),
  Host = os:getenv("PG_HOST"),
  User = os:getenv("PG_USER"),
  Pass = os:getenv("PG_PASS"),
  Db = os:getenv("PG_DATABASE"),
  {ok, Pid} = epgsql:connect(Host, User, Pass, #{
      database => Db,
      timeout => 4000
  }),
  lager:info("Connected to: ~p", [Pid]),
  {ok, #state{conn = Pid}}.

handle_call({get_central_privkey, CentralId}, _From, State) ->
  Conn = State#state.conn,
  case epgsql:equery(Conn, "SELECT * FROM \"Central\" WHERE \"UUID\" = $1 LIMIT 1", [CentralId]) of
    {ok, _, Rows} ->
      case Rows of
        [{CentralId, _, PrivKey, _, Name, _, _, _, _}] ->
          {reply, PrivKey, State};
        [] ->
          {reply, error, State}
      end;
    {error, Error} ->
      lager:info("Error on query: ~p", [Error]),
      {reply, error, State}
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  epgsql:close(State#state.conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

