-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(ROUTES, [
                 {"/central", central_handler, []},
                 {"/app", app_handler, []},
                 {"/[...]", http_handler, []}
                ]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  application:ensure_all_started(jwt),
  lager:start(),
  quickrand:seed(),
  Ret = server_sup:start_link(),
  Dispatch = cowboy_router:compile([
                                    {'_', ?ROUTES}
                                   ]),
  {ok, _} = cowboy:start_clear(http, [{port, 3000}], #{
    env => #{dispatch => Dispatch}}),
  Ret.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
