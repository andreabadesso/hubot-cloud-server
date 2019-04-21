-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(ROUTES, [
                 {"/central", central_handler, []},
                 {"/websocket", app_handler, []},
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
  {ok, _} = cowboy:start_clear(http, [{port, 80}], #{
                                         env => #{dispatch => Dispatch},
                                         middlewares => [
                                                         cowboy_router,
                                                         ca_cowboy_middleware,
                                                         cowboy_handler]
                                        }),
  Ret.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
