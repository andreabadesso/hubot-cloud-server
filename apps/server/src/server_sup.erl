-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  AppController = {app_controller,
                {app_controller, start_link, []},
                permanent,
                5000,
                worker,
                [app_controller]},
  CentralController = {central_controller,
                {central_controller, start_link, []},
                permanent,
                5000,
                worker,
                [central_controller]},
  {ok, {{rest_for_one, 3, 10}, [AppController, CentralController]}}.

%%====================================================================
%% Internal functions
%%====================================================================
