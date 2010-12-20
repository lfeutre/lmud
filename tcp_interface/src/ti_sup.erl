-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock, Handler) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock, Handler]).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([LSock, Handler]) ->
  Server = {ti_server, {ti_server, start_link, [LSock, Handler]},
            temporary, brutal_kill, worker, [ti_server]},
  Children = [Server],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

