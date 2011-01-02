-module(em_output_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Socket) ->
  supervisor:start_child(?SERVER, [Socket]).

init([]) ->
  OutputHandler = {em_output, {em_output, start_link, []},
                   temporary, brutal_kill, worker, [em_output]},
  Children = [OutputHandler],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

