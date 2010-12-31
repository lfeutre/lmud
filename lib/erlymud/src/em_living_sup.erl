-module(em_living_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name, Room, Client) ->
  supervisor:start_child(?SERVER, [Name, Room, Client]).

init([]) ->
  Living = {em_living, {em_living, start_link, []},
            temporary, brutal_kill, worker, [em_living]},
  Children = [Living],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

