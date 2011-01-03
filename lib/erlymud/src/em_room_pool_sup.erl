-module(em_room_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Title, Desc) ->
  supervisor:start_child(?SERVER, [Title, Desc]).

init([]) ->
  Room = {em_room, {em_room, start_link, []},
          transient, brutal_kill, worker, [em_room]},
  Children = [Room],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

