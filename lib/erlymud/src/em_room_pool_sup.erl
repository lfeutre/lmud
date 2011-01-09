%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Room pool supervisor.
%%% Manages the pool of instantiated rooms in the game. Only used by the
%%% em_room_mgr, where room instantiation happens. This is really just a
%%% cache to avoid loading the room each time people walk into it.
%%% @end
%%% =========================================================================
-module(em_room_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, which_children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name, Title, Desc) ->
  supervisor:start_child(?SERVER, [Name, Title, Desc]).

which_children() ->
  supervisor:which_children(?SERVER).

init([]) ->
  Room = {em_room, {em_room, start_link, []},
          temporary, brutal_kill, worker, [em_room]},
  Children = [Room],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

