%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Main room supervisor.
%%% Keeps track of the room manager (em_room_mgr) and room pool supervisor
%%% (em_room_pool_sup), making sure they are restarted if they crash.
%%% @end
%%% =========================================================================
-module(em_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RoomPoolSup = ?CHILD(em_room_pool_sup, supervisor),
  RoomMgr = ?CHILD(em_room_mgr, worker),
  Children = [RoomPoolSup, RoomMgr],
  RestartStrategy = {one_for_one, 5, 10},
  {ok, {RestartStrategy, Children}}.

