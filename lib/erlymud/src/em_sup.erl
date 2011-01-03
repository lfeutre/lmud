-module(em_sup).

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
  GameServer = ?CHILD(em_game, worker),
  RoomSup = ?CHILD(em_room_sup, worker),
  LivingSup = ?CHILD(em_living_sup, worker),
  UserSup = ?CHILD(em_user_sup, worker),
  ConnSup = ?CHILD(em_conn_sup, worker),
  Children = [GameServer, RoomSup, LivingSup, UserSup, ConnSup],
  RestartStrategy = {one_for_one, 5, 10},
  {ok, {RestartStrategy, Children}}.

