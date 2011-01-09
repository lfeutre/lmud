%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Top-level supervisor.
%%% This is the top of the supervision tree, where all necessary game
%%% components are started in the required order.
%%% @end
%%% =========================================================================
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
  RoomSup = ?CHILD(em_room_sup, supervisor),
  LivingSup = ?CHILD(em_living_sup, supervisor),
  UserSup = ?CHILD(em_user_sup, supervisor),
  SessionSup = ?CHILD(em_session_sup, supervisor),
  ReqSup = ?CHILD(em_req_sup, supervisor),
  ConnSup = ?CHILD(em_conn_sup, supervisor),
  Children = [GameServer, RoomSup, LivingSup, UserSup,
              ReqSup, SessionSup, ConnSup],
  RestartStrategy = {one_for_one, 5, 10},
  {ok, {RestartStrategy, Children}}.

