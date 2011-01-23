%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc The connection supervisor.
%%% @end
%%% =========================================================================
-module(em_conn_sup).
-include("types.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(socket()) -> any().
start_child(Socket) ->
  supervisor:start_child(?SERVER, [Socket]).

init([]) ->
  Connection = {em_conn, {em_conn, start_link, []},
                temporary, brutal_kill, worker, [em_conn]},
  Children = [Connection],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

