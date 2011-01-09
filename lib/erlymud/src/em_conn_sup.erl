%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc The connection supervisor.
%%% Opens a listening socket, then hands it off to any new children so that
%%% they can wait for incoming connections.
%%% @end
%%% =========================================================================
-module(em_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 2155).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([]) ->
  Port = case application:get_env(port) of
           {ok, P} -> P;
           undefined -> ?DEFAULT_PORT
         end,
  {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                      {nodelay, true},
                                      {reuseaddr, true}]),
  Connection = {em_conn, {em_conn, start_link, [LSock]},
                temporary, brutal_kill, worker, [em_conn]},
  Children = [Connection],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

