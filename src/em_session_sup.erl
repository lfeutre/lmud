%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Session supervisor.
%%% Basic simple_one_for_one supervisor that starts new sessions on request.
%%% @end
%%% =========================================================================
-module(em_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Conn) ->
  supervisor:start_child(?SERVER, [Conn]).

init([]) ->
  Session = {em_session, {em_session, start_link, []},
          temporary, brutal_kill, supervisor, [em_session]},
  Children = [Session],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

