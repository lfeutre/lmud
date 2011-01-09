%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Supervisor for em_living servers.
%%% A plain simple_one_for_one supervisor that allows em_living processes to
%%% be started when a player logs in.
%%% @end
%%% =========================================================================
-module(em_living_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name, Client) ->
  supervisor:start_child(?SERVER, [Name, Client]).

init([]) ->
  % Allow 2000ms for em_living to clean up; removing itself
  % from the game world..
  Living = {em_living, {em_living, start_link, []},
            temporary, 2000, worker, [em_living]},
  Children = [Living],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

