%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc The acceptor pool.
%%% Acceptor children that will wait for incoming connections can be started,
%%% given a listener socket. With a transient restart strategy, the acceptors
%%% in the pool will always be restarted if they die unexpectedly.
%%% @end
%%% =========================================================================
-module(em_acceptor_pool).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(LSock) ->
  supervisor:start_child(?SERVER, [LSock]).

init([]) ->
  Acceptor = {em_acceptor, {em_acceptor, start_link, []},
              transient, brutal_kill, worker, [em_acceptor]},
  Children = [Acceptor],
  RestartStrategy = {simple_one_for_one, 5, 10},
  {ok, {RestartStrategy, Children}}.

