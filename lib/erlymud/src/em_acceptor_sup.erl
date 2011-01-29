%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Main acceptor supervisor. Starts an acceptor pool and a listener,
%%% in one_for_all supervision. This prevents issues if the pool dies; the
%%% listener will also be restarted, and subsequently start up the acceptors
%%% in the pool again.
%%% @end
%%% =========================================================================
-module(em_acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_listener/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_listener(Port, Acceptors) ->
  Listener = ?CHILD(em_listener, [Port, Acceptors], worker),
  supervisor:start_child(?SERVER, Listener).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  AcceptorPool = ?CHILD(em_acceptor_pool, supervisor),
  Children = [AcceptorPool],
  RestartStrategy = {one_for_all, 5, 10},
  {ok, {RestartStrategy, Children}}.

