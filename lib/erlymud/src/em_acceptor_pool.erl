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
-include("types.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Start an empty acceptor pool.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start an acceptor child process. It will become a transient process
%% that must exit with reason 'normal' to gracefully go away.
-spec start_child(socket()) -> any().
start_child(LSock) ->
  supervisor:start_child(?SERVER, [LSock]).

%% ==========================================================================
%% Supervisor Callbacks
%% ==========================================================================

init([]) ->
  Acceptor = {em_acceptor, {em_acceptor, start_link, []},
              transient, brutal_kill, worker, [em_acceptor]},
  Children = [Acceptor],
  RestartStrategy = {simple_one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.

