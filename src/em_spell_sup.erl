%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Spell supervisor, used to initialize new spells given a callback
%%% module and args.
%%% @end
%%% =========================================================================
-module(em_spell_sup).
-include("types.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Start an empty spell supervisor.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a spell child process.
start_child(Spell, Args) ->
  supervisor:start_child(?SERVER, [Spell, Args]).

%% ==========================================================================
%% Supervisor Callbacks
%% ==========================================================================

init([]) ->
  Spell = {em_spell, {em_spell, start_link, []},
           temporary, brutal_kill, worker, [em_spell]},
  Children = [Spell],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

