%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Launch a request, collect the result, and kill it.
%%% @end
%%% =========================================================================
-module(em_req_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, request/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(MFA) ->
  supervisor:start_child(?SERVER, [MFA]).

request(MFA) ->
  {ok, Req} = start_child(MFA),
  Result = em_req:run(Req),
  exit(Req, normal),
  Result.

init([]) ->
  Request = {em_req, {em_req, start_link, []},
             temporary, brutal_kill, worker, [em_req]},
  Children = [Request],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

