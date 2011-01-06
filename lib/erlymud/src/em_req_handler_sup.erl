-module(em_req_handler_sup).

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
  RequestHandler = {em_req_handler, {em_req_handler, start_link, []},
          temporary, brutal_kill, supervisor, [em_req_handler]},
  Children = [RequestHandler],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

