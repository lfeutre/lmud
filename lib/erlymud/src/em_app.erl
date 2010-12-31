-module(erlymud_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case erlymud_sup:start_link() of
      {ok, Pid} ->
        ti_sup:start_child({erlymud, connect, []}),
        {ok, Pid};
      Other ->
        {error, Other}
    end.

stop(_State) ->
    ok.
