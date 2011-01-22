%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Application module for ErlyMUD.
%%% @end
%%% =========================================================================
-module(em_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case em_sup:start_link() of
      {ok, Pid} ->
        {ok, Pid};
      Other ->
        {error, Other}
    end.

stop(_State) ->
    ok.

