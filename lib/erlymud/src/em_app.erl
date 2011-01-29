%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Application module for ErlyMUD. Will start listening for connections
%%% once the application startup has completed.
%%% @end
%%% =========================================================================
-module(em_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 2155).
-define(DEFAULT_ACCEPTORS, 1).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case em_sup:start_link() of
      {ok, Pid} ->
        Port = case application:get_env(port) of
                 {ok, P} -> P;
                 undefined -> ?DEFAULT_PORT
               end,
        Acceptors = case application:get_env(acceptors) of
                      {ok, A} -> A;
                      undefined -> ?DEFAULT_ACCEPTORS
                    end,
        em_acceptor_sup:start_listener(Port, Acceptors),
        {ok, Pid};
      Other ->
        {error, Other}
    end.

stop(_State) ->
    ok.

