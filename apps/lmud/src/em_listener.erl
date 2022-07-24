%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Listen for new TCP connections.
%%% This gen_server will open a listening port for incoming TCP connections,
%%% then start a number of acceptors in the acceptor pool.
%%% @end
%%% =========================================================================
-module(em_listener).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("apps/lmud/include/types.hrl").
-include_lib("apps/lmud/include/state.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Start the server and begin listening on the specified port, spawning
%% a number of acceptors as we go.
-spec start_link(inet_port(), count()) -> any().
start_link(Port, Acceptors) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Acceptors], []).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

init([Port, Acceptors]) ->
  {ok, #state_listener{port=Port, acceptors=Acceptors}, 0}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state_listener{port=Port, acceptors=Acceptors}=State) ->
  % Listen on our configured port
  {ok, LSock} = gen_tcp:listen(Port, [{active, once},
                                      {nodelay, true},
                                      {reuseaddr, true}]),
  % Start up the acceptor processes
  start_acceptors(LSock, Acceptors),
  {noreply, State#state_listener{lsock=LSock}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state_listener.lsock of
    undefined ->
      ok;
    LSock ->
      gen_tcp:close(LSock),
      ok
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

-spec start_acceptors(socket(), count()) -> ok.
start_acceptors(_LSock, 0) -> ok;
start_acceptors(LSock, Acceptors) when is_integer(Acceptors), Acceptors > 0 ->
  'lmud-acceptor-pool':start_child(LSock),
  start_acceptors(LSock, Acceptors-1).
