%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Listen for new TCP connections.
%%% This gen_server will start listening for incoming TCP connections, then
%%% start a number of acceptors in the acceptor pool.
%%% @end
%%% =========================================================================
-module(em_listener).

-behaviour(gen_server).

-export([start_link/0, listen/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock}).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 2155).
-define(DEFAULT_ACCEPTORS, 1).


%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

listen(Port, Acceptors) ->
  gen_server:call(?SERVER, {listen, Port, Acceptors}).

%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call({listen, Port, Acceptors}, _From, State) ->
  % Listen on our configured port
  {ok, LSock} = gen_tcp:listen(Port, [{active, once},
                                      {nodelay, true},
                                      {reuseaddr, true}]),
  % Start up the acceptor processes
  start_acceptors(LSock, Acceptors),
  {reply, ok, State#state{lsock=LSock}};
handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.lsock of
    undefined -> 
      ok;
    LSock ->
      gen_tcp:close(LSock),
      ok
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

start_acceptors(_LSock, 0) -> ok;
start_acceptors(LSock, Acceptors) when is_integer(Acceptors), Acceptors > 0 ->
  em_acceptor_pool:start_child(LSock),
  start_acceptors(LSock, Acceptors-1).
