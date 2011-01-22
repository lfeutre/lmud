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

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock}).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 2155).


%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% gen_server callbacks

init([]) ->
  Port = case application:get_env(port) of
           {ok, P} -> P;
           undefined -> ?DEFAULT_PORT
         end,
  % Listen on our configured port
  {ok, LSock} = gen_tcp:listen(Port, [{active, once},
                                      {nodelay, true},
                                      {reuseaddr, true}]),
  % Start up two acceptor processes
  em_acceptor_pool:start_child(LSock),
  em_acceptor_pool:start_child(LSock),
  {ok, #state{lsock=LSock}}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  gen_tcp:close(State#state.lsock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

