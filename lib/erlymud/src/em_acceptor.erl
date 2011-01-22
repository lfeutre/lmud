%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Accept new TCP connections.
%%% This gen_server will wait in a gen_tcp:accept() call until someone makes
%%% a connection, then it will immediately launch a new em_conn server and
%%% hand over the new socket for further processing, before it returns with
%%% a timeout so that it'll go back into accept mode again.
%%% @end
%%% =========================================================================
-module(em_acceptor).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock}).


%% API

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).


%% gen_server callbacks

init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{lsock=LSock}=State) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      {ok, Conn} = em_conn_sup:start_child(Socket),
      case gen_tcp:controlling_process(Socket, Conn) of
        ok ->
          ok;
        {error, Reason} ->
          exit(Conn, {error, Reason})
      end,
      {noreply, State, 0};
    {error, closed} ->
      {stop, normal, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.

