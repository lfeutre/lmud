%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Accept and manage new TCP connections.
%%% This gen_server will wait in a gen_tcp:accept() call until someone makes
%%% a connection, then it will immediately spawn a new em_conn server to
%%% handle the next incoming connection, and launch a new session for the
%%% current connection.
%%% @end
%%% =========================================================================
-module(em_conn).
-include("telnet.hrl").

-behaviour(gen_server).

-export([start_link/1, echo_off/1, echo_on/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, socket, session, telnet_session}).


%% API

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

%% Turn client echo off
echo_off(Conn) ->
  gen_server:call(Conn, echo_off).

%% Turn client echo on
echo_on(Conn) ->
  gen_server:call(Conn, echo_on).

print(Conn, Format) ->
  gen_server:call(Conn, {print, Format}).

print(Conn, Format, Args) ->
  gen_server:call(Conn, {print, Format, Args}).

%% gen_server callbacks

init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call(echo_off, _From, State) ->
  {ok, TelSess} = em_telnet:will(?ECHO, State#state.telnet_session),
  {reply, ok, State#state{telnet_session=TelSess}};
handle_call(echo_on, _From, State) ->
  {ok, TelSess} = em_telnet:wont(?ECHO, State#state.telnet_session),
  {reply, ok, State#state{telnet_session=TelSess}};
handle_call({print, Format}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format, []),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, _Socket, RawData}, State) ->
  {ok, NewSession} = em_telnet:parse(State#state.telnet_session, RawData),
  {noreply, State#state{telnet_session=NewSession}};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, tcp_closed, State};
handle_info(timeout, #state{lsock=LSock}=State) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      em_conn_sup:start_child(),
      {ok, Session} = em_session_sup:start_child(self()),
      link(Session),
      PrintFun = fun(Line) -> em_session:receive_line(Session, Line) end,
      TelnetSession = em_telnet:new(Socket, PrintFun),
      {noreply, State#state{socket=Socket, session=Session, 
                            telnet_session=TelnetSession}};
    {error, closed} ->
      {stop, normal, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

write(Socket, Format, Args) ->
  Data = process_output(Format, Args),
  gen_tcp:send(Socket, Data).

process_output(Format, Args) ->
  Data = em_text:colorize(io_lib:format(Format, Args)),
  re:replace(Data, "\n", "\r\n", [global, {return, list}]).
