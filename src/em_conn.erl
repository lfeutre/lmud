%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Manage new TCP connections.
%%% This gen_server will launch a new em_session server for the new
%%% connection, then handle any input/output on the socket.
%%% @end
%%% =========================================================================
-module(em_conn).
-include("types.hrl").
-include("telnet.hrl").

-behaviour(gen_server).

-export([start_link/1, echo_off/1, echo_on/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket::socket(), session::pid(), telnet_session}).

-type conn_pid() :: pid().
-export_type([conn_pid/0]).

%% API

-spec start_link(socket()) -> any().
start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%% Turn client echo off
-spec echo_off(pid()) -> any().
echo_off(Conn) ->
  gen_server:call(Conn, echo_off).

%% Turn client echo on
-spec echo_on(pid()) -> any().
echo_on(Conn) ->
  gen_server:call(Conn, echo_on).

-spec print(pid(), iolist()) -> any().
print(Conn, Format) ->
  gen_server:call(Conn, {print, Format}).

-spec print(pid(), iolist(), list()) -> any().
print(Conn, Format, Args) ->
  gen_server:call(Conn, {print, Format, Args}).

%% gen_server callbacks

init([Socket]) ->
  {ok, #state{socket = Socket}, 0}.

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

handle_info({tcp, Socket, RawData}, State) ->
  {ok, NewSession} = em_telnet:parse(State#state.telnet_session, RawData),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{telnet_session=NewSession}};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, tcp_closed, State};
handle_info(timeout, #state{socket=Socket}=State) ->
  {ok, Session} = em_session_sup:start_child(self()),
  link(Session),
  PrintFun = fun(Line) -> em_session:receive_line(Session, Line) end,
  TelnetSession = em_telnet:new(Socket, PrintFun),
  {noreply, State#state{socket=Socket, session=Session, 
                        telnet_session=TelnetSession}}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

-spec write(socket(), iolist(), list()) -> ok | {error, any()}.
write(Socket, Format, Args) ->
  Data = process_output(Format, Args),
  gen_tcp:send(Socket, Data).

-spec process_output(iolist(), list()) -> string().
process_output(Format, Args) ->
  Data = em_text:colorize(io_lib:format(Format, Args)),
  re:replace(Data, "\n", "\r\n", [global, {return, list}]).
