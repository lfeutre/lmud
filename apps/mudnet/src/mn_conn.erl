%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Manage new TCP connections.
%%% This gen_server will launch a new mn_session server for the new
%%% connection, then handle any input/output on the socket.
%%% @end
%%% =========================================================================
-module(mn_conn). %% TODO: rename to mn_io

-behaviour(gen_server).

-export([start_link/1, echo_off/1, echo_on/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("apps/lmud/include/types.hrl").

-include("apps/mudnet/include/state.hrl").
-include("apps/mudnet/include/telnet.hrl").



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
  {ok, #state_conn{socket = Socket}, 0}.

handle_call(echo_off, _From, State) ->
  {ok, TelSess} = mn_telnet:will(?ECHO, State#state_conn.telnet_session),
  {reply, ok, State#state_conn{telnet_session=TelSess}};
handle_call(echo_on, _From, State) ->
  {ok, TelSess} = mn_telnet:wont(?ECHO, State#state_conn.telnet_session),
  {reply, ok, State#state_conn{telnet_session=TelSess}};
handle_call({print, Format}, _From, #state_conn{socket=Socket}=State) ->
  write(Socket, Format, []),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state_conn{socket=Socket}=State) ->
  write(Socket, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State) ->
  {ok, NewSession} = mn_telnet:parse(State#state_conn.telnet_session, RawData),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state_conn{telnet_session=NewSession}};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, tcp_closed, State};
handle_info(timeout, #state_conn{socket=Socket}=State) ->
  {ok, Session} = 'mn-session-sup':start_child(self()),
  link(Session),
  PrintFun = fun(Line) -> mn_session:receive_line(Session, Line) end,
  TelnetSession = mn_telnet:new(Socket, PrintFun),
  {noreply, State#state_conn{socket=Socket, session=Session,
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
  Data = io_lib:format(Format, Args),
  re:replace(Data, "\n", "\r\n", [global, {return, list}]).
