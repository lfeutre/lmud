-module(em_conn).
-include("telnet.hrl").

-behaviour(gen_server).

-export([start_link/1, echo_off/1, echo_on/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, socket, session}).


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

handle_call(echo_off, _From, #state{socket=Socket}=State) ->
  gen_tcp:send(Socket, [?IAC,?WILL,?ECHO]),
  {reply, ok, State};
handle_call(echo_on, _From, #state{socket=Socket}=State) ->
  gen_tcp:send(Socket, [?IAC,?WONT,?ECHO]),
  {reply, ok, State};
handle_call({print, Format}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format, []),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State) ->
  case handle_telnet(Socket, RawData, State) of
    {"", NewState} ->
      {noreply, NewState};
    {Data, NewState} ->
      handle_data(Socket, Data, NewState),
      {noreply, NewState}
  end;
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(timeout, #state{lsock=LSock}=State) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      em_conn_sup:start_child(),
      {ok, Session} = em_session_sup:start_child(self()),
      link(Session),
      {noreply, State#state{socket=Socket, session=Session}};
    {error, closed} ->
      {stop, normal, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

handle_telnet(_Socket, RawData, State) ->
  {process_telnet(RawData), State}.

handle_data(_Socket, RawData, State) ->
  em_session:receive_line(State#state.session, RawData).

write(Socket, Format, Args) ->
  Data = em_text:colorize(io_lib:format(Format, Args)),
  gen_tcp:send(Socket, Data).

%% Telnet protocol handling

process_telnet(RawData) ->
  process_telnet(RawData, []).

process_telnet([], Output) ->
  lists:reverse(Output);
process_telnet([?IAC|RawData], Output) ->
  process_cmd(RawData, Output);
process_telnet([Ch|RawData], Output) ->
  process_telnet(RawData, [Ch|Output]).

process_cmd([], Output) ->
  Output;
process_cmd([?WILL,_Opt|RawData], Output) ->
  process_telnet(RawData, Output);
process_cmd([?WONT,_Opt|RawData], Output) ->
  process_telnet(RawData, Output);
process_cmd([?DO,_Opt|RawData], Output) ->
  process_telnet(RawData, Output);
process_cmd([?DONT,_Opt|RawData], Output) ->
  process_telnet(RawData, Output);
process_cmd([_|RawData], Output) ->
  process_telnet(RawData, Output).

