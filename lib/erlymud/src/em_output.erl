-module(em_output).

-behaviour(gen_server).

-export([start_link/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).


%% API

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

print({_In, Out}, Format) ->
  print(Out, Format);
print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print({_In, Out}, Format, Args) ->
  print(Out, Format, Args);
print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).


%% gen_server callbacks

init([Socket]) ->
  {ok, #state{socket = Socket}}.

handle_call({print, Format}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{socket=Socket}=State) ->
  write(Socket, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

write(Socket, Format) ->
  write(Socket, Format, []).

write(Socket, Format, Args) ->
  Output = em_text:colorize(Format),
  gen_tcp:send(Socket, io_lib:format(Output, Args)).

