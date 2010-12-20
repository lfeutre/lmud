-module(ti_server).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, handlers}).

start_link(LSock, Handler) ->
  gen_server:start_link(?MODULE, [LSock, Handler], []).

init([LSock, Handler]) ->
  {ok, #state{lsock = LSock, handlers = [Handler]}, 0}.

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  {ok, NewState} = handle_data(Socket, RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#state.handlers of
    [] -> 
      gen_tcp:send(Socket, "Goodbye!\n"),
      {stop, normal, NewState};
    _Other -> {noreply, NewState}
  end;
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  ti_sup:start_child(),
  gen_tcp:send(Socket, "\n\nWelcome to ErlyMUD!\n"),
  gen_tcp:send(Socket, "  - to quit, type 'quit'\n"),
  gen_tcp:send(Socket, "> "),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_data(Socket, RawData, State) ->
  Data = re:replace(RawData, "\r\n$", "", [{return, list}]),
  [Handler | Rest] = State#state.handlers,
  {M, F, HandlerState} = Handler,
  {Text, Result} = case M:F(Data, HandlerState) of
    {done, Response} -> 
      {Response, {ok, State#state{handlers = Rest}}};
    {ok, Response, NewState} -> 
      {Response, {ok, State#state{handlers = [{M, F, NewState} | Rest]}}};
    {push, Response, NextHandler, NewState} -> 
      {Response, {ok, State#state{handlers = [NextHandler, {M, F, NewState} | Rest]}}};
    Other -> 
      {"ERROR: Handler returned an unexpected result!", {error, Other}}
  end,
  ProperText = re:replace(Text, "\n", "\r\n", [{return, list}]),
  gen_tcp:send(Socket, ProperText),
  gen_tcp:send(Socket, "> "),
  Result.

