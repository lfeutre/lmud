-module(ti_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, handlers}).

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
  {ok, #state{lsock = LSock, handlers = []}, 0}.

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  {ok, NewState} = handle_data(Socket, RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#state.handlers of
    [] -> 
      {stop, normal, NewState};
    _Other -> {noreply, NewState}
  end;
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(timeout, #state{lsock = LSock, handlers = Handlers} = State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  ti_sup:start_child(),
  NewHandlers = [erlymud:connect(Socket) | Handlers],
  {noreply, State#state{handlers = NewHandlers}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle_data(Socket, RawData, State) ->
  Data = re:replace(RawData, "\r\n$", "", [{return, list}]),
  [Handler | Rest] = State#state.handlers,
  {M, F, HandlerState} = Handler,
  case M:F(Socket, Data, HandlerState) of
    done -> 
      {ok, State#state{handlers = Rest}};
    {ok, NewState} -> 
      {ok, State#state{handlers = [{M, F, NewState} | Rest]}};
    {next, NextHandler} -> 
      {ok, State#state{handlers = [NextHandler | Rest]}};
    {push, NewHandler, NewState} -> 
      {ok, State#state{handlers = [NewHandler, {M, F, NewState} | Rest]}};
    Other -> 
      {error, Other}
  end.

