-module(ti_server).

-behaviour(gen_server).

-export([start_link/2, reset_connection/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, connectfunc, handlers}).

start_link(LSock, ConnectFunc) ->
  gen_server:start_link(?MODULE, [LSock, ConnectFunc], []).

init([LSock, ConnectFunc]) ->
  {ok, #state{lsock = LSock, connectfunc = ConnectFunc, handlers = []}, 0}.

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
handle_info(timeout, #state{lsock = LSock, connectfunc = ConnectFunc, handlers = Handlers} = State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  ti_sup:start_child(ConnectFunc),
  {M, F, A} = ConnectFunc,
  NewHandlers = [apply(M, F, A ++ [Socket]) | Handlers],
  {noreply, State#state{handlers = NewHandlers}}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  Handlers = [{?MODULE, reset_connection, []}],
  {ok, State#state{handlers = Handlers}}.

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

reset_connection(Socket, _Data, _HandlerState) ->
  gen_tcp:send(Socket, "Your connection has been upgraded; sorry for any inconvenience..\n\n"),
  Handler = erlymud:connect(Socket),
  {next, Handler}.

