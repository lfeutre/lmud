-module(em_req_handler).

-behaviour(gen_server).

%% API
-export([start_link/1, receive_line/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn, user, living, queue=[], handlers=[]}).


%% API

start_link(Conn) ->
  gen_server:start_link(?MODULE, [Conn], []).

%% @doc Push received data onto our queue and return with a timeout; 
%%      this will allow the calling process to proceed while we do our work, 
%%      while still maintaining order of input (which isn't guaranteed if 
%%      using cast).
%% @end
receive_line(Pid, Data) ->
  gen_server:call(Pid, {receive_line, Data}).

%% gen_server callbacks

init([Conn]) ->
  {ok, #state{conn=Conn, queue=[init]}, 0}.

handle_call({receive_line, Data}, _From, #state{queue=Queue}=State) ->
  {reply, ok, State#state{queue=Queue ++ [{input, Data}]}, 0}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  process_queue(State).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

process_queue(#state{queue=[]}=State) ->
  {noreply, State};
process_queue(#state{queue=[init|Queue],conn=Conn}=State) ->
  em_login:welcome(Conn),
  Handlers=[{em_login, login, [got_user]}],
  process_queue(State#state{queue=Queue, handlers=Handlers});
process_queue(#state{queue=[{input, RawData}|Queue]}=State) ->
  {ok, NewState} = handle_data(RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#state.handlers of
    [] ->
      {stop, normal, NewState};
    _Other ->
      process_queue(NewState#state{queue=Queue})
  end.

strip_linefeed(RawData) ->
  re:replace(RawData, "\r\n$", "", [{return, list}]).

handle_data(RawData, State) ->
  Data = strip_linefeed(RawData),
  [Handler | Rest] = State#state.handlers,
  {M, F, A} = Handler,
  case em_req_sup:request({M, F, A ++ [Data, State]}) of
    {done, NewState} -> 
      {ok, NewState#state{handlers = Rest}};
    {link, NewMFA, NewState} -> 
      link(NewState#state.user),
      link(NewState#state.living),
      {ok, NewState#state{handlers = [NewMFA | Rest]}};
    {ok, NewMFA, NewState} -> 
      {ok, NewState#state{handlers = [NewMFA | Rest]}};
    {push, NewMFA, NewState} -> 
      {ok, NewState#state{handlers = [NewMFA, {M, F, A} | Rest]}};
    Other -> 
      {error, Other}
  end.

