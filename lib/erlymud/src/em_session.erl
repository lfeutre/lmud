%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Session manager.
%%% Will handle user input from the user's connection, passing it to the
%%% request handler on top of its stack by launching a request with the
%%% current handler function specified as an MFA.
%%% @end
%%% =========================================================================
-module(em_session).

-behaviour(gen_server).

%% API
-export([start_link/1, receive_line/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("request.hrl").


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
  {ok, #req{conn=Conn, queue=[init]}, 0}.

handle_call({receive_line, Data}, _From, #req{queue=Queue}=State) ->
  {reply, ok, State#req{queue=Queue ++ [{input, Data}]}, 0}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  process_queue(State).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

process_queue(#req{queue=[]}=State) ->
  {noreply, State};
process_queue(#req{queue=[init|Queue],conn=Conn}=State) ->
  em_rh_login:welcome(Conn),
  Handlers=[{em_rh_login, login, [got_user]}],
  process_queue(State#req{queue=Queue, handlers=Handlers});
process_queue(#req{queue=[{input, RawData}|Queue]}=State) ->
  {ok, NewState} = handle_data(RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#req.handlers of
    [] ->
      {stop, user_logout, NewState};
    _Other ->
      process_queue(NewState#req{queue=Queue})
  end.

strip_linefeed(RawData) ->
  re:replace(RawData, "\r\n$", "", [{return, list}]).

handle_data(RawData, State) ->
  Data = strip_linefeed(RawData),
  [Handler | Rest] = State#req.handlers,
  {M, F, A} = Handler,
  case em_req_sup:request({M, F, A ++ [Data, State]}) of
    {done, NewState} -> 
      {ok, NewState#req{handlers = Rest}};
    {link, NewMFA, NewState} -> 
      link(NewState#req.user),
      link(NewState#req.living),
      {ok, NewState#req{handlers = [NewMFA | Rest]}};
    {ok, NewMFA, NewState} -> 
      {ok, NewState#req{handlers = [NewMFA | Rest]}};
    {push, NewMFA, NewState} -> 
      {ok, NewState#req{handlers = [NewMFA, {M, F, A} | Rest]}};
    Other -> 
      {error, Other}
  end.

