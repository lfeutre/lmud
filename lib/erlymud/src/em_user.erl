%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc User server.
%%% Represents a single connected user, and hosts user-related state.
%%% @end
%%% =========================================================================
-module(em_user).

-behaviour(gen_server).

-export([start_link/2, get_name/1, print/2, print/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, conn}).


%% API

start_link(Name, Conn) ->
  gen_server:start_link(?MODULE, [Name, Conn], []).

get_name(Pid) ->
  gen_server:call(Pid, get_name).

print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).


%% gen_server callbacks

init([Name, Conn]) ->
  {ok, #state{name=Name, conn=Conn}}.

handle_call(get_name, _From, #state{name=Name}=State) ->
  {reply, Name, State};
handle_call({print, Format}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format, Args),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

