%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc User server.
%%% Represents a single connected user, and hosts user-related state.
%%% @end
%%% =========================================================================
-module(em_user).

-behaviour(gen_server).

-export([start_link/2, 
         has_privilege/2,
         get_name/1, 
         print/2, print/3,
         load/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, conn, privileges=ordsets:new()}).

-type user_name() :: string().
-type user_pid() :: pid().
-export_type([user_name/0, user_pid/0]).

%% API

start_link(Name, Conn) ->
  gen_server:start_link(?MODULE, [Name, Conn], []).

has_privilege(Pid, Priv) ->
  gen_server:call(Pid, {has_privilege, Priv}).

get_name(Pid) ->
  gen_server:call(Pid, get_name).

print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).

load(Pid) ->
  gen_server:call(Pid, load).


%% gen_server callbacks

init([Name, Conn]) ->
  {ok, #state{name=Name, conn=Conn}}.

handle_call({has_privilege, Priv}, _From, #state{privileges=Privs}=State) ->
  Result = ordsets:is_element(Priv, Privs),
  {reply, Result, State};
handle_call(get_name, _From, #state{name=Name}=State) ->
  {reply, Name, State};
handle_call({print, Format}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format, Args),
  {reply, ok, State};
handle_call(load, _From, State) ->
  case do_load(State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Load

do_load(#state{name=Name}=State) ->
  File = make_filename(Name),
  load_user(File, State).

make_filename(Name) ->
  filename:join([em_game:data_dir(), "users", Name ++ ".dat"]).

load_user(Filename, State) ->
  io:format("loading user: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      NewState = update_user(Data, State),
      {ok, NewState};
    {error, _Reason} ->
      {error, not_found}
  end.

update_user([], State) ->
  State;
update_user([{privileges, PrivList}|Data], State) ->
  update_user(Data, State#state{privileges=ordsets:from_list(PrivList)});
update_user([_Other|Data], State) ->
  update_user(Data, State).

