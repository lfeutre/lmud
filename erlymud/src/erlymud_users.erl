-module(erlymud_users).

-behaviour(gen_server).

-export([start_link/0, add/2, get/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {users}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(User, Pid) ->
  gen_server:cast(?SERVER, {add, User, Pid}).

get() ->
  gen_server:call(?SERVER, get).

%% --------------------------------------------------------------------------

init([]) ->
  {ok, #state{users = dict:new()}}.

handle_call(get, _From, State) ->
  Response = dict:to_list(State#state.users),
  {reply, {ok, Response}, State}.

handle_cast({add, User, Pid}, State) ->
  Users = dict:store(User, Pid, State#state.users),
  {noreply, State#state{users = Users}}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
