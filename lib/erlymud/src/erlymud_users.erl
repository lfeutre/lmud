-module(erlymud_users).

-behaviour(gen_server).

-export([start_link/0, add/2, get/0, get/1, remove/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {users}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(User, Pid) ->
  gen_server:call(?SERVER, {add, User, Pid}).

get() ->
  gen_server:call(?SERVER, get).

get(Who) ->
  gen_server:call(?SERVER, {get, Who}).

remove(User) ->
  gen_server:cast(?SERVER, {remove, User}).

%% --------------------------------------------------------------------------

init([]) ->
  {ok, #state{users = dict:new()}}.

handle_call({add, User, Pid}, _From, State) ->
  case dict:is_key(User, State#state.users) of
    true ->
      {reply, {error, user_exists}, State};
    false ->
      Users = dict:store(User, Pid, State#state.users),
      {reply, ok, State#state{users = Users}}
  end;
handle_call(get, _From, State) ->
  UserList = dict:to_list(State#state.users),
  {reply, {ok, UserList}, State};
handle_call({get, Who}, _From, State) ->
  Reply = case dict:is_key(Who, State#state.users) of
            true ->
              {ok, dict:fetch(Who, State#state.users)};
            false ->
              {error, not_found}
          end,
  {reply, Reply, State}.

handle_cast({remove, User}, State) ->
  Users = dict:erase(User, State#state.users),
  {noreply, State#state{users = Users}}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
