-module(em_game).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, 
         add_rooms/1, get_rooms/0, 
         get_users/0, lookup_user/1, lookup_user_pid/1,
         login/2, logout/1,
         print_while/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {users=[], rooms=[]}).

-define(SERVER, ?MODULE).


%% API functions

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

add_rooms(Rooms) ->
  gen_server:call(?SERVER, {add_rooms, Rooms}).

get_rooms() ->
  gen_server:call(?SERVER, get_rooms).

get_users() ->
  gen_server:call(?SERVER, get_users).

login(Name, Client) ->
  gen_server:call(?SERVER, {login, Name, Client}).

logout(User) ->
  gen_server:call(?SERVER, {logout, User}).

lookup_user(Name) ->
  gen_server:call(?SERVER, {lookup_user, Name}).

lookup_user_pid(Pid) ->
  gen_server:call(?SERVER, {lookup_user_pid, Pid}).

print_while(Pred, Format, Args) ->
  gen_server:call(?SERVER, {print_while, Pred, Format, Args}).

%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call({add_rooms, NewRooms}, _From, #state{rooms=Rooms}=State) ->
  {reply, ok, State#state{rooms = Rooms ++ NewRooms}};
handle_call(get_rooms, _From, #state{rooms=Rooms}=State) ->
  {reply, Rooms, State};
handle_call(get_users, _From, #state{users=Users}=State) ->
  {reply, Users, State};
handle_call({login, Name, Client}, _From, State) ->
  {Result, NewState} = do_login(Name, Client, State),
  {reply, Result, NewState};
handle_call({logout, User}, _From, State) ->
  {Result, NewState} = do_logout(User, State),
  {reply, Result, NewState};
handle_call({lookup_user, Name}, _From, #state{users=Users}=State) 
    when is_list(Name) ->
  Result = case lists:keyfind(em_text:capitalize(Name), 1, Users) of
             false -> {error, not_found};
             UserTuple -> {ok, UserTuple}
           end,
  {reply, Result, State};
handle_call({lookup_user_pid, Pid}, _From, #state{users=Users}=State) 
    when is_pid(Pid) ->
  Result = case lists:keyfind(Pid, 2, Users) of
             false -> {error, not_found};
             UserTuple -> {ok, UserTuple}
           end,
  {reply, Result, State};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  Pids = [Pid || {_User, Pid} <- State#state.users],
  ToPids = lists:filter(Pred, Pids),
  PrintFun = fun(Pid) -> em_living:print(Pid, Format, Args) end,
  lists:map(PrintFun, ToPids),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal functions

%% User Handling

do_login(Name, Client, #state{users=Users,rooms=[Room|_Rooms]}=State) ->
  case lists:keyfind(Name, 1, Users) of
    {_Name, _User} ->
      {{error, user_exists}, State};
    false ->
      case em_living_sup:start_child(Name, Room, Client) of
        {ok, User} ->
          em_room:enter(Room, User),
          {{ok, User}, State#state{users=[{Name, User}|Users]}};
        Error ->
          {Error, State}
      end
  end.

do_logout(User, #state{users=Users}=State) ->
  case lists:keyfind(User, 2, Users) of
    {_Name, User} ->
      {ok, State#state{users=lists:keydelete(User, 2, Users)}};
    false ->
      {{error, not_found}, State}
  end.
