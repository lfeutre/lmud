-module(em_room).

-behaviour(gen_server).

-export([start_link/2, start/2, add_exit/3, get_exit/2, get_exits/1, 
         describe/1, describe_except/2, enter/2, leave/2, print_while/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {title="A small room", 
                desc="This is a small, non-descript room.", 
                people=[], exits=[]}).

start_link(Title, Desc) ->
  gen_server:start_link(?MODULE, [Title, Desc], []).

start(Title, Desc) ->
  gen_server:start(?MODULE, [Title, Desc], []).

add_exit(Room, Dir, Dest) ->
  gen_server:call(Room, {add_exit, Dir, Dest}).

get_exit(Room, Dir) ->
  gen_server:call(Room, {get_exit, Dir}).

get_exits(Room) ->
  gen_server:call(Room, get_exits).

describe(Room) ->
  gen_server:call(Room, describe).

describe_except(Room, User) ->
  gen_server:call(Room, {describe_except, User}).

enter(Room, Who) ->
  gen_server:call(Room, {enter, Who}).

leave(Room, Who) ->
  gen_server:call(Room, {leave, Who}).

print_while(Room, Pred, Format, Args) ->
  gen_server:call(Room, {print_while, Pred, Format, Args}).

% --

init([Title, Desc]) ->
  {ok, #state{title=Title, desc=Desc}}.

handle_call({add_exit, Dir, Dest}, _From, #state{exits=Exits} = State) ->
  {reply, ok, State#state{exits=[{Dir, Dest}|Exits]}};
handle_call({get_exit, Dir}, _From, #state{exits=Exits} = State) ->
  Response = case lists:keyfind(Dir, 1, Exits) of
               false -> {error, not_found};
               Exit -> {ok, Exit}
             end,
  {reply, Response, State};
handle_call(get_exits, _From, #state{exits=Exits} = State) ->
  {reply, Exits, State};
handle_call(describe, _From, State) ->
  {reply, do_describe(State), State};
handle_call({describe_except, User}, _From, State) ->
  {reply, do_describe_except(User, State), State};
handle_call({enter, Who}, _From, #state{people=People} = State) ->
  {reply, ok, State#state{people=[Who|People]}};
handle_call({leave, Who}, _From, #state{people=People} = State) ->
  {reply, ok, State#state{people=People -- [Who]}};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  People = lists:filter(Pred, State#state.people),
  PrintFun = fun(Liv) -> em_living:print(Liv, Format, Args) end,
  lists:map(PrintFun, People),
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

do_describe(#state{title=Title, desc=Desc, people=People, exits=Exits}) ->
  ["%^ROOM_TITLE%^", Title, "%^RESET%^\n", Desc, "\n", 
   "%^ROOM_EXITS%^[Exits: ", list_exits(Exits), "]%^RESET%^\n",
    list_people(People)].

do_describe_except(User, #state{people=People}=State) ->
  do_describe(State#state{people = People -- [User]}).

list_exits(Exits) ->
  list_exits([Dir || {Dir, _Dest} <- Exits], []).

list_exits([], ExitList) ->
  ExitList;
list_exits([Exit], ExitList) ->
  ExitList ++ Exit;
list_exits([Exit|Exits], ExitList) ->
  list_exits(Exits, ExitList ++ [Exit,", "]).

list_people([]) ->
  "";
list_people(People) ->
  GetName = fun(Pid) -> {ok, {Name, _Pid}} = em_game:lookup_user_pid(Pid), Name end,
  Names = lists:map(GetName, People),
  ["\n", [[N, " is here.\n"] || N <- Names]].
  
