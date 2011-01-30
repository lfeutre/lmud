%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc In-game room representation.
%%% This gen_server holds room state, and handles moving things in/out of
%%% the room etc.
%%% @end
%%% =========================================================================
-module(em_room).

-behaviour(gen_server).
-behaviour(em_event_source).

-export([start_link/3, 
         add_exit/3, add_object/2, add_reset/2,
         get_exit/2, get_exits/1, get_name/1, get_objects/1, get_people/1,
         set_title/2, set_brief/2, set_long/2,
         remove_object/2,
         describe/1, describe_except/2, looking/2,
         enter/2, leave/2, 
         print_except/4, print_while/4,
         save/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([add_event_listener/2, notify/2]).

-record(state, {name, title, desc, long, people=[], exits=[], objects=[], 
                resets=[], event_listeners=[]}).

-type room_name() :: string().
-type room_pid() :: pid().
-export_type([room_name/0, room_pid/0]).

start_link(Name, Title, Desc) ->
  gen_server:start_link(?MODULE, [Name, Title, Desc], []).

add_exit(Room, Dir, Dest) ->
  gen_server:call(Room, {add_exit, Dir, Dest}).

add_object(Room, Ob) ->
  gen_server:call(Room, {add_object, Ob}).

add_reset(Room, Reset) ->
  gen_server:call(Room, {add_reset, Reset}).

get_exit(Room, Dir) ->
  gen_server:call(Room, {get_exit, Dir}).

get_exits(Room) ->
  gen_server:call(Room, get_exits).

get_name(Room) ->
  gen_server:call(Room, get_name).

get_objects(Room) ->
  gen_server:call(Room, get_objects).

get_people(Room) ->
  gen_server:call(Room, get_people).

remove_object(Room, Ob) ->
  gen_server:call(Room, {remove_object, Ob}).

describe(Room) ->
  gen_server:call(Room, describe).

describe_except(Room, User) ->
  gen_server:call(Room, {describe_except, User}).

looking(Room, User) ->
  gen_server:call(Room, {looking, User}).

enter(Room, Who) ->
  gen_server:call(Room, {enter, Who}).

leave(Room, Who) ->
  gen_server:call(Room, {leave, Who}).

print_except(Room, User, Format, Args) ->
  Pred = fun(L) -> User =/= L end,
  print_while(Room, Pred, Format, Args).

print_while(Room, Pred, Format, Args) ->
  gen_server:call(Room, {print_while, Pred, Format, Args}).

set_title(Room, Title) ->
  gen_server:call(Room, {set_title, Title}).

set_brief(Room, Brief) ->
  gen_server:call(Room, {set_brief, Brief}).

set_long(Room, Long) ->
  gen_server:call(Room, {set_long, Long}).

save(Room) ->
  gen_server:call(Room, save).

% --

init([Name, Title, Desc]) ->
  process_flag(trap_exit, true),
  {ok, #state{name=Name, title=Title, desc=Desc}}.

handle_call({add_exit, Dir, Dest}, _From, #state{exits=Exits} = State) ->
  {reply, ok, State#state{exits=[{Dir, Dest}|Exits]}};
handle_call({add_object, Ob}, _From, #state{objects=Objects} = State) ->
  {reply, ok, State#state{objects=[Ob|Objects]}};
handle_call({add_reset, Reset}, _From, #state{resets=Resets} = State) ->
  {reply, ok, State#state{resets=[Reset|Resets]}};
handle_call({get_exit, Dir}, _From, #state{exits=Exits} = State) ->
  Response = case lists:keyfind(Dir, 1, Exits) of
               false -> {error, not_found};
               Exit -> {ok, Exit}
             end,
  {reply, Response, State};
handle_call(get_exits, _From, #state{exits=Exits} = State) ->
  {reply, Exits, State};
handle_call(get_name, _From, #state{name=Name} = State) ->
  {reply, Name, State};
handle_call(get_objects, _From, #state{objects=Objects} = State) ->
  {reply, Objects, State};
handle_call(get_people, _From, #state{people=People} = State) ->
  {reply, People, State};
handle_call({remove_object, Ob}, _From, #state{objects=Objects} = State) ->
  {reply, ok, State#state{objects=lists:delete(Ob, Objects)}};
handle_call(describe, _From, State) ->
  {reply, do_describe(State), State};
handle_call({describe_except, User}, _From, State) ->
  {reply, do_describe_except(User, State), State};
handle_call({looking, User}, _From, State) ->
  {reply, do_looking(User, State), State};
handle_call({enter, Who}, _From, #state{people=People} = State) ->
  link(Who),
  do_notify({enter_room, Who}, State),
  {reply, ok, State#state{people=[Who|People]}};
handle_call({leave, Who}, _From, #state{people=People} = State) ->
  unlink(Who),
  {reply, ok, State#state{people=People -- [Who]}};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  People = lists:filter(Pred, State#state.people),
  PrintFun = fun(Liv) -> em_living:print(Liv, Format, Args) end,
  lists:map(PrintFun, People),
  {reply, ok, State};
handle_call({set_title, Title}, _From, State) ->
  {reply, ok, State#state{title=Title}};
handle_call({set_brief, Brief}, _From, State) ->
  {reply, ok, State#state{desc=Brief}};
handle_call({set_long, Long}, _From, State) ->
  {reply, ok, State#state{long=Long}};
handle_call(save, _From, State) ->
  {ok, NewState} = do_save(State),
  {reply, ok, NewState}.

handle_cast({add_event_listener, Listener}, State) ->
  EventListeners = State#state.event_listeners,
  {noreply, State#state{event_listeners=[Listener|EventListeners]}};
handle_cast({notify, Event}, State) ->
  NewListeners = do_notify(Event, State),
  {noreply, State#state{event_listeners=NewListeners}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', From, Reason}, #state{people=People}=State) ->
  case lists:member(From, People) of
    true ->
      {noreply, State#state{people=lists:delete(From, People)}};
    false ->
      {stop, Reason, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% em_event_source

add_event_listener(Room, Listener) ->
  gen_server:cast(Room, {add_event_listener, Listener}).

notify(Room, Event) ->
  gen_server:cast(Room, {notify, Event}).

%% Internal functions

do_notify(Event, #state{event_listeners=Listeners}) ->
  do_notify(Event, Listeners, []).

do_notify(_Event, [], Remaining) ->
  Remaining;
do_notify(Event, [{Mod, Args}|Rest], Remaining) ->
  Mod:handle_event(Args ++ [Event]),
  do_notify(Event, Rest, [{Mod, Args}|Remaining]).

do_describe(#state{title=Title, desc=Desc, people=People, exits=Exits, objects=Objects}) ->
  ["%^ROOM_TITLE%^", Title, "%^RESET%^\n", em_text:wrapline(Desc, 78), "\n", 
   "%^ROOM_EXITS%^[Exits: ", list_exits(Exits), "]%^RESET%^\n",
    list_objects(Objects),
    list_people(People)].

do_describe_except(User, #state{people=People}=State) ->
  do_describe(State#state{people = People -- [User]}).

do_looking(User, #state{long=undefined}=State) ->
  do_describe_except(User, State);
do_looking(User, #state{long=Long}=State) ->
  do_describe_except(User, State#state{desc=Long}).

list_exits(Exits) ->
  list_exits([Dir || {Dir, _Dest} <- Exits], []).

list_exits([], ExitList) ->
  ExitList;
list_exits([Exit], ExitList) ->
  ExitList ++ Exit;
list_exits([Exit|Exits], ExitList) ->
  list_exits(Exits, ExitList ++ [Exit,", "]).

list_objects(Objects) ->
  list_objects(Objects, []).

list_objects([], ObDesc) ->
  ObDesc;
list_objects([Ob|Obs], ObDesc) ->
  Desc = case em_object:is_attached(Ob) of
           true -> ObDesc;
           false -> ObDesc ++ em_object:show_in_room(Ob)
         end,
  list_objects(Obs, Desc).

list_people([]) ->
  "";
list_people(People) ->
  Names = lists:map(fun em_living:get_name/1, People),
  ["\n", [[N, " is here.\n"] || N <- Names]].

%% Save

make_filename(Name) ->
  filename:join([em_game:data_dir(), "rooms", Name ++ ".dat"]).

do_save(State) ->
  Data = save_room(State),
  File = make_filename(State#state.name),
  io:format("saving room: ~s~n~p~n", [File, Data]),
  case file:write_file(File, Data) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, file:format_error(Reason)}
  end.

save_room(State) ->
  lists:flatten([
    "{version, 1}.\n",
    "{title, \"", State#state.title, "\"}.\n",
    "{desc, \"", State#state.desc, "\"}.\n",
    save_long(State),
    "{exits, ", save_exits(State), "}.\n",
    save_resets(State)
  ]).

save_long(#state{long=undefined}) ->
  "";
save_long(#state{long=Long}) ->
  ["{long, \"", Long, "\"}.\n"].

save_exits(State) ->
  ["[", save_exits(State#state.exits, []), "]"].

save_exits([], Result) ->
  string:join(Result, ",");
save_exits([{Dir, Dest}|Exits], Result) ->
  save_exits(Exits, Result ++ [["{\"",Dir,"\",\"",Dest,"\"}"]]).

save_resets(State) ->
  Resets = State#state.resets,
  ["{objects, ", save_stringlist(Resets), "}.\n"].

save_stringlist(List) ->
  ["[", string:join([["\"",Str,"\""]||Str <- List], ", "), "]"].

