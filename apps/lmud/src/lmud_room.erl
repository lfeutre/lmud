%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc In-game room representation.
%%% This gen_server holds room state, and handles moving things in/out of
%%% the room etc.
%%% @end
%%% =========================================================================
-module(lmud_room).

-behaviour(gen_server).
-behaviour('lmud-event-source').

-export([start_link/3,
         add_exit/3, add_object/2, add_reset/2,
         get_exit/2, get_exits/1, get_name/1, get_objects/1, get_people/1,
         set_title/2, set_brief/2, set_desc/2,
         remove_object/2,
         describe/1, describe_except/2, looking/2,
         enter/2, leave/2,
         print_except/4, print_except/5, print_while/4,
         save/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_event_listener/2, notify/2]).

-include_lib("logjam/include/logjam.hrl").
-include("apps/lmud/include/state.hrl").

-type room_name() :: string().
-type room_pid() :: pid().
-type direction() :: string().
-type destination() :: string().
-type room_exit() :: {direction(), destination()}.
-type exits() :: [room_exit()].

-export_type([room_name/0, room_pid/0, direction/0, destination/0, room_exit/0, exits/0]).

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

print_except(Color, Room, User, Format, Args) ->
  print_except(Room, User, 'lmud-util':'format-color'(Color, Format), Args).

print_while(Room, Pred, Format, Args) ->
  gen_server:call(Room, {print_while, Pred, Format, Args}).

set_title(Room, Title) ->
  gen_server:call(Room, {set_title, Title}).

set_brief(Room, Brief) ->
  gen_server:call(Room, {set_brief, Brief}).

set_desc(Room, Desc) ->
  gen_server:call(Room, {set_desc, Desc}).

save(Room) ->
  gen_server:call(Room, save).

% --

init([Name, Title, Desc]) ->
  process_flag(trap_exit, true),
  {ok, #state_room{name=Name, title=Title, desc=Desc}}.

handle_call({add_exit, Dir, Dest}, _From, #state_room{exits=Exits} = State) ->
  {reply, ok, State#state_room{exits=[{Dir, Dest}|Exits]}};
handle_call({add_object, Ob}, _From, #state_room{objects=Objects} = State) ->
  {reply, ok, State#state_room{objects=[Ob|Objects]}};
handle_call({add_reset, Reset}, _From, #state_room{resets=Resets} = State) ->
  {reply, ok, State#state_room{resets=[Reset|Resets]}};
handle_call({get_exit, Dir}, _From, #state_room{exits=Exits} = State) ->
  Response = case lists:keyfind(Dir, 1, Exits) of
               false -> {error, not_found};
               Exit -> {ok, Exit}
             end,
  {reply, Response, State};
handle_call(get_exits, _From, #state_room{exits=Exits} = State) ->
  {reply, Exits, State};
handle_call(get_name, _From, #state_room{name=Name} = State) ->
  {reply, Name, State};
handle_call(get_objects, _From, #state_room{objects=Objects} = State) ->
  {reply, Objects, State};
handle_call(get_people, _From, #state_room{people=People} = State) ->
  {reply, People, State};
handle_call({remove_object, Ob}, _From, #state_room{objects=Objects} = State) ->
  {reply, ok, State#state_room{objects=lists:delete(Ob, Objects)}};
handle_call(describe, _From, State) ->
  {reply, do_describe(State), State};
handle_call({describe_except, User}, _From, State) ->
  {reply, do_describe_except(User, State), State};
handle_call({looking, User}, _From, State) ->
  {reply, do_looking(User, State), State};
handle_call({enter, Who}, _From, #state_room{people=People} = State) ->
  link(Who),
  do_notify({enter_room, Who}, State),
  {reply, ok, State#state_room{people=[Who|People]}};
handle_call({leave, Who}, _From, #state_room{people=People} = State) ->
  unlink(Who),
  {reply, ok, State#state_room{people=People -- [Who]}};
handle_call({print_while, Pred, Format, Args}, _From, State) ->
  People = lists:filter(Pred, State#state_room.people),
  PrintFun = fun(Liv) -> lmud_character:print(Liv, Format, Args) end,
  lists:map(PrintFun, People),
  {reply, ok, State};
handle_call({set_title, Title}, _From, State) ->
  {reply, ok, State#state_room{title=Title}};
handle_call({set_brief, Brief}, _From, State) ->
  {reply, ok, State#state_room{brief=Brief}};
handle_call({set_desc, Desc}, _From, State) ->
  {reply, ok, State#state_room{desc=Desc}};
handle_call(save, _From, State) ->
  {ok, NewState} = do_save(State),
  {reply, ok, NewState}.

handle_cast({add_event_listener, Listener}, State) ->
  EventListeners = State#state_room.event_listeners,
  {noreply, State#state_room{event_listeners=[Listener|EventListeners]}};
handle_cast({notify, Event}, State) ->
  NewListeners = do_notify(Event, State),
  {noreply, State#state_room{event_listeners=NewListeners}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', From, Reason}, #state_room{people=People}=State) ->
  case lists:member(From, People) of
    true ->
      {noreply, State#state_room{people=lists:delete(From, People)}};
    false ->
      {stop, Reason, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% lmud-event-source

add_event_listener(Room, Listener) ->
  gen_server:cast(Room, {add_event_listener, Listener}).

notify(Room, Event) ->
  gen_server:cast(Room, {notify, Event}).

%% Internal functions

do_notify(Event, #state_room{event_listeners=Listeners}) ->
  do_notify(Event, Listeners, []).

do_notify(_Event, [], Remaining) ->
  Remaining;
do_notify(Event, [{Mod, Args}|Rest], Remaining) ->
  Mod:handle_event(Args ++ [Event]),
  do_notify(Event, Rest, [{Mod, Args}|Remaining]).

do_describe(#state_room{title=Title, desc=Desc, people=People, exits=Exits, objects=Objects}) ->
  ["\n", color:greenb(Title), "\n\n",
   msh_text:wrapline(Desc, 'lmud-config':'wrap-width'()), "\n\n",
   color:blackb("[Exits: " ++ list_exits(Exits) ++ "]"), "\n\n",
   list_objects(Objects),
   list_people(People)].

do_describe_except(User, #state_room{people=People}=State) ->
  do_describe(State#state_room{people = People -- [User]}).

do_looking(User, #state_room{desc=undefined}=State) ->
  do_describe_except(User, State);
do_looking(User, #state_room{desc=Desc}=State) ->
  do_describe_except(User, State#state_room{desc=Desc}).

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
  Desc = case lmud_object:is_attached(Ob) of
           true -> ObDesc;
           false -> ObDesc ++ lmud_object:show_in_room(Ob)
         end,
  list_objects(Obs, Desc).

list_people([]) ->
  "";
list_people(People) ->
  Names = lists:map(fun lmud_character:name/1, People),
  ["\n", [[N, " is here.\n"] || N <- Names]].

%% Save

do_save(#state_room{name=Name}=State) ->
  Data = mudstore:serialise(State),
  ?'log-info'("saving room: ~s~n", [Name]),
  ?'log-debug'("room data: ~n~p~n", Data),
  case mudstore:dump("rooms", Name, Data) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, file:format_error(Reason)}
  end.
