%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc In-game request handler for user sessions.
%%% When a request is spawned with an MFA during in-game play, it points
%%% here, to the parse/2 function.
%%% @end
%%% =========================================================================
-module(em_rh_game).

-export([parse/2]).

%% game commands
-export([cmd_look/2, cmd_north/2, cmd_east/2, cmd_south/2, cmd_west/2,
         cmd_go/2, cmd_quit/2, cmd_emote/2, cmd_say/2, cmd_tell/2,
         cmd_who/2, cmd_get/2, cmd_drop/2, cmd_inv/2, cmd_glance/2,
         cmd_save/2, cmd_setlong/2, cmd_help/2]).

-include("request.hrl").


%% API functions

parse(Line, Req) ->
  case string:tokens(Line, " ") of
    [] ->
      print("\n> ", Req),
      ?req_next(parse);
    [Cmd|Args] ->
      Result = parse_cmd(Cmd, Args, Line, Req),
      print("\n> ", Req),
      Result
  end.

parse_cmd(Cmd, Args, Line, Req) ->
  try list_to_existing_atom("cmd_" ++ string:to_lower(Cmd)) of
    Fun ->
      case apply(?MODULE, Fun, [Args, Req]) of
        {ok, Req} ->
          ?req_next(parse);
        {stop, Req} ->
          ?req_done;
        {error, Reason} ->
          print(Reason, Req),
          ?req_next(parse);
        Other ->
          print("Error occurred while processing '~s':~n~p~n", 
            [Line, Other], Req),
          ?req_next(parse)
      end
  catch
    error:badarg ->
      print("I don't understand what you mean by '~s'~n", [Line], Req),
      ?req_next(parse)
  end.


%% Game commands

%% Inventory
cmd_inv(_Args, #req{living=Liv}=Req) ->
  Obs = em_living:get_objects(Liv),
  do_inv(Obs, Req),
  {ok, Req}.

do_inv([], Req) ->
  print("You're not carrying anything.\n", Req);
do_inv(Obs, Req) ->
  print("You're carrying:\n", Req),
  print(desc_inv(Obs, []), Req).

desc_inv([], Result) -> Result;
desc_inv([Ob|Obs], Result) ->
  Line = [" ", em_object:a_short(Ob), "\n"],
  desc_inv(Obs, [Result, Line]).

%% Drop
cmd_drop([], Req) ->
  print("Drop what?\n", Req),
  {ok, Req};
cmd_drop([Id|_Args], #req{living=Liv}=Req) ->
  Obs = em_living:get_objects(Liv),
  try_drop(Id, Obs, Req),
  {ok, Req}.

try_drop(_Id, [], Req) ->
  print("You don't have anything like that.\n", Req);
try_drop(Id, [Ob|Obs], Req) ->
  case em_object:has_id(Ob, Id) of
    true ->
      do_drop(Ob, Req);
    false ->
      try_drop(Id, Obs, Req)
  end.

do_drop(Ob, #req{living=Liv}=Req) ->
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  AShort = em_object:a_short(Ob),
  TheShort = em_object:the_short(Ob),
  try 
    ok = em_living:move_object(Liv, Ob, {to_room, Room}),
    print("You drop ~s.\n", [TheShort], Req),
    em_room:print_except(Room, Liv, "~s drops ~s.~n", [Name, AShort])
  catch
    throw:{em_living, not_found} ->
      print("You don't have anything like that.\n", Req)
  end.

%% Get
cmd_get([], Req) ->
  print("Get what?\n", Req),
  {ok, Req};
cmd_get([Id|_Args], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  Obs = lists:filter(fun(Ob) -> not em_object:is_attached(Ob) end,
                     em_room:get_objects(Room)),
  do_get(Id, Obs, Req),
  {ok, Req}.

do_get(_Id, [], Req) ->
  print("There's no such thing here.\n", Req);
do_get(Id, [Ob|Obs], #req{living=Liv}=Req) ->
  case em_object:has_id(Ob, Id) of
    true ->
      Name = em_living:get_name(Liv),
      Room = em_living:get_room(Liv),
      TheShort = em_object:the_short(Ob),
      print("You take ~s.\n", [TheShort], Req),
      em_room:remove_object(Room, Ob),
      em_room:print_except(Room, Liv, "~s takes ~s.~n", [Name, TheShort]),
      em_living:add_object(Liv, Ob);
    false ->
      do_get(Id, Obs, Req)
  end.

%% Quit
cmd_quit(_Args, #req{user=User, living=Liv}=Req) ->
  print("Goodbye!\n", Req),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s leaves.~n", [Name]),
  ok = em_game:logout(User),
  {stop, Req}.

%% Glance
cmd_glance(_Args, #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  print(em_room:describe_except(Room, Liv), Req),
  {ok, Req}.

%% Look
cmd_look([], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  print(em_room:looking(Room, Liv), Req),
  {ok, Req};
cmd_look([Id|_Args], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  Obs = em_room:get_objects(Room),
  case do_look_ob(string:to_lower(Id), Obs, Req) of
    ok -> {ok, Req};
    {error, not_found} -> 
      People = lists:delete(Liv, em_room:get_people(Room)),
      case do_look_liv(string:to_lower(Id), People, Req) of
        ok -> {ok, Req};
        {error, not_found} ->
          print("There's no such thing here.\n", Req),
          {ok, Req}
      end
  end.

do_look_ob(_Id, [], _Req) ->
  {error, not_found};
do_look_ob(Id, [Ob|Obs], Req) ->
  case em_object:has_id(Ob, Id) of
    true ->
      Long = em_object:long(Ob),
      print("~s\n", [em_text:wrapline(Long, 78)], Req),
      ok;
    false ->
      do_look_ob(Id, Obs, Req)
  end.

do_look_liv(_Id, [], _Req) ->
  {error, not_found};
do_look_liv(Id, [Liv|People], Req) ->
  case string:to_lower(em_living:get_name(Liv)) of
    Id ->
      Long = em_living:long(Liv),
      print("~s\n", [em_text:wrapline(Long, 78)], Req),
      ok;
    _Other ->
      do_look_liv(Id, People, Req)
  end.

% Go / North / East / South / West 
cmd_north(_Args, Req) ->
  cmd_go(["north"], Req).
cmd_east(_Args, Req) ->
  cmd_go(["east"], Req).
cmd_south(_Args, Req) ->
  cmd_go(["south"], Req).
cmd_west(_Args, Req) ->
  cmd_go(["west"], Req).

cmd_go([Dir|_Args], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  do_go(em_room:get_exit(Room, Dir), Req), 
  {ok, Req}.

do_go({error, not_found}, Req) ->
  print("You can't go in that direction.\n", Req);
do_go({ok, {Dir, Dest}}, #req{living=Liv}=Req) ->
  {ok, DestRoom} = em_room_mgr:get_room(Dest),
  print("You leave " ++ Dir ++ ".\n\n", Req),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s leaves ~s.~n", [Name, Dir]),
  em_room:leave(Room, Liv),
  em_living:set_room(Liv, DestRoom),
  em_room:enter(DestRoom, Liv),
  em_room:print_except(DestRoom, Liv, "~s arrives.~n", [Name]),
  cmd_glance([], Req).

%% Emote
cmd_emote(Args, #req{living=Liv}=Req) ->
  Text = em_grammar:punctuate(string:join(Args, " ")),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s ~s~n", [Name, Text]),
  print("~s ~s~n", [Name, Text], Req),
  {ok, Req}.

%% Say
cmd_say([FirstWord|Rest], #req{living=Liv}=Req) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s says, \"~s\"~n", [Name, Text]),
  print("You say, \"~s\"~n", [Text], Req),
  {ok, Req}.

%% Tell
cmd_tell([Who,FirstWord|Rest], #req{user=User}=Req) ->
  Name = em_user:get_name(User),
  case em_game:lookup_user(Who) of
    {error, not_found} ->
      print("There's no such user.\n", Req);
    {ok, {Name, _}} ->
      print("Talking to yourself, huh?\n", Req);
    {ok, {OtherName, OtherUser}} ->
      Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
      em_user:print(OtherUser, "~s tells you, \"~s\"~n", [Name, Text]),
      print("You tell ~s, \"~s\"~n", [OtherName, Text], Req)
  end,
  {ok, Req}.

%% Who
cmd_who(_Args, Req) ->
  print(["Users:\n",
    [[" ", Name, "\n"] || {Name, _Pid} <- em_game:get_users()]], Req),
  {ok, Req}.

%% Save
cmd_save(_Args, #req{living=Liv}=Req) ->
  print("Saving..\n", Req),
  case em_living:save(Liv) of
    ok ->
      {ok, Req};
    {error, Reason} ->
      print("Error: ~s\n", [Reason], Req),
      {ok, Req}
  end.

%% Setlong
cmd_setlong(Args, #req{living=Liv}=Req) ->
  em_living:set_long(Liv, string:join(Args, " ")),
  {ok, Req}.

%% Help
cmd_help(_Args, Req) ->
  print(
  "Welcome to ErlyMud!\n\n"
  "The following commands, more or less, are available right now:\n"
  "  look                    View the long description of the room.\n"
  "  look [person|item]      View description of person/item.\n"
  "  glance                  View the brief description of the room.\n"
  "  go <dir>                Leave in the specified direction.\n"
  "  north                     - shortcut for 'go north'\n"
  "  east                      - shortcut for 'go east'\n"
  "  south                     - shortcut for 'go south'\n"
  "  west                      - shortcut for 'go west'\n"
  "  say <what>              Say something, all in room will see it:\n"
  "                            Jack says, \"Hello there!\"\n"
  "  emote <what>            Emote, for roleplaying:\n"
  "                            'emote grumpily kicks at a small rock'\n"
  "                            -> Jack grumpily kicks at a small rock.\n"
  "  tell <person> <what>    Send a private message to another user.\n"
  "  get <item>              Pick up an item in the room.\n"
  "  drop <item>             Drop an item from your inventory.\n"
  "  inv                     Show your inventory.\n"
  "  save                    Save your character, will remember your\n"
  "                            location and inventory for next login.\n"
  "  setlong <desc>          Set the description others see when they\n"
  "                            look at you.\n"
  "  who                     Display all logged in users.\n",
  Req),
  {ok, Req}.

%% Utility functions

print(Format, Req) ->
  print(Format, [], Req).
print(Format, Args, #req{conn=Conn}) ->
  em_conn:print(Conn, Format, Args).
    
