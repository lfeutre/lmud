%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc In-game request handler for user sessions.
%%% When a request is spawned with an MFA during in-game play, it points
%%% here, to the parse/2 function.
%%% @end
%%% =========================================================================
-module(em_rh_game).

%% API
-export([parse/2]).

%% game commands
-export([cmd_look/2, cmd_north/2, cmd_east/2, cmd_south/2, cmd_west/2,
         cmd_go/2, cmd_quit/2, cmd_emote/2, cmd_say/2, cmd_tell/2,
         cmd_who/2, cmd_get/2, cmd_drop/2, cmd_inv/2, cmd_glance/2,
         cmd_save/2, cmd_setlong/2, cmd_help/2,
         cmd_redit/2, cmd_addexit/2,
         cmd_cast/2]).

-include("request.hrl").

%% Type Specifications
-include("types.hrl").
-type ob_list() :: [em_object:object()].
-type liv_list() :: [em_living:living_pid()].

-type cmd_ok()     :: {ok, req()}.
-type cmd_stop()   :: {stop, req()}.

%% ==========================================================================
%% API functions
%% ==========================================================================

-spec parse(string(), req()) -> req_any().
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

-spec parse_cmd(string(), [string()], string(), req()) -> req_any().
parse_cmd(Cmd, Args, Line, Req) ->
  try list_to_existing_atom("cmd_" ++ string:to_lower(Cmd)) of
    Fun ->
      try
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
        throw:not_allowed ->
          print("You're not allowed to use the '~s' command.~n", [Line], Req),
          ?req_next(parse)
      end
  catch
    error:badarg ->
      print("I don't understand what you mean by '~s'~n", [Line], Req),
      ?req_next(parse)
  end.


%% ==========================================================================
%% Game commands
%% ==========================================================================

%% Inventory
-spec cmd_inv([string()], req()) -> cmd_ok().
cmd_inv(_Args, #req{living=Liv}=Req) ->
  Obs = em_living:get_objects(Liv),
  do_inv(Obs, Req),
  {ok, Req}.

-spec do_inv(ob_list(), req()) -> ok.
do_inv([], Req) ->
  print("You're not carrying anything.\n", Req);
do_inv(Obs, Req) ->
  print("You're carrying:\n", Req),
  print(desc_inv(Obs, []), Req).

-spec desc_inv(ob_list(), iolist()) -> iolist().
desc_inv([], Result) -> Result;
desc_inv([Ob|Obs], Result) ->
  Line = [" ", em_object:a_short(Ob), "\n"],
  desc_inv(Obs, [Result, Line]).

%% Drop
-spec cmd_drop([string()], req()) -> cmd_ok().
cmd_drop([], Req) ->
  print("Drop what?\n", Req),
  {ok, Req};
cmd_drop([Id|_Args], #req{living=Liv}=Req) ->
  Obs = em_living:get_objects(Liv),
  try_drop(Id, Obs, Req),
  {ok, Req}.

-spec try_drop(string(), ob_list(), req()) -> ok.
try_drop(_Id, [], Req) ->
  print("You don't have anything like that.\n", Req);
try_drop(Id, [Ob|Obs], Req) ->
  case em_object:has_id(Ob, Id) of
    true ->
      do_drop(Ob, Req);
    false ->
      try_drop(Id, Obs, Req)
  end.

-spec do_drop(em_object:object(), req()) -> ok.
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
-spec cmd_get([string()], req()) -> cmd_ok().
cmd_get([], Req) ->
  print("Get what?\n", Req),
  {ok, Req};
cmd_get([Id|_Args], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  Obs = lists:filter(fun(Ob) -> not em_object:is_attached(Ob) end,
                     em_room:get_objects(Room)),
  do_get(Id, Obs, Req),
  {ok, Req}.

-spec do_get(string(), ob_list(), req()) -> ok.
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
-spec cmd_quit([string()], req()) -> cmd_stop().
cmd_quit(_Args, #req{user=User, living=Liv}=Req) ->
  print("Goodbye!\n", Req),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s leaves.~n", [Name]),
  ok = em_game:logout(User),
  {stop, Req}.

%% Glance
-spec cmd_glance([string()], req()) -> cmd_ok().
cmd_glance(_Args, #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  print(em_room:describe_except(Room, Liv), Req),
  {ok, Req}.

%% Look
-spec cmd_look([string()], req()) -> cmd_ok().
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

-spec do_look_ob(string(), ob_list(), req()) -> ok | {error, not_found}.
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

-spec do_look_liv(string(), liv_list(), req()) -> ok | {error, not_found}.
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
-spec cmd_north([string()], req()) -> cmd_ok().
cmd_north(_Args, Req) ->
  cmd_go(["north"], Req).
-spec cmd_east([string()], req()) -> cmd_ok().
cmd_east(_Args, Req) ->
  cmd_go(["east"], Req).
-spec cmd_south([string()], req()) -> cmd_ok().
cmd_south(_Args, Req) ->
  cmd_go(["south"], Req).
-spec cmd_west([string()], req()) -> cmd_ok().
cmd_west(_Args, Req) ->
  cmd_go(["west"], Req).

-spec cmd_go([string()], req()) -> cmd_ok().
cmd_go([Dir|_Args], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  do_go(em_room:get_exit(Room, Dir), Req), 
  {ok, Req}.

-spec do_go({error, not_found}, req()) -> ok
         ; ({ok, {string(), string()}}, req()) -> cmd_ok().
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
-spec cmd_emote([string()], req()) -> cmd_ok().
cmd_emote(Args, #req{living=Liv}=Req) ->
  Text = em_grammar:punctuate(string:join(Args, " ")),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s ~s~n", [Name, Text]),
  print("~s ~s~n", [Name, Text], Req),
  {ok, Req}.

%% Say
-spec cmd_say([string()], req()) -> cmd_ok().
cmd_say([FirstWord|Rest], #req{living=Liv}=Req) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(Room, Liv, "~s says, \"~s\"~n", [Name, Text]),
  print("You say, \"~s\"~n", [Text], Req),
  {ok, Req}.

%% Tell
-spec cmd_tell([string()], req()) -> cmd_ok().
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
-spec cmd_who([string()], req()) -> cmd_ok().
cmd_who(_Args, Req) ->
  print(["Users:\n",
    [[" ", Name, "\n"] || {Name, _Pid} <- em_game:get_users()]], Req),
  {ok, Req}.

%% Save
-spec cmd_save([string()], req()) -> cmd_ok().
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
-spec cmd_setlong([string()], req()) -> cmd_ok().
cmd_setlong(Args, #req{living=Liv}=Req) ->
  em_living:set_long(Liv, string:join(Args, " ")),
  {ok, Req}.

%% addexit
-spec cmd_addexit([string()], req()) -> cmd_ok().
cmd_addexit([Dir, ToName|_Rest], #req{living=Liv}=Req) ->
  ok = verify_privilege(admin, Req),
  case em_room_mgr:get_room(ToName) of
    {ok, _ToRoom} ->
      FromRoom = em_living:get_room(Liv),
      em_room:add_exit(FromRoom, Dir, ToName),
      ok = em_room:save(FromRoom);
    {error, not_found} ->
      print("No such room exists!\n", Req)
  end,
  {ok, Req};
cmd_addexit(_Args, Req) ->
  ok = verify_privilege(admin, Req),
  print(
  "Usage: addexit <dir> <name>\n\n"
  "  Add an exit in direction <dir> to the existing room <name>.\n",
  Req),
  {ok, Req}.

%% REdit
-spec cmd_redit([string()], req()) -> cmd_ok().
cmd_redit(["dig", Dir, ToName|_Rest], #req{living=Liv}=Req) ->
  ok = verify_privilege(admin, Req),
  case em_room_mgr:new_room(ToName) of
    {ok, ToRoom} ->
      FromRoom = em_living:get_room(Liv),
      FromName = em_room:get_name(FromRoom),
      em_room:add_exit(FromRoom, Dir, ToName),
      ok = em_room:save(FromRoom),
      em_room:add_exit(ToRoom, reverse_dir(Dir), FromName),
      ok = em_room:save(ToRoom);
    {error, room_exists} ->
      print("That room already exists!\n", Req)
  end,
  {ok, Req};
cmd_redit(["title", What|Rest], #req{living=Liv}=Req) ->
  ok = verify_privilege(admin, Req),
  Room = em_living:get_room(Liv),
  Title = string:join([What|Rest], " "),
  em_room:set_title(Room, Title),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["brief", What|Rest], #req{living=Liv}=Req) ->
  ok = verify_privilege(admin, Req),
  Room = em_living:get_room(Liv),
  Brief = string:join([What|Rest], " "),
  em_room:set_brief(Room, Brief),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["long", What|Rest], #req{living=Liv}=Req) ->
  ok = verify_privilege(admin, Req),
  Room = em_living:get_room(Liv),
  Long = string:join([What|Rest], " "),
  em_room:set_long(Room, Long),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(_Args, Req) ->
  ok = verify_privilege(admin, Req),
  print(
  "Edit / create rooms. Changes are immediately saved.\n"
  "Usage: redit <cmd> <args>\n\n"
  "Commands:\n"
  "  dig <dir> <name>       Add an exit <dir> in the current room, leading\n"
  "                         to the new room <name>\n"
  "  title <what>           Set the room title to <what>\n"
  "  brief <what>           Set the room brief desc to <what>\n"
  "  long <what>            Set the room long desc to <what>\n",
  Req),
  {ok, Req}.

cmd_cast(["ward"], #req{living=Liv}=Req) ->
  Room = em_living:get_room(Liv),
  Name = em_living:get_name(Liv),
  em_living:print(Liv, 
    "As you quietly vocalize your chosen mnemonics, "
    "the spell takes shape.\n"),
  em_room:print_except(Room, Liv, 
    "~s starts muttering something incomprehensible.\n", [Name]),
  em_spell_ward:start(Liv, Room),
  {ok, Req};
cmd_cast(_Args, Req) ->
  print(
  "You can cast the following spells:\n"
  "  ward - Will let you know if someone enters the protected room.\n"
  ,Req),
  {ok, Req}.

-spec reverse_dir(string()) -> string().
reverse_dir("north") -> "south";
reverse_dir("east") -> "west";
reverse_dir("south") -> "north";
reverse_dir("west") -> "east".

-spec verify_privilege(atom(), req()) -> ok.
verify_privilege(Priv, #req{user=User}) ->
  case em_user:has_privilege(User, Priv) of
    true -> ok;
    false -> throw(not_allowed)
  end.

%% Help
-spec cmd_help([string()], req()) -> cmd_ok().
cmd_help(["privileges"], Req) ->
  print(
  "Privileges are used to control what commands users have access to.\n"
  "Currently it's not possible to set them in-game; instead, edit the\n"
  "file data/users/<username>.dat and add a line like this:\n\n"
  "{privileges, [admin]}.\n\n"
  "The 'admin' privilege is the only one in use for now, to restrict\n"
  "access to commands like 'redit' etc that will modify the game.\n",
  Req),
  {ok, Req};
cmd_help(_Args, Req) ->
  print(
  "Welcome to ErlyMud!\n\n"
  "The following commands, more or less, are available right now:\n\n"
  "  drop <item>             Drop an item from your inventory.\n"
  "  get <item>              Pick up an item in the room.\n"
  "  glance                  View the brief description of the room.\n"
  "  look                    View the long description of the room.\n"
  "  look [person|item]      View description of person/item.\n"
  "  ----------------------------------------------------------------------\n"
  "  go <dir>                Leave in the specified direction.\n"
  "  north                     - shortcut for 'go north'\n"
  "  east                      - shortcut for 'go east'\n"
  "  south                     - shortcut for 'go south'\n"
  "  west                      - shortcut for 'go west'\n"
  "  ----------------------------------------------------------------------\n"
  "  say <what>              Say something, all in room will see it:\n"
  "                            Jack says, \"Hello there!\"\n"
  "  emote <what>            Emote, for roleplaying:\n"
  "                            'emote grumpily kicks at a small rock'\n"
  "                            -> Jack grumpily kicks at a small rock.\n"
  "  tell <person> <what>    Send a private message to another user.\n"
  "  ----------------------------------------------------------------------\n"
  "  inv                     Show your inventory.\n"
  "  save                    Save your character, will remember your\n"
  "                            location and inventory for next login.\n"
  "  setlong <desc>          Set the description others see when they\n"
  "                            look at you.\n"
  "  who                     Display all logged in users.\n"
  "  ----------------------------------------------------------------------\n"
  "  cast <spell>            Cast a spell. Try 'cast' for more info.\n"
  "  ----------------------------------------------------------------------\n"
  "  addexit <dir> <room>    Add an exit to an existing room. (*)\n"
  "  redit <cmd> <args>      Edit / create rooms. Try 'redit' for info. (*)\n"
  "\n"
  "(*) Privileged command, see 'help privileges'.\n"
  ,Req),
  {ok, Req}.

%% Utility functions

-spec print(iolist(), req()) -> ok.
print(Format, Req) ->
  print(Format, [], Req).

-spec print(iolist(), list(), req()) -> ok.
print(Format, Args, #req{conn=Conn}) ->
  em_conn:print(Conn, Format, Args).
    
