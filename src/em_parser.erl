%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc In-game request handler for user sessions.
%%% When a request is spawned with an MFA during in-game play, it points
%%% here, to the parse/2 function.
%%% @end
%%% =========================================================================
-module(em_parser).

%% API
-export([parse/2]).

%% game commands
-export([cmd_go/2,
         cmd_emote/2, cmd_emote_ns/2,
         cmd_say/2, cmd_tell/2, cmd_think/2,
         cmd_redit/2, cmd_addexit/2,
         cmd_cast/2]).

-include("request.hrl").
-include("types.hrl").

%-type ob_list() :: [em_object:object()].
%-type liv_list() :: [em_living:living_pid()].
-type cmd_ok() :: {ok, req()}.

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
parse_cmd(Cmd, PassedArgs, Line, Req) ->
  LowerCmd = string:to_lower(Cmd),
  try
    case 'lmud-commands':'get-command-or-alias'(LowerCmd, 'lmud-commands':'base'()) of
      [{args,DefinedArgs},_,{func,Func},{mod,Mod},_] ->
        Args = lists:merge([DefinedArgs,PassedArgs]),
        % io:format("PassedArgs: ~p~n",[PassedArgs]),
        % io:format("DefinedArgs: ~p~n",[DefinedArgs]),
        % io:format("Mod: ~p~n",[Mod]),
        % io:format("Func: ~p~n",[Func]),
        % io:format("Args: ~p~n",[Args]),
        try
          case apply(Mod, Func, [Args, Req]) of
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
              % print("Mod: ~p~n",[Mod],Req),
              % print("Func: ~p~n",[Func],Req),
              % print("Args: ~p~n",[Args],Req),
              ?req_next(parse)
          end
        catch
          throw:not_allowed ->
            print("You're not allowed to use the '~s' command.~n", [Line], Req),
            ?req_next(parse)
        end;
      _ ->
        print("I don't understand what you mean by '~s'~n", [Line], Req),
        ?req_next(parse)
    end
  catch
    _ ->
      print("I don't understand what you mean by '~s'~n", [Line], Req),
      ?req_next(parse)
  end.


%% ==========================================================================
%% Game commands
%% ==========================================================================

-spec cmd_go([], req()) -> cmd_ok().
cmd_go([], Req) ->
  print("Go where?", Req),
  {ok, Req};
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
  em_room:print_except(yellowb, Room, Liv, "~s leaves ~s.~n", [Name, Dir]),
  em_room:leave(Room, Liv),
  em_living:set_room(Liv, DestRoom),
  em_room:enter(DestRoom, Liv),
  em_room:print_except(yellowb, DestRoom, Liv, "~s arrives.~n", [Name]),
  'lmud-cmd-interact':glance([], Req).

%% Emote/Pose
-spec cmd_emote([string()], req()) -> cmd_ok().
cmd_emote(Args, #req{living=Liv}=Req) ->
  Text = em_english:punctuate(string:join(Args, " ")),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(yellowb, Room, Liv, "~s ~s~n", [Name, Text]),
  print(yellowb, "~s ~s~n", [Name, Text], Req),
  {ok, Req}.

%% Emote/Pose (no space)
cmd_emote_ns(Args, #req{living=Liv}=Req) ->
  Text = em_english:punctuate(string:join(Args, " ")),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(yellowb, Room, Liv, "~s~s~n", [Name, Text]),
  print(yellowb, "~s~s~n", [Name, Text], Req),
  {ok, Req}.

%% Think
cmd_think(Args, #req{living=Liv}=Req) ->
  Text = em_english:punctuate(string:join(Args, " ")),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(blackb, Room, Liv, "~s is pondering.~n", [Name]),
  print(blackb, "~s thinks ~s~n", [Name, Text], Req),
  {ok, Req}.

%% Say
-spec cmd_say([string()], req()) -> cmd_ok().
cmd_say([FirstWord|Rest], #req{living=Liv}=Req) ->
  Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
  Name = em_living:get_name(Liv),
  Room = em_living:get_room(Liv),
  em_room:print_except(yellowb, Room, Liv, "~s says, \"~s\"~n", [Name, Text]),
  print(yellowb, "You say, \"~s\"~n", [Text], Req),
  {ok, Req}.

%% Tell/Whisper/Page
-spec cmd_tell([], req()) -> cmd_ok().
cmd_tell([], Req) ->
  print("Tell whom what?", Req),
  {ok, Req};
cmd_tell([Who,FirstWord|Rest], #req{user=User}=Req) ->
  Name = em_user:get_name(User),
  case em_game:lookup_user(Who) of
    {error, not_found} ->
      print("There's no such user.\n", Req);
    {ok, {Name, _}} ->
      print("Talking to yourself, huh?\n", Req);
    {ok, {OtherName, OtherUser}} ->
      Text = string:join([em_text:capitalize(FirstWord)|Rest], " "),
      em_user:print(OtherUser,
                    color:magenta("[Whisper] ") ++
                    "~s tells you, \"~s\"~n", [Name, Text]),
      print(color:magenta("[Whisper] ") ++
            "You tell ~s, \"~s\"~n", [OtherName, Text], Req)
  end,
  {ok, Req};
cmd_tell([Who], Req) ->
  print("Tell " ++ Who ++ " what?", Req),
  {ok, Req}.

%% addexit
-spec cmd_addexit([string()], req()) -> cmd_ok().
cmd_addexit([Dir, ToName|_Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(admin, Req),
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
  ok = 'lmud-perms':verify(admin, Req),
  print(
  "Usage: addexit <dir> <name>\n\n"
  "  Add an exit in direction <dir> to the existing room <name>.\n",
  Req),
  {ok, Req}.

%% REdit
-spec cmd_redit([string()], req()) -> cmd_ok().
cmd_redit(["dig", Dir, ToName|_Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(admin, Req),
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
  ok = 'lmud-perms':verify(admin, Req),
  Room = em_living:get_room(Liv),
  Title = string:join([What|Rest], " "),
  em_room:set_title(Room, Title),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["brief", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(admin, Req),
  Room = em_living:get_room(Liv),
  Brief = string:join([What|Rest], " "),
  em_room:set_brief(Room, Brief),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["long", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(admin, Req),
  Room = em_living:get_room(Liv),
  Long = string:join([What|Rest], " "),
  em_room:set_desc(Room, Long),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(_Args, Req) ->
  ok = 'lmud-perms':verify(admin, Req),
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

%% Utility functions

-spec print(iolist(), req()) -> ok.
print(Format, Req) ->
  print(Format, [], Req).

-spec print(iolist(), list(), req()) -> ok.
print(Format, Args, #req{conn=Conn}) ->
  em_conn:print(Conn, Format, Args).

print(Color, Format, Args, #req{conn=Conn}) ->
  em_conn:print(Conn, 'lmud-util':'format-color'(Color, Format), Args).
