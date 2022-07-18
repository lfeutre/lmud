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
-export([cmd_redit/2,
         cmd_open/2,
         cmd_dig/2]).

-include("apps/lmud/include/request.hrl").
-include("apps/lmud/include/types.hrl").
-include_lib("logjam/include/logjam.hrl").

-type cmd_ok() :: {ok, req()}.

-spec parse(string(), req()) -> req_any().
parse(Line, Req) ->
  case string:tokens(Line, " ") of
    [] ->
      'lmud-util':print("\n> ", Req),
      ?req_next(parse);
    [Cmd|Args] ->
      Result = parse_cmd(Cmd, Args, Line, Req),
      'lmud-util':print("\n> ", Req),
      Result
  end.

-spec parse_cmd(string(), [string()], string(), req()) -> req_any().
parse_cmd(Cmd, PassedArgs, Line, Req) ->
  LowerCmd = string:to_lower(Cmd),
  try
    case 'lmud-commands':'get-command-or-alias'(LowerCmd, 'lmud-commands':'base+admin'()) of
      [{args,DefinedArgs},_,{func,Func},{mod,Mod},_] ->
        Args = lists:merge([DefinedArgs,PassedArgs]),
        ?'log-debug'("PassedArgs: ~p",[PassedArgs]),
        ?'log-debug'("DefinedArgs: ~p",[DefinedArgs]),
        ?'log-debug'("Mod: ~p",[Mod]),
        ?'log-debug'("Func: ~p",[Func]),
        ?'log-debug'("Args: ~p",[Args]),
        try
          case apply(Mod, Func, [Args, Req]) of
            {ok, Req} ->
              ?req_next(parse);
            {stop, Req} ->
              ?req_done;
            {error, Reason} ->
              'lmud-util':print(Reason, Req),
              ?req_next(parse);
            Other ->
              'lmud-util':print("Error occurred while processing '~s':~n~p~n",
                [Line, Other], Req),
              ?'log-debug'("Mod: ~p",[Mod],Req),
              ?'log-debug'("Func: ~p",[Func],Req),
              ?'log-debug'("Args: ~p",[Args],Req),
              ?req_next(parse)
          end
        catch
          throw:not_allowed ->
            'lmud-util':print("You're not allowed to use the '~s' command.~n", [Line], Req),
            ?req_next(parse)
        end;
      _ ->
        'lmud-util':print("I don't understand what you mean by '~s'~n", [Line], Req),
        ?req_next(parse)
    end
  catch
    _ ->
      'lmud-util':print("I don't understand what you mean by '~s'~n", [Line], Req),
      ?req_next(parse)
  end.


%% ==========================================================================
%% Game commands
%% ==========================================================================


%% @open
-spec cmd_open([string()], req()) -> cmd_ok().
cmd_open([InputDir, ToName|_Rest], #req{living=Liv}=Req) ->
  {ok, Dir} = check_dir(InputDir),
  ok = 'lmud-perms':verify(aesir, Req),
  case em_room_mgr:get_room(ToName) of
    {ok, _ToRoom} ->
      FromRoom = em_living:get_room(Liv),
      em_room:add_exit(FromRoom, Dir, ToName),
      ok = em_room:save(FromRoom);
    {error, not_found} ->
      'lmud-util':print("No such room exists!\n", Req)
  end,
  {ok, Req};
cmd_open(_Args, Req) ->
  'lmud-util':print(
  "Usage: @open <dir> <name>\n\n"
  "  Add an exit in direction <dir> to the existing room <name>.\n",
  Req),
  {ok, Req}.

%% @dig
cmd_dig([InputDir, ToName|_Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(aesir, Req),
  case check_dir(InputDir) of
    {ok, Dir} ->
      case em_room_mgr:new_room(ToName) of
        {ok, ToRoom} ->
          FromRoom = em_living:get_room(Liv),
          FromName = em_room:get_name(FromRoom),
          em_room:add_exit(FromRoom, Dir, ToName),
          ok = em_room:save(FromRoom),
          em_room:add_exit(ToRoom, reverse_dir(Dir), FromName),
          ok = em_room:save(ToRoom);
        {error, room_exists} ->
          'lmud-util':print("That room already exists!\n", Req)
      end;
    {error, {invalid_direction, Dir}} ->
      'lmud-util':print("'" ++ Dir ++ "' is not a valid direction.", Req)
    end,
  {ok, Req};
cmd_dig(_Args, Req) ->
  'lmud-util':print(
  "Usage: @dig <dir> <name>\n\n"
  "  Add an exit <dir> in the current room, leading to the new room <name>\n",
  Req),
  {ok, Req}.

%% REdit
-spec cmd_redit([string()], req()) -> cmd_ok().
cmd_redit(["title", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(aesir, Req),
  Room = em_living:get_room(Liv),
  Title = string:join([What|Rest], " "),
  em_room:set_title(Room, Title),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["brief", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(aesir, Req),
  Room = em_living:get_room(Liv),
  Brief = string:join([What|Rest], " "),
  em_room:set_brief(Room, Brief),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(["desc", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(aesir, Req),
  Room = em_living:get_room(Liv),
  Desc = string:join([What|Rest], " "),
  em_room:set_desc(Room, Desc),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(_Args, Req) ->
  ok = 'lmud-perms':verify(aesir, Req),
  'lmud-util':print(
  "Edit / create rooms. Changes are immediately saved.\n"
  "Usage: redit <cmd> <args>\n\n"
  "Commands:\n"
  "  title <what>           Set the room title to <what>\n"
  "  brief <what>           Set the room brief desc to <what>\n"
  "  desc <what>            Set the room long description to <what>\n",
  Req),
  {ok, Req}.

-spec reverse_dir(string()) -> string().
reverse_dir("north") -> "south";
reverse_dir("east") -> "west";
reverse_dir("south") -> "north";
reverse_dir("west") -> "east".

check_dir("n") -> {ok, "north"};
check_dir("e") -> {ok, "east"};
check_dir("s") -> {ok, "south"};
check_dir("w") -> {ok, "west"};
check_dir("north"=Dir) -> {ok, Dir};
check_dir("east"=Dir) -> {ok, Dir};
check_dir("south"=Dir) -> {ok, Dir};
check_dir("west"=Dir) -> {ok, Dir};
check_dir(Dir) -> {error, {invalid_direction, Dir}}.
