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
         cmd_addexit/2,
         cmd_cast/2]).

-include("request.hrl").
-include("types.hrl").

-type cmd_ok() :: {ok, req()}.

%% ==========================================================================
%% API functions
%% ==========================================================================

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
              'lmud-util':print(Reason, Req),
              ?req_next(parse);
            Other ->
              'lmud-util':print("Error occurred while processing '~s':~n~p~n",
                [Line, Other], Req),
              % 'lmud-util':print("Mod: ~p~n",[Mod],Req),
              % 'lmud-util':print("Func: ~p~n",[Func],Req),
              % 'lmud-util':print("Args: ~p~n",[Args],Req),
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
      'lmud-util':print("No such room exists!\n", Req)
  end,
  {ok, Req};
cmd_addexit(_Args, Req) ->
  ok = 'lmud-perms':verify(admin, Req),
  'lmud-util':print(
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
      'lmud-util':print("That room already exists!\n", Req)
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
cmd_redit(["desc", What|Rest], #req{living=Liv}=Req) ->
  ok = 'lmud-perms':verify(admin, Req),
  Room = em_living:get_room(Liv),
  Desc = string:join([What|Rest], " "),
  em_room:set_desc(Room, Desc),
  ok = em_room:save(Room),
  {ok, Req};
cmd_redit(_Args, Req) ->
  ok = 'lmud-perms':verify(admin, Req),
  'lmud-util':print(
  "Edit / create rooms. Changes are immediately saved.\n"
  "Usage: redit <cmd> <args>\n\n"
  "Commands:\n"
  "  dig <dir> <name>       Add an exit <dir> in the current room, leading\n"
  "                         to the new room <name>\n"
  "  title <what>           Set the room title to <what>\n"
  "  brief <what>           Set the room brief desc to <what>\n"
  "  desc <what>            Set the room long description to <what>\n",
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
  'lmud-util':print(
  "You can cast the following spells:\n"
  "  ward - Will let you know if someone enters the protected room.\n"
  ,Req),
  {ok, Req}.

-spec reverse_dir(string()) -> string().
reverse_dir("north") -> "south";
reverse_dir("east") -> "west";
reverse_dir("south") -> "north";
reverse_dir("west") -> "east".
