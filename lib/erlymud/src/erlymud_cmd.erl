-module(erlymud_cmd).

%% API
-export([parse/3]).

-record(state, {userid, user}).


%% ===================================================================
%% Application Interface
%% ===================================================================

parse(Socket, Data, #state{userid=UserId, user=User} = State) ->
  case Data of
    "" ->
      em_user:write(User, "> "),
      {ok, State};
    "look" ->
      display_room(Socket, UserId),
      em_user:write(User, "> "),
      {ok, State};
    "who" ->
      display_users(User),
      em_user:write(User, "> "),
      {ok, State};
    "quit" ->
      em_user:write(User, "Goodbye!\n"),
      erlymud_users:remove(UserId),
      done;
    Text ->
      case try_parse(Socket, Text, State) of
        ok ->
          em_user:write(User, "> "),
          {ok, State};
        {error, no_parse_match} ->
          em_user:write(User, "From within the Void, you hear an echo.. '" ++ Text ++ "'\n"),
          em_user:write(User, "> "),
          {ok, State}
      end
  end.


%% ===================================================================
%% Private functions
%% ===================================================================

try_parse(Socket, Text, State) ->
  [Verb | Rest ] = string:tokens(Text, " "),
  case Verb of
    "tell" ->
      try_parse_tell(Socket, Rest, State);
    _Other ->
      {error, no_parse_match}
  end.

try_parse_tell(Socket, [], #state{user=User} = State) ->
  em_user:write(User, "Syntax: tell <who> <what>\n");
try_parse_tell(Socket, [Token | []], #state{user=User} = State) ->
  Who = erlymud_text:capitalize(Token),
  em_user:write(User, "Tell " ++ Who ++ " what?\n");
try_parse_tell(Socket, [Token | What], #state{userid=UserId, user=User} = State) ->
  Who = erlymud_text:capitalize(Token),
  case erlymud_users:get(Who) of
    {error, not_found} ->
      em_user:write(User, "No such user found.\n");
    {ok, User} -> 
      em_user:write(User, "Talking to yourself, huh?\n");
    {ok, OtherUser} ->
      MyText = io_lib:format("You tell ~s, \"~s\"~n",
        [Who, string:join(What, " ")]),
      em_user:write(User, MyText),
      OtherText = io_lib:format("~s tells you, \"~s\"~n",
        [UserId, string:join(What, " ")]),
      em_user:write(OtherUser, OtherText)
  end.

display_room(Socket, UserId) ->
  {ok, User} = erlymud_users:get(UserId),
  RName = em_user:room(User),
  {ok, Room} = erlymud_room_mgr:lookup(RName),
  Text = erlymud_room:describe(Room),
  em_user:write(User, Text).

display_users(User) ->
  {ok, List} = erlymud_users:get(),
  Users = [U || {U, _Pid} <- List],
  em_user:write(User, "Users:\n"),
  lists:foreach(
    fun(U) -> 
      em_user:write(User, " " ++ U ++ "\n") 
    end,
    Users).
