-module(erlymud).

%% API
-export([connect/1, login/3, parse/3]).

-record(state, {userid}).

%% ===================================================================
%% Application Interface
%% ===================================================================

connect(Socket) ->
  gen_tcp:send(Socket, "\n\nWelcome to ErlyMUD!\n\n"),
  gen_tcp:send(Socket, "User: "),
  {?MODULE, login, got_user}.

login(Socket, Data, got_user) ->
  UserName = capitalize(Data),
  case erlymud_users:add(UserName, Socket) of
    ok -> 
      gen_tcp:send(Socket, "Logging in..\n\n"),
      gen_tcp:send(Socket, "You're in the Void. Type 'quit' to leave.\n"),
      gen_tcp:send(Socket, " (hint: you can try 'who' and 'tell' as well..)\n"),
      gen_tcp:send(Socket, "> "),
      {next, {?MODULE, parse, #state{userid = UserName}}};
    {error, user_exists} ->
      gen_tcp:send(Socket, "User already logged in, pick another.\n\n"),
      gen_tcp:send(Socket, "User: "),
      {ok, got_user}
  end.

parse(Socket, Data, State) ->
  case Data of
    "" ->
      gen_tcp:send(Socket, "> "),
      {ok, State};
    "who" ->
      display_users(Socket),
      gen_tcp:send(Socket, "> "),
      {ok, State};
    "quit" ->
      gen_tcp:send(Socket, "Goodbye!\n"),
      erlymud_users:remove(State#state.userid),
      done;
    Text ->
      case try_parse(Socket, Text, State) of
        ok ->
          gen_tcp:send(Socket, "> "),
          {ok, State};
        {error, no_parse_match} ->
          gen_tcp:send(Socket, "From within the Void, you hear an echo.. '" ++ Text ++ "'\n"),
          gen_tcp:send(Socket, "> "),
          {ok, State}
      end
  end.

%% Private functions

try_parse(Socket, Text, State) ->
  [Verb | Rest ] = string:tokens(Text, " "),
  case Verb of
    "tell" ->
      try_parse_tell(Socket, Rest, State);
    _Other ->
      {error, no_parse_match}
  end.

try_parse_tell(Socket, [], _State) ->
  gen_tcp:send(Socket, "Syntax: tell <who> <what>\n");
try_parse_tell(Socket, [Token | []], _State) ->
  Who = capitalize(Token),
  gen_tcp:send(Socket, "Tell " ++ Who ++ " what?\n");
try_parse_tell(Socket, [Token | What], State) ->
  Who = capitalize(Token),
  case erlymud_users:get(Who) of
    {ok, Socket} -> 
      gen_tcp:send(Socket, "Talking to yourself, huh?\n");
    {ok, OtherUser} ->
      MyText = io_lib:format("You tell ~s, \"~s\"~n",
        [Who, string:join(What, " ")]),
      gen_tcp:send(Socket, MyText),
      OtherText = io_lib:format("~s tells you, \"~s\"~n",
        [State#state.userid, string:join(What, " ")]),
      gen_tcp:send(OtherUser, OtherText);
    {error, not_found} ->
      gen_tcp:send(Socket, "No such user found.\n")
  end.

capitalize(S) ->
  F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
  string:join(lists:map(F, string:tokens(S, " ")), " ").

display_users(Socket) ->
  {ok, List} = erlymud_users:get(),
  Users = [User || {User, _Pid} <- List],
  gen_tcp:send(Socket, "Users:\n"),
  lists:foreach(
    fun(User) -> 
      gen_tcp:send(Socket, " " ++ User ++ "\n") 
    end,
    Users).
