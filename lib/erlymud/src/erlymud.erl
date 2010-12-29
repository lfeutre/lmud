-module(erlymud).

%% API
-export([connect/1, login/3]).

-record(state, {userid, user}).


%% ===================================================================
%% Application Interface
%% ===================================================================

connect(Socket) ->
  User = em_user:new("Unknown", Socket),
  em_user:write(User, "\n\nWelcome to ErlyMUD!\n\n"),
  em_user:write(User, "User: "),
  {?MODULE, login, {got_user, User}}.


login(Socket, Data, {got_user, AnonUser}) ->
  UserName = erlymud_text:capitalize(Data),
  User = em_user:set_name(AnonUser, UserName),
  case erlymud_users:add(UserName, User) of
    ok -> 
      em_user:write(User, "Logging in..\n\n"),
      em_user:write(User, "You're in the Void. Type 'quit' to leave.\n"),
      em_user:write(User, " (hint: you can try 'who' and 'tell' as well..)\n"),
      em_user:write(User, "> "),
      {next, {erlymud_cmd, parse, #state{userid = UserName, user = User}}};
    {error, user_exists} ->
      em_user:write(User, "User already logged in, pick another.\n\n"),
      em_user:write(User, "User: "),
      {ok, got_user};
    _Other ->
      em_user:write(User, "Unknown error, please reconnect.\n"),
      done
  end.

