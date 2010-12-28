-module(erlymud).

%% API
-export([connect/1, login/3]).

-record(state, {userid}).


%% ===================================================================
%% Application Interface
%% ===================================================================

connect(Socket) ->
  gen_tcp:send(Socket, "\n\nWelcome to ErlyMUD!\n\n"),
  gen_tcp:send(Socket, "User: "),
  {?MODULE, login, got_user}.


login(Socket, Data, got_user) ->
  UserName = erlymud_text:capitalize(Data),
  User = em_user:new(UserName, Socket),
  case erlymud_users:add(UserName, User) of
    ok -> 
      gen_tcp:send(Socket, "Logging in..\n\n"),
      gen_tcp:send(Socket, "You're in the Void. Type 'quit' to leave.\n"),
      gen_tcp:send(Socket, " (hint: you can try 'who' and 'tell' as well..)\n"),
      gen_tcp:send(Socket, "> "),
      {next, {erlymud_cmd, parse, #state{userid = UserName}}};
    {error, user_exists} ->
      gen_tcp:send(Socket, "User already logged in, pick another.\n\n"),
      gen_tcp:send(Socket, "User: "),
      {ok, got_user};
    _Other ->
      gen_tcp:send(Socket, "Unknown error, please reconnect.\n"),
      done
  end.

