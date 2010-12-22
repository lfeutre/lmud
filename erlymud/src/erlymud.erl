-module(erlymud).

%% API
-export([connect/1, login/3, parse/3]).


%% ===================================================================
%% Application Interface
%% ===================================================================

connect(Socket) ->
  gen_tcp:send(Socket, "\n\nWelcome to ErlyMUD!\n\n"),
  gen_tcp:send(Socket, "User: "),
  {?MODULE, login, got_user}.

login(Socket, UserName, got_user) ->
  erlymud_users:add(UserName, self()),
  gen_tcp:send(Socket, "Logging in..\n\n"),
  gen_tcp:send(Socket, "You're in the Void. Type 'quit' to leave.\n"),
  gen_tcp:send(Socket, "> "),
  {next, {?MODULE, parse, []}}.

parse(Socket, Data, State) ->
  case Data of
    "who" ->
      display_users(Socket),
      gen_tcp:send(Socket, "\n> "),
      {ok, State};
    "quit" ->
      gen_tcp:send(Socket, "Goodbye!\n"),
      done;
    Text ->
      gen_tcp:send(Socket, "From within the Void, you hear an echo.. '" ++ Text ++ "'\n"),
      gen_tcp:send(Socket, "\n> "),
      {ok, State}
  end.

%% Private functions

display_users(Socket) ->
  {ok, List} = erlymud_users:get(),
  Users = [User || {User, _Pid} <- List],
  gen_tcp:send(Socket, "Users:\n"),
  lists:foreach(
    fun(User) -> 
      gen_tcp:send(Socket, " " ++ User ++ "\n") 
    end,
    Users).
