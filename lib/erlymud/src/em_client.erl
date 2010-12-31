-module(em_client).

-export([start/0, loop/0]).

start() ->
  Name = io:get_line("User: "),
  Client = spawn(?MODULE, loop, []),
  {ok, User} = em_game:login(Name, Client),
  input(User).

loop() ->
  receive
    {print, What} ->
      io:format(What),
      loop();
    {print, Format, Args} ->
      io:format(Format, Args),
      loop();
    Any ->
      io:format("em_client(): Unknown message: ~p~n", [Any]),
      loop()
  end.

input(User) ->
  [$\n|RevLine] = lists:reverse(io:get_line("> ")),
  case lists:reverse(RevLine) of
    "quit" ->
      em_living:cmd(User, "quit"),
      io:format("Quitting..\n");
    Cmd ->
      em_living:cmd(User, Cmd),
      input(User)
  end.
