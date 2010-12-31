-module(em_conn).

-behaviour(gen_server).

-export([start_link/1, print/2, print/3, reset_connection/2, welcome/1, 
         login/2, parse/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, socket, handlers}).


%% API

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

print(Conn, Format) ->
  gen_server:cast(Conn, {print, Format}).

print(Conn, Format, Args) ->
  gen_server:cast(Conn, {print, Format, Args}).

%% gen_server callbacks

init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
  {reply, {ok, Msg}, State}.

handle_cast({print, Format}, #state{socket=Socket}=State) ->
  gen_tcp:send(Socket, io_lib:format(Format, [])),
  {noreply, State};
handle_cast({print, Format, Args}, #state{socket=Socket}=State) ->
  gen_tcp:send(Socket, io_lib:format(Format, Args)),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  {ok, NewState} = handle_data(Socket, RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#state.handlers of
    [] ->
      {stop, normal, NewState};
    _Other -> {noreply, NewState}
  end;
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(timeout, #state{lsock=LSock}=State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  em_conn_sup:start_child(),
  welcome(Socket),
  Handlers=[{?MODULE, login, [got_user]}],
  {noreply, State#state{socket=Socket, handlers=Handlers}}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, #state{socket=Socket}=State, _Extra) ->
  Handlers = [{?MODULE, reset_connection, [Socket]}],
  {ok, State#state{handlers = Handlers}}.


%% Internal functions

input_cleanup(RawData) ->
  string:to_lower(string:strip(strip_linefeed(RawData), both)).

strip_linefeed(RawData) ->
  re:replace(RawData, "\r\n$", "", [{return, list}]).

handle_data(_Socket, RawData, State) ->
  Data = input_cleanup(RawData),
  [Handler | Rest] = State#state.handlers,
  {M, F, A} = Handler,
  case apply(M, F, A ++ [Data]) of
    done -> 
      {ok, State#state{handlers = Rest}};
    {ok, NewArgs} -> 
      {ok, State#state{handlers = [{M, F, NewArgs} | Rest]}};
    {next, NextHandler} -> 
      {ok, State#state{handlers = [NextHandler | Rest]}};
    {push, NewHandler, NewArgs} -> 
      {ok, State#state{handlers = [NewHandler, {M, F, NewArgs} | Rest]}};
    Other -> 
      {error, Other}
  end.

reset_connection(Socket, _Data) ->
  gen_tcp:send(Socket, "Your connection has been upgraded; sorry for any inconvenience..\n\n"),
  welcome(Socket),
  Handler = {?MODULE, login, [got_user]},
  {next, Handler}.

%% Input handling on a higher level; "shell" stuff etc

welcome(Socket) ->
  gen_tcp:send(Socket, "\nWelcome to ErlyMUD 0.2.0\n\n"),
  gen_tcp:send(Socket, "Login: ").

login(got_user, Name) ->
  UserName = em_text:capitalize(Name),
  case em_game:login(UserName, self()) of
    {ok, User} ->
      NotMe = fun(Liv) -> Liv =/= User end,
      em_game:print_while(NotMe, "[Notice] ~s has logged in.~n", [UserName]),
      print(self(), "\n"),
      em_living:cmd(User, "look"),
      print(self(), "\n> "),
      {next, {?MODULE, parse, [User]}};
    {error, user_exists} ->
      print(self(), "User already logged in, try again.\n\n"),
      print(self(), "Login: "),
      {ok, [got_user]}
  end.

parse(User, Line) ->
  case Line of
    "" ->
      print(self(), "> "),
      {ok, [User]};
    "quit" ->
      em_living:cmd(User, "quit"),
      print(self(), "Quitting..\n"),
      done;
    Cmd ->
      em_living:cmd(User, Cmd),
      print(self(), "\n> "),
      {ok, [User]}
  end.


