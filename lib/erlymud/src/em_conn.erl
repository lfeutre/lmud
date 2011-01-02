-module(em_conn).

-behaviour(gen_server).

-export([start_link/1, print/2, print/3, reset_connection/2, welcome/1, 
         login/3, parse/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock, socket, output, handlers}).


%% API

start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

print(Conn, Format) ->
  gen_server:call(Conn, {print, Format}).

print(Conn, Format, Args) ->
  gen_server:call(Conn, {print, Format, Args}).

%% gen_server callbacks

init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call({print, Format}, _From, #state{output=Out}=State) ->
  em_output:print(Out, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{output=Out}=State) ->
  em_output:print(Out, Format, Args),
  {reply, ok, State}.

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
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      em_conn_sup:start_child(),
      % Start an output process and link to it, so we're
      % always mutually destroyed if either dies..
      {ok, OutPid} = em_output_sup:start_child(Socket),
      link(OutPid),
      welcome(OutPid),
      Handlers=[{?MODULE, login, [got_user]}],
      {noreply, State#state{socket=Socket, output=OutPid, handlers=Handlers}};
    {error, closed} ->
      {stop, normal, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  Handlers = [{?MODULE, reset_connection, []}],
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
  case apply(M, F, A ++ [Data, State]) of
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

reset_connection(_Data, #state{output=Out}) ->
  em_output:print(Out, "Your connection has been upgraded; sorry for any inconvenience..\n\n"),
  welcome(Out),
  Handler = {?MODULE, login, [got_user]},
  {next, Handler}.

%% Input handling on a higher level; "shell" stuff etc

welcome(Out) ->
  em_output:print(Out, "\nWelcome to ErlyMUD 0.2.2\n\n"),
  em_output:print(Out, "Login: ").

%% Got a username, do something with it
login(got_user, "", #state{output=Out}) ->
  em_output:print(Out, "Invalid username.\n\n"),
  em_output:print(Out, "Login: "),
  {ok, [got_user]};
login(got_user, Name, #state{output=Out}) ->
  UserName = em_text:capitalize(Name),
  UserFile = filename:join([code:priv_dir(erlymud), "users",
                            [UserName, ".dat"]]),
  case file:consult(UserFile) of
    {ok, Settings} ->
      em_output:print(Out, "Password: "),
      {ok, [{got_password, Settings, UserName}]};
    {error, _Reason} ->
      em_output:print(Out, "New user account \"~s\".\n", [UserName]),
      em_output:print(Out, "Are you sure (Y/N)? "),
      {ok, [{new_user_confirm, UserName}]}
  end;
%% It's a new user, confirm if name is correct
login({new_user_confirm, Name}, "y", #state{output=Out}) ->
  em_output:print(Out, "\nPick a password: "),
  {ok, [{new_user_pw, Name}]};
login({new_user_confirm, Name}, "yes", #state{output=Out}) ->
  em_output:print(Out, "\nPick a password: "),
  {ok, [{new_user_pw, Name}]};
login({new_user_confirm, _Name}, _Other, #state{output=Out}) ->
  em_output:print(Out, "\nLogin: "),
  {ok, [got_user]};
%% Pick a password for new user account
login({new_user_pw, Name}, "", #state{output=Out}) ->
  em_output:print(Out, "Invalid password.\n\n"),
  em_output:print(Out, "Pick a password: "),
  {ok, [{new_user_pw, Name}]};
login({new_user_pw, Name}, Password, #state{output=Out}) ->
  em_output:print(Out, "Repeat the password: "),
  {ok, [{new_user_pw_confirm, Name, Password}]};
%% Confirm the password
login({new_user_pw_confirm, Name, Password}, Password, #state{output=Out}) ->
  UserFile = filename:join([code:priv_dir(erlymud), "users", [Name, ".dat"]]),
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  file:write_file(UserFile, lists:flatten([
    "{version, 1}.\n",
    "{password, \"", CryptPw, "\"}.\n"])),
  em_output:print(Out, ["\nWelcome, ", Name, "!\n\n"]),
  do_login(Name, Out);
login({new_user_pw_confirm, Name, _Password}, _WrongPassword, #state{output=Out}) ->
  em_output:print(Out, "Passwords don't match, please try again.\n\n"),
  em_output:print(Out, "Pick a password: "),
  {ok, [{new_user_pw, Name}]};
%% Existing user; load file and compare passwords, log in if correct
login({got_password, Settings, Name}, Password, #state{output=Out}) ->
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  case lists:keyfind(password, 1, Settings) of
    false ->
      em_output:print(Out, "Error: Invalid user file.\n\n"),
      em_output:print(Out, "Login: "),
      {ok, [got_user]};
    {password, CryptPw} ->
      do_login(Name, Out);
    {password, _Other} ->
      em_output:print(Out, "Invalid password.\n\n"),
      em_output:print(Out, "Login: "),
      {ok, [got_user]}
  end.

do_login(Name, Out) ->
  case em_game:login(Name, {self(), Out}) of
    {ok, User} ->
      NotMe = fun(Liv) -> Liv =/= User end,
      em_game:print_while(NotMe, "[Notice] ~s has logged in.~n", [Name]),
      em_output:print(Out, "\n"),
      em_living:cmd(User, "look"),
      em_output:print(Out, "\n> "),
      {next, {?MODULE, parse, [User]}};
    {error, user_exists} ->
      em_output:print(Out, "User already logged in, try again.\n\n"),
      em_output:print(Out, "Login: "),
      {ok, [got_user]}
  end.

parse(User, Line, #state{output=Out}) ->
  case Line of
    "" ->
      em_output:print(Out, "> "),
      {ok, [User]};
    "quit" ->
      em_living:cmd(User, "quit"),
      em_output:print(Out, "Quitting..\n"),
      done;
    Cmd ->
      em_living:cmd(User, Cmd),
      em_output:print(Out, "\n> "),
      {ok, [User]}
  end.


