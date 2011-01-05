% receive(): Push received data onto our queue and return with a 
% timeout; this will allow the calling process to proceed while we 
% do our work, while still maintaining order of input (which isn't 
% guaranteed if using cast).
-module(em_user).

-behaviour(gen_server).

-export([start_link/1, print/2, print/3, reset_connection/2, welcome/1, 
         login/3, parse/3, receive_line/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn, queue=[], handlers=[]}).


%% API

start_link(Conn) ->
  gen_server:start_link(?MODULE, [Conn], []).

print(Pid, Format) ->
  gen_server:call(Pid, {print, Format}).

print(Pid, Format, Args) ->
  gen_server:call(Pid, {print, Format, Args}).

receive_line(Pid, Data) ->
  gen_server:call(Pid, {receive_line, Data}).

%% gen_server callbacks

init([Conn]) ->
  {ok, #state{conn=Conn, queue=[init]}, 0}.

handle_call({print, Format}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format),
  {reply, ok, State};
handle_call({print, Format, Args}, _From, #state{conn=Conn}=State) ->
  em_conn:print(Conn, Format, Args),
  {reply, ok, State};
handle_call({receive_line, Data}, _From, #state{queue=Queue}=State) ->
  {reply, ok, State#state{queue=Queue ++ [{input, Data}]}, 0}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(timeout, State) ->
  process_queue(State).

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Extra) ->
  Handlers = [{?MODULE, reset_connection, []}],
  {ok, State#state{handlers = Handlers}}.


%% Internal functions

process_queue(#state{queue=[]}=State) ->
  {noreply, State};
process_queue(#state{queue=[init|Queue],conn=Conn}=State) ->
  welcome(Conn),
  Handlers=[{?MODULE, login, [got_user]}],
  process_queue(State#state{queue=Queue, handlers=Handlers});
process_queue(#state{queue=[{input, RawData}|Queue]}=State) ->
  {ok, NewState} = handle_data(RawData, State),
  % Make sure we stop if there are no handlers on the stack!
  case NewState#state.handlers of
    [] ->
      {stop, normal, NewState};
    _Other ->
      process_queue(NewState#state{queue=Queue})
  end.

strip_linefeed(RawData) ->
  re:replace(RawData, "\r\n$", "", [{return, list}]).

handle_data(RawData, State) ->
  Data = strip_linefeed(RawData),
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

reset_connection(_Data, #state{conn=Conn}) ->
  em_conn:print(Conn, "Your connection has been upgraded; sorry for any inconvenience..\n\n"),
  welcome(Conn),
  Handler = {?MODULE, login, [got_user]},
  {next, Handler}.

%% Input handling on a higher level; "shell" stuff etc

welcome(Conn) ->
  em_conn:print(Conn, "\nWelcome to ErlyMUD 0.2.7\n\n"),
  em_conn:print(Conn, "Login: ").

%% Got a username, do something with it
login(got_user, "", #state{conn=Conn}) ->
  em_conn:print(Conn, "Invalid username.\n\n"),
  em_conn:print(Conn, "Login: "),
  {ok, [got_user]};
login(got_user, Name, #state{conn=Conn}) ->
  UserName = em_text:capitalize(string:to_lower(Name)),
  UserFile = filename:join([code:priv_dir(erlymud), "users",
                            [UserName, ".dat"]]),
  case file:consult(UserFile) of
    {ok, Settings} ->
      em_conn:print(Conn, "Password: "),
      em_conn:echo_off(Conn),
      {ok, [{got_password, Settings, UserName}]};
    {error, _Reason} ->
      em_conn:print(Conn, "New user account \"~s\".\n", [UserName]),
      em_conn:print(Conn, "Are you sure (Y/N)? "),
      {ok, [{new_user_confirm, UserName}]}
  end;
%% It's a new user, confirm if name is correct
login({new_user_confirm, Name}, YesNo, #state{conn=Conn}) ->
  case string:to_lower(YesNo) of
    [$y|_Rest] ->
      em_conn:print(Conn, "\nPick a password: "),
      em_conn:echo_off(Conn),
      {ok, [{new_user_pw, Name}]};
    _Other ->
      em_conn:print(Conn, "\nLogin: "),
      {ok, [got_user]}
  end;
%% Pick a password for new user account
login({new_user_pw, Name}, "", #state{conn=Conn}) ->
  em_conn:print(Conn, "Invalid password.\n\n"),
  em_conn:print(Conn, "Pick a password: "),
  {ok, [{new_user_pw, Name}]};
login({new_user_pw, Name}, Password, #state{conn=Conn}) ->
  em_conn:print(Conn, "Repeat the password: "),
  {ok, [{new_user_pw_confirm, Name, Password}]};
%% Confirm the password
login({new_user_pw_confirm, Name, Password}, Password, #state{conn=Conn}) ->
  em_conn:echo_on(Conn),
  UserFile = filename:join([code:priv_dir(erlymud), "users", [Name, ".dat"]]),
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  file:write_file(UserFile, lists:flatten([
    "{version, 1}.\n",
    "{password, \"", CryptPw, "\"}.\n"])),
  LivFile = filename:join([code:priv_dir(erlymud), "livings", [Name, ".dat"]]),
  file:write_file(LivFile, lists:flatten([
    "{version, 1}.\n",
    "{long, \"", Name, " looks pretty ordinary.\"}.\n"])),
  em_conn:print(Conn, ["\nWelcome, ", Name, "!\n\n"]),
  do_login(Name, Conn);
login({new_user_pw_confirm, Name, _Password}, _WrongPassword, #state{conn=Conn}) ->
  em_conn:print(Conn, "Passwords don't match, please try again.\n\n"),
  em_conn:print(Conn, "Pick a password: "),
  {ok, [{new_user_pw, Name}]};
%% Existing user; load file and compare passwords, log in if correct
login({got_password, Settings, Name}, Password, #state{conn=Conn}) ->
  em_conn:echo_on(Conn),
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  case lists:keyfind(password, 1, Settings) of
    false ->
      em_conn:print(Conn, "Error: Invalid user file.\n\n"),
      em_conn:print(Conn, "Login: "),
      {ok, [got_user]};
    {password, CryptPw} ->
      do_login(Name, Conn);
    {password, _Other} ->
      em_conn:print(Conn, "Invalid password.\n\n"),
      em_conn:print(Conn, "Login: "),
      {ok, [got_user]}
  end.

do_login(Name, Conn) ->
  {ok, Living} = em_living_sup:start_child(Name, {self(), Conn}),
  ok = em_living:load(Living),
  case em_game:login(Living) of
    {ok, Living} ->
      link(Living),
      em_conn:print(Conn, "\n"),
      em_living:cmd(Living, "glance"),
      em_conn:print(Conn, "\n> "),
      {next, {?MODULE, parse, [Living]}};
    {error, user_exists} ->
      em_conn:print(Conn, "User already logged in, try again.\n\n"),
      em_conn:print(Conn, "Login: "),
      {ok, [got_user]}
  end.

parse(User, Line, #state{conn=Conn}) ->
  case Line of
    "" ->
      em_conn:print(Conn, "> "),
      {ok, [User]};
    "quit" ->
      em_living:cmd(User, "quit"),
      done;
    Cmd ->
      em_living:cmd(User, Cmd),
      em_conn:print(Conn, "\n> "),
      {ok, [User]}
  end.


