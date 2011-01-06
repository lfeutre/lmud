-module(em_login).

-export([welcome/1, login/3, parse/3]).

-define(request, #state{conn=ReqConn, user=ReqUser, living=ReqLiving}=Request).

-define(req_done, {done, Request}).
-define(req_next(Fun, Args), {ok, {?MODULE, Fun, Args}, Request}).
-define(req_next(Fun, Args, State), {ok, {?MODULE, Fun, Args}, State}).
-define(req_link(Fun, Args, State), {link, {?MODULE, Fun, Args}, State}).

-record(state, {conn, user, living, queue=[], handlers=[]}).

%% API

welcome(Conn) ->
  em_conn:print(Conn, "\nWelcome to ErlyMUD 0.2.7\n\n"),
  em_conn:print(Conn, "Login: ").

%% Got a username, do something with it
login(got_user, "", #state{conn=ReqConn}=Request) ->
  em_conn:print(ReqConn, "Invalid username.\n\n"),
  em_conn:print(ReqConn, "Login: "),
  ?req_next(login, [got_user]);
login(got_user, Name, #state{conn=ReqConn}=Request) ->
  UserName = em_text:capitalize(string:to_lower(Name)),
  UserFile = filename:join([code:priv_dir(erlymud), "users",
                            [UserName, ".dat"]]),
  case file:consult(UserFile) of
    {ok, Settings} ->
      em_conn:print(ReqConn, "Password: "),
      em_conn:echo_off(ReqConn),
      ?req_next(login, [{got_password, Settings, UserName}]);
    {error, _Reason} ->
      em_conn:print(ReqConn, "New user account \"~s\".\n", [UserName]),
      em_conn:print(ReqConn, "Are you sure (Y/N)? "),
      ?req_next(login, [{new_user_confirm, UserName}])
  end;
%% It's a new user, confirm if name is correct
login({new_user_confirm, Name}, YesNo, #state{conn=ReqConn}=Request) ->
  case string:to_lower(YesNo) of
    [$y|_Rest] ->
      em_conn:print(ReqConn, "\nPick a password: "),
      em_conn:echo_off(ReqConn),
      ?req_next(login, [{new_user_pw, Name}]);
    _Other ->
      em_conn:print(ReqConn, "\nLogin: "),
      ?req_next(login, [got_user])
  end;
%% Pick a password for new user account
login({new_user_pw, Name}, "", #state{conn=ReqConn}=Request) ->
  em_conn:print(ReqConn, "Invalid password.\n\n"),
  em_conn:print(ReqConn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
login({new_user_pw, Name}, Password, #state{conn=ReqConn}=Request) ->
  em_conn:print(ReqConn, "\r\nRepeat the password: "),
  ?req_next(login, [{new_user_pw_confirm, Name, Password}]);
%% Confirm the password
login({new_user_pw_confirm, Name, Password}, Password, #state{conn=ReqConn}=Request) ->
  em_conn:echo_on(ReqConn),
  UserFile = filename:join([code:priv_dir(erlymud), "users", [Name, ".dat"]]),
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  file:write_file(UserFile, lists:flatten([
    "{version, 1}.\n",
    "{password, \"", CryptPw, "\"}.\n"])),
  LivFile = filename:join([code:priv_dir(erlymud), "livings", [Name, ".dat"]]),
  file:write_file(LivFile, lists:flatten([
    "{version, 1}.\n",
    "{long, \"", Name, " looks pretty ordinary.\"}.\n"])),
  em_conn:print(ReqConn, ["\nWelcome, ", Name, "!\n\n"]),
  do_login(Name, Request);
login({new_user_pw_confirm, Name, _Password}, _WrongPassword, #state{conn=ReqConn}=Request) ->
  em_conn:print(ReqConn, "Passwords don't match, please try again.\n\n"),
  em_conn:print(ReqConn, "Pick a password: "),
  ?req_next(login, [{new_user_pw, Name}]);
%% Existing user; load file and compare passwords, log in if correct
login({got_password, Settings, Name}, Password, #state{conn=ReqConn}=Request) ->
  em_conn:echo_on(ReqConn),
  CryptPw = base64:encode_to_string(crypto:sha(Password)),
  case lists:keyfind(password, 1, Settings) of
    false ->
      em_conn:print(ReqConn, "Error: Invalid user file.\n\n"),
      em_conn:print(ReqConn, "Login: "),
      ?req_next(login, [got_user]);
    {password, CryptPw} ->
      do_login(Name, Request);
    {password, _Other} ->
      em_conn:print(ReqConn, "Invalid password.\n\n"),
      em_conn:print(ReqConn, "Login: "),
      ?req_next(login, [got_user])
  end.

do_login(Name, #state{conn=ReqConn}=Request) ->
  {ok, User} = em_user_sup:start_child(ReqConn),
  {ok, Living} = em_living_sup:start_child(Name, {User, ReqConn}),
  ok = em_living:load(Living),
  case em_game:login(Living) of
    {ok, Living} ->
      em_conn:print(ReqConn, "\n"),
      em_living:cmd(Living, "glance"),
      em_conn:print(ReqConn, "\n> "),
      ?req_link(parse, [Living], Request#state{user=User, living=Living});
    {error, user_exists} ->
      em_conn:print(ReqConn, "User already logged in, try again.\n\n"),
      em_conn:print(ReqConn, "Login: "),
      ?req_next(login, [got_user])
  end.

parse(User, Line, #state{conn=ReqConn}=Request) ->
  case Line of
    "" ->
      em_conn:print(ReqConn, "> "),
      ?req_next(parse, [User]);
    "quit" ->
      em_living:cmd(User, "quit"),
      ?req_done;
    Cmd ->
      em_living:cmd(User, Cmd),
      em_conn:print(ReqConn, "\n> "),
      ?req_next(parse, [User])
  end.
