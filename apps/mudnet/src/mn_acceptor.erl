%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Accept new TCP connections.
%%% This gen_server will wait in a gen_tcp:accept() call until someone makes
%%% a connection, then it will immediately launch a new mn_conn server and
%%% hand over the new socket for further processing, before it returns with
%%% a timeout so that it'll go back into accept mode again.
%%% @end
%%% =========================================================================
-module(mn_acceptor).

-behaviour(gen_server).

%% API exports
-export([start_link/1]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("apps/lmud/include/types.hrl").
-include("apps/mudnet/include/state.hrl").

%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Start an acceptor process and return the pid.
-spec start_link(socket()) ->
  {ok, pid()} | {error, term()} | ignore.
start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).


%% ==========================================================================
%% gen_server callbacks
%% ==========================================================================

%% @doc Set up local state, then return with a timeout. We use a 0 timeout to
%% allow the mn-acceptor-pool to go on with its business, while we start an
%% accept call to wait for a connection.
-spec init([socket()]) -> {ok, #state_acceptor{}, integer()}.
init([LSock]) ->
  {ok, #state_acceptor{lsock = LSock}, 0}.

%% @doc We don't listen to any calls, just empty the queue.
-spec handle_call(any(), pid(), #state_acceptor{}) -> {reply, ok, #state_acceptor{}}.
handle_call(_Req, _From, State) ->
  {reply, ok, State}.

%% @doc We don't listen to any casts, just empty the queue.
-spec handle_cast(any(), #state_acceptor{}) -> {noreply, #state_acceptor{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @doc Handle other messages. If it's a timeout, start listening on the
%% given socket; otherwise, just empty the queue.
-spec handle_info(timeout | any(), #state_acceptor{}) ->
  {noreply, #state_acceptor{}, integer()} | {stop, normal, #state_acceptor{}}.
handle_info(timeout, #state_acceptor{lsock=LSock}=State) ->
  case gen_tcp:accept(LSock) of
    {ok, Socket} ->
      {ok, Conn} = 'mn-conn-sup':start_child(Socket),
      case gen_tcp:controlling_process(Socket, Conn) of
        ok ->
          ok;
        {error, Reason} ->
          exit(Conn, {error, Reason})
      end,
      {noreply, State, 0};
    {error, closed} ->
      {stop, normal, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(any(), #state_acceptor{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(string(), #state_acceptor{}, any()) -> {ok, #state_acceptor{}}.
code_change(_Vsn, State, _Extra) ->
  {ok, State}.

