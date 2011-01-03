-module(em_room_mgr).

-behaviour(gen_server).

-export([start_link/0, get_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rooms}).

%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_room(Name) ->
  gen_server:call(?SERVER, {get_room, Name}).

%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call({get_room, _Name}, _From, State) ->
  {reply, {error, not_found}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
