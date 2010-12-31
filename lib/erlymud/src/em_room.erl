-module(em_room).

-behaviour(gen_server).

-export([start_link/2, start/2, add_exit/3, get_exit/2, get_exits/1, 
         describe/1, enter/2, leave/2, print_while/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {title="A small room", 
                desc="This is a small, non-descript room.", 
                people=[], exits=[]}).

start_link(Title, Desc) ->
  gen_server:start_link(?MODULE, [Title, Desc], []).

start(Title, Desc) ->
  gen_server:start(?MODULE, [Title, Desc], []).

add_exit(Room, Dir, Dest) ->
  gen_server:call(Room, {add_exit, Dir, Dest}).

get_exit(Room, Dir) ->
  gen_server:call(Room, {get_exit, Dir}).

get_exits(Room) ->
  gen_server:call(Room, get_exits).

describe(Room) ->
  gen_server:call(Room, describe).

enter(Room, Who) ->
  gen_server:call(Room, {enter, Who}).

leave(Room, Who) ->
  gen_server:call(Room, {leave, Who}).

print_while(Room, Pred, Format, Args) ->
  gen_server:cast(Room, {print_while, Pred, Format, Args}).

% --

init([Title, Desc]) ->
  {ok, #state{title=Title, desc=Desc}}.

handle_call({add_exit, Dir, Dest}, _From, #state{exits=Exits} = State) ->
  {reply, ok, State#state{exits=[{Dir, Dest}|Exits]}};
handle_call({get_exit, Dir}, _From, #state{exits=Exits} = State) ->
  Response = case lists:keyfind(Dir, 1, Exits) of
               false -> {error, not_found};
               Exit -> {ok, Exit}
             end,
  {reply, Response, State};
handle_call(get_exits, _From, #state{exits=Exits} = State) ->
  {reply, Exits, State};
handle_call(describe, _From, #state{title=Title, desc=Desc} = State) ->
  Response = [Title, "\n", Desc, "\n"],
  {reply, Response, State};
handle_call({enter, Who}, _From, #state{people=People} = State) ->
  {reply, ok, State#state{people=[Who|People]}};
handle_call({leave, Who}, _From, #state{people=People} = State) ->
  {reply, ok, State#state{people=People -- [Who]}}.

handle_cast({print_while, Pred, Format, Args}, State) ->
  People = lists:filter(Pred, State#state.people),
  PrintFun = fun(Liv) -> em_living:print(Liv, Format, Args) end,
  lists:map(PrintFun, People),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

