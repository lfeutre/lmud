%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Interface for dealing with objects in the game.
%%% Here we define the things that are common for all types of items in the 
%%% game - plain items, weapons, NPCs, PCs etc.
%%% @end
%%% =========================================================================
-module(em_object).

-export([new/1, new/2, load/1, load_obs/1,
         has_id/2, has_plural_id/2,
         add_id/2, add_primary_id/2, %add_plural/2,
         add_adj/2, add_primary_adj/2,
         set_name/2, % set_proper_name/2, set_plural/1, set_unique/1,
         set_long/2,
         a_short/1, the_short/1,
%         short/1, the_short/1, a_short/1, plural_short/1,
         long/1, show_in_room/1,
         is_attached/1, set_attached/2,
         get_template/1
        ]).

-record(object, {ids=[], plurals=[], adjs=[], 
                 primary_id="", primary_adj="",
                 short="nondescript thing", long="", 
                 show_in_room = "",
                 proper_name = "",
                 quantity = 0,
                 is_attached=false, is_plural=false, is_unique=false,
                 template}).


%%===========================================================================
%% API Functions
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Create new object with 'short desc' string; we yank out adjs and an 
%%      id from there.
%%      Example: 
%%        "small wooden table" -> adjs=["small", "wooden"], ids=["table"]
%% @spec new(Name::string()) -> object()
%% @end
%%---------------------------------------------------------------------------
new(Name) ->
  set_name(#object{}, Name).

%%---------------------------------------------------------------------------
%% @doc New object with list of ids and adjs
%% @spec new(Ids::list(), Adjs::list()) -> object()
%% @end
%%---------------------------------------------------------------------------
new(Ids, Adjs) ->
  resync_names(#object{ids=Ids, adjs=Adjs}).

load(Name) ->
  File = filename:join([code:priv_dir(erlymud), "objects",
                        Name ++ ".dat"]),
  load_object(Name, File).

load_object(Name, Filename) ->
  io:format("loading object: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      Ob = make_object(Data, #object{template=Name}),
      {ok, Ob};
    {error, _Reason} ->
      {error, not_found}
  end.

make_object([], Ob) ->
  Ob;
make_object([{primary_id, Id}|Data], Ob) ->
  make_object(Data, add_primary_id(Ob, Id));
make_object([{primary_adj, Adj}|Data], Ob) ->
  make_object(Data, add_primary_adj(Ob, Adj));
make_object([{show_in_room, Desc}|Data], Ob) ->
  make_object(Data, set_show_in_room(Ob, Desc));
make_object([{long, Long}|Data], Ob) ->
  make_object(Data, set_long(Ob, Long));
make_object([{is_attached, Flag}|Data], Ob) ->
  make_object(Data, set_attached(Ob, Flag));
make_object([_Other|Data], Ob) ->
  make_object(Data, Ob).

load_obs(Names) ->
  load_obs(Names, []).

load_obs([], ObList) ->
  ObList;
load_obs([Name|Rest], ObList) ->
  {ok, Ob} = load(Name),
  load_obs(Rest, [Ob|ObList]).

a_short(#object{short=Short}) ->
  em_grammar:add_article(Short).

the_short(#object{short=Short}) ->
  ["the ", Short].

long(#object{long=Long}) ->
  Long.

has_id(#object{ids=Ids}, Id) ->
  lists:member(Id, Ids).

has_plural_id(#object{plurals=Plurals}, Id) ->
  lists:member(Id, Plurals).

%%---------------------------------------------------------------------------
%% @doc Add an id to the specified object
%% @spec add_id(Ob::object(), Id::object()) -> object()
%% @end
%%---------------------------------------------------------------------------
add_id(#object{ids=Ids} = Ob, Id) ->
  Ob#object{ids = Ids ++ [Id]}.

add_primary_id(#object{ids=Ids} = Ob, Id) ->
  resync_names(Ob#object{ids = [Id|Ids]}).

add_adj(#object{adjs=Adjs} = Ob, Adj) ->
  Ob#object{adjs = Adjs ++ [Adj]}.

add_primary_adj(#object{adjs=Adjs} = Ob, Adj) ->
  resync_names(Ob#object{adjs = Adjs ++ [Adj]}).

%%---------------------------------------------------------------------------
%% @doc Set name of an object given a plain "short desc" string, will
%%      replace any previous ids / adjs
%% @spec set_name(Ob::object(), Name::string()) -> object()
%% @end
%%---------------------------------------------------------------------------
set_name(Ob, Name) ->
  Toks = string:tokens(Name, " "),
  set_name(Ob, Toks, [], []).

set_name(Ob, [], [], []) ->
  resync_names(Ob);
set_name(Ob, [], Ids, Adjs) ->
  Plurals = lists:map(fun em_grammar:pluralize/1, Ids),
  resync_names(Ob#object{ids=Ids, plurals=Plurals, adjs=Adjs});
%
set_name(Ob, [LastToken], Ids, Adjs) ->
  set_name(Ob, [], Ids ++ [LastToken], Adjs);
set_name(Ob, [Token|Toks], Ids, Adjs) ->
  set_name(Ob, Toks, Ids, Adjs ++ [Token]).

set_long(Ob, Long) ->
  Ob#object{long=Long}.

set_show_in_room(Ob, Desc) ->
  Ob#object{show_in_room=Desc}.

show_in_room(#object{show_in_room="", short=Short}) ->
  A_Short =em_grammar:add_article(Short),
  [em_text:capitalize(A_Short), " lies here, discarded.\n"];
show_in_room(#object{show_in_room=Desc}) ->
  Len = string:len(Desc),
  case string:rchr(Desc, $\n) of
    Len -> Desc;
    _Other -> string:concat(Desc, "\n")
  end.

set_attached(Ob, Flag) ->
  Ob#object{is_attached=Flag}.

is_attached(Ob) ->
  Ob#object.is_attached.

get_template(Ob) ->
  Ob#object.template.

%%===========================================================================
%% Internal Functions
%%===========================================================================

%% @doc Set primary id, primary adj and short description based on the
%%      ids and adjs lists.
%% @spec resync_names(Ob) -> object()

%% use proper_name, if there is one
resync_names(#object{proper_name=[Ch|Str]} = Ob) ->
  Ob#object{short=[Ch|Str]};
%% otherwise, use first id and first adj
resync_names(#object{ids=[Id|_Ids], adjs=[Adj|_Adjs]} = Ob) ->
  Ob#object{primary_id=Id, primary_adj=Adj, short=Adj ++ " " ++ Id};
%% no adjs, but at least one id? use that.
resync_names(#object{ids=[Id|_Ids], adjs=[]} = Ob) ->
  Ob#object{primary_id=Id, short=Id};
%% we have nothing..
resync_names(#object{ids=[]} = Ob) ->
  Ob#object{short="nondescript thing"}.
