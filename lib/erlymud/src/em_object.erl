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

-record(object, {ids=[]::id_list(), plurals=[]::id_list(), 
                 adjs=[]::adj_list(), 
                 primary_id=""::id(), primary_adj=""::adj(),
                 short="nondescript thing"::string(), long=""::string(), 
                 show_in_room = ""::string(),
                 proper_name = ""::string(),
                 quantity = 0::count(),
                 is_attached=false::boolean(), 
                 is_plural=false::boolean(), 
                 is_unique=false::boolean(),
                 template="dummy"::string()}).

-opaque object() :: #object{}.
-export_type([object/0]).

%% Type Specifications
-include("types.hrl").
-type name() :: string().
-type id() :: string().
-type id_list() :: [id()].
-type adj() :: string().
-type adj_list() :: [adj()].

%% ===========================================================================
%% API Functions
%% ===========================================================================

%% @doc Create new object with 'short desc' string; we yank out adjs and an 
%%      id from there.
%%      Example: 
%%        "small wooden table" -> adjs=["small", "wooden"], ids=["table"]
-spec new(name()) -> object().
new(Name) ->
  set_name(#object{}, Name).

%% @doc New object with list of ids and adjs
-spec new(id_list(), adj_list()) -> object().
new(Ids, Adjs) ->
  resync_names(#object{ids=Ids, adjs=Adjs}).

-spec load(name()) -> {ok, object()} | {error, not_found}.
load(Name) ->
  File = filename:join([em_game:data_dir(), "objects",
                        Name ++ ".dat"]),
  load_object(Name, File).

-spec load_object(name(), file_path()) -> {ok, object()} | {error, not_found}.
load_object(Name, Filename) ->
  io:format("loading object: ~s~n", [Filename]),
  case file:consult(Filename) of
    {ok, Data} ->
      Ob = make_object(Data, #object{template=Name}),
      {ok, Ob};
    {error, _Reason} ->
      {error, not_found}
  end.

-spec make_object(proplist(), object()) -> object().
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

-spec load_obs([name()]) -> [object()].
load_obs(Names) ->
  load_obs(Names, []).

-spec load_obs([name()], [object()]) -> [object()].
load_obs([], ObList) ->
  ObList;
load_obs([Name|Rest], ObList) ->
  {ok, Ob} = load(Name),
  load_obs(Rest, [Ob|ObList]).

-spec a_short(object()) -> string().
a_short(#object{short=Short}) ->
  em_grammar:add_article(Short).

-spec the_short(object()) -> nonempty_string().
the_short(#object{short=Short}) ->
  ["the "|Short].

-spec long(object()) -> string().
long(#object{long=Long}) ->
  Long.

-spec has_id(object(), id()) -> boolean().
has_id(#object{ids=Ids}, Id) ->
  lists:member(Id, Ids).

-spec has_plural_id(object(), id()) -> boolean().
has_plural_id(#object{plurals=Plurals}, Id) ->
  lists:member(Id, Plurals).

%% @doc Add an id to the specified object
-spec add_id(object(), id()) -> object().
add_id(#object{ids=Ids} = Ob, Id) ->
  Ob#object{ids = Ids ++ [Id]}.

-spec add_primary_id(object(), id()) -> object().
add_primary_id(#object{ids=Ids} = Ob, Id) ->
  resync_names(Ob#object{ids = [Id|Ids]}).

-spec add_adj(object(), adj()) -> object().
add_adj(#object{adjs=Adjs} = Ob, Adj) ->
  Ob#object{adjs = Adjs ++ [Adj]}.

-spec add_primary_adj(object(), adj()) -> object().
add_primary_adj(#object{adjs=Adjs} = Ob, Adj) ->
  resync_names(Ob#object{adjs = Adjs ++ [Adj]}).

%% @doc Set name of an object given a plain "short desc" string, will
%%      replace any previous ids / adjs
-spec set_name(object(), string()) -> object().
set_name(Ob, Name) ->
  Toks = string:tokens(Name, " "),
  set_name(Ob, Toks, [], []).

-spec set_name(object(), [string()], id_list(), adj_list()) -> object().
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

-spec set_long(object(), string()) -> object().
set_long(Ob, Long) ->
  Ob#object{long=Long}.

-spec set_show_in_room(object(), string()) -> object().
set_show_in_room(Ob, Desc) ->
  Ob#object{show_in_room=Desc}.

-spec show_in_room(object()) -> string().
show_in_room(#object{show_in_room="", short=Short}) ->
  A_Short =em_grammar:add_article(Short),
  [em_text:capitalize(A_Short), " lies here, discarded.\n"];
show_in_room(#object{show_in_room=Desc}) ->
  Len = string:len(Desc),
  case string:rchr(Desc, $\n) of
    Len -> Desc;
    _Other -> string:concat(Desc, "\n")
  end.

-spec set_attached(object(), boolean()) -> object().
set_attached(Ob, Flag) ->
  Ob#object{is_attached=Flag}.

-spec is_attached(object()) -> boolean().
is_attached(Ob) ->
  Ob#object.is_attached.

-spec get_template(object()) -> name().
get_template(Ob) ->
  Ob#object.template.

%%===========================================================================
%% Internal Functions
%%===========================================================================

%% @doc Set primary id, primary adj and short description based on the
%%      ids and adjs lists.
-spec resync_names(object()) -> object().

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
