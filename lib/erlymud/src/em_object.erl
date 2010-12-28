%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010 Johan Warlander
%%% @doc Interface for dealing with objects in the game; here we define the
%%%      things that are common for all things in the game - plain items,
%%%      weapons, NPCs, PCs etc.
%%% @end
%%% =========================================================================
-module(em_object).

-export([create/1, create/2,
%         has_id/2, has_plural_id/2,
         add_id/2, add_primary_id/2, %add_plural/2,
         add_adj/2, add_primary_adj/2,
         set_name/2 %, set_proper_name/2, set_plural/1, set_unique/1,
%         short/1, the_short/1, a_short/1, plural_short/1,
%         long/1,
        ]).

-record(object, {ids=[], plurals=[], adjs=[], 
                 primary_id="", primary_adj="",
                 short="nondescript thing", long="", 
                 proper_name = "",
                 quantity = 0,
                 is_plural=false, is_unique=false}).


%%===========================================================================
%% API Functions
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Create new object with 'short desc' string; we yank out adjs and an 
%%      id from there.
%%      Example: 
%%        "small wooden table" -> adjs=["small", "wooden"], ids=["table"]
%% @spec create(Name::string()) -> object()
%% @end
%%---------------------------------------------------------------------------
create(Name) ->
  set_name(#object{}, Name).

%%---------------------------------------------------------------------------
%% @doc New object with list of ids and adjs
%% @spec create(Ids::list(), Adjs::list()) -> object()
%% @end
%%---------------------------------------------------------------------------
create(Ids, Adjs) ->
  resync_names(#object{ids=Ids, adjs=Adjs}).

%%---------------------------------------------------------------------------
%% @doc Add an id to the specified object
%% @spec add_id(Ob::object(), Id::object()) -> object()
%% @end
%%---------------------------------------------------------------------------
add_id(#object{ids=Ids} = Ob, Id) ->
  Ob#object{ids = Ids ++ [Id]}.

add_primary_id(#object{ids=Ids} = Ob, Id) ->
  Ob#object{ids = [Id|Ids]}.

add_adj(#object{adjs=Adjs} = Ob, Adj) ->
  Ob#object{adjs = Adjs ++ [Adj]}.

add_primary_adj(#object{adjs=Adjs} = Ob, Adj) ->
  Ob#object{adjs = Adjs ++ [Adj]}.

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
