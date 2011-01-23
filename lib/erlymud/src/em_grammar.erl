%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Grammar support.
%%% Handle common tasks like articles, pluralization etc for output 
%%% functions, item name manipulation and so on.
%%% @end
%%% =========================================================================
-module(em_grammar).

%% API
-export([add_article/1, number_of/2, number_word/1, pluralize/1, punctuate/1]).

%% Type Specifications
-include("types.hrl").


%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Add 'a', 'an' to a string as appropriate
-spec add_article(string()) -> nonempty_string().
add_article(["a"|Str]) ->
  ["an ", "a"|Str];
add_article(["o"|Str]) ->
  ["an ", "o"|Str];
add_article(["u"|Str]) ->
  ["an ", "u"|Str];
add_article(["i"|Str]) ->
  ["an ", "i"|Str];
add_article(["e"|Str]) ->
  ["an ", "e"|Str];
add_article(Str) ->
  ["a "|Str].

%% @doc Handle proper pluralization in expressions like "0 bottles",
%% "1 bottle", "2 bottles", "3 bottles"
-spec number_of(count(), string()) -> string().
number_of(1, What) ->
  "1 " ++ What;
number_of(Num, What) ->
  integer_to_list(Num) ++ " " ++ pluralize(What).

%% @doc Convert a number to the correct word as appropriate for use in
%% in-game descriptions etc
-spec number_word(count()) -> string().
number_word(Num) ->
  case Num of
    1 -> "one";
    2 -> "two";
    3 -> "three";
    4 -> "four";
    5 -> "five";
    6 -> "six";
    7 -> "seven";
    8 -> "eight";
    9 -> "nine";
    _Other -> integer_to_list(Num)
  end.

%% @doc Return the plural version of a given word
-spec pluralize(string()) -> string().
pluralize([Str|"ff"]) ->  Str ++ "s";
pluralize([Str|"f"]) ->   Str ++ "ves";
pluralize([Str|"s"]) ->   Str ++ "es";
pluralize([Str|"x"]) ->   Str ++ "es";
pluralize(Str) ->         Str ++ "s".

%% @doc Punctuate a string if it ends with a-z or A-Z
-spec punctuate(string()) -> string().
punctuate(Str) ->
  NewStr = string:strip(Str, right),
  punctuate(NewStr, lists:last(NewStr)).
punctuate(Str, Last) when Last >= $a, Last =< $z;
                          Last >= $A, Last =< $Z ->
  Str ++ ".";
punctuate(Str, _) ->
  Str.
