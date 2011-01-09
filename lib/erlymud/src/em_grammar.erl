%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Grammar support.
%%% Handle common tasks like articles, pluralization etc for output 
%%% functions, item name manipulation and so on.
%%% @end
%%% =========================================================================

-module(em_grammar).

-export([add_article/1, number_of/2, number_word/1, pluralize/1, punctuate/1]).

%% --------------------------------------------------------------------------
%% @spec add_article(Str::string()) -> string()
%% @doc Add 'a', 'an' to a string as appropriate
%% @end
%% --------------------------------------------------------------------------
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

%% --------------------------------------------------------------------------
%% @spec number_of(Num::integer(), What::string()) -> string()
%% @doc Handle proper pluralization in expressions like "0 bottles",
%%      "1 bottle", "2 bottles", "3 bottles"
%% @end
%% --------------------------------------------------------------------------
number_of(1, What) ->
  "1 " ++ What;
number_of(Num, What) ->
  integer_to_list(Num) ++ " " ++ pluralize(What).

%% --------------------------------------------------------------------------
%% @spec number_word(Num::integer()) -> string()
%% @doc Convert a number to the correct word as appropriate for use in
%%      in-game descriptions etc
%% @end
%% --------------------------------------------------------------------------
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

%% --------------------------------------------------------------------------
%% @spec pluralize(Str::string()) -> string()
%% @doc Return the plural version of a given word
%% @end
%% --------------------------------------------------------------------------
pluralize([Str|"ff"]) ->  Str ++ "s";
pluralize([Str|"f"]) ->   Str ++ "ves";
pluralize([Str|"s"]) ->   Str ++ "es";
pluralize([Str|"x"]) ->   Str ++ "es";
pluralize(Str) ->         Str ++ "s".

%% --------------------------------------------------------------------------
%% @spec punctuate(Str::string()) -> string()
%% @doc Punctuate a string if it ends with a-z or A-Z
%% @end
%% --------------------------------------------------------------------------
punctuate(Str) ->
  NewStr = string:strip(Str, right),
  punctuate(NewStr, lists:last(NewStr)).
punctuate(Str, Last) when Last >= $a, Last =< $z;
                          Last >= $A, Last =< $Z ->
  Str ++ ".";
punctuate(Str, _) ->
  Str.
