%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Text processing module.
%%% Provide utility functions for basic text processing like capitalizing,
%%% colorizing text, etc.
%%% @end
%%% =========================================================================
-module(em_text).
-include_lib("eunit/include/eunit.hrl").
-include("telnetcolors.hrl").

%% API
-export([capitalize/1, colorize/1, title_caps/1, wrap/2, wrapline/2]).

%% Type Specifications
-include("types.hrl").

%% ==========================================================================
%% API Functions
%% ==========================================================================

%% @doc Return given string, with first letter in upper case.
-spec capitalize(string()) -> string().
capitalize([]) ->
  [];
capitalize([H|T]) ->
  [string:to_upper(H) | T].

%% @doc Return given string, with first letter of each word in upper case.
-spec title_caps(string()) -> string().
title_caps(S) ->
  F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
  string:join(lists:map(F, string:tokens(S, " ")), " ").

%% @doc Colorize the given string by replacing special color tags with ANSI
%% color codes. The tags look like %^MY_TAG%^, and have to be defined below.
%% @todo Move color tags into a gen_server where it's possible to register
%% new tags dynamically, and save/load the mappings.
-spec colorize(iolist()) -> iolist().
colorize(Input) ->
  colorize(text, lists:flatten(Input), []).

-spec colorize(text|parse, string(), iolist()) -> iolist().
% plain text; collect output
colorize(text, [], Output) ->
  lists:reverse(Output) ++ ?RESET;
colorize(text, [$%,$^|Input], Output) ->
  colorize(parse, Input, Output, []);
colorize(text, [Ch|Input], Output) ->
  colorize(text, Input, [Ch|Output]).

% parse a color code
colorize(parse, [], Output, _Code) ->
  lists:reverse(Output) ++ ?RESET;
colorize(parse, [$%,$^|Input], Output, Code) ->
  Color = color_lookup(lists:reverse(Code)),
  colorize(text, Input, [Color|Output]);
colorize(parse, [Ch|Input], Output, Code) ->
  colorize(parse, Input, Output, [Ch|Code]).

-spec color_lookup(string()) -> string().
color_lookup("ROOM_TITLE")    -> ?F_RED;
color_lookup("ROOM_EXITS")    -> ?F_GREY;
color_lookup("RESET")         -> ?RESET;
color_lookup(Code)            -> ["<<MISSING_COLOR:",Code,">>"].

%% @doc Wrap a string at the specified length, inserting newline characters.
-spec wrapline(iolist(), count()) -> string().
wrapline(StrList, Len) ->
  string:join(wrap(StrList, Len), "\n").

%% @doc Wrap a string at the specified length, returning a list of lines to
%% be handled as appropriate.
-spec wrap(iolist(), count()) -> [string()].
wrap(StrList, Len) ->
  Str = lists:flatten(StrList),
  Tokens = string:tokens(Str, " "),
  lists:reverse(wrap(Len, Tokens, [])).
wrap(_Len, [], Result) ->
  Result;
wrap(Len, [Word|Rest], []) ->
  wrap(Len, Rest, [Word]);
wrap(Len, [Word|Rest], [Line|Result]) when length(Word) + length(Line) < Len ->
  wrap(Len, Rest, [string:join([Line, Word], " ")|Result]);
wrap(Len, [Word|Rest], Result) ->
  wrap(Len, Rest, [Word|Result]).
  

%% ==========================================================================
%% Tests
%% ==========================================================================

title_caps_test_() ->
  [?_assertMatch("Cat", title_caps("cat")),
   ?_assertMatch("Cats And Dogs", title_caps("cats and dogs"))
  ].

wrap_test_() -> 
  [?_assertMatch([], wrap("", 10)),
   ?_assertMatch(["cat"], wrap("cat", 10)),
   ?_assertMatch(["the cat is", "chasing", "the dog"], 
      wrap("the cat is chasing the dog", 10)),
   ?_assertMatch(["the cat is", "chasing", "the dog"], 
      wrap([["the "], ["cat is ", ["chasing the dog"]]], 10))
  ].

