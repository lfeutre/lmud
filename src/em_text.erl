%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Text processing module.
%%% Provide utility functions for basic text processing like capitalizing
%%% text, etc.
%%% @end
%%% =========================================================================
-module(em_text).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([capitalize/1, title_caps/1, wrap/2, wrapline/2]).

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

