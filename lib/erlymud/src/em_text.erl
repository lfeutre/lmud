-module(erlymud_text).

-export([capitalize/1]).

capitalize(S) ->
  F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
  string:join(lists:map(F, string:tokens(S, " ")), " ").

