-module(em_text).
-include("telnetcolors.hrl").
-export([capitalize/1, colorize/1]).

capitalize(S) ->
  F = fun([H|T]) -> [string:to_upper(H) | string:to_lower(T)] end,
  string:join(lists:map(F, string:tokens(S, " ")), " ").

colorize(Input) ->
  colorize(text, lists:flatten(Input), []).

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

color_lookup("ROOM_TITLE")    -> ?F_RED;
color_lookup("ROOM_EXITS")    -> ?F_GREY;
color_lookup("RESET")         -> ?RESET;
color_lookup(Code)            -> ["<<MISSING_COLOR:",Code,">>"].
