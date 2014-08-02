-module(em_util).
-export([get_version/0]).

get_version() ->
  {ok,[App]}=file:consult("lib/erlymud/src/erlymud.app.src"),
  proplists:get_value(vsn,element(3,App)).
