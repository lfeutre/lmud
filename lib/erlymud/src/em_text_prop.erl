-module(em_text_prop).
-include_lib("proper/include/proper.hrl").

% When first char() is not a-z, string should remain the same
prop_capitalize_non_az() ->
  ?FORALL(S, string(),
    ?IMPLIES(string:len(S) > 0,
      ?IMPLIES(begin [Ch|_] = S, Ch < 'a' orelse Ch > 'z' end,
        em_text:capitalize(S) == S))).
