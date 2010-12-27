%%
%% Not implemented:
%% <r_input> ::= <sentence> { end_of_sentence <sentence> } end_of_line
%% <sentence> ::= <command> { conjunction <command> }
%%
%% Done:
%% <r_input> ::= <r_command>
%% <r_command> ::= <r_command1> | <r_command2>
%% <r_command1> ::= { adverb } verb { adverb }
%%                 [ <noun phrase> { adverb }
%%                   [ preposition <noun phrase> { adverb } ]
%%                 ]
%% <r_command2> ::= { adverb } preposition <noun phrase> { adverb } verb
%%                 { adverb } <noun phrase> { adverb }
%% <noun phrase> ::= <noun group> { preposition <noun group> }
%% <noun group> ::= [ article ] [ integer ] { adjective } ( noun | pronoun | string)
%%
-module(mp_parser).

-export([parse/1, test/0]).

-record(match, {rules=[], tokens=[], ptree=[]}).
-record(error, {etree=[]}).

%% -define(DEBUG(Rule,Rest,Tokens), io:format("[~p | ~p] (~p)~n", [Rule,Rest,Tokens])).
-define(DEBUG(Rule,Rest,Tokens), false).

test() ->
  % noun phrase; simple case
  [{noun_phrase,
    [{noun_group,
      [{article,"the"},
       {adjective,"small"},
       {noun,"box"}
      ]}
     ]}
  ] = parse(["the", "small", "box"]),
  % noun phrase; full
  [{noun_phrase,
    [{noun_group,
      [{article,"the"},
       {adjective,"small"},
       {noun,"box"}]
     },
     {preposition,"on"},
     {noun_group,
       [{article,"the"},
        {noun,"table"}]
     }
    ]}
  ] = parse(["the", "small", "box", "on", "the", "table"]).

parse(String) ->
  Tokens = string:tokens(String, " "),
  case parse([r_input], Tokens) of
    #error{}=Error -> Error;
    #match{ptree=Tree} -> Tree
  end.

%% Rules and tokens are empty, we have a match
parse([], []) ->
  #match{};

%%
%% <r_input> ::= <sentence> { end_of_sentence <sentence> } end_of_line
%%

parse([r_input|Rest], Tokens) ->
  ?DEBUG(r_input,Rest,Tokens),
  case parse([r_command|Rest], Tokens) of
    #match{ptree=Tree}=Match -> Match#match{ptree = [{input, Tree}]};
    #error{}=Error -> Error
  end;

%%
%% <sentence> ::= <command> { conjunction <command> }
%%

%%
%% <r_command> ::= <r_command1> | <r_command2>
%%
parse([r_command|Rest], Tokens) ->
  ?DEBUG(r_command,Rest,Tokens),
  case parse([r_command1|Rest], Tokens) of
    #match{ptree=Tree}=Match -> 
      Match#match{ptree = [{input, Tree}]};
    #error{}=_Error -> 
      case parse([r_command2|Rest], Tokens) of
        #match{ptree=Tree}=Match -> 
          Match#match{ptree = [{input, Tree}]};
        #error{}=Error -> 
          Error
      end
  end;


%%
%% <r_command1> ::= { adverb } verb { adverb }
%%                 [ <noun phrase> { adverb }
%%                   [ preposition <noun phrase> { adverb } ]
%%                 ]
%%

parse([r_command1|Rest], [Current|Tokens]) ->
  ?DEBUG(r_command1,Rest,[Current|Tokens]),
  case adverb(Current) of
    true -> 
      case parse([r_command1|Rest], Tokens) of
        #match{ptree=Tree}=Match -> Match#match{ptree = [{r_command1, [{adverb, Current}|Tree]}]};
        #error{}=_Error -> parse([r_cmd1a|Rest], [Current|Tokens])
      end;
    false ->
      case parse([r_cmd1a|Rest], [Current|Tokens]) of
        #match{ptree=Tree}=Match -> Match#match{ptree = [{r_command1, Tree}]};
        #error{}=Error -> Error
      end
  end;
%% verb
parse([r_cmd1a|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd1a,Rest,[Current|Tokens]),
  case verb(Current) of
    true ->
      case parse([r_cmd1b|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {verb, Current});
        #error{}=Error -> add_error(Error, {verb, ok, Current})
      end;
    false ->
      add_error(#error{}, {verb, failed, Current})
  end;
%% { adverb } [ <noun phrase> ..
parse([r_cmd1b|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd1b,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd1b|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=Error -> add_error(Error, {adverb, ok, Current})
      end;
    false ->
      % cmd1c encapsulates the entire optional end of command1
      case parse([r_noun_phrase, r_cmd1c|Rest], [Current|Tokens]) of
        #match{}=Match -> Match;
        % no match, but it was optional, so go ahead with any remaining rules
        #error{}=_Error -> #match{rules=Rest, tokens=[Current|Tokens], ptree=[{adverb, Current}]}
      end
  end;
%% .. { adverb } [ prepo..
parse([r_cmd1c|Rest], []) ->
  ?DEBUG(r_cmd1c,Rest,[]),
  #match{rules=Rest};
parse([r_cmd1c|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd1c,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd1c|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=Error -> add_error(Error, {adverb, ok, Current})
      end;
    false ->
      % cmd1d encapsulates an optional subset of command1
      case parse([r_cmd1d|Rest], [Current|Tokens]) of
        #match{}=Match -> Match;
        % no match, but it was optional, so go ahead with any remaining rules
        #error{}=_Error -> #match{rules=Rest, tokens=[Current|Tokens], ptree=[{adverb, Current}]}
      end
  end;
%% [ preposition <noun phrase> 
parse([r_cmd1d|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd1d,Rest,[Current|Tokens]),
  case preposition(Current) of
    true ->
      case parse([r_noun_phrase, r_cmd1e|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {preposition, Current});
        #error{}=Error -> add_error(Error, {preposition, ok, Current})
      end;
    false ->
      add_error(#error{}, {preposition, failed, Current})
  end;
%% { adverb } ] ]
parse([r_cmd1e|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd1e,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd1e|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=_Error -> #match{rules = Rest, tokens = [Current|Tokens]}
      end;
    false ->
      #match{rules = Rest, tokens = [Current|Tokens]}
  end;

%%
%% <r_command2> ::= { adverb } preposition <noun phrase> { adverb } verb
%%                 { adverb } <noun phrase> { adverb }
%%

parse([r_command2|Rest], [Current|Tokens]) ->
  ?DEBUG(r_command2,Rest,[Current|Tokens]),
  case adverb(Current) of
    true -> 
      case parse([r_command2|Rest], Tokens) of
        #match{ptree=Tree}=Match -> Match#match{ptree = [{r_command2, [{adverb, Current}|Tree]}]};
        #error{}=_Error -> parse([r_cmd2a|Rest], [Current|Tokens])
      end;
    false ->
      case parse([r_cmd2a|Rest], [Current|Tokens]) of
        #match{ptree=Tree}=Match -> Match#match{ptree = [{r_command2, Tree}]};
        #error{}=Error -> Error
      end
  end;
%% preposition <noun phrase> 
parse([r_cmd2a|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd2a,Rest,[Current|Tokens]),
  case preposition(Current) of
    true ->
      case parse([r_noun_phrase, r_cmd2b|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {preposition, Current});
        #error{}=Error -> add_error(Error, {preposition, ok, Current})
      end;
    false ->
      add_error(#error{}, {preposition, failed, Current})
  end;
%% { adverb } 
parse([r_cmd2b|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd2b,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd2b|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=Error -> add_error(Error, {adverb, ok, Current})
      end;
    false ->
      parse([r_cmd2c|Rest], [Current|Tokens])
  end;
%% verb
parse([r_cmd2c|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd2c,Rest,[Current|Tokens]),
  case verb(Current) of
    true ->
      case parse([r_noun_phrase|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {verb, Current});
        #error{}=Error -> add_error(Error, {verb, ok, Current})
      end;
    false ->
      add_error(#error{}, {verb, failed, Current})
  end;
%% { adverb } <noun phrase> 
parse([r_cmd2d|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd2d,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd2d|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=Error -> add_error(Error, {adverb, ok, Current})
      end;
    false ->
      parse([r_noun_phrase, r_cmd2e|Rest], [Current|Tokens])
  end;
%% { adverb }
parse([r_cmd2e|Rest], [Current|Tokens]) ->
  ?DEBUG(r_cmd2e,Rest,[Current|Tokens]),
  case adverb(Current) of
    true ->
      case parse([r_cmd2e|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adverb, Current});
        #error{}=_Error -> #match{rules = Rest, tokens = [Current|Tokens]}
      end;
    false ->
      #match{rules = Rest, tokens = [Current|Tokens]}
  end;
      
%%
%% <noun phrase> ::= <noun group> { preposition <noun group> }
%%
%% - head of an independent, reusable match group, so we must
%%   always try to parse remaining rules if we match
%%

parse([r_noun_phrase|Rest], Tokens) ->
  ?DEBUG(r_noun_phrase,Rest,Tokens),
  case parse([r_noun_group, r_npa|Rest], Tokens) of
    #match{rules=_Rules, tokens=Toks, ptree=Tree} -> 
      case parse(Rest, Toks) of
        #match{}=Match -> add_node(Match, {noun_phrase, Tree});
        #error{}=Error -> add_error(Error, {noun_phrase, Tree})
      end;
    #error{}=Error -> add_error(Error, {noun_phrase, failed, Tokens})
  end;
%% { preposition <noun group> }
%% - end of match group, and is optional, so need to consider
%%   we might now have any more tokens..
parse([r_npa|Rest], []) ->
  ?DEBUG(r_npa,Rest,[]),
  #match{rules=Rest};
parse([r_npa|Rest], [Current|Tokens]) ->
  ?DEBUG(r_npa,Rest,[Current|Tokens]),
  case preposition(Current) of
    true ->
      case parse([r_noun_group|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {preposition, Current});
        % #error{}=_Error -> parse(Rest, [Current|Tokens])
        #error{}=_Error -> #match{rules=Rest, tokens=[Current|Tokens]}
      end;
    false ->
      %parse(Rest, [Current|Tokens])
      #match{rules=Rest, tokens=[Current|Tokens]}
  end;

%%
%% <noun group> ::= [ article ] [ integer ] { adjective } ( noun | pronoun | string)
%%
%% - head of an independent, reusable match group, so we must
%%   always try to parse remaining rules if we match
%%

parse([r_noun_group|Rest], Tokens) ->
  ?DEBUG(r_noun_group,Rest,Tokens),
  case parse([r_nga|Rest], Tokens) of
    #match{rules=_Rules, tokens=Toks, ptree=Tree} -> 
      % io:format("CONT: [r_noun_group | ~p] (~p, ~p)~n", [Rules, nil, Toks]),
      case parse(Rest, Toks) of
        #match{}=Match -> add_node(Match, {noun_group, Tree});
        #error{}=Error -> add_error(Error, {noun_group, ok, Tree})
      end;
    #error{}=Error -> add_error(Error, {noun_group, failed, Tokens})
  end;
%% [ article ]
parse([r_nga|Rest], [Current|Tokens]) ->
  ?DEBUG(r_nga,Rest,[Current|Tokens]),
  case article(Current) of
    true ->
      case parse([r_ngb|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {article, Current});
        #error{}=Error -> add_error(Error, {article, ok, Current})
      end;
    false ->
      parse([r_ngb|Rest], [Current|Tokens])
  end;
%% [ integer ]
parse([r_ngb|Rest], [Current|Tokens]) ->
  ?DEBUG(r_ngb,Rest,[Current|Tokens]),
  case integer(Current) of
    true ->
      case parse([r_ngc|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {integer, Current});
        #error{}=Error -> add_error(Error, {integer, ok, Current})
      end;
    false ->
      parse([r_ngc|Rest], [Current|Tokens])
  end;
%% { adjective }
parse([r_ngc|Rest], [Current|Tokens]) ->
  ?DEBUG(r_ngc,Rest,[Current|Tokens]),
  case adjective(Current) of
    true -> 
      case parse([r_ngc|Rest], Tokens) of
        #match{}=Match -> add_node(Match, {adjective, Current});
        #error{}=_Error -> parse([r_ngd|Rest], [Current|Tokens])
      end;
    false ->
      parse([r_ngd|Rest], [Current|Tokens])
  end;
%% ( noun | pronoun | string )
parse([r_ngd|Rest], [Current|Tokens]) ->
  ?DEBUG(r_ngd,Rest,[Current|Tokens]),
  case noun(Current) of
    true -> #match{rules=Rest, tokens=Tokens, ptree=[{noun, Current}]};
    false -> 
      case pronoun(Current) of
        true -> #match{rules=Rest, tokens=Tokens, ptree=[{pronoun, Current}]};
        false ->
          case string(Current) of
            true -> #match{rules=Rest, tokens=Tokens, ptree=[{string, Current}]};
            false -> #error{etree=[{noun_pronoun_string, failed, Current}]}
          end
      end
  end;
%%
%% ..and we're done!
%%
parse([], _Tokens) ->
  #error{}.

%% Match Management
add_node(#match{ptree = Tree} = Match, Node) ->
  Match#match{ptree=[Node|Tree]}.

add_error(#error{etree = Tree} = Error, Node) ->
  Error#error{etree=[Node|Tree]}.

%% adjective
adjective("small") -> true;
adjective(_Token) -> false.

%% adverb
adverb("quickly") -> true;
adverb(_Token) -> false.

%% article
article("the") -> true;
article(_Token) -> false.

%% integer
integer(_Token) -> false.

%% noun
noun("box") -> true;
noun("stick") -> true;
noun("table") -> true;
noun(_Token) -> false.

%% preposition
preposition("on") -> true;
preposition("with") -> true;
preposition(_Token) -> false.

%% pronoun
pronoun(_Token) -> false.

%% string
string(_Token) -> false.

%% verb
verb("hit") -> true;
verb(_Token) -> false.

%%
%% Tokenizer utility functions
%%
%% 1> regexp_split_inclusive("How about a nice hawaiian punch?" " +"). 
%% ["How"," ","about"," ","a"," ","nice"," ","hawaiian"," ","punch"]
%%

%% regexp_loop(Str, Parts, Index, []) -> 
%%   lists:reverse([string:substr(Str, Index)] ++ Parts); 
%% regexp_loop(Str, Parts, Index, Rem_Matches) -> 
%%   {NextPt,PtLen} = hd(Rem_Matches), 
%%   regexp_loop(Str, [string:substr(Str, NextPt, PtLen), 
%%                     string:substr(Str, Index, NextPt - Index)] ++ Parts, 
%%                     NextPt + PtLen, 
%%                     tl(Rem_Matches)). 

%% regexp_split_inclusive(Str, Regex) -> 
%%   {match, Matches} = regexp:matches(Str, Regex), 
%%   regexp_loop(Str, [], 1, Matches).
