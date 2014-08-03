-module(em_event_listener).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{handle_event, 1}];
behaviour_info(_Other) ->
  undefined.
