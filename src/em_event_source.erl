-module(em_event_source).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{add_event_listener, 2},
   {notify, 2}
  ];
behaviour_info(_Other) ->
  undefined.
