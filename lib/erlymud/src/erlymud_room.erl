-module(erlymud_room).

-export([new/2]).

-record(room, {title, desc}).

new(Title, Desc) ->
  #room{title = Title, desc = Desc}.
