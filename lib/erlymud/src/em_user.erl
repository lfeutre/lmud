-module(em_user).

-export([new/2, name/1, socket/1, room/1]).

-record(user, {name, socket, room}).

new(Name, Socket) ->
  #user{name=Name, socket=Socket, room="room1"}.

name(#user{name = Name}) ->
  Name.

socket(#user{socket = Socket}) ->
  Socket.

room(#user{room = Room}) ->
  Room.
