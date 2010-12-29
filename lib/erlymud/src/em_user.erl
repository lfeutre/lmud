-module(em_user).

-export([new/2, write/2, name/1, set_name/2, socket/1, room/1]).

-record(user, {name, socket, room}).

new(Name, Socket) ->
  #user{name=Name, socket=Socket, room="room1"}.

write(#user{socket = Socket}, Data) ->
  gen_tcp:send(Socket, Data).

name(#user{name = Name}) ->
  Name.

set_name(#user{} = User, Name) ->
  User#user{name = Name}.

socket(#user{socket = Socket}) ->
  Socket.

room(#user{room = Room}) ->
  Room.
