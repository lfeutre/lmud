## Technical Stuff

Processes are heavily used in order to obtain concurrency and fault
isolation.

When someone connects, they initially get a Connection process, which just
handles gen_tcp abstraction and telnet option negotiation, immediately
passing input along to a Session process. The Session has a stack of Request
Handlers, in the form of {M, F, A} tuples. When a line of input is received,
the Session initiates a Request process, passing along the MFA tuple for the
current Request Handler. When the input has been acted upon, the Request
process goes away and the Session awaits the next input line.

During login, the User and Living processes get created, representing the
user account and the in-game incarnation, respectively. The Connection,
Session, User and Living processes are all linked - which currently means
that if one dies, so will the rest. This is to avoid process leakage in case
of errors.

In the future, the intent is to intelligently handle process failures, so
that if for example the Living process dies, the User process can just start
a new one, allowing the player to go on as if nothing happened (mostly).
Likewise, if the User process dies, the Session should kick you back to the
login prompt (without disconnecting the socket), and when you've enter
username and password, you should get reconnected to the existing Living
process.

This kind of error handling is already done for the Game and Room processes,
which keep player info in their state. The Game process will trap exit
signals from the User process, and remove the user from its list of logged-
on users. The Room process will trap exit signals from the Living process
and remove it from its inventory of people in the room.

Room loading happens completely dynamically. When a player logs on, the game
tries to load the room they were in by calling em_room_mgr:get_room(RoomName)
which will first see if the room is already loaded, and if it's not then it
will simply load the room.

To avoid problems, em_room_mgr:get_room/1 will always verify that a Room Pid
is alive before returning it, since a Room might have crashed. If it's not
alive, then again it'll simply be loaded and replaced in the ETS table.

