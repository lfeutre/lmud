# ErlyMUD

ErlyMUD is a rather minimalistic MUD server, written in Erlang and making
use of the excellent OTP libraries. The aim is to have solid support for
exploration and roleplaying, within a highly fault-tolerant environment
where system crashes or reboots are more of an exotic curiosity than a
commonplace thing.

Erlang/OTP is an excellent match for MUD development, with strong support
for concurrency through light-weight processes, hot code upgrades, near-
transparent mechanisms for distributed computing, etc.

This project started out as a way for me to learn Erlang/OTP, within a
context that I knew something about. Since those early days three weeks
ago, it's actually developed into something marginally usable. We'll see
where this goes.


## Getting Started

ErlyMUD expects that you have Erlang 17 and rebar installed. Once you have
that taken care of, follow these steps:

  1. Download the latest:

     ```sh
     $ git clone https://github.com/lfex/erlymud.git
     ```

  1. Change directory, compile the source, and make a release:

     ```sh
     $ cd erlymud
     $ make rel
     ```

  1. Start up ErlyMUD:

     ```sh
     $ make run
     ```

  1. From another terminal, connect to the game and create a user:

     ```sh
     $ telnet localhost 2155
     ```

  1. Have fun!


## Current Status

ErlyMUD presents what's currently at least a minimally playable environment.
It's possible to connect, create a password-protected user account, and log
in to the game. Once there, you can communicate with other players, walk
around between rooms, and handle items.

## Game Features

  * Rooms have a title, and brief + long descriptions.
    * The brief description is used when walking into / through a room,
      and is intended to only show the most obvious features of the room.
    * When using the "look" command, the long description will be shown
      instead.
  * Items can be picked up, dropped and looked at;
    * "get sword", "drop sword"
    * If an item is 'attached', it can't be picked up. Instead it belongs to
      the room, and is used to add detail descriptions so that you can for
      example do "look painting" and see a more in-depth description of that
      part of the room.
  * You can see who is logged on, talk to other players, and use emotes.
    * "who", "tell <who> <what>", "say <something>", "emote <something>"
  * Navigation is currently restricted to the basics;
    * "go west", "west", etc..


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


## Testing

Currently PropEr is being used together with the ta-qc branch of rebar for
testing of ErlyMUD modules. To run tests, first download / compile PropEr,
set it up in your Erlang environment, and then just do "./rebar qc" in the
top-level directory of ErlyMUD.

On a more general level, ErlyMUD has been at least occasionally tested in the
following environments; please do report if you get it running in a different
configuration:

  * Ubuntu 10.04 (x86_64), Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
  * Ubuntu 10.10 (x86_64), Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
  * Mac OS X 10.6.6, Erlang/OTP R14B01
    * Jeejo Pallayi <jeejo@pallayi.com>
  * Windows XP SP3, Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
    * NOTE: Currently requires manual compilation of *.erl files,
            then they have to be moved to lib\erlymud\ebin\ before
            starting everything:

        ```sh
        $ cd lib\erlymud\src
        $ erlc -I ..\include\ <file1>.erl ... <fileN>.erl
        $ move *.beam ..\ebin\
        $ cd ..\..\..\
        $ erl -pa lib\erlymud\ebin
        1> systools:make_script("erlymud-0.3.2", [local]).
        2> q().
        $ erl -boot erlymud-0.3.2
        ```
