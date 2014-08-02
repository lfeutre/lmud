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
