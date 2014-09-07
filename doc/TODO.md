# TODO

This is the new TODO list. See the bottom of this document for the
old TODO.

## Bugs

* When writing non-wrappd (short) descriptions, extra quotes are added to
  the data files.


## Usability

* [ ] Convert stdout messages to log messages, conditional on debug setting
* [ ] add "connected since" and "member since" to user data
* [ ] add support for a "NEWS" file
  * [ ] have news read for a NEWS file in the game directory
  * [ ] every time the news func is called, read the file


## Regions, Towns, and Dungeons

* [ ] Add support for the ability to group rooms
  * [ ] groups need to have the ability to be named
  * [ ] a group can have entrances
* [ ] Add support for group types:
  * [ ] house
  * [ ] dungeon
  * [ ] neighborhood
  * [ ] town/city
  * [ ] region (political)
  * [ ] country
  * [ ] geological formation (e.g., mountain, valley, cave system)
  * [ ] ecological area (e.g., woods, jungle, tundra)
  * [ ] region (physical)
  * We would need to figure out how interfaces between groups work, e.g.:
    * houses in neighbords
    * neighborhoos in cities
    * cities in regions
    * regions in countries
  * We would need to figure out how group overlaps work, e.g.:
    * a town in a valley with dungeons that connect to mountain caves
    * a town that is both in a valley and on a lake
    * a forest that is in two countries


## Command Parsing

Support the following additional 'info' commands:

* [ ] add "whoami" command with alias "id"
* [ ] add support for "whois <username>" and retun new user data (e.g.,
      "member since" & "connected since") as well as existing user data
      (such as current location)


Support the following movement commands:

* wizard-level
  * [ ] teleport ``<room>``
  * [ ] teleport ``<player>`` (to player location)
  * [ ] teleport ``<player> <room>`` (teleport a player to a room)
  * [ ] teleport ``<player1> <player2>`` (teleport player1 to player2's
    location)


Support the following world-creation commands:

* aesir-level
  * [ ] title ``<room name> <title text>``
  * [ ] brief ``<room name> <brief text>``
  * [ ] desc ``<room name> <desc text>``


Support the following object-creation commands:

* wizard-level (valid only for objects in the same room as the wizard)
  * [ ] create object ``<name>``
  * [ ] title ``<object name> <title text>``
  * [ ] brief ``<object name> <brief text>``
  * [ ] desc ``<object name> <desc text>``
* wizard-level (for any object in the game)
  * [ ] create object ``<room name> <name>``
  * [ ] title ``<room name> <object name> <title text>``
  * [ ] brief ``<room name> <object name> <brief text>``
  * [ ] desc ``<room name> <object name> <desc text>``


Add commands for building an ecnomic system:

* vanir-level
  * [ ] setting up finance
  * [ ] markets
  * [ ] trade
  * [ ] auctions


Support the following development workflow:

* [ ] BUG - when calling an alias, pass (and merge) the alias args to the
      command args


Support the following command-related commands:

* [ ] a function will be needed to list the command, it's help, its
      destination module, it's destination function, the lowest permission
      role allowed in order to call it, and maybe its aliases as well ...
* [ ] possibly an is-alias function?


## Breakout and Dependencies

* [ ] Add lager for use in debugging
* [ ] Look at using a plugin system
  * Heinz has written one here: https://github.com/Licenser/eplugin
    * this uses different mechanisms than the one that lfetool defines
  * port to LFE as lplug?
    * provide the option of either using ETS tables like eplugin, or
    * use behaviors and ``beam_lib`` calls
* [ ] Remote shell generalization
  * Move telnet shell into own project
  * is there another telnet shell we could use a dependency?
    * otp /lib/common_test/test/telnet_server.erl?
  * add support for an SSH MUD server via ssh_sshd?
    * what would it take to modify ssh_sshd?


## Permissions

For a massive MUD, there will likely need to be a greater number of
permissions. Possible permission levels:

* complete world control:
  * creating rooms/tunnels/open spaces/etc,
  * creating creatures
  * name: Deva? Valar? Aesir?
  * command prefix: d@- ?
* world-modification and defying laws of physics:
  * creating things, teleporting people/things, changing rooms/etc.
  * name: wizard? wysard?
  * command prefix: w@- ?
* control over worlds' commodities, goods, money, markets, harvests;
  also creating game groups (towns, regions, etc.):
  * name: Asura? Vanir?
  * command prefix: a@- ?
* NPC
* Player

Maybe have a special "room" for objects that haven't been created, that only
wizards can pull stuff from (but not enter) and only gods can actually enter?
Both wizards and gods could:

* create things in the room
* teleport things out of it
* list the items they created that are in it


## Colorizing

Update outputs with the following colors:

* [ ] Wall -> red
* [ ] Yelling -> red (bold)
* [x] Notification -> blue
  * [ ] one of the "notice" messages isn't getting coloured (logging out)


## Game Data

Game data is currently written to files; we should moved to a database
instead.

Desired features for existing code:

* [ ] Add support for saving room state (dropping things in a room and then
      restaring the game causes the dropped items to be lost).

With the read/write code abstracted out to a sigle module, it will be easier
to swap out with something in the future. Perhaps along these lines:

* [ ] Migrate game data to ETS table(s)
* [ ] Add support to flushing to disk (DETS)
* [ ] Add support for Mnesia
* [ ] Add support for creating rooms that get written to the DB
* [ ] Load rooms on start from DB


## Talking

* [ ] Add support for WALL
* [ ] Add support for YELL (nearest rooms only)
* [ ] Add public channel
* [ ] Add support for talking on a public channel
* [ ] Add channel for every guild
* [ ] Add support for talking on a guild channel
* [ ] Add support for country, region, town channels


## NPCs

* [ ] add NCP abstraction
* [ ] add simple conversation to NPCs
* [ ] add Elizabot "AI" to NPCs
* [ ] add support for specialized NPCs
* Add specialized NPCs:
  * [ ] add banker
  * [ ] add shopkeeper
  * [ ] add auctioneer


## Groups

* Add support for Guilds
  * [ ] come up with a mechanism for players to create guilds
  * [ ] put guild in "petition" queue
  * [ ] with enough signatures by people who *aren't* currently in a guild,
        move to "approved" queue
  * [ ] provide a means of wizards overriding the guild status and moving
        to a different queue or moving to "active" status


## Time

* [ ] Add a process(es) for tracking time
* [ ] Have a dedicated "time" channel that emits time-related messages
  * [ ] "it's now morning"
  * [ ] "it's now evening"
  * [ ] "the moon is full"
  * [ ] "the moon is new"
  * [ ] "it is now Spring/Summer/Autumn/Winter"
  * [ ] Define time state for gamne:
    * year
    * season
    * moon state
    * sun state
    * hidden state, perhaps revealable with spells/"science" exp points:
      * month
      * day of month
      * hour of day
      * minute of hour
  * [ ] Provide an API for querying the time process for current game time
        state
  * Perform specific actions on time state transitions:
    * Day -> Evening:
      * [ ] display map colors in monochrome
    * Summer -> Autumn in temperate zones:
      * [ ] enable multi-colored deciduous trees
    * Autumn -> Winter in temperate zones:
      * [ ] Change deciduous ASCII from "*" to "+" and make color brown
      * [ ] Change low mountain color from brown to white
      * [ ] Change grasslands from gree to white
      * [ ] Change water from blue to white


## Player State

* [ ] Define player state
  * coordinates
  * player map - list of coordinates that have been visited
  * ?
* [ ] Publish player state data to internal message queue
* [ ] Player message queue should be used for all queries about players
  * e.g., who, whois, teleport (to player location)



## Mapping Areas

* [ ] Update game to require a map.ascii or map.coord file
  * [ ] support plain text
  * [ ] support ANSI terminal colors
  * [ ] provide a utility to convert a .ascii map to a .ccord one
  * [ ] and vice versa
* [ ] Add a new map process and supervisor?
* [ ] Provide a utility for generating default rooms from ASCII map
* Add new map-related commands
  * [ ] "map" - display the ASCII map with player location annotated
    * this will require the lmud game to be aware of player locations
    * and of a coordinate system in general
    * room names as the unique identifier might go away, to be replaced by
      coordinates
  * [ ] "map zoom (+)n" - zoom in by a factor of n
  * [ ] "map zoom -n" - zoom out by a factor of n
  * [ ] "map +" - zoom in by a pre-defined constant
  * [ ] "map -" - zoom out by a pre-defined constant
  * [ ] "map guild" - show all the locations of guildmates
  * [ ] "map team" - show all the locations of players in your team
  * [ ] "map towns" - hide all landscape ASCII info and only show player
                      location and town/city locations on the map
* [ ] Only display the map for areas that have been visited
  * [ ] this will require that upon entering any room, player metadata for
        that room's coordinates need to set a ``#(visited true)`` value


## Creating Buildings

TBD


## World Items

TBD


## In-Game Trade

TBD


## Migration to LFE

* [ ] migrate remaining modules to LFE


## Client/Server Architecture

* [ ] Standardize current messaging approach with a well-defined API
  * [ ] identify which processes currently send messages
  * [ ] identify who they send their messages to
  * [ ] sketch out what this might look like in an HTTP/websockets world
  * [ ] do the same for a telnet world
  * is there a way to refactor the current code that approximate that?
  * how does one create a process (game entity)?
  * what are the similarities between the currently supervised processes?
    * acceptor
    * connection
    * living
    * player
    * request
    * room pool
    * room
    * session
    * spell
* Convert current messaging system (direct to telnet connection)
  * [ ] Usage message queues instead
  * [ ] create protocol  / message format that the lmud server will generate
* Update telnet code to become a true client of the lmud server
  * [ ] each client connection subscribes to game messages on a
        per-connection basis
        * [ ] figure out how to send config data read from disk
  * [ ] read messages from queues
  * [ ] write messages to queues
  * [ ] when appropriate, format messages to telnet connection for
        consumption by player(s)
  * [ ] publish player state to lmud server players data queue
* Create ncurses game client
  * [ ] have chat area
  * [ ] have read-only messages area (notices, system msgs, time updates)
  * [ ] have map-display area
* Create Qt game client
* Create HTML/CSS/Websockets game client
* Create Unity 3D game client


# Change Log

## Version 0.4

### Command Parsing

Support the following world-creation commands:

* aesir-level
  * [x] open ``<dir> <name>``
  * [x] dig ``<dir> <name>``

Support the following permissions-related commands:

* [x] add a god-level permission for granting god permissions in-game
* [x] add a wizard-level permission for granting wizard permissions in-game
* [x] split up wizard commands and regular commands


Support the following usage workflow:

1. [x] Enter a command in the MUD
1. [x] The command is checked against a list of aliases
1. [x] If matched, the actual command is used instead of the alias
1. [x] Command and args are parsed
1. [x] Appropriate mod:func + args is called
1. [x] Results of call are printed to terminal session


Support the following development workflow:

* [x] Base commands as well as aliases have metadata for which group of
      commands they belong to (useful for printing help and extended help
      for commands)
* [x] A base module exists which has a list of all supported commands +
      help text + mod:func they dispatch to
* [x] A new module is created with a list of aliases + actual command names
* [x] A data amalgamation function is updated to include calling the data
      function of the new module, adding its aliases/commands to the list of
      command data
* [x] A parsing function pulls in the base commands as well as all the
      defined aliases in the alias modules and the aliases are mapped to
      actual mod:func
* [x] parse_cmd checks the passed command (from user input) against this data
      structure to see if alias, if base command, which mod:func to call


Support the following command-related commands:

* [x] help (and aliases) - display base commands only
* [x] help aliases - display all aliases, grouped by type (e.g., IRC, WoW,
      etc.)
* [x] help commands - display aliases as well as base commands

### Breakout and Dependencies

* [x] move mud_parser into its own repo/project
  * add as a dependency
* [x] add erlang color as a dep:
  * https://github.com/julianduque/erlang-color
  * [x] replace custom color macros
* [x] Move lib/erlymud into top-level dir


### Colorizing

Update outputs with the following colors:

* [x] whisper -> magenta
* [x] say -> yellow
* [x] emote -> yellow (bold)
* [x] leave & arrive messages -> yellow (bold)
* [x] think -> black (bold); really grey
* [x] room title -> green (bold)


### Game Data

Game data is currently written to files; we should moved to a database
instead. Before doing that, though, it might be nice to put all data
read/write functions in a common module:

* [x] create a new filestore module
* [x] update "living" code to use it
* [x] update login code to use it
* [x] update object code to use it
* [x] update room code to use it
* [x] update user code to use it


### Talking

* [x] Add support for EMIT/MSG


### Migration to LFE

* [x] include LFE as a dep
* [x] all new functionality, write in LFE
  * [x] port newly-added em_util to lmud-util
* [x] rename to lmud or λMUD
* continue to use em_* Erlang modules (as legacy)
  * slowly port these to LFE, as time and interest allows

### User Experience

* [x] Add readline support (done via rlwrap)
* [x] use this ASCII:
   ```
          ___       ___           ___           ___
         /\__\     /\__\         /\__\         /\  \
        /:/  /    /::|  |       /:/  /        /::\  \
       /:/  /    /:|:|  |      /:/  /        /:/\:\  \
      /:/  /    /:/|:|__|__   /:/  /  ___   /:/  \:\__\
     /:/__/    /:/ |::::\__\ /:/__/  /\__\ /:/__/ \:|__|
     \:\  \    \/__/~~/:/  / \:\  \ /:/  / \:\  \ /:/  /
      \:\  \         /:/  /   \:\  /:/  /   \:\  /:/  /
       \:\  \       /:/  /     \:\/:/  /     \:\/:/  /
        \:\__\     /:/  /       \::/  /       \::/__/
         \/__/     \/__/         \/__/         ~~


        __       M"""""`'"""`YM M""MMMMM""M M""""""'YMM
        \ \      M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
         \ \     M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
          > \    M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
         / ^ \   M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
        /_/ \_\  M  MMM  MMM  M Mb       dM M       .MM
                 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM



        __       8""8""8 8   8 8""""8
        \ \      8  8  8 8   8 8    8
         \ \     8e 8  8 8e  8 8e   8
          > \    88 8  8 88  8 88   8
         / ^ \   88 8  8 88  8 88   8
        /_/ \_\  88 8  8 88ee8 88eee8

                              ....
                            .'   ,:
                          .'      \.___..
                        .'      .-'   _.'
                        '.\  \/...-''`\
                          :.'   /   \  :
                           :    () () /
                           (_ .  '--' ':
                             / |_'-- .'
                             \   \  .'_\
                            .|__  \/_/:
                           /          :\.
                          .' -./      .'{\|))
        __        .        :    ...    ::::::::::-.
        \ \       ;;,.    ;;;   ;;     ;;; ;;,   `';,
         \ \      [[[[, ,[[[[, [['     [[[ `[[     [[
          > \     $$$$$$$$"$$$ $$      $$$  $$,    $$
         / ^ \  o_888 Y88" 888o88    .d888  888_,o8P'
        /_/ \_\ "MMMM  M'  "MMM "YmmMMMM""  MMMMP"`

                              ....
                            .'   ,:
                          .'      \.___..
                        .'      .-'   _.'
                        '.\  \/...-''`\
                          :.'   /   \  :
                           :    () () /
                           (_ .  '--' ':
                             / |_'-- .'
                             \   \  .'_\
                            .|__  \/_/:
                           /          :\.
                          .' -./      .'{\|))
        __       M"""""`'"""`YM M""MMMMM""M M""""""'YMM
        \ \      M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
         \ \     M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
          > \    M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
         / ^ \   M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
        /_/ \_\  M  MMM  MMM  M Mb       dM M       .MM
                 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM


   ```


# Old TODO


## 0.3.4

* Add typespecs, documentation for (at least) API functions
  -status: continue with em_room
* Write some tests?


## 0.3.5

* Add item building commands
  - oedit: Edit / Create Objects
* Fix so that failure to load start room puts you in safe room
* Add 'goto' command for admins, to move to other rooms
* Add 'transfer' command for admins, to move another player
* Extend support for navigation and communication
  - n/e/s/w for movement
  - '/" for say, : for emote
  - nod/smile/grin/shrug/..
* Idle timer in 'who' list


## Future

* Improve password hashing security
  - at least use password + secret
  - or look at implementing HMAC?
* Test fix for leaking processes when load_living() fails, etc
  - see em_rh_login:do_login()
* Work on cmd_* implementations to improve error handling etc
  - 'redit' / 'addexit' should check validity of direction
* Send IAC GA sequence after printing prompts?
* Put new parser in place
* Use 'areas' for organizing rooms and objects
* Add 'destroy' command for destroying objects in own inventory?
* Add 'clone' command for instantiating object templates
* Shutdown / reboot command
* Improve connection lifecycle;
  - If an em_living dies;
    * Then em_user can inform about it, then run a do_login() again to get
      a new living
    * Also the em_room must clean out the original living id
    * Not to mention em_game, same there?
  - If an em_user dies;
    * Then em_conn can create a new one and put it into login phase, just
      like when someone connects
    * The em_game should clean up also
    * The em_living should get a timer, say 60 seconds, and if not reconnected
      after that time it should stop
    * When user logs in again, reconnect it to the em_living and the game
* Put the {In, Out} "client" Pid pair into a record definition for clarity?
