# Change Log

## Version 0.4

### Command Parsing

Support the following world-creation commands:

* [x] aesir-level
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
* Added login banner support.
* Added get_version function.
* Minor code cleanup/formatting.
* Updated QUIT to also call SAVE.
* Added better login message (from TinyMUSH).
* Added NEWS command.
* Added ME and EM as aliases for EMOTE (taken from IRC and WoW,
  respectively).
* Added TinyMUSH aliases POSE, WHISPER.
* Added TinyMUSH THINK.

## 0.3.4

* Add typespecs, documentation for (at least) API functions
  -status: continue with em_room

## 0.3.2

* Incorporated Steve Vinoski's SHA2 module (as em_util_sha2.erl) to avoid
  dependency on the OTP crypto app, meaning ErlyMUD will now run in Windows
  without too much extra work
* Basic support for telnet protocol negotiation, which mostly does nothing
  right now except decline any option requests; implemented according to
  the Q Method <http://www.faqs.org/rfcs/rfc1143.html>
* Input handling now also works even if client is in character mode, by
  buffering until we encounter \r\n
* Documented what a successful login sequence looks like, see the file
  doc/LoginSequence.html (which renders using websequencediagrams.com)

## 0.3.1

* Initial 'public' release


## 0.2.3

* Will now hide password during entry, as long as client complies to
  IAC WILL/WONT ECHO

