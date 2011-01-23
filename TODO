ErlyMUD TODO
============

0.3.4
-----
* Add typespecs, documentation for (at least) API functions
  -status: continue with em_room
* Write some tests?

0.3.5
-----
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

Future
------
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
