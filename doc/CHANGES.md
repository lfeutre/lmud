ErlyMUD Change Log
==================


0.4.0
-----
* Renamed project tp λMUD ("El-MUD").
* Removed custom color management and used @julianduque's erlang-color.
* Added support for TinyMUSH emote/pose with no space after the name.
* Added TinyMUSH aliases for :, ;, \\ and ".
* Added ' and PAGE as aliases for WHISPER/TELL.
* Added color output to most of the talk and emote functions.


0.3.5
-----
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


0.3.2
-----
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


0.3.1
-----
* Initial 'public' release


0.2.3
-----
* Will now hide password during entry, as long as client complies to
  IAC WILL/WONT ECHO

