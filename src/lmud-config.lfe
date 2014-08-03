(defmodule lmud-config
  (export all))

(defun banner1 ()
'"
MM\"\"\"\"\"\"\"\"`M          dP          M\"\"\"\"\"`'\"\"\"`YM M\"\"MMMMM\"\"M M\"\"\"\"\"\"'YMM
MM  mmmmmmmM          88          M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
M`      MMMM 88d888b. 88 dP    dP M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
MM  MMMMMMMM 88'  `88 88 88    88 M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
MM  MMMMMMMM 88       88 88.  .88 M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
MM        .M dP       dP `8888P88 M  MMM  MMM  M Mb       dM M       .MM
MMMMMMMMMMMM                  .88 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM
                          d8888P
")

(defun banner2 ()
'"
 _______       _       ______  _     _ _____
(_______)     | |     |  ___ \\| |   | (____ \\
 _____    ____| |_   _| | _ | | |   | |_   \\ \\
|  ___)  / ___) | | | | || || | |   | | |   | |
| |_____| |   | | |_| | || || | |___| | |__/ /
|_______)_|   |_|\\__  |_||_||_|\\______|_____/
                (____/
")

(defun banner3 ()
'"
   __     _                      ___
  /__\\ __| |_   _  /\\/\\  /\\ /\\  /   \\
 /_\\| '__| | | | |/    \\/ / \\ \\/ /\\ /
//__| |  | | |_| / /\\/\\ \\ \\_/ / /_//
\\__/|_|  |_|\\__, \\/    \\/\\___/___,'
            |___/

")

(defun login-instructions ()
'"
*** If you are loging in for the first time, enter the character name
*** you would like to have (case insensitive) at the \"Login\" prompt.
")

(defun post-login-msg ()
'"

You have logged into the server.

------------------------------------------------------------------------------
  \"WHO\" tells you who is logged in to the game.
  \"NEWS\" informs you about recent program changes and items of interest.
  \"HELP\" gives help on the commands, \"help commands\" for a list.
  \"QUIT\" saves your character exits the game.
------------------------------------------------------------------------------

")
