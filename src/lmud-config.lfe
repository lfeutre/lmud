(defmodule lmud-config
  (export all))

(defun wrap-width () 64)
(defun divider-char () '"-")
(defun divider () (string:copies (divider-char) (wrap-width)))
(defun help-pad-cmd () 20)

(defun banner-1 ()
'"
          ___       ___           ___           ___
         /\\__\\     /\\__\\         /\\__\\         /\\  \\
        /:/  /    /::|  |       /:/  /        /::\\  \\
       /:/  /    /:|:|  |      /:/  /        /:/\\:\\  \\
      /:/  /    /:/|:|__|__   /:/  /  ___   /:/  \\:\\__\\
     /:/__/    /:/ |::::\\__\\ /:/__/  /\\__\\ /:/__/ \\:|__|
     \\:\\  \\    \\/__/~~/:/  / \\:\\  \\ /:/  / \\:\\  \\ /:/  /
      \\:\\  \\         /:/  /   \\:\\  /:/  /   \\:\\  /:/  /
       \\:\\  \\       /:/  /     \\:\\/:/  /     \\:\\/:/  /
        \\:\\__\\     /:/  /       \\::/  /       \\::/__/
         \\/__/     \\/__/         \\/__/         ~~

")

(defun banner-2 ()
'"
        __       M\"\"\"\"\"`'\"\"\"`YM M\"\"MMMMM\"\"M M\"\"\"\"\"\"'YMM
        \\ \\      M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
         \\ \\     M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
          > \\    M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
         / ^ \\   M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
        /_/ \\_\\  M  MMM  MMM  M Mb       dM M       .MM
                 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM
")

(defun banner-3 ()
'"
        __       8\"\"8\"\"8  8   8  8\"\"\"\"8
        \\ \\      8  8  8  8   8  8    8
         \\ \\     8e 8  8  8e  8  8e   8
          > \\    88 8  8  88  8  88   8
         / ^ \\   88 8  8  88  8  88   8
        /_/ \\_\\  88 8  8  88ee8  88eee8
")

(defun banner-4 ()
'"
                              ....
                            .'   ,:
                          .'      \\.___..
                        .'      .-'   _.'
                        '.\\  \\/...-''`\\
                          :.'   /   \\  :
                           :    () () /
                           (_ .  '--' ':
                             / |_'-- .'
                             \\   \\  .'_\\
                            .|__  \\/_/:
                           /          :\\.
                          .' -./      .'{\\|))
        __        .        :    ...    ::::::::::-.
        \\ \\       ;;,.    ;;;   ;;     ;;; ;;,   `';,
         \\ \\      [[[[, ,[[[[, [['     [[[ `[[     [[
          > \\     $$$$$$$$\"$$$ $$      $$$  $$,    $$
         / ^ \\  o_888 Y88\" 888o88    .d888  888_,o8P'
        /_/ \\_\\ \"MMMM  M'  \"MMM \"YmmMMMM\"\"  MMMMP\"`

")

(defun banner-5 ()
'"
                              ....
                            .'   ,:
                          .'      \\.___..
                        .'      .-'   _.'
                        '.\\  \\/...-''`\\
                          :.'   /   \\  :
                           :    () () /
                           (_ .  '--' ':
                             / |_'-- .'
                             \\   \\  .'_\\
                            .|__  \\/_/:
                           /          :\\.
                          .' -./      .'{\\|))
        __       M\"\"\"\"\"`'\"\"\"`YM M\"\"MMMMM\"\"M M\"\"\"\"\"\"'YMM
        \\ \\      M  mm.  mm.  M M  MMMMM  M M  mmmm. `M
         \\ \\     M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
          > \\    M  MMM  MMM  M M  MMMMM  M M  MMMMM  M
         / ^ \\   M  MMM  MMM  M M  `MMM'  M M  MMMM' .M
        /_/ \\_\\  M  MMM  MMM  M Mb       dM M       .MM
                 MMMMMMMMMMMMMM MMMMMMMMMMM MMMMMMMMMMM
")

(defun get-banner ()
  (let ((func-name
          (list_to_atom
            (++ "banner-" (integer_to_list
                            (lmud-util:rand-int 1 5))))))
    (color:yellow (call (MODULE) func-name))))

(defun login-instructions ()
  (++ (divider)
"
  If you are loging in for the first time, enter the character
  name you would like to have (case insensitive) at the \"Login\"
  prompt.
"
  (divider)))

(defun post-login-msg ()
  (++
"

You are now logged into the game server.

"
    (divider)
"
  \"WHO\"  tells you who is logged in to the game.
  \"NEWS\" informs you about recent program changes and items of
         interest.
  \"HELP\" gives help on the commands, \"help commands\" for a list.
  \"QUIT\" saves your character exits the game.
"
    (divider) "\n"))

(defun simple-welcome ()
  (++ "Welcome to " (lmud-const:display-name) "!"))
