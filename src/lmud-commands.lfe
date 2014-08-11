(defmodule lmud-commands
  (export all))

(defun all ()
  (lists:merge
    (list (base)
          ; (wizard)
          ; (god)
          (lmud-aliases:all))))

(defun base ()
  `(#("Game Command Group" ,(game-commands))
    #("Interaction Command Group" ,(interaction-commands))
    #("Movement Command Group" ,(movement-commands))
    #("Speaking Command Group" ,(speaking-commands))
    #("Magic Command Group" ,(magic-commands))
    #("Information Command Group" ,(information-commands))))

(defun game-commands ()
  `((#(name "help")
     #(desc "Display \"base\" help info.")
     #(mod em_rh_game)
     #(func cmd_help)
     #(args ()))
    (#(name "help aliases")
     #(desc "Display all the documented aliases.")
     #(mod em_rh_game)
     #(func cmd_help)
     #(args ("aliases")))
    ;; XXX help @wzard
    ;; XXX help @god
    ;; XXX help @commands
    (#(name "help all")
     #(desc "Display all help info.")
     #(mod em_rh_game)
     #(func cmd_help)
     #(args ("all")))
    (#(name "save")
     #(desc ,(++ "Save the one's character state; location and "
                 "inventory will be restored at login."))
     #(mod em_rh_game)
     #(func cmd_save)
     #(args ()))
    (#(name "quit")
     #(desc "Save and exit the game.")
     #(mod em_rh_game)
     #(func cmd_quit)
     #(args ()))))

(defun interaction-commands ()
  `((#(name "drop")
     #(desc "Drop an item from your inventory.")
     #(mod em_rh_game)
     #(func cmd_drop)
     #(args ()))
    (#(name "take")
     #(desc "Place up an item from the room in inventory.")
     #(mod em_rh_game)
     #(func cmd_take)
     #(args ()))
    (#(name "glance")
     #(desc ,(++ "View the brief description of something; "
                 "not detectable by others."))
     #(mod em_rh_game)
     #(func cmd_glance)
     #(args ()))
    (#(name "look")
     #(desc "View the long description of something.")
     #(mod em_rh_game)
     #(func cmd_look)
     #(args ()))))

(defun movement-commands ()
  `((#(name "go")
     #(desc "Move in a given direction.")
     #(mod em_rh_game)
     #(func cmd_go)
     #(args ()))))

(defun speaking-commands ()
  "Speaking, Thinking, and Body Language Group"
  `((#(name "say")
     #(desc "Speak; audible by anyone in the same room.")
     #(mod em_rh_game)
     #(func cmd_say)
     #(args ()))
    (#(name "emote")
     #(desc ,(++ "Indicate an action taking place, as performed by "
                 "one's character."))
     #(mod em_rh_game)
     #(func cmd_emote)
     #(args ()))
    (#(name "emote-ns")
     #(desc ,(++ "Indicate an action taking place, as performed by "
                 "one's character, but with no space (useful for "
                 "suffixes like apostrophes)."))
     #(mod em_rh_game)
     #(func cmd_emote_ns)
     #(args ()))
    (#(name "whisper")
     #(desc "Send a private message to another player.")
     #(mod em_rh_game)
     #(func cmd_tell)
     #(args ()))
    (#(name "think")
     #(desc ,(++ "Ponder something. Others notice as an emote, but with "
                  "no details."))
     #(mod em_rh_game)
     #(func cmd_think)
     #(args ()))))

(defun magic-commands ()
  `((#(name "cast")
     #(desc "Cast a spell. \"help cast\" will display available spells.")
     #(mod em_rh_game)
     #(func cmd_cast)
     #(args ()))))

(defun information-commands ()
  `((#(name "inv")
     #(desc "Show character inventory.")
     #(mod em_rh_game)
     #(func cmd_inv)
     #(args ()))
    (#(name "setdesc")
     #(desc ,(++ "Set one's charater description; will be seen by "
                 "other players when they \"look\"."))
     #(mod em_rh_game)
     #(func cmd_setdesc)
     #(args ()))
    (#(name "who")
     #(desc "Display all logged-in players.")
     #(mod em_rh_game)
     #(func cmd_who)
     #(args ()))
    (#(name "news")
     #(desc "Display info about latest server changes, etc.")
     #(mod em_rh_game)
     #(func cmd_news)
     #(args ()))))

(defun wizard ()
  `(()))

(defun god ()
  `(()))

(defun get-groups-names (prop-list)
  (proplists:get_keys prop-list))

(defun get-group (name prop-list)
  (proplists:get_value name prop-list))

(defun get-command (name prop-list)
  (lists:filter
    (lambda (x)
      (=/= x 'false))
    (check-command name prop-list)))

(defun get-command-or-alias (name prop-list)
  (lists:merge
    (list
      (get-command name prop-list)
      (lmud-aliases:get-command name (lmud-aliases:all)))))

(defun check-command (name prop-list)
  (lists:map
    (lambda (x)
      (if (== (proplists:get_value 'name x) name) x))
    (get-commands prop-list)))

(defun get-commands (prop-list)
  (lists:merge
    (lists:map
      (lambda (x)
        (element 2 x))
      prop-list)))

(defun get-command-names (prop-list)
  (proplists:get_all_values
    'name
    (lists:flatten
      (get-commands prop-list))))

(defun get-longest-command-length (prop-list)
  (lists:max
    (lists:map
      #'length/1
      (get-command-names prop-list))))
