(defmodule lmud-commands
  (export all))

(include-file "include/commands.lfe")

(defun base ()
  `(
    #("Game Command Group"
       ((#(name "help")
         #(desc "Display \"base\" help info.")
         #(mod em_rh_game)
         #(func cmd_help))
        (#(name "save")
         #(desc ,(++ "Save the one's character state; location and "
                     "inventory will be restored at login."))
         #(mod em_rh_game)
         #(func cmd_save))
        ;; XXX help
        ;; XXX help aliases
        ;; XXX help commands
        ;; XXX help @commands
        ;; XXX save
        ))
    #("Interaction Command Group"
       ((#(name "drop")
         #(desc "Drop an item from your inventory.")
         #(mod em_rh_game)
         #(func cmd_drop))
        (#(name "take")
         #(desc "Place up an item from the room in inventory.")
         #(mod em_rh_game)
         #(func cmd_take))
        (#(name "glance")
         #(desc ,(++ "View the brief description of something; "
                     "not detectable by others."))
         #(mod em_rh_game)
         #(func cmd_glance))
        (#(name "look")
         #(desc "View the long description of something.")
         #(mod em_rh_game)
         #(func cmd_look))))
    #("Movement Command Group"
       ((#(name "go")
         #(desc "Move in a given direction.")
         #(mod em_rh_game)
         #(func cmd_go))))
    ;; Speaking, Thinking, and Body Language Group
    #("Speaking Command Group"
       ((#(name "say")
         #(desc "Speak; audible by anyone in the same room.")
         #(mod em_rh_game)
         #(func cmd_say))
        (#(name "emote")
         #(desc ,(++ "Indicate an action taking place, as performed by "
                     "one's character."))
         #(mod em_rh_game)
         #(func cmd_emote))
        (#(name "whisper")
         #(desc "Send a private message to another player.")
         #(mod em_rh_game)
         #(func cmd_whisper))))
    #("Magic Command Group"
       ((#(name "cast")
         #(desc "Cast a spell. \"help cast\" will display available spells.")
         #(mod em_rh_game)
         #(func cmd_cast))))
    #("Information Command Group"
       ((#(name "inv")
         #(desc "Show character inventory.")
         #(mod em_rh_game)
         #(func cmd_inv))
        (#(name "setdesc")
         #(desc ,(++ "Set one's charater description; will be seen by "
                     "other players when they \"look\"."))
         #(mod em_rh_game)
         #(func cmd_setdesc))
        (#(name "who")
         #(desc "Display all logged-in players.")
         #(mod em_rh_game)
         #(func cmd_who))
        (#(name "news")
         #(desc "Display info about latest server changes, etc.")
         #(mod em_rh_game)
         #(func cmd_news))
        ;; XXX save
        ))
  ))

(defun aliases-wow ()
  'noop)

(defun aliases-irc ()
  'noop)

(defun aliases-tinymud ()
  'noop)

(defun all ()
  `(#(commands ,(base))
    #(aliases ,(++ (aliases-tinymud)
                   (aliases-irc)
                   (aliases-wow)))))

(defun get-groups-names (prop-list)
  (proplists:get_keys prop-list))

(defun get-group (name prop-list)
  (proplists:get_value name prop-list))

(defun get-command-names (prop-list)
  (proplists:get_all_values
    'name
    (lists:flatten
      (lists:map
        (lambda (x)
          (element 2 x))
        prop-list))))

(defun get-longest-command-length (prop-list)
  (lists:max
    (lists:map
      #'length/1
      (get-command-names prop-list))))
