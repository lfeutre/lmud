(defmodule lmud-cmd-game
  (export all))

(include-lib "apps/lmud/include/request.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun help
  (('("privileges") req)
   (lmud-io:print
    (++ "\nPrivileges are used to control what commands users have access "
        "to.\n"
        "Currently it's not possible to set them in-game; instead, edit the\n"
        "file data/users/<username>.dat and add a line like this:\n\n"
        "{privileges, [aesir,vanir,wysard]}.\n\n"
        "The 'aesir' privilege is the only one in use for now, to restrict\n"
        "access to commands like '@open' or '@dig'  that will modify the "
        "game.\n")
    req)
   `#(ok ,req))
  (('("aliases") req)
   (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                      "\n" (lmud-cmd-help:aliases)) req)
   `#(ok ,req))
  (('("all") req)
   (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                      "\n" (lmud-cmd-help:all)) req)
   `#(ok ,req))
  (('("admin") req)
   (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                      "\n" (lmud-cmd-help:admin)) req)
   `#(ok ,req))
  ;; alias help commands
  (('("priv") req)
   (help '("privileges") req))
  (('("commands") req)
   (help '("all") req))
  (('("alias") req)
   (help '("aliases") req))
  (('("cast") req)
   (lmud-cmd-magic:cast '() req))
  ;; catch-all
  (('() req)
   (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                      "\n" (lmud-cmd-help:base)) req)
   `#(ok ,req)))

(defun quit
  ((args (= (match-req user user living living) req))
   (save args req)
   (lmud-io:print "Goodbye!\n" req)
   (let ((name (em_living:get_name living))
         (room (em_living:get_room living)))
     (log-debug "User '~s' is leaving room '~s' and quitting ..." (list name (em_room:get_name room)))
     (em_room:print_except room living "~s leaves.~n" (list name))
     (log-debug "Notified others in room of departure.")
     (em_game:logout user)
     `#(stop ,req))))

(defun save
  ((_ (= (match-req living living) req))
   (lmud-io:print "Saving ...\n" req)
   (log-debug "Saving (~p) ..." (list living))
   (case (em_living:save living)
     ('ok `#(ok ,req))
     ((tuple 'error reason)
      (lmud-io:print "Error saving user data: ~s\n" (list reason) req)
      `#(ok ,req)))))
