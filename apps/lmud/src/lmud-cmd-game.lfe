(defmodule lmud-cmd-game
  (export all))

(include-lib "apps/lmud/include/request.hrl")

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
                         "\n" (lmud-help:get-aliases-help)) req)
    `#(ok ,req))
  (('("all") req)
    (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                         "\n" (lmud-help:get-all-help)) req)
    `#(ok ,req))
  (('("admin") req)
    (lmud-io:print (++ "\n" (lmud-config:simple-welcome)
                         "\n" (lmud-help:get-admin-help)) req)
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
                     "\n" (lmud-help:get-base-help)) req)
    `#(ok ,req)))

(defun quit
  ((args (= (match-req user user living living) req))
    (save args req)
    (lmud-io:print "Goodbye!\n" req)
    (let ((name (em_living:get_name living))
          (room (em_living:get_room living)))
      (em_room:print_except room living "~s leaves.~n" (list name))
      (em_game:logout user)
      `#(stop ,req))))

(defun save
  ((_ (= (match-req living living) req))
    (lmud-io:print "Saving ...\n" req)
    (case (em_living:save living)
      ('ok `#(ok ,req))
      ((tuple 'error reason)
        (lmud-io:print "Error: ~s\n" (list reason) req)
        `#(ok ,req)))))
