(defmodule lmud-cmd-magic
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun cast
  ((`("ward") (= (match-req living living) req))
    (let ((room (em_living:get_room living))
          (name (em_living:get_name living)))
      (em_living:print
        living
        (++ "As you quietly vocalize your chosen mnemonics, "
            "the spell takes shape.\n"))
      (em_room:print_except
        room living
        (++ "~s starts muttering something incomprehensible. "
            "It's probably a spell.\n")
        (list name))
      (em_spell_ward:start living room))
    `#(ok ,req))
  ((_ req)
    (lmud-util:print
      (++ "You can cast the following spells:\n"
          "  ward - Will let you know if someone enters the protected room.\n")
      req)
    `#(ok ,req)))
