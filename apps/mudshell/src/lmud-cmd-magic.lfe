(defmodule lmud-cmd-magic
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun cast
  ((`("ward") (= (match-req character character) req))
    (let ((room (em_character:get_room character))
          (name (em_character:name character)))
      (em_character:print
        character
        (++ "As you quietly vocalize your chosen mnemonics, "
            "the spell takes shape.\n"))
      (em_room:print_except
        room character
        (++ "~s starts muttering something incomprehensible. "
            "It's probably a spell.\n")
        (list name))
      (em_spell_ward:start character room))
    `#(ok ,req))
  ((_ req)
    (lmud-io:print
      (++ "You can cast the following spells:\n"
          "  ward - Will let you know if someone enters the protected room.\n")
      req)
    `#(ok ,req)))
