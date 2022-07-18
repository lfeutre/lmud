(defmodule lmud-cmd-move
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun go
  (('() req)
    (lmud-io:print "Go where?" req)
    `#(ok ,req))
  (((cons direction _) (= (match-req living living) req))
    (let ((room (em_living:get_room living)))
      (do-go (em_room:get_exit room direction) req))
    `#(ok ,req)))

(defun do-go
  ((`#(error not_found) req)
    (lmud-io:print "You can't go in that direction.\n" req))
  ((`#(ok #(,direction ,destination)) (= (match-req living living) req))
    (let ((`#(ok ,destination-room) (em_room_mgr:get_room destination))
          (name (em_living:get_name living))
          (room (em_living:get_room living)))
      (lmud-io:print (++ "You leave " direction ".\n\n") req)
      (em_room:print_except
        'yellowb room living "~s leaves ~s.~n" (list name direction))
      (em_room:leave room living)
      (em_living:set_room living destination-room)
      (em_room:enter destination-room living)
      (em_room:print_except
        'yellowb destination-room living "~s arrives.~n" (list name))
      (lmud-cmd-interact:glance '() req))))
