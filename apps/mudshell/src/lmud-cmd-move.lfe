(defmodule lmud-cmd-move
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun go
  (('() req)
    (lmud-io:print "Go where?" req)
    `#(ok ,req))
  (((cons direction _) (= (match-req character character) req))
    (let ((room (em_character:get_room character)))
      (do-go (em_room:get_exit room direction) req))
    `#(ok ,req)))

(defun do-go
  ((`#(error not_found) req)
    (lmud-io:print "You can't go in that direction.\n" req))
  ((`#(ok #(,direction ,destination)) (= (match-req character character) req))
    (let ((`#(ok ,destination-room) (em_room_mgr:get_room destination))
          (name (em_character:name character))
          (room (em_character:get_room character)))
      (lmud-io:print (++ "You leave " direction ".\n\n") req)
      (em_room:print_except
        'yellowb room character "~s leaves ~s.~n" (list name direction))
      (em_room:leave room character)
      (em_character:set_room character destination-room)
      (em_room:enter destination-room character)
      (em_room:print_except
        'yellowb destination-room character "~s arrives.~n" (list name))
      (lmud-cmd-interact:glance '() req))))
