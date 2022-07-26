(defmodule lmud-cmd-move
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun go
  (('() req)
    (lmud-io:print "Go where?" req)
    `#(ok ,req))
  (((cons direction _) (= (match-req character character) req))
    (let ((room (lmud_character:get_room character)))
      (do-go (lmud_room:get_exit room direction) req))
    `#(ok ,req)))

(defun do-go
  ((`#(error not_found) req)
    (lmud-io:print "You can't go in that direction.\n" req))
  ((`#(ok #(,direction ,destination)) (= (match-req character character) req))
    (let ((`#(ok ,destination-room) (lmud_room_mgr:get_room destination))
          (name (lmud_character:name character))
          (room (lmud_character:get_room character)))
      (lmud-io:print (++ "You leave " direction ".\n\n") req)
      (lmud_room:print_except
        'yellowb room character "~s leaves ~s.~n" (list name direction))
      (lmud_room:leave room character)
      (lmud_character:set_room character destination-room)
      (lmud_room:enter destination-room character)
      (lmud_room:print_except
        'yellowb destination-room character "~s arrives.~n" (list name))
      (lmud-cmd-interact:glance '() req))))
