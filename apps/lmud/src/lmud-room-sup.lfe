;;;; Keeps track of the room manager (em_room_mgr) and room pool supervisor
;;;; (lmud-room-pool-sup), making sure they are restarted if they crash.
;;;;
(defmodule lmud-room-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun init
  (('())
     `#(ok #(#(one_for_one 5 10)
              (,(lmud-util:make-child 'lmud-room-pool-sup 'supervisor)
               ,(lmud-util:make-child 'em_room_mgr 'worker))))))
