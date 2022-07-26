;;;; Manages the pool of instantiated rooms in the game. Only used by the
;;;; lmud_room_mgr, where room instantiation happens. This is really just a
;;;; cache to avoid loading the room each time people walk into it.
(defmodule lmud-room-pool-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (name title desc)
  (supervisor:start_child (server) `(,name ,title ,desc)))

(defun which_children ()
  (supervisor:which_children (server)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(lmud_room
                 #(lmud_room start_link ())
                 temporary
                 brutal_kill
                 worker
                 (lmud_room)))))))
