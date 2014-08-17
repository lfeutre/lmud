;;;; Instantiates user processes on demand.
(defmodule lmud-player-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (name conn)
  (supervisor:start_child (server) `(,name ,conn)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_player
               #(em_player start_link ())
               temporary
               brutal_kill
               worker
               (em_player)))))))
