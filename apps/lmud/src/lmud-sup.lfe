;;;; This is the top of the supervision tree, where all necessary game
;;;; components are started in the required order.
(defmodule lmud-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun init
  (('())
     `#(ok #(#(one_for_one 5 10)
              (,(lmud-util:make-child 'em_game 'worker)
               ,(lmud-util:make-child 'lmud-spell-sup 'supervisor)
               ,(lmud-util:make-child 'lmud-room-sup 'supervisor)
               ,(lmud-util:make-child 'lmud-character-sup 'supervisor)
               ,(lmud-util:make-child 'lmud-user-sup 'supervisor)
               ,(lmud-util:make-child 'mn-session-sup 'supervisor)
               ,(lmud-util:make-child 'lmud-req-sup 'supervisor)
               ,(lmud-util:make-child 'mn-conn-sup 'supervisor)
               ,(lmud-util:make-child 'mn-acceptor-sup 'supervisor))))))
