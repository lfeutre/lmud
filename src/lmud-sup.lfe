;;;; This is the top of the supervision tree, where all necessary game
;;;; components are started in the required order.
(defmodule lmud-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun make-child (module type)
  "Helper funtion for declaring children of supervisor."
  `#(,module #(,module start_link ()) permanent 5000 ,type (,module)))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun init
  (('())
     `#(ok #(#(one_for_one 5 10)
              (,(make-child 'em_game 'worker)
               ,(make-child 'em_spell_sup 'supervisor)
               ,(make-child 'em_room_sup 'supervisor)
               ,(make-child 'lmud-living-sup 'supervisor)
               ,(make-child 'em_user_sup 'supervisor)
               ,(make-child 'em_session_sup 'supervisor)
               ,(make-child 'lmud-req-sup 'supervisor)
               ,(make-child 'lmud-conn-sup 'supervisor)
               ,(make-child 'lmud-acceptor-sup 'supervisor))))))
