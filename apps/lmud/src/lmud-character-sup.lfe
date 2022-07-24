;;;; A plain simple_one_for_one supervisor that allows em_character processes to
;;;; be started when a user logs in.
(defmodule lmud-character-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (name, client)
  (supervisor:start_child (server) `(,name, ,client)))

(defun init
  ;; Allow 2000ms for em_character to clean up; removing itself
  ;; from the game world..
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_character
               #(em_character start_link ())
               temporary
               2000
               worker
               (em_character)))))))
