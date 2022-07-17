;;;; A plain simple_one_for_one supervisor that allows em_living processes to
;;;; be started when a player logs in.
(defmodule lmud-living-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (name, client)
  (supervisor:start_child (server) `(,name, ,client)))

(defun init
  ;; Allow 2000ms for em_living to clean up; removing itself
  ;; from the game world..
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_living
               #(em_living start_link ())
               temporary
               2000
               worker
               (em_living)))))))
