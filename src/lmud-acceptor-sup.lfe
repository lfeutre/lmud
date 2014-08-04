;;;; Main acceptor supervisor. Starts an acceptor pool and a listener,
;;;; in one_for_all supervision. This prevents issues if the pool dies; the
;;;; listener will also be restarted, and subsequently start up the acceptors
;;;; in the pool again.
(defmodule lmud-acceptor-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))


(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_listener (port acceptors)
  (supervisor:start_child
    (server)
    (lmud-util:supervisor-child 'em_listener `(,port ,acceptors) 'worker)))

(defun init
  (('())
     `#(ok #(#(one_for_all 5 10)
              (,(lmud-util:supervisor-child 'em_acceptor_pool 'supervisor))))))
