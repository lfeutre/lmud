;;;; Acceptor children that will wait for incoming connections can be started,
;;;; given a listener socket. With a transient restart strategy, the acceptors
;;;; in the pool will always be restarted if they die unexpectedly.
(defmodule lmud-acceptor-pool
  (behaviour supervisor)
  (export all))

(include-lib "apps/lmud/include/types.hrl")

(defun server () (MODULE))

(defun start_link ()
  "Start an empty acceptor pool."
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (l-sock)
  "Start an acceptor child process. It will become a transient process
  that must exit with reason 'normal' to gracefully go away."
  (supervisor:start_child (server) `(,l-sock)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 4 3600)
              (#(em_acceptor
               #(em_acceptor start_link ())
               transient
               brutal_kill
               worker
               (em_acceptor)))))))
