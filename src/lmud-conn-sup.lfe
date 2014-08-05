;;;; The connection supervisor.
(defmodule lmud-conn-sup
  (behaviour supervisor)
  (export all))

(include-file "include/types.hrl")

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (socket)
  (supervisor:start_child (server) `(,socket)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_conn
               #(em_conn start_link ())
               temporary
               brutal_kill
               worker
               (em_conn)))))))
