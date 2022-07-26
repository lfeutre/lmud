;;;; The connection supervisor.
(defmodule mn-conn-sup
  (behaviour supervisor)
  (export all))

(include-lib "apps/lmud/include/types.hrl")

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (socket)
  (supervisor:start_child (server) `(,socket)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(mn_conn
               #(mn_conn start_link ())
               temporary
               brutal_kill
               worker
               (mn_conn)))))))
