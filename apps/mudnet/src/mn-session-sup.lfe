;;;; Basic simple_one_for_one supervisor that starts new sessions on request.
(defmodule mn-session-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (conn)
  (supervisor:start_child (server) `(,conn)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(mn_session
               #(mn_session start_link ())
               temporary
               brutal_kill
               supervisor
               (mn_session)))))))
