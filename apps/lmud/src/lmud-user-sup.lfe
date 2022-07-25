;;;; Instantiates user processes on demand.
(defmodule lmud-user-sup
  (behaviour supervisor)
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (name conn)
  (log-debug "starting child ~p with connection ~p" (list name conn))
  (supervisor:start_child (server) `(,name ,conn)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(lmud-user
               #(lmud-user start_link ())
               temporary
               brutal_kill
               worker
               (lmud-user)))))))
