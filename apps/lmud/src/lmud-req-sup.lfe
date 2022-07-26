;;;; Launch a request, collect the result, and kill it.
(defmodule lmud-req-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(include-lib "apps/lmud/include/types.hrl")

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (mfa)
  (supervisor:start_child (server) `(,mfa)))

(defun request (mfa)
  (let* (((tuple 'ok req) (start_child mfa))
         (result (msh_req:run req)))
    (exit req 'normal)
    result))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(msh_req
               #(msh_req start_link ())
               temporary
               brutal_kill
               worker
               (msh_req)))))))
