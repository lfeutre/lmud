;;;; Spell supervisor, used to initialize new spells given a callback
;;;; module and args.
(defmodule lmud-spell-sup
  (behaviour supervisor)
  (export all))

(include-lib "apps/lmud/include/types.hrl")

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (spell args)
  (supervisor:start_child (server) `(,spell ,args)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_spell
               #(em_spell start_link ())
               temporary
               brutal_kill
               worker
               (em_spell)))))))
