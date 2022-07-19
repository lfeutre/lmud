(defmodule lmud-datamodel
  (export all))

(include-lib "apps/lmud/include/state.hrl")

(defun living (state)
  (living 1 state))

(defun living
  ((version (match-state_living desc d room r objects os))
   `(#(version ,version)
     #(desc ,d)
     #(room ,(em_room:get_name r))
     #(objects ,(em_object:get_templates os)))))

(defun player
  ()
  "")

(defun object
  ()
  "")

(defun room
  ()
  "")