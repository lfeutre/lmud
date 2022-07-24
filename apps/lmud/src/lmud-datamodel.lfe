(defmodule lmud-datamodel
  (export all))

(include-lib "apps/lmud/include/state.hrl")

(defun character (state)
  (character 1 state))

(defun character
  ((version (match-state_character desc d room r objects os))
   `(#(version ,version)
     #(desc ,d)
     #(room ,(em_room:get_name r))
     #(objects ,(em_object:get_templates os)))))

(defun user
  ()
  "")

(defun object
  ()
  "")

(defun room
  ()
  "")