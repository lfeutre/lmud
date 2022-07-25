;;;; This module defines functions which serialize versioned data to property
;;;; lists, suitable for output to files (due to human readability).
(defmodule lmud-datamodel
  (export all))

(include-lib "apps/lmud/include/state.hrl")

(defun character()
  (character 1 (make-state_character)))

(defun character (state)
  (character 1 state))

(defun character
  ((version (match-state_character desc d room 'undefined objects '()))
   `(#(version ,version)
     #(desc ,d)))
  ((version (match-state_character desc d room r objects os))
   `(#(version ,version)
     #(desc ,d)
     #(room ,(em_room:get_name r))
     #(objects ,(em_object:get_templates os)))))

(defun object
  ()
  "")

(defun room ()
  (room 1 (make-state_room)))

(defun room (state)
  (room 1 state))

(defun room
  ((version (match-state_room title t desc d exits es resets rs))
   `(#(version ,version)
     #(title ,t)
     #(desc ,d)
     #(exits ,es)
     #(objects ,(em_object:get_templates rs)))))

(defun user ()
  (user 1 (make-state_user)))

(defun user (state)
  (user 1 state))

(defun user
  ((version (match-state_user password pwd privileges privs))
   `(#(version ,version)
     #(password ,pwd)
     #(privileges ,privs))))
