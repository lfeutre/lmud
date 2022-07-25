(defmodule filestore-v1
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "apps/lmud/include/state.hrl")

(defun version () 1)
(defun file-extension () ".dat")

;;; -------------
;;; Datamodel API
;;; -------------
;;;
;;; Thse functions determine how game data gets transformed (e.g., to property
;;; lists), suitable for output to files. The intent is that, when serialised,
;;; the results will be both machine- and human-readable.

(defun character()
  (character (make-state_character)))

(defun character
  (((match-state_character desc d room 'undefined objects '()))
   `(#(version ,(version))
     #(desc ,d)))
  (((match-state_character desc d room r objects os))
   `(#(version ,(version))
     #(desc ,d)
     #(room ,(em_room:get_name r))
     #(objects ,(em_object:get_templates os)))))

(defun object
  ()
  "")

(defun room ()
  (room (make-state_room)))

(defun room
  (((match-state_room title t desc d exits es resets rs))
   `(#(version ,(version))
     #(title ,t)
     #(desc ,d)
     #(exits ,es)
     #(objects ,(em_object:get_templates rs)))))

(defun user ()
  (user (make-state_user)))

(defun user
  (((match-state_user password pwd privileges privs))
   `(#(version ,(version))
     #(password ,pwd)
     #(privileges ,privs))))

;; -------------------
;; Import / Export API
;; -------------------

(defun load (table-name row-name)
  (let ((filename (table-file table-name row-name)))
    (log-debug "loading file: ~s" (list filename))
    (file:consult filename)))

(defun dump (table-name row-name data)
  (let ((filename (table-file table-name row-name)))
    (log-debug "dumping file: ~s" (list filename))
    (file:write_file filename data)))

(defun serialise
  ((record) (when (is_record record 'state_character))
   (serialise (character record)))
  ((record) (when (is_record record 'state_room))
   (serialise (room record)))
  ((record) (when (is_record record 'state_user))
   (serialise (user record)))
  ((data) (when (is_tuple data))
   (io_lib:format "~p.~n" (list data)))
  ((data)
   (serialise data '())))

(defun serialise
  (('() acc)
   acc)
  ((`(,head . ,tail) acc)
   (serialise tail (lists:append acc (list (serialise head))))))

;; Utility functions

(defun table-file (table-name row-name)
  (filename:join
    (list (em_game:data_dir) table-name (++ row-name (file-extension)))))
