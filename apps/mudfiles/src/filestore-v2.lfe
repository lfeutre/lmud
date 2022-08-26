(defmodule filestore-v2
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "apps/lmud/include/state.hrl")

(defun file-extension () ".dat")
(defun version () 2)

;;; -------------
;;; Datamodel API
;;; -------------
;;;
;;; These functions determine how game data gets transformed (e.g., to property
;;; lists), suitable for output to files. The intent is that, when serialised,
;;; the results will be both machine- and human-readable.

(defun character()
  (character (make-state_character)))

(defun character
  (((match-state_character id i name n desc d room 'undefined objects '()))
   `(#(version ,(version))
     #(id ,i)
     #(name ,n)
     #(desc ,d)))
  (((match-state_character id i name n desc d level l type t subtype st species s room r objects os))
   `(#(version ,(version))
     #(id ,i)
     #(name ,n)
     #(desc ,d)
     #(level ,l)
     #(type ,t)
     #(subtype ,st)
     #(species ,s)
     #(room ,(lmud_room:get_name r))
     #(objects ,(lmud_object:get_templates os)))))

(defun object
  ()
  "")

(defun room ()
  (room (make-state_room)))

(defun room
  (((match-state_room id i name n desc d exits es resets rs))
   `(#(version ,(version))
     #(id ,i)
     #(name ,n)
     #(desc ,d)
     #(exits ,es)
     #(objects ,(lmud_object:get_templates rs)))))

(defun user ()
  (user (make-state_user)))

(defun user
  (((match-state_user id i name n email e password pw privileges ps member-since ms))
   `(#(version ,(version))
     #(id ,i)
     #(name ,n)
     #(email ,e)
     #(password ,pw)
     #(privileges ,ps)
     #(member-since ,ms))))

;; -------------------
;; Import / Export API
;; -------------------

(defun load (table-name row-name)
  (let ((filename (file table-name row-name)))
    (log-debug "loading file: ~s" (list filename))
    (file:consult filename)))

(defun dump (table-name row-name data)
  (let ((filename (file table-name row-name)))
    (log-debug "dumping file: ~s" (list filename))
    (file:write_file filename data)))

(defun record->proplist
  ((record) (when (is_record record 'state_character))
   (character record))
  ((record) (when (is_record record 'state_room))
   (room record))
  ((record) (when (is_record record 'state_user))
   (user record))
  ((data)
   data))

(defun serialise (record)
  (serialise (record->proplist record) '()))

(defun serialise
  (('() acc)
   acc)
  ((data _) (when (is_tuple data))
   (io_lib:format "~p.~n" (list data)))
  ((`(,head . ,tail) acc)
   (serialise tail (lists:append acc (list (serialise head 'ignore))))))

;; v2 utility functions

(defun file
  (((= "users" table-name) row-name)
   (filename:join
    (list (lmud-files:data-dir)
          table-name
          (++ row-name (file-extension)))))
  ((table-name row-name)
   (file (lmud-config:default-game) table-name row-name)))

(defun file (game-name table-name row-name)
  (filename:join
    (list (lmud-files:data-dir)
          (lmud-config:games-dir)
          game-name
          table-name
          (++ row-name (file-extension)))))

(defun row-names
  (((= "users" table-name))
   (dat-files (filename:join (lmud-files:data-dir) table-name)))
  ((table-name)
   (row-names (lmud-config:default-game) table-name)))

(defun row-names (game-name table-name)
  (dat-files
   (filename:join
    (list (lmud-files:data-dir)
          (lmud-config:games-dir)
          game-name
          table-name))))

(defun dat-files (file-path)
  (let ((`#(ok ,files) (file:list_dir file-path))
        (`#(ok ,regex) (re:compile (io_lib:format "(\~s$)" (list (file-extension))))))
    (lists:filtermap
     (lambda (x)
       (case (re:split x regex '(#(return list)))
         (`(,table-name ,_ext ()) `#(true ,table-name))
         (_ 'false)))
     files)))
