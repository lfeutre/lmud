(defmodule mudstore
  (export all))

(defun SERVER () 'ms-server)

;;; -------------
;;; Datamodel API
;;; -------------

(defun character (character-state)
  (gen_server:call (SERVER) `#(backend character (,character-state))))

(defun room (room-state)
  (gen_server:call (SERVER) `#(backend room (,room-state))))

(defun user (user-state)
  (gen_server:call (SERVER) `#(backend user (,user-state))))

;;; -------------------
;;; Import / Export API
;;; -------------------

(defun load (table-name row-name)
  (gen_server:call (SERVER) `#(backend load (,table-name ,row-name))))

(defun dump (table-name row-name)
  (gen_server:call (SERVER) `#(backend dump (,table-name ,row-name))))

(defun serialise
  ((record) (is_record record)
   (case (element 1 record)
     ('state_character (serialise (character record)))
     ('state_room (serialise (room record)))
     ('state_user (serialise (user record)))
     (_ #(error unknown-record-type))))
  ((data-model)
   (gen_server:call (SERVER) `#(backend serialise (,data-model)))))
