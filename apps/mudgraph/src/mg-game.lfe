(defmodule mg-game
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   HIGH LEVEL API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun games ()
  (let ((server-map (mg:find-vertex 'name "server")))
    (mg:out-neighbours server-map 'type 'world)))

(defun users ()
  (let ((server-map (mg:find-vertex 'name "server")))
    (mg:out-neighbours server-map 'type 'account)))

(defun exits (room-map)
  (mg:out-neighbours room-map 'type 'transit))

(defun inventory
  (((= `#m(type character) char-map))
   (mg:out-neighbours char-map 'type 'inventory)))

(defun objects
  (((= `#m(type room) room-map))
   'tbd)
  (((= `#m(type character) char-map))
   (inventory char-map)))

(defun characters
  (((= `#m(type room) room-map))
   ;; All characters linked to a given room
   (mg:out-neighbours room-map 'type 'location))
  (((= `#m(type user) user-map))
   ;; All characters created by a user (across all games)
   'tbd))

(defun location
  (((= `#m(type character) char-map))
   (mg:in-neighbours char-map 'type 'location)))

(defun characters (user game)
  'tbd)

(defun contents
  (((= `#m(type room) room-map))
   (mg:out-neighbours room-map 'type 'object)))
