(defmodule mudgraph-v1
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "apps/lmud/include/state.hrl")

(defun version () 1)
(defun file-extension () ".dat")

;; Thoughts:
;; * can we change these to maps for easier matching?
;; * with that done, can we delete state? just use these instead?
;; * how easy is it to get the map #m() data out of the vertex? or node?
;; * shall we create a migration function for v2 filestore -> v1 mudgraph?

;;; -------------
;;; Datamodel API
;;; -------------
;;;
;;; These functions determine how game data gets transformed (e.g., to property
;;; lists), suitable for output to files. The intent is that, when serialised,
;;; the results will be both machine- and human-readable.

(defun character()
  (character (make-state_character)))

(defun character (data)
  (proplists:to_map (filestore-v2:character data)))

;; Character location is a lookup-node: you can always query a character's
;; location by looking up the vertex with the vertex id formatting in the
;; following manner:
;;   #(<character id value> location)
;;
;; Maybe only out-neighbours type=room is sufficient?
(defun character-location
  (((= `#m(id ,id) label-data)) (when (is_map label-data))
   #(error not-implemented))
  ((id) (when (is_list id))
   #(error not-implemented)))

;; Character inventory: a custom node directed in towards the character (i.e.,
;; "owned by") ... a query for in-neighbours on a character filtered by
;; type=container?
(defun character-inventory ()
  #(error not-implemented))

;; In addition to looking up a character, whenever a character moves to a new
;; location, that an edge is created from the character to the node that
;; represents the room they are in (after the edge for their previous location
;; has been deleted).
(defun room-contents ()
  #(error not-implemented))

(defun room-objects ()
  #(error not-implemented))

(defun room-residents ()
  #(error not-implemented))

(defun room-exits ()
  #(error not-implemented))

(defun object ()
  "")

(defun room ()
  (room (make-state_room)))

(defun room (data)
  (proplists:to_map (filestore-v2:room data)))

(defun user ()
  (user (make-state_user)))

(defun user (data)
  (proplists:to_map (filestore-v2:user data)))

;; Given a user ID, finding all in-neighbours (belongs to) of type=character
;; will return all the characters created by/associated with a given user
;; account.
(defun user-characters ()
  #(error not-implemented))

;; All the characters that exist on a given game can be found by querying
;; in-neighbours (belongs to) of a game (found with the game ID).
(defun game-users ()
  #(error not-implemented))

;; All installed games can be found by querying the top-most games node for
;; in-neighbours (belongs to) with type=games and status=active.
(defun games ()
  #(error not-implemented))

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

(defun serialise
  ((record) (when (is_record record 'state_character))
   (serialise (character record)))
  ((record) (when (is_record record 'state_room))
   (serialise (room record)))
  ((record) (when (is_record record 'state_user))
   (serialise (user record)))
  ((data)
   (serialise data '())))

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
   (filename:join
    (list (lmud-files:data-dir)
          (lmud-config:games-dir)
          (lmud-config:default-game)
          table-name
          (++ row-name (file-extension))))))
