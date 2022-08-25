(defmodule mg-game
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   HIGH LEVEL API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exits (room)
  (lists:filtermap
   (match-lambda
     (((= `#m(label ,label to ,to) x))
      (case (andalso (maps:is_key 'type label) (== "transit" (mref label 'type)))
        ('true `#(true ,(mg:vertex to)))
        (_ 'false))))
   (mg:out-edges room)))

(defun inventory (character)
  'tbd)

(defun objects (room)
  'tbd)

(defun characters (user game)
  'tbd)

(defun games ()
  'tbd)
