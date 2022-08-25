(defmodule mg-import
  (export all))

(defun from-fs ()
  (lists:flatten
   (lists:map #'from-fs/1 (filestore-v2:table-names))))

(defun from-fs (table-name)
  (lists:map (lambda (x)
               (from-fs table-name x))
             (filestore-v2:row-names table-name)))

(defun from-fs (table-name row-name)
  (case (filestore-v2:load table-name row-name)
    (`#(ok ,data) (maps:merge (proplists:to_map data)
                              `#m(type ,(table->type table-name)
                                  name ,row-name)))
    (err err)))

(defun table->type (table-name)
  (list_to_atom (string:strip table-name 'right #\s)))

(defun from-maps ()
  (from-maps (from-fs)))

(defun from-maps (list-of-maps)
  (list
   (lists:map #'vertices-from-map/1 list-of-maps)
   (lists:map #'edges-from-map/1 list-of-maps)))

(defun vertices-from-map
  (((= `#m(type ,type) m))
   (let ((supported '(room object character user)))
     (case (lists:member type supported)
       ('true (mg:add-vertex m))
       (x `#(unsupported-vertex-type ,x))))))

(defun edges-from-map
  (((= `#m(type ,type) m))
   (case type
     ('room (list (exits m) (room-items m)))
     ('character (list (inventory m) (location m)))
     (x `#(unsupported-edge-type ,x)))))

(defun exits (room-map)
  (lists:map
   (lambda (x) (exit->edges room-map x))
   (maps:get 'exits room-map '())))

(defun room-items (room-map)
  (let ((objects (maps:get 'objects room-map '())))
    'tbd))

(defun inventory (char-map)
  (let ((objects (lists:map (lambda (x) (mg:find-vertex 'name x))
                            (maps:get 'objects char-map '()))))
    'tbd))

(defun location (char-map)
  ;; For looking up a user's location ...
  ;; For finding all the users in a location ...
  'tbd)

(defun exit->edges
  ((origin `#(,dir ,dest-name))
   (let* ((dest (mg:find-vertex 'name dest-name))
          (return-dir (return-direction origin dest)))
      (mg:add-edge origin dest `#m(direction ,dir
                                   type "transit"
                                   subtype "door"
                                   size "standard"))
      (mg:add-edge dest origin `#m(direction ,return-dir
                                   type "transit"
                                   subtype "door"
                                   size "standard")))))

(defun return-direction
  ((`#m(name ,origin-name) `#m(exits ,return-exits))
   (lists:flatten
    (lists:filtermap
     (match-lambda
       ((`#(,return-dir ,return-name)) (when (== origin-name return-name))
        `#(true ,return-dir))
       ((_)
        'false))
     return-exits))))
