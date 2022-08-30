(defmodule mg-import
  (export all))

(defun supported-vertex-types ()
  '(root game room object character user))

(defun game-data ()
  (lists:flatten
   (lists:map #'game-data/1 (mudstore:games))))

(defun game-data (game-name)
  (lists:flatten
   (lists:map
    (lambda (x)
      (game-data game-name x))
    (mudstore:table-names game-name))))

(defun game-data (game-name table-name)
  (lists:map
   (lambda (x)
     (game-data game-name table-name x))
   (filestore-v2:game-row-names game-name table-name)))

(defun game-data (game-name table-name row-name)
  (case (filestore-v2:load game-name table-name row-name)
    (`#(ok ,data) (maps:merge (proplists:to_map data)
                              `#m(type ,(table->type table-name)
                                  name ,row-name
                                  game ,game-name)))
    (err err)))

(defun user-data ()
  (lists:map
   #'user-data/1
   (filestore-v2:data-row-names "users")))

(defun user-data (row-name)
  (let ((table-name "users"))
    (case (filestore-v2:load table-name row-name)
      (`#(ok ,data) (maps:merge (proplists:to_map data)
                                `#m(type ,(table->type table-name)
                                    name ,row-name))))))


(defun table->type (table-name)
  (list_to_atom (string:strip table-name 'right #\s)))

(defun games ()
  (list-comp ((<- x (mudstore:games)))
    `#m(id ,(lmud:id)
        name ,x
        type game)))

(defun from-fs ()
  (lists:append
   (list
    (games)
    (user-data)
    (game-data))))

(defun from-maps ()
  (from-maps (from-fs)))

(defun from-maps (list-of-maps)
  (let ((list-of-maps (lists:append
                       list-of-maps
                       (list
                        `#m(id ,(lmud:id)
                            name "server"
                            type root)))))
    (list
     (lists:map #'vertices-from-map/1 list-of-maps)
     (lists:map #'edges-from-map/1 list-of-maps))))

(defun vertices-from-map
  (((= `#m(type ,type) m))
     (case (lists:member type (supported-vertex-types))
       ('true (mg:add-vertex m))
       (x `#(unsupported-vertex-type ,x)))))

(defun edges-from-map
  (((= `#m(type ,type) m))
   (case type
     ('root (list (users m) (games m)))
     ('room (list (exits m) (objects m)))
     ('character (list (inventory m) (location m)))
     ('user `#(added-via-server ,type))
     ('game `#(added-via-server ,type))
     (_ `#(unsupported-edge-type ,type)))))

(defun users (server-map)
  (lists:map
   (lambda (x)
     (mg:add-edge server-map x #m(type account)))
   (mg:find-vertices 'type 'user)))

(defun games (server-map)
  (lists:map
   (lambda (x)
     (mg:add-edge server-map x #m(type world)))
   (mg:find-vertices 'type 'game)))

(defun exits (room-map)
  (lists:map
   (lambda (x) (exit->edges room-map x))
   (maps:get 'exits room-map '())))

(defun objects
  ((`#m(objects ()))
   '())
  (((= `#m(objects ,obj-names) room-map))
   (let ((obj-maps (lists:map (lambda (x)
                                (mg:find-vertex 'name x))
                              obj-names)))
     (lists:map
      (lambda (x)
        (mg:add-edge room-map x `#m(type object)))
      obj-maps)))
  ((_)
   '()))

(defun inventory
  ((`#m(objects ()))
   '())
  ((char-map)
   (let ((obj-maps (lists:map (lambda (x)
                                (mg:find-vertex 'name x))
                              (maps:get 'objects char-map '()))))
     (lists:map
      (lambda (x)
        (mg:add-edge char-map x `#m(type inventory)))
      obj-maps))))

(defun location
  (((= `#m(room ,room-name) char-map))
   (let ((room-map (mg:find-vertex 'name room-name)))
     (mg:add-edge room-map char-map #m(type location))))
  ((`#m(name ,character))
   `#(no-location-info ,character)))

(defun exit->edges
  ((origin `#(,dir ,dest-name))
   (let* ((dest (mg:find-vertex 'name dest-name))
          (return-dir (return-direction origin dest)))
      (mg:add-edge origin dest `#m(direction ,dir
                                   type transit
                                   subtype door
                                   size standard))
      (mg:add-edge dest origin `#m(direction ,return-dir
                                   type transit
                                   subtype door
                                   size standard)))))

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
