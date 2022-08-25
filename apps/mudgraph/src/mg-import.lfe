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
  (lists:map #'vertices-from-map/1 list-of-maps)
  (lists:map #'edges-from-map/1 list-of-maps))

(defun vertices-from-map
  (((= `#m(type ,type) m))
   (case type
     ('room (room m))
     ('object (object m))
     ('character (character m))
     ('user (user m))
     (x `#(error unsupported-type ,x)))))

(defun edges-from-map
  (((= `#m(type ,type) m))
   (case type
     ('room (list (exits m) (room-items m)))
     ('character (inventory m))
     (x `#(error unsupported-type ,x)))))

(defun room (room-map)
  (let ((room-keys '(id name title brief desc type version)))
    (mg:add-vertex (maps:with room-keys room-map))))

(defun exits (room-map)
  (let* ((exits (lists:map #'exit->map/1 (maps:get 'exits room-map '()))))
    'tbd))

(defun room-items (room-map)
  (let ((objects (maps:get 'objects room-map '())))
    'tbd))

(defun object (object-map)
  (mg:add-vertex object-map))

(defun character (char-map)
  (mg:add-vertex char-map))

(defun inventory (char-map)
  (let ((objects (lists:map (lambda (x) (mg:find-vertex 'name x))
                            (maps:get 'objects char-map '()))))
    'tbd))

(defun user (user-map)
  (mg:add-vertex user-map))

(defun exit->map
  ((`#(,dir ,room-name))
   `#m(destination ,room-name
                   direction ,dir)))
