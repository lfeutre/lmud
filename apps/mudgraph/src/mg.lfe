(defmodule mg
  (export all))

(defun SERVER () 'mg-server)

(defun add-edge (from to)
  (add-edge from to '()))

(defun add-edge
  ((`#m(id ,from-id) `#m(id ,to-id) data)
   (gen_server:call (SERVER) `#(add-edge ,from-id ,to-id ,data))))

(defun add-vertex
  (((= `#m(id ,id) data))
   (gen_server:call (SERVER) `#(add-vertex ,id ,data))))

(defun find-vertices (key value)
  (lists:filtermap
   (lambda (x)
     (case (andalso (maps:is_key key x) (== (mref x key) value))
       ('true `#(true ,x))
       (_ 'false)))
   (vertices)))

(defun find-vertex (key value)
  (car (find-vertices key value)))

(defun find-edges (key value)
  (lists:filtermap
   (lambda (x)
     (case (andalso (maps:is_key key x) (== (mref x key) value))
       ('true `#(true ,x))
       (_ 'false)))
   (edges)))

(defun find-edge (key value)
  (car (find-edges key value)))

(defun graph ()
  (gen_server:call (SERVER) `#(graph)))

(defun in-edges
  ((`#m(id ,vertex-id))
   (gen_server:call (SERVER) `#(in-edges ,vertex-id))))

(defun in-neighbours
  ((`#m(id ,vertex-id))
   (gen_server:call (SERVER) `#(in-neighbours ,vertex-id))))

(defun in-neighbours
  ((`#m(id ,vertex-id) type)
   (gen_server:call (SERVER) `#(in-neighbours ,vertex-id ,type))))

(defun out-edges
  ((`#m(id ,vertex-id))
   (gen_server:call (SERVER) `#(out-edges ,vertex-id))))

(defun out-neighbours
  ((`#m(id ,vertex-id))
   (gen_server:call (SERVER) `#(out-neighbours ,vertex-id))))

(defun out-neighbours
  ((`#m(id ,vertex-id) type)
   (gen_server:call (SERVER) `#(out-neighbours ,vertex-id ,type))))

(defun edge-ids ()
  (gen_server:call (SERVER) `#(edges)))

(defun edge (id)
  (gen_server:call (SERVER) `#(edge ,id)))

(defun edges ()
  (lists:map #'edge/1 (edge-ids)))

(defun vertex-ids ()
  (gen_server:call (SERVER) `#(vertices)))

(defun vertices ()
  (lists:map #'vertex/1 (vertex-ids)))

(defun vertex (id)
  (gen_server:call (SERVER) `#(vertex ,id)))
