(defmodule mg-server
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; server API
  (export
   (ping 0)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         '()
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (_)
  `#(ok ,(digraph:new)))

(defun handle_cast (_msg graph)
  `#(noreply ,graph))

(defun handle_call
  ;; Graph API
  ((`#(add-edge ,from-id ,to-id ,data) _from graph)
   (let* ((edge-id (uuid:uuid_to_string (uuid:get_v5 (iolist_to_binary `(,from-id ,to-id)))))
          (result (digraph:add_edge graph edge-id from-id to-id data)))
     `#(reply ,result ,graph)))
  ((`#(add-vertex ,id ,data) _from graph)
   (let ((result (digraph:add_vertex graph id data)))
     `#(reply ,result ,graph)))
  ((`#(in-edges ,vertex-id) _from graph)
   `#(reply ,(in-edges graph vertex-id) ,graph))
  ((`#(in-neighbours ,vertex-id) _from graph)
   `#(reply ,(in-neighbours graph vertex-id) ,graph))
  ((`#(in-neighbours ,vertex-id ,type) _from graph)
   `#(reply ,(in-neighbours graph vertex-id type) ,graph))
  ((`#(out-edges ,vertex-id) _from graph)
   `#(reply ,(out-edges graph vertex-id) ,graph))
  ((`#(out-neighbours ,vertex-id) _from graph)
   `#(reply ,(out-neighbours graph vertex-id) ,graph))
  ((`#(out-neighbours ,vertex-id ,type) _from graph)
   `#(reply ,(out-neighbours graph vertex-id type) ,graph))
  ((`#(graph) _from graph)
   `#(reply ,graph ,graph))
  ((`#(edge ,id) _from graph)
   (let ((`#(,_id ,from ,to ,label) (digraph:edge graph id)))
     `#(reply #m(from ,from to ,to label ,label) ,graph)))
  ((`#(edges) _from graph)
   `#(reply ,(digraph:edges graph) ,graph))
  ((`#(vertex ,id) _from graph)
   (let ((`#(,_id ,data) (digraph:vertex graph id)))
     `#(reply ,data ,graph)))
  ((`#(vertices) _from graph)
   `#(reply ,(digraph:vertices graph) ,graph))
  ;; General ops / debugging
  (('stop _from graph)
   `#(stop shutdown ok ,graph))
  ((`#(ping) _from graph)
   `#(reply pong ,graph))
  ;; Fallback
  ((message _from graph)
   `#(reply ,(unknown-command) ,graph)))

(defun handle_info
  ((`#(EXIT ,_from normal) graph)
   `#(noreply ,graph))
  ((`#(EXIT ,pid ,reason) graph)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,graph))
   `#(noreply ,graph))
  ((_msg graph)
   `#(noreply ,graph)))

(defun terminate (_reason _graph)
  'ok)

(defun code_change (_old-version graph _extra)
  `#(ok ,graph))

;;; --------------
;;; debugging
;;; --------------

(defun ping ()
  (gen_server:call (SERVER) `#(ping)))

;;; --------------
;;; utility
;;; --------------

(defun in-edges (graph vertex-id)
  (list-comp
    ((<- e (digraph:in_edges graph vertex-id)))
    (digraph:edge graph e)))

(defun out-edges (graph vertex-id)
  (list-comp
    ((<- e (digraph:out_edges graph vertex-id)))
    (digraph:edge graph e)))

(defun in-neighbours (graph vertex-id)
  (list-comp
    ((<- `#(,_ ,id ,_ ,_) (in-edges graph vertex-id)))
    (let ((`#(,_ ,data) (digraph:vertex graph id)))
      data)))

;; TODO: need a better, more general solution for filtering ... support arbitrary map keys
(defun in-neighbours (graph vertex-id sought)
  (lists:filtermap (match-lambda
                    (((= `#(,_ ,id ,_ #m(type ,type)) e))
                     (case (== type sought)
                       ('true `#(true ,(digraph:vertex graph id)))
                       (x x))))
                     (in-edges graph vertex-id)))

(defun out-neighbours (graph vertex-id)
  (list-comp
    ((<- `#(,_ ,_ ,id ,_) (out-edges graph vertex-id)))
    (let ((`#(,_ ,data) (digraph:vertex graph id)))
      data)))

;; TODO: need a better, more general solution for filtering ... support arbitrary map keys
(defun out-neighbours (graph vertex-id sought)
  (lists:filtermap (match-lambda
                    (((= `#(,_ ,_ ,id #m(type ,type)) e))
                     (case (== type sought)
                       ('true `#(true ,(digraph:vertex graph id)))
                       (x x))))
                     (out-edges graph vertex-id)))
