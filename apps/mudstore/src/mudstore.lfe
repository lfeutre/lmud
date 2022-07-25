(defmodule mudstore
  (export all))

(include-lib "apps/lmud/include/state.hrl")

(defun SERVER () 'ms-server)

;;; -------------------
;;; Import / Export API
;;; -------------------

(defun load (table-name row-name)
  (gen_server:call (SERVER) `#(backend load (,table-name ,row-name))))

(defun dump (table-name row-name data)
  (gen_server:call (SERVER) `#(backend dump (,table-name ,row-name ,data))))

(defun serialise (data)
  (gen_server:call (SERVER) `#(backend serialise (,data))))
