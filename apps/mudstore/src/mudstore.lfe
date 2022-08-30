(defmodule mudstore
  (export all))

(include-lib "apps/lmud/include/state.hrl")

(defun SERVER () 'ms-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   IMPORT / EXPORT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load (table-name row-name)
  (gen_server:call (SERVER) `#(backend load (,table-name ,row-name))))

(defun dump (table-name row-name data)
  (gen_server:call (SERVER) `#(backend dump (,table-name ,row-name ,data))))

(defun serialise (data)
  (gen_server:call (SERVER) `#(backend serialise (,data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   TABLES, ETC.   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun games ()
  (let ((`#(ok ,files) (file:list_dir
                        (filename:join
                         (list (lmud-files:data-dir)
                               (lmud-config:games-dir))))))
    files))

(defun table-names (game-name)
  (let ((`#(ok ,dirs) (file:list_dir
                       (filename:join
                        (list (lmud-files:data-dir)
                              (lmud-config:games-dir)
                              game-name)))))
    dirs))
