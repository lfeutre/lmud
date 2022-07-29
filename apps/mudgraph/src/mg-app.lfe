(defmodule mg-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 0)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (logger:info "Starting mudgraph application ...")
  (mg-sup:start_link))

(defun stop ()
  (mg-sup:stop)
  'ok)
