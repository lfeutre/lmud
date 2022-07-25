;;;; Application module for MUD Store.
;;;;
;;;; Will start managing the store once the application startup has
;;;; completed.
(defmodule ms-app
  (behaviour application)
  (export all))

;;; Application callbacks
(defun start (start-type start-args)
  (ms-sup:start_link))

(defun stop (state)
  (ms-sup:stop)
  'ok)
