(defmodule lmud-io
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun print (format req)
  (print format '() req))

(defun print
  ((format args (match-req conn conn))
    (em_conn:print conn format args)))

(defun print
  ((color format args (match-req conn conn))
    (em_conn:print conn (lmud-util:format-color color format) args)))
