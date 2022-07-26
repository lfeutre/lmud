(defmodule lmud-io
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun print (format req)
  (print format '() req))

(defun print
  ((format args (match-req conn conn))
    (mn_conn:print conn format args)))

(defun print
  ((color format args (match-req conn conn))
    (mn_conn:print conn (lmud-util:format-color color format) args)))

(defun read (filename)
  (let ((`#(ok ,data) (file:read_file filename)))
    data))

(defun read-news ()
  (read (filename:join (lmud-util:text-dir) "NEWS.txt")))
