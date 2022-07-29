(defmodule lmud
  (export all))

(defun id ()
  (uuid:uuid_to_string (uuid:get_v4)))
