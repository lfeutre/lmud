(defmodule filestore-v2
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "apps/lmud/include/state.hrl")

(defun version () 2)
(defun file-extension () ".dat")
