(defmodule ms-config
  (export all))

(defun all ()
  `#m(backend (backend)
     lookup (lookup)))

(defun backend ()
  (let ((`#(ok ,storage) (application:get_env 'mudstore 'backend)))
    storage))

(defun lookup ()
  (let ((`#(ok ,storage) (application:get_env 'mudstore 'lookup)))
    storage))
