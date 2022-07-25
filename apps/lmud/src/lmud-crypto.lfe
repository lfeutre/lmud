(defmodule lmud-crypto
  (export all))

(defun hash (data)
  (base64:encode_to_string (crypto:hash (lmud-config:hash-algo) data)))
