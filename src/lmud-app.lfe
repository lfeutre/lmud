;;;; Application module for El-MUD.
;;;;
;;;; Will start listening for connections once the application startup has
;;;; completed.
(defmodule lmud-app
  (behaviour application)
  (export all))

;;; Application callbacks
(defun start (start-type start-args)
  (case (em_sup:start_link)
    ((tuple 'ok pid)
      (let ((port (lmud-util:get-env 'port (lmud-util:get-port)))
            (acceptors (lmud-util:get-env 'acceptors (lmud-const:default-acceptors))))
        (em_acceptor_sup:start_listener port acceptors)
        `#(ok ,pid)))
    (other `#(error ,other))))

(defun stop (state)
  'ok)
