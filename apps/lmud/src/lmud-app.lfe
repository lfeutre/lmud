;;;; Application module for El-MUD.
;;;;
;;;; Will start listening for connections once the application startup has
;;;; completed.
(defmodule lmud-app
  (behaviour application)
  (export all))

;;; Application callbacks
(defun start (start-type start-args)
  (case (lmud-sup:start_link)
    ((tuple 'ok pid)
       (lmud-acceptor-sup:start_listener
        (lmud-config:port)
        (lmud-config:acceptors))
       `#(ok ,pid)))
    (other `#(error ,other)))
  `#(ok ,(self)))

(defun stop (state)
  'ok)
