(defmodule lmud-event-listener
  (export all))

(defun behaviour_info
  (('callbacks)
    `(#(handle_event 1)))
  ((_)
    'undefined))
