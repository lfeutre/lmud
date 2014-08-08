(defmodule lmud-event-source
  (export all))

(defun behaviour_info
  (('callbacks)
    `(#(add_event_listener 2)
      #(notify 2)))
  ((_)
    'undefined))
