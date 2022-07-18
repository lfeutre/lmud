(defmodule lmud-cmd-help
  (export all))

(defun base ()
  (lmud-help:get-groups-help "BASE HELP" (lmud-cmd:base)))

(defun admin ()
  (lmud-help:get-groups-help "ADMIN HELP" (lmud-cmd:admin)))

(defun aliases ()
  (lmud-help:get-groups-help "ALIASES HELP" (lmud-aliases:all)))

(defun all ()
  (++ (base)
      (admin)
      (aliases)))

(defun display-base-help ()
  (io:format (base)))

(defun display-aliases-help ()
  (io:format (aliases)))
