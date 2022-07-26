(defmodule msh-cmd-help
  (export all))

(defun base ()
  (msh-help:get-groups-help "BASE HELP" (msh-cmd:base)))

(defun admin ()
  (msh-help:get-groups-help "ADMIN HELP" (msh-cmd:admin)))

(defun aliases ()
  (msh-help:get-groups-help "ALIASES HELP" (msh-aliases:all)))

(defun all ()
  (++ (base)
      (admin)
      (aliases)))

(defun display-base-help ()
  (io:format (base)))

(defun display-aliases-help ()
  (io:format (aliases)))
