(defmodule lmud-perms
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun verify
  ((priv (match-req user user))
    (case (lmud-user:has-privilege? user priv)
      ('true 'ok)
      ('false (throw 'not_allowed)))))
