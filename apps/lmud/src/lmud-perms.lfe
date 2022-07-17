(defmodule lmud-perms
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun verify
  ((priv (match-req user user))
    (case (lmud-player:has_privilege user priv)
      ('true 'ok)
      ('false (throw 'not_allowed)))))
