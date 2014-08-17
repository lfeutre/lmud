(defmodule lmud-perms
  (export all))

(include-file "include/request.hrl")

(defun verify
  ((priv (match-req user user))
    (case (em_player:has_privilege user priv)
      ('true 'ok)
      ('false (throw 'not_allowed)))))
