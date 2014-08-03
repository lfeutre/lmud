(defmodule lmud-util
  (export all))

(defun get-version ()
  (let (((tuple 'ok (list app)) (file:consult "src/erlymud.app.src")))
    (proplists:get_value 'vsn (element 3 app))))
