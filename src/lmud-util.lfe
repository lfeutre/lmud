(defmodule lmud-util
  (export all))

(defun get-app-src ()
  (let (((tuple 'ok (list app)) (file:consult "src/erlymud.app.src")))
    app))

(defun get-version ()
  (proplists:get_value 'vsn (element 3 (get-app-src))))

(defun get-desc ()
  (proplists:get_value 'description (element 3 (get-app-src))))
