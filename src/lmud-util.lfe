(defmodule lmud-util
  (export all))

(defun get-app-src ()
  (let (((tuple 'ok (list app)) (file:consult "src/erlymud.app.src")))
    app))

(defun get-version ()
  (proplists:get_value 'vsn (element 3 (get-app-src))))

(defun get-desc ()
  (proplists:get_value 'description (element 3 (get-app-src))))

;; XXX more this until lutil library
(defun rand-int ()
  (rand-int 1 10))

;; XXX more this until lutil library
(defun rand-int (start end)
  (let (((tuple mega-sec sec micro-sec) (now)))
    (random:seed mega-sec sec micro-sec)
    (+ (trunc (* (random:uniform) (- end (- start 1)))) start)))
