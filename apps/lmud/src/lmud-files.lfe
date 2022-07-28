(defmodule lmud-files
  (export all))

(defun parent-dirs
  ((path 0)
   path)
  ((path count)
   (parent-dirs (filename:dirname path) (- count 1))))

(defun proj-dir ()
  (parent-dirs (code:priv_dir 'lmud) 5))

(defun data-dir ()
  (let ((base-dir (proj-dir)))
    (case (lists:last (filename:split base-dir))
      ("default" (filename:join `(,(proj-dir) "rel" "lmud" "data")))
      (_ (filename:join `(,(proj-dir) "data"))))))

(defun text-dir ()
  (filename:join `(,(data-dir) "text")))

(defun src-dir ()
  (let ((base-dir (proj-dir)))
    (case (lists:last (filename:split base-dir))
      ("default" (filename:join `(,(proj-dir) "lib" "lmud" "src")))
      (_ (filename:join `(,(proj-dir) "apps" "lmud" "src"))))))

(defun app-src-file ()
  (filename:join `(,(src-dir) "lmud.app.src")))

(defun app-src ()
  (let ((`#(ok ,data) (file:consult (app-src-file))))
    data))

(defun app-cfg ()
  (element 3 (car (app-src))))
