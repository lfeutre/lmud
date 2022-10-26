(defmodule lmud-twine
  (export all))

(defun read (path)
  (let ((`#(ok ,json) (file:read_file path)))
    (jsx:decode json '())))

(defun passages (data)
  (-extract #"passages" data))

(defun -extract (key data)
  (list-comp ((<- p (mref data key))) (mref p #"name")))

