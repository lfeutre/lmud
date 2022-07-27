(defmodule mf-table
  (export all))

(defun file-extension () ".dat")

(defun file (table-name row-name)
  (filename:join
    (list (lmud_game:data_dir) table-name (++ row-name (file-extension)))))
