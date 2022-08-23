(defmodule mg-import
  (export all))

(defun from-fs ()
  (lists:flatten
   (lists:map #'from-fs/1 (filestore-v2:table-names))))

(defun from-fs (table-name)
  (lists:map (lambda (x)
               (from-fs table-name x))
             (filestore-v2:row-names table-name)))

(defun from-fs (table-name row-name)
  (case (filestore-v2:load table-name row-name)
    (`#(ok ,data) (maps:merge (proplists:to_map data)
                              `#m(type ,(table->type table-name)
                                  name ,row-name)))
    (err err)))

(defun table->type (table-name)
  (list_to_atom (string:strip table-name 'right #\s)))
