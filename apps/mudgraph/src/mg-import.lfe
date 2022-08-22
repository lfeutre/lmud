(defmodule mg-import
  (export all))

(defun from-fs (table-name row-name)
  (case (filestore-v2:load table-name row-name)
    (`#(ok ,data) (maps:merge (proplists:to_map data)
                              `#m(type ,(table-type table-name)
                                  name ,row-name)))
    (err err)))

(defun table->type (table-name)
  (list_to_atom (string:strip table-name 'right #\s))
