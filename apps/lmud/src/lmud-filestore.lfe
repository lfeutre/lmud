(defmodule lmud-filestore
  (export all))

(defun create-schema ()
  'noop)

(defun start ()
  'noop)

(defun create-table ()
  'noop)

(defun info ()
  'noop)

(defun read (table-name row-name)
  (file:consult
    (table-file table-name row-name)))

(defun write (table-name row-name data)
  (file:write_file
    (table-file table-name row-name)
    data))

(defun serialise
  ((data) (when (is_tuple data))
   (io_lib:format "~p.~n" (list data)))
  ((data)
   (serialise data '())))

(defun serialise
  (('() acc)
   acc)
  ((`(,head . ,tail) acc)
   (serialise tail (lists:append acc (list (serialise head))))))

(defun user-file (row-name)
  (table-file "users" row-name))

(defun character-file (row-name)
  (table-file "characters" row-name))

(defun object-file (row-name)
  (table-file "objects" row-name))

(defun room-file (row-name)
  (table-file "rooms" row-name))

(defun table-file (table-name row-name)
  (filename:join
    (list (em_game:data_dir) table-name (++ row-name ".dat"))))
