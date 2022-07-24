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

(defun read (table-name user-name)
  (file:consult
    (table-file table-name user-name)))

(defun write (table-name user-name data)
  (file:write_file
    (table-file table-name user-name)
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

(defun user-file (user-name)
  (table-file "users" user-name))

(defun character-file (user-name)
  (table-file "characters" user-name))

(defun object-file (user-name)
  (table-file "objects" user-name))

(defun room-file (room-name)
  (table-file "rooms" room-name))

(defun table-file (table-name name)
  (filename:join
    (list (em_game:data_dir) table-name (++ name ".dat"))))

