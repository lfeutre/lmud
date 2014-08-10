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
    (get-table-file table-name user-name)))

(defun write (table-name user-name data)
  (file:write_file
    (get-table-file table-name user-name)
    data))

(defun get-user-file (user-name)
  (get-table-file "users" user-name))

(defun get-living-file (user-name)
  (get-table-file "livings" user-name))

(defun get-object-file (user-name)
  (get-table-file "objects" user-name))

(defun get-room-file (room-name)
  (get-table-file "rooms" room-name))

(defun get-table-file (table-name name)
  (filename:join
    (list (em_game:data_dir) table-name (++ name ".dat"))))
