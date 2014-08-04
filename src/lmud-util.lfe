(defmodule lmud-util
  (export all))

(defun get-app-src ()
  (let* ((filename (++ (filename:join "src" (lmud-const:name)) ".app.src"))
         ((tuple 'ok (list app)) (file:consult filename)))
    app))

(defun get-sys-cfg ()
  (let* (((tuple 'ok (list (list _ (tuple 'lmud cfg))))
            (file:consult "sys.config")))
    cfg))

(defun get-version ()
  (proplists:get_value 'vsn (element 3 (get-app-src))))

(defun print-version ()
  (io:format "~s~n" (list (get-version))))

(defun get-name ()
  (element 2 (get-app-src)))

(defun print-name ()
  (io:format "~s~n" (list (get-name))))

(defun get-port ()
  (proplists:get_value 'port (get-sys-cfg)))

(defun get-desc ()
  (proplists:get_value 'description (element 3 (get-app-src))))

;; XXX move this into lutil library
(defun rand-int ()
  (rand-int 1 10))

;; XXX move this into lutil library
(defun rand-int (start end)
  (let (((tuple mega-sec sec micro-sec) (now)))
    (random:seed mega-sec sec micro-sec)
    (+ (trunc (* (random:uniform) (- end (- start 1)))) start)))

;; XXX move this into lutil library
(defun get-lib-dir (mod)
  (filename:basename (code:lib_dir mod)))

;; XXX move part of this into lutil library?
(defun get-release-data ()
  "A function for generating the data needed by the .rel file."
  `#(release
    #("lmud" ,(get-version))
    #(erts ,(erlang:system_info 'version))
    (#(kernel ,(cadr (string:tokens (get-lib-dir 'kernel) "-")))
     #(stdlib ,(cadr (string:tokens (get-lib-dir 'stdlib) "-")))
     #(sasl ,(cadr (string:tokens (get-lib-dir 'sasl) "-")))
     #(eunit ,(cadr (string:tokens (get-lib-dir 'eunit) "-")))
     #(lmud ,(get-version)))))

(defun print-release-data ()
  (io:format "~p.~n" (list (get-release-data))))

(defun format-color (color text)
  (lists:flatten
    (lists:map
      (lambda (x)
        (binary_to_list x))
      (call 'color color (list_to_binary text)))))

(defun get-env (key fallback)
  (case (application:get_env key)
    ((tuple 'ok value) value)
    ('undefined fallback)))
