(defmodule lmud-util
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun get-port-digits ()
  "This silly little function is how we got the port number for L-MUD.

  It does a modulus 10 on each letter of the input (in this case, 'lmud'),
  and retuns digits between 0 and 9 for each (in this case: 1, 2, 0, and 3)."
  (lists:map
    (lambda (x)
      (rem (- x 97) 10)) "lmud"))

(defun get-app-src ()
  (let* ((filename (++ (filename:join "src" (lmud-const:name)) ".app.src"))
         ((tuple 'ok (list app)) (file:consult filename)))
    app))

(defun get-sys-cfg ()
  (let* (((tuple 'ok (list (list _ (tuple 'lmud cfg))))
            (file:consult "sys.config")))
    cfg))

(defun print-version ()
  (io:format "~s~n" (list (lmud-config:version))))

(defun get-name ()
  (element 2 (get-app-src)))

(defun print-name ()
  (io:format "~s~n" (list (get-name))))

(defun print-port ()
  (io:format "~p~n" (list (lmud-config:port))))

(defun get-desc ()
  (proplists:get_value 'description (element 3 (get-app-src))))

;; XXX move this into lutil library
(defun rand-int ()
  (rand-int 1 10))

;; XXX move this into lutil library
(defun rand-int (start end)
  (+ (trunc (* (rand:uniform) (- end (- start 1)))) start))

;; XXX move this into lutil library
(defun get-lib-dir (mod)
  (filename:basename (code:lib_dir mod)))

;; XXX move part of this into lutil library?
(defun get-release-data ()
  "A function for generating the data needed by the .rel file."
  `#(release
    #(lmud ,(lmud-config:version))
    #(erts ,(erlang:system_info 'version))
    (#(kernel ,(cadr (string:tokens (get-lib-dir 'kernel) "-")))
     #(stdlib ,(cadr (string:tokens (get-lib-dir 'stdlib) "-")))
     #(sasl ,(cadr (string:tokens (get-lib-dir 'sasl) "-")))
     #(eunit ,(cadr (string:tokens (get-lib-dir 'eunit) "-"))))))

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

(defun make-child (module type)
  "Helper funtion for declaring children of supervisor."
  `#(,module #(,module start_link ()) permanent 5000 ,type (,module)))

(defun make-child (module args type)
  "Helper funtion for declaring children of supervisor."
  `#(,module #(,module start_link ,args) permanent 5000 ,type (,module)))

(defun supervisor-child (supervisor type)
  (make-child supervisor type))

(defun supervisor-child (supervisor args type)
  (make-child supervisor args type))

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