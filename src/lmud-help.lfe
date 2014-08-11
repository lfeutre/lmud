(defmodule lmud-help
  (export all))

(defun display-groups-help (name prop-list)
  (io:format (get-groups-help name prop-list)))

(defun display-base-help ()
  (io:format (get-base-help)))

(defun get-base-help ()
  (get-groups-help "BASE HELP" (lmud-commands:base)))

(defun get-wizard-help ()
  (get-groups-help "WIZARD HELP" (lmud-commands:wizard)))

(defun get-god-help ()
  (get-groups-help "GOD HELP" (lmud-commands:god)))

(defun display-aliases-help ()
  (io:format (get-aliases-help)))

(defun get-aliases-help ()
  (get-groups-help "ALIASES HELP" (lmud-aliases:all)))

(defun get-all-help ()
  (++ (get-base-help)
      ; (get-god-help)
      ; (get-wizard-help)
      (get-aliases-help)))

(defun get-groups-help (name prop-list)
  (++ "\n" name "\n"
      (lists:map
        (lambda (x)
          (get-group-help
            x (lmud-commands:get-longest-command-length prop-list)))
        prop-list)
      (lmud-config:divider)
      "\n"))

(defun get-group-help
  (((tuple group-name prop-list) max-len)
    (++ (get-group-heading group-name)
        (format-help prop-list max-len))))

(defun get-group-heading (group-name)
  (let* ((prefix-len 4)
         (prefix-len-str (integer_to_list prefix-len))
         (pad-len 1)
         (non-tail-len (+ prefix-len (* pad-len 2)))
         (name (string:substr group-name 1 (- (lmud-config:wrap-width)
                                              non-tail-len)))
         (len (length name))
         (len-str (integer_to_list len))
         (tail-len (- (lmud-config:wrap-width)
                      (+ len non-tail-len)))
         (tail-len-str (integer_to_list tail-len))
         (format-str (++ "~n~" prefix-len-str "." prefix-len-str ".-s ~"
                         len-str "s ~" tail-len-str "." tail-len-str
                         ".-s~n~n")))
    (lists:flatten
      (io_lib:fwrite
        format-str (list "" name "")))))

(defun wrap-help (pad-len wrap-width help)
  (let* ((text (lutil-text:wrap-text help wrap-width))
         ((cons first-line rest) (string:tokens text "\n")))
    (lists:foldl
      (lambda (x acc)
        (++ acc "\n" (string:copies " " pad-len) x))
      first-line
      rest)))

(defun format-help
  (((list (tuple 'name cmd) (tuple 'desc help) _ _ _)
      max-cmd-len max-pad wrap-width)
    (io_lib:format (++ "~-"
                       (integer_to_list (lmud-config:help-pad-cmd)) "."
                       (integer_to_list max-cmd-len) "s" "~s~n")
                   (list cmd (wrap-help max-pad wrap-width help))))
  (((list (tuple 'name name) (tuple 'command command) (tuple 'args args))
      max-cmd-len max-pad wrap-width)
    (let ((help (++ "Alias for the \""
                    (string:join (++ (list command) args) " ")
                    "\" command.")))
      (io_lib:format (++ "~-"
                         (integer_to_list (lmud-config:help-pad-cmd)) "."
                         (integer_to_list max-cmd-len) "s" "~s~n")
                     (list name (wrap-help max-pad wrap-width help)))))
  ((data max-cmd-len max-pad wrap-width)
    (io:format "Could not parse data: ~p~n" (list data))))

(defun format-help (help max-cmd-len)
  (let* ((max-pad (lmud-config:help-pad-cmd))
         (wrap-width (- (lmud-config:wrap-width) max-pad)))
    (lists:map
      (lambda (x)
        (format-help x max-cmd-len max-pad wrap-width))
      help)))
