;;;; Represents a single connected user, and hosts user-related state.
(defmodule lmud-user
  (behaviour gen_server)
  (export
   all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "apps/lmud/include/state.hrl")

(deftype user-pid (pid))

(defun start_link (name conn)
  (gen_server:start_link (MODULE) `(,name ,conn) '()))

(defun init
  ((`(,n ,c))
   (let ((initial-state (make-state_user name n conn c)))
     (log-debug "setting initial state: ~p" (list initial-state))
     `#(ok ,initial-state))))

(defun handle_call
  (('get-character _from (= (match-state_user character c) state))
   `#(reply ,c ,state))
  ((`#(has-privilege? ,priv) _from (= (match-state_user privileges privs) state))
   `#(reply ,(ordsets:is_element priv privs) ,state))
  ;; TODO: Add 'logged-in? ...
  (('name _from (= (match-state_user name name) state))
   `#(reply ,name ,state))
  ((`#(print ,format) _from (= (match-state_user conn conn) state))
   (mn_conn:print conn format)
   `#(reply ok ,state))
  ((`#(print ,format ,args) _from (= (match-state_user conn conn) state))
   (mn_conn:print conn format args)
   `#(reply ok ,state))
  ((`#(set-character ,pid) _from state)
   `#(reply ok ,(update-state_user state character pid)))
  (('load _from state)
   (case (do-load state)
     ((tuple 'ok new-state)
      `#(reply ok ,new-state))
     ((tuple 'error reason)
      `#(reply #(error ,reason) ,state))))
  (('state _from state)
   `#(reply ,state ,state)))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_info (_info state)
  `#(noreply ,state))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;; -------------------
;; API
;; -------------------

(defun get-character (pid)
  (gen_server:call pid 'get-character))

(defun has-privilege? (pid priv)
  (gen_server:call pid `#(has-privilege? ,priv)))

(defun name (pid)
  (gen_server:call pid 'name))

(defun print (pid format)
  (gen_server:call pid `#(print ,format)))

(defun print (pid format args)
  (gen_server:call pid `#(print ,format ,args)))

(defun load (pid)
  (gen_server:call pid 'load))

(defun set-character (pid char-pid)
  (gen_server:call pid `#(set-character ,char-pid)))

(defun state (pid)
  (gen_server:call pid 'state))

;; Private functions

(defun do-load
  (((= (match-state_user name name) state))
   (log-info "loading user: ~s" (list name))
   (case (mudstore:load "users" name)
     (`#(ok ,data)
      `#(ok ,(update-state_user state privileges (proplists:get_value 'privileges data))))
     (`#(error ,reason)
      (log-debug "Couldn't load user from store: ~p" (list reason))
      `#(error not_found)))))
