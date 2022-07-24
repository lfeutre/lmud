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
  (((list name conn))
    `#(ok ,(make-state_user name name conn conn))))

(defun handle_call
  (((tuple 'has-privilege? priv) _from (= (match-state_user privileges privs) state))
    `#(reply ,(ordsets:is_element priv privs) ,state))
  (('name _from (= (match-state_user name name) state))
    `#(reply ,name ,state))
  (((tuple 'print format) _from (= (match-state_user conn conn) state))
    (em_conn:print conn format)
    `#(reply ok ,state))
  (((tuple 'print format args) _from (= (match-state_user conn conn) state))
    (em_conn:print conn format args)
    `#(reply ok ,state))
  (('load _from state)
    (case (do-load state)
      ((tuple 'ok new-state)
        `#(reply ok ,new-state))
      ((tuple 'error reason)
        `#(reply #(error ,reason) ,state)))))

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

;; Private functions

(defun do-load
  (((= (match-state_user name name) state))
    (load-user name state)))

(defun load-user (name state)
  (log-info "loading user: ~s" (list (lmud-filestore:user-file name)))
  (case (lmud-filestore:read "users" name)
    ((tuple 'ok data)
      `#(ok ,(update-user data state)))
    ((tuple 'error _reason)
      `#(error not_found))))

(defun update-user
  (('() state)
    state)
  (((cons (tuple 'privileges priv-list) data) state)
    (update-user data (make-state_user privileges (ordsets:from_list priv-list))))
  (((cons _ data) state)
    (update-user data state)))
