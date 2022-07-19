;;;; Represents a single connected player, and hosts player-related state.
(defmodule lmud-player
  (behaviour gen_server)
  (export
   all))

(include-lib "logjam/include/logjam.hrl")

(deftype player-pid (pid))

(defrecord state
  name
  conn
  (privileges (ordsets:new)))

(defun start_link (name conn)
  (gen_server:start_link (MODULE) `(,name ,conn) '()))

(defun has_privilege (pid priv)
  (gen_server:call pid `#(has_privilege ,priv)))

(defun get_name (pid)
  (gen_server:call pid 'get_name))

(defun print (pid format)
  (gen_server:call pid `#(print ,format)))

(defun print (pid format args)
  (gen_server:call pid `#(print ,format ,args)))

(defun load (pid)
  (gen_server:call pid 'load))

(defun init
  (((list name conn))
    `#(ok ,(make-state name name conn conn))))

(defun handle_call
  (((tuple 'has_privilege priv) _from (= (match-state privileges privs) state))
    `#(reply ,(ordsets:is_element priv privs) ,state))
  (('get_name _from (= (match-state name name) state))
    `#(reply ,name ,state))
  (((tuple 'print format) _from (= (match-state conn conn) state))
    (em_conn:print conn format)
    `#(reply ok ,state))
  (((tuple 'print format args) _from (= (match-state conn conn) state))
    (em_conn:print conn format args)
    `#(reply ok ,state))
  (('load _from state)
    (case (do_load state)
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

(defun do_load
  (((= (match-state name name) state))
    (load_user name state)))

(defun load_user (name state)
  (log-info "loading user: ~s" (list (lmud-filestore:get-user-file name)))
  (case (lmud-filestore:read "users" name)
    ((tuple 'ok data)
      `#(ok ,(update_user data state)))
    ((tuple 'error _reason)
      `#(error not_found))))

(defun update_user
  (('() state)
    state)
  (((cons (tuple 'privileges priv-list) data) state)
    (update_user data (make-state privileges (ordsets:from_list priv-list))))
  (((cons _ data) state)
    (update_user data state)))
