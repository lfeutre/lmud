(defmodule ms-server
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; server API
  (export
   (ping 0)
   (echo 1)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

(defrecord state
  config
  modules
  module)

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

;; TODO: once we move to supporting different backends/versions for different
;; games running at the same time, we'll need to revist this setup.
(defun start_link (cfg)
  (let* ((lookup (mref cfg 'lookup))
         (key (tuple (clj:get-in cfg '(backend name))
                     (clj:get-in cfg '(backend version)))) 
         (mod (proplists:get_value key lookup)))
    (gen_server:start_link `#(local ,(SERVER))
                           (MODULE)
                           (make-state config cfg
                                       modules lookup
                                       module mod)
                           (genserver-opts))))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  `#(ok ,state))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_call
  ((`#(backend ,func ,args) _from (= (match-state module mod) state))
   `#(reply ,(erlang:apply mod func args) ,state))
  (('stop _from state)
    `#(stop shutdown ok ,state))
  ((`#(ping) _from state)
    `#(reply pong ,state))
  ((`#(echo ,msg) _from state)
    `#(reply ,msg ,state))
  ((message _from state)
    `#(reply ,(unknown-command) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; --------------
;;; For debugging
;;; --------------

(defun ping ()
  (gen_server:call (SERVER) #(ping)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))
