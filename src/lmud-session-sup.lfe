;;;; Basic simple_one_for_one supervisor that starts new sessions on request.
(defmodule lmud-session-sup
  (behaviour supervisor)
  (export all))

(defun server () (MODULE))

(defun start_link ()
  (supervisor:start_link `#(local ,(server)) (MODULE) '()))

(defun start_child (conn)
  (supervisor:start_child (server) `(,conn)))

(defun init
  (('())
     `#(ok #(#(simple_one_for_one 0 1)
              (#(em_session
               #(em_session start_link ())
               temporary
               brutal_kill
               supervisor
               (em_session)))))))

; init([]) ->
;   Session = {em_session, {em_session, start_link, []},
;           temporary, brutal_kill, supervisor, [em_session]},
;   Children = [Session],
;   RestartStrategy = {simple_one_for_one, 0, 1},
;   {ok, {RestartStrategy, Children}}.

