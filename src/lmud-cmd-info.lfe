(defmodule lmud-cmd-info
  (export all))

(include-file "include/request.hrl")

(defun inv
  ((_ (= (match-req living living) req))
    (do-inv
      (em_living:get_objects living)
      req)
    `#(ok ,req)))

; cmd_inv(_Args, #req{living=Liv}=Req) ->
;   Obs = em_living:get_objects(Liv),
;   do_inv(Obs, Req),
;   {ok, Req}.

(defun do-inv
  (('() req)
    (lmud-util:print "You're not carrying anything.\n" req))
  ((objs req)
    (lmud-util:print "You're carrying:\n" req)
    (lmud-util:print (desc-inv objs '()) req)))

; do_inv([], Req) ->
;   print("You're not carrying anything.\n", Req);
; do_inv(Obs, Req) ->
;   print("You're carrying:\n", Req),
;   print(desc_inv(Obs, []), Req).

(defun desc-inv
  (('() result)
    result)
  (((cons obj objs) result)
    (let ((line `(" " ,(em_object:a_short obj) "\n")))
      (desc-inv objs `(,result ,line)))))

; desc_inv([], Result) -> Result;
; desc_inv([Ob|Obs], Result) ->
;   Line = [" ", em_object:a_short(Ob), "\n"],
;   desc_inv(Obs, [Result, Line]).

(defun who (_ req)
  (let ())
  (lmud-util:print
    (list "Users:\n" (em_game:get_user_names))
    req)
  `#(ok ,req))

(defun news (_ req)
  (lmud-util:print
    (list "\nHeadlines\n---------\n\n"
          "There is no new news. Which, of course, is good news.\n")
    req)
  `#(ok ,req))

(defun setdesc
  ((args (= (match-req living living) req))
    (em_living:set_desc
      living
      (io_lib:format "~p" (list (string:join args " "))))
    `#(ok ,req)))
