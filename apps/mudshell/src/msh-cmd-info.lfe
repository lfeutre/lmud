(defmodule msh-cmd-info
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun inv
  ((_ (= (match-req character character) req))
   (do-inv
    (lmud_character:get_objects character)
    req)
   `#(ok ,req)))

(defun do-inv
  (('() req)
   (lmud-io:print "You're not carrying anything.\n" req))
  ((objs req)
   (lmud-io:print "You're carrying:\n" req)
   (lmud-io:print (desc-inv objs '()) req)))

(defun desc-inv
  (('() result)
   result)
  (((cons obj objs) result)
   (let ((line `(" " ,(lmud_object:a_short obj) "\n")))
     (desc-inv objs `(,result ,line)))))

(defun who (_ req)
  (lmud-io:print (list "Users:\n" (lmud_game:get_user_names)) req)
  `#(ok ,req))

(defun news (_ req)
  (lmud-io:print
   (list "\nHeadlines\n---------\n\n"
         (lmud-io:read-news))
   req)
  `#(ok ,req))

(defun setdesc
  ((args (= (match-req character character) req))
   (lmud_character:set_desc
    character
    (io_lib:format "~p" (list (string:join args " "))))
   `#(ok ,req)))
