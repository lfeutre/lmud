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

(defun member
  (('() req)
   (lmud-io:print "which one?" req)
   `#(ok ,req))
  (((cons name _) req)
   (case (mudstore:load "users" name)
     (`#(ok ,data) (lmud-io:print "~s" (list (proplists:get_value 'member-since data)) req))
     (_ (lmud-io:print "Couldn't find user '~s'" (list name) req)))
   `#(ok ,req)))

(defun members (_ req)
  (lmud-io:print (list "Logged-in Users:\n" (lmud_game:get_user_names)) req)
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

(defun when
  (('() req)
   (lmud-io:print "when what?" req)
   `#(ok ,req))
  (((cons name _) req)
   (case (lmud_game:connected-since name)
     (`#(ok ,since) (lmud-io:print "~s" (list since) req))
     (_ (lmud-io:print "~s isn't currently playing." (list name) req)))
   `#(ok ,req)))

(defun who (_ req)
  (lmud-io:print (list "Active Characters:\n" (lmud_game:get_character_names)) req)
  `#(ok ,req))
