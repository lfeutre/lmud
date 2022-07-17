(defmodule lmud-cmd-speak
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun base-emote
  ((args (= (match-req living living) req) format-str)
    (let ((text (em_english:punctuate (string:join args " ")))
          (name (em_living:get_name living))
          (room (em_living:get_room living)))
      (em_room:print_except 'yellowb room living format-str (list name text))
      (lmud-util:print 'yellowb format-str (list name text) req)
      `#(ok ,req))))

(defun emote
  ((args (= (match-req living living) req))
    (base-emote args req "~s ~s~n")))

(defun emote-ns
  ((args (= (match-req living living) req))
    (base-emote args req "~s~s~n")))

(defun think
  ((args (= (match-req living living) req))
    (let ((text (em_english:punctuate (string:join args " ")))
          (name (em_living:get_name living))
          (room (em_living:get_room living)))
      (em_room:print_except 'blackb room living
                            "~s is pondering something.~n" (list name))
      (lmud-util:print 'blackb "~s thinks ~s~n" (list name text) req)
      `#(ok ,req))))

(defun say
  (((cons word words) (= (match-req living living) req))
    (let ((text (string:join (cons (em_text:capitalize word) words) " "))
          (name (em_living:get_name living))
          (room (em_living:get_room living)))
      (em_room:print_except 'yellowb room living
                            "~s says, \"~s\"~n" (list name text))
      (lmud-util:print 'yellowb "You say, \"~s\"~n" (list text) req)
      `#(ok ,req))))

(defun whisper
  (('() req)
    (lmud-util:print "Whisper what to whom?" req)
    `#(ok ,req))
  (((cons who (cons word words)) (= (match-req user user) req))
    (let ((name (lmud-player:get_name user))
          (text (string:join (cons (em_text:capitalize word) words) " ")))
      (case (em_game:lookup_user who)
        (`#(error not_found)
          (lmud-util:print "There is not such user.\n" req))
        (`#(ok #(,addressee ,_)) (when (== addressee name))
          (lmud-util:print "Talking to yourself again, eh?\n" req))
        (`#(ok #(,other-name ,other-user))
          (lmud-player:print
            other-user
            (++ (color:magenta "[Whisper] ") "From ~s: \"~s\"~n")
            (list name text))
          (lmud-util:print
            (++ (color:magenta "[Whisper] ") "To ~s: \"~s\"~n")
            (list other-name text) req))))
    `#(ok ,req))
  (((list who) req)
    (lmud-util:print (++ "Whisper what to " who "?") req)
    `#(ok ,req)))
