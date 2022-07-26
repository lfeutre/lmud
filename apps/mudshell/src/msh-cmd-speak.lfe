(defmodule msh-cmd-speak
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun base-emote
  ((args (= (match-req character character) req) format-str)
    (let ((text (msh_english:punctuate (string:join args " ")))
          (name (lmud_character:name character))
          (room (lmud_character:get_room character)))
      (lmud_room:print_except 'yellowb room character format-str (list name text))
      (lmud-io:print 'yellowb format-str (list name text) req)
      `#(ok ,req))))

(defun emote
  ((args (= (match-req character character) req))
    (base-emote args req "~s ~s~n")))

(defun emote-ns
  ((args (= (match-req character character) req))
    (base-emote args req "~s~s~n")))

(defun think
  ((args (= (match-req character character) req))
    (let ((text (msh_english:punctuate (string:join args " ")))
          (name (lmud_character:name character))
          (room (lmud_character:get_room character)))
      (lmud_room:print_except 'blackb room character
                            "~s is pondering something.~n" (list name))
      (lmud-io:print 'blackb "~s thinks ~s~n" (list name text) req)
      `#(ok ,req))))

(defun say
  (((cons word words) (= (match-req character character) req))
    (let ((text (string:join (cons (msh_text:capitalize word) words) " "))
          (name (lmud_character:name character))
          (room (lmud_character:get_room character)))
      (lmud_room:print_except 'yellowb room character
                            "~s says, \"~s\"~n" (list name text))
      (lmud-io:print 'yellowb "You say, \"~s\"~n" (list text) req)
      `#(ok ,req))))

(defun whisper
  (('() req)
    (lmud-io:print "Whisper what to whom?" req)
    `#(ok ,req))
  (((cons who (cons word words)) (= (match-req user user) req))
    (let ((name (lmud-user:name user))
          (text (string:join (cons (msh_text:capitalize word) words) " ")))
      (case (lmud_game:lookup_user who)
        (`#(error not_found)
          (lmud-io:print "There is not such user.\n" req))
        (`#(ok #(,addressee ,_)) (when (== addressee name))
          (lmud-io:print "Talking to yourself again, eh?\n" req))
        (`#(ok #(,other-name ,other-user))
          (lmud-user:print
            other-user
            (++ (color:magenta "[Whisper] ") "From ~s: \"~s\"~n")
            (list name text))
          (lmud-io:print
            (++ (color:magenta "[Whisper] ") "To ~s: \"~s\"~n")
            (list other-name text) req))))
    `#(ok ,req))
  (((list who) req)
    (lmud-io:print (++ "Whisper what to " who "?") req)
    `#(ok ,req)))
