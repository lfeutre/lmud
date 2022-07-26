(defmodule lmud-cmd-interact
  (export all))

(include-lib "apps/mudshell/include/request.hrl")

(defun drop
  (('() req)
    (lmud-io:print "Drop what?\n" req)
    `#(ok ,req))
  (((cons id _) (= (match-req character character) req))
    (try-drop
      id
      (lmud_character:get_objects character)
      req)
    `#(ok ,req)))

(defun try-drop
  ((_ '() req)
    (lmud-io:print "You don't have anything like that.\n" req))
  ((id (cons obj objs) req)
    (case (lmud_object:has_id obj id)
      ('true (do-drop obj req))
      ('false (try-drop id objs req)))))

(defun do-drop
  ((obj (= (match-req character character) req))
    (let ((name (lmud_character:name character))
          (room (lmud_character:get_room character))
          (a-short (lmud_object:a_short obj))
          (the-short (lmud_object:the_short obj)))
      (try
        (progn (lmud_character:move_object character obj `#(to_room ,room))
          (lmud-io:print "You drop ~s.\n" `(,the-short) req)
          (lmud_room:print_except room character "~s drops ~s.~n" `(,name ,a-short)))
        (catch ((tuple 'throw (tuple 'lmud_character 'not_found) _)
          (lmud-io:print "You don't have anything like that.\n" req)))))))

(defun take
  (('() req)
    (lmud-io:print "Take what?\n" req)
    `#(ok ,req))
  (((cons id _) (= (match-req character character) req))
    (let* ((room (lmud_character:get_room character))
           (objs (lists:filter
                   (lambda (obj)
                     (not (lmud_object:is_attached obj)))
                   (lmud_room:get_objects room))))
      (check-take id objs req)
      `#(ok ,req))))

(defun check-take
  ((_ '() req)
    (lmud-io:print "There's no such thing here.\n" req))
  ((id (cons obj objs) (= (match-req character character) req))
    (case (lmud_object:has_id obj id)
      ('true (do-take obj character req))
      ('false (check-take id objs req)))))

(defun do-take (obj character req)
  (let ((name (lmud_character:name character))
        (room (lmud_character:get_room character))
        (the-short (lmud_object:the_short obj)))
    (lmud-io:print "You take ~s.\n" `(,the-short) req)
    (lmud_room:remove_object room obj)
    (lmud_room:print_except room character "~s takes ~s.~n" `(,name ,the-short))
    (lmud_character:add_object character obj)))

(defun glance
  ((_ (= (match-req character character) req))
    (let* ((room (lmud_character:get_room character))
           (desc (lmud_room:describe_except room character)))
      (lmud-io:print desc req)
      `#(ok ,req))))

(defun look
  (('() (= (match-req character character) req))
    (let* ((room (lmud_character:get_room character))
           (msg (lmud_room:looking room character)))
      (lmud-io:print msg req)
      `#(ok ,req)))
  (((cons raw-id _) (= (match-req character character) req))
    (let* ((room (lmud_character:get_room character))
           (objs (lmud_room:get_objects room))
           (id (string:to_lower raw-id))
           (people (lists:delete character (lmud_room:get_people room))))
      (case (do-look-obj id objs req)
        ('ok
          `#(ok ,req))
        ((tuple 'error 'not_found)
          (case (do-look-character id people req)
            ('ok
              `#(ok ,req))
            ((tuple 'error 'not_found)
              (lmud-io:print "There's no such thing here.\n" req)
              `#(ok ,req))))))))

(defun do-look-obj
  ((_ '() req)
    `#(error not_found))
  ((id (cons obj objs) req)
    (case (lmud_object:has_id obj id)
      ('true
        (lmud-io:print
          "~s\n"
          (list (em_text:wrapline
                  (lmud_object:desc obj)
                  (lmud-config:wrap-width)))
          req)
          'ok)
      ('false
        (do-look-obj id objs req)))))

(defun do-look-character
  ((_ '() req)
    `#(error not_found))
  ((id (cons character people) req)
    (case (string:to_lower (lmud_character:name character))
      (name (when (== name id))
        (lmud-io:print
          (++
            (em_text:wrapline (lmud_character:desc character)
              (lmud-config:wrap-width))
            "\n")
          req))
      (_
        (do-look-character id people req)))))
