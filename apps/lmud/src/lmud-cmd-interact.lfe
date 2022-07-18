(defmodule lmud-cmd-interact
  (export all))

(include-lib "apps/lmud/include/request.hrl")

(defun drop
  (('() req)
    (lmud-io:print "Drop what?\n" req)
    `#(ok ,req))
  (((cons id _) (= (match-req living living) req))
    (try-drop
      id
      (em_living:get_objects living)
      req)
    `#(ok ,req)))

(defun try-drop
  ((_ '() req)
    (lmud-io:print "You don't have anything like that.\n" req))
  ((id (cons obj objs) req)
    (case (em_object:has_id obj id)
      ('true (do-drop obj req))
      ('false (try-drop id objs req)))))

(defun do-drop
  ((obj (= (match-req living living) req))
    (let ((name (em_living:get_name living))
          (room (em_living:get_room living))
          (a-short (em_object:a_short obj))
          (the-short (em_object:the_short obj)))
      (try
        (progn (em_living:move_object living obj `#(to_room ,room))
          (lmud-io:print "You drop ~s.\n" `(,the-short) req)
          (em_room:print_except room living "~s drops ~s.~n" `(,name ,a-short)))
        (catch ((tuple 'throw (tuple 'em_living 'not_found) _)
          (lmud-io:print "You don't have anything like that.\n" req)))))))

(defun take
  (('() req)
    (lmud-io:print "Take what?\n" req)
    `#(ok ,req))
  (((cons id _) (= (match-req living living) req))
    (let* ((room (em_living:get_room living))
           (objs (lists:filter
                   (lambda (obj)
                     (not (em_object:is_attached obj)))
                   (em_room:get_objects room))))
      (check-take id objs req)
      `#(ok ,req))))

(defun check-take
  ((_ '() req)
    (lmud-io:print "There's no such thing here.\n" req))
  ((id (cons obj objs) (= (match-req living living) req))
    (case (em_object:has_id obj id)
      ('true (do-take obj living req))
      ('false (check-take id objs req)))))

(defun do-take (obj living req)
  (let ((name (em_living:get_name living))
        (room (em_living:get_room living))
        (the-short (em_object:the_short obj)))
    (lmud-io:print "You take ~s.\n" `(,the-short) req)
    (em_room:remove_object room obj)
    (em_room:print_except room living "~s takes ~s.~n" `(,name ,the-short))
    (em_living:add_object living obj)))

(defun glance
  ((_ (= (match-req living living) req))
    (let* ((room (em_living:get_room living))
           (desc (em_room:describe_except room living)))
      (lmud-io:print desc req)
      `#(ok ,req))))

(defun look
  (('() (= (match-req living living) req))
    (let* ((room (em_living:get_room living))
           (msg (em_room:looking room living)))
      (lmud-io:print msg req)
      `#(ok ,req)))
  (((cons raw-id _) (= (match-req living living) req))
    (let* ((room (em_living:get_room living))
           (objs (em_room:get_objects room))
           (id (string:to_lower raw-id))
           (people (lists:delete living (em_room:get_people room))))
      (case (do-look-obj id objs req)
        ('ok
          `#(ok ,req))
        ((tuple 'error 'not_found)
          (case (do-look-living id people req)
            ('ok
              `#(ok ,req))
            ((tuple 'error 'not_found)
              (lmud-io:print "There's no such thing here.\n" req)
              `#(ok ,req))))))))

(defun do-look-obj
  ((_ '() req)
    `#(error not_found))
  ((id (cons obj objs) req)
    (case (em_object:has_id obj id)
      ('true
        (lmud-io:print
          "~s\n"
          (list (em_text:wrapline
                  (em_object:desc obj)
                  (lmud-config:wrap-width)))
          req)
          'ok)
      ('false
        (do-look-obj id objs req)))))

(defun do-look-living
  ((_ '() req)
    `#(error not_found))
  ((id (cons living people) req)
    (case (string:to_lower (em_living:get_name living))
      (name (when (== name id))
        (lmud-io:print
          (++
            (em_text:wrapline (em_living:desc living)
              (lmud-config:wrap-width))
            "\n")
          req))
      (_
        (do-look-living id people req)))))
