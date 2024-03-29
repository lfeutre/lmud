(defmodule msh-aliases
  (export all))

(defun base ()
  `((#(name "?")
     #(command "help")
     #(args ()))
    (#(name "h")
     #(command "help")
     #(args ()))
    (#(name "help alias")
     #(command "help")
     #(args ("aliases")))
    (#(name "help commands")
     #(command "help")
     #(args ("all")))
    (#(name "help priv")
     #(command "help")
     #(args ("privileges")))
    (#(name "examine")
     #(command "look")
     #(args ()))
    (#(name "get")
     #(command "take")
     #(args ()))
    (#(name "tell")
     #(command "whisper")
     #(args ()))
    (#(name "walk")
     #(command "go")
     #(args ()))
    (#(name "north")
     #(command "go")
     #(args ("north")))
    (#(name "east")
     #(command "go")
     #(args ("east")))
    (#(name "west")
     #(command "go")
     #(args ("west")))
    (#(name "south")
     #(command "go")
     #(args ("south")))
    (#(name "n")
     #(command "go")
     #(args ("north")))
    (#(name "e")
     #(command "go")
     #(args ("east")))
    (#(name "w")
     #(command "go")
     #(args ("west")))
    (#(name "s")
     #(command "go")
     #(args ("south")))
    (#(name "exit")
     #(command "quit")
     #(args ()))))

(defun aliases-irc ()
  `((#(name "/?")
     #(command "help")
     #(args ()))
    (#(name "/h")
     #(command "help")
     #(args ()))
    (#(name "/help")
     #(command "help")
     #(args ()))
    (#(name "/me")
     #(command "emote")
     #(args ()))
    (#(name "/me-ns")
     #(command "emote-ns")
     #(args ()))
    (#(name "/msg")
     #(command "whisper")
     #(args ()))
    (#(name "/q")
     #(command "quit")
     #(args ()))
    (#(name "/quit")
     #(command "quit")
     #(args ()))))

(defun aliases-tinymud ()
  `((#(name "\"")
     #(command "say")
     #(args ()))
    (#(name "pose")
     #(command "emote")
     #(args ()))
    (#(name ":")
     #(command "emote")
     #(args ()))
    (#(name ";")
     #(command "emote-ns")
     #(args ()))
    (#(name "page")
     #(command "whisper")
     #(args ()))
    (#(name "'")
     #(command "whisper")
     #(args ()))
    ; (#(name '"\\\\")
    ;  #(command "whisper")
    ;  #(args ()))
    ))


; %% Below are WoW aliases/shortcuts for commands.
; % parse_cmd("?", Args, Line, Req) ->
; %   parse_cmd("say", Args, Line, Req);
; /emote -> emote
; /em -> emote
(defun aliases-wow ()
  `((#(name "/say")
     #(command "say")
     #(args ()))
    (#(name "/s")
     #(command "say")
     #(args ()))
    (#(name "/emote")
     #(command "emote")
     #(args ()))
    (#(name "/em")
     #(command "emote")
     #(args ()))
    (#(name "/e")
     #(command "emote")
     #(args ()))
    (#(name "/whisper")
     #(command "whisper")
     #(args ()))
    (#(name "/tell")
     #(command "whisper")
     #(args ()))
    (#(name "/send")
     #(command "whisper")
     #(args ()))
    (#(name "/w")
     #(command "whisper")
     #(args ()))
    (#(name "/t")
     #(command "whisper")
     #(args ()))))

(defun all ()
  `(#("Common Alias Group" ,(base))
    #("IRC Alias Group" ,(aliases-irc))
    #("TinyMU* Alias Group" ,(aliases-tinymud))
    #("WoW Alias Group" ,(aliases-wow))))

(defun get-alias (name prop-list)
  (msh-cmd:get-command name prop-list))

(defun get-value (alias-name prop-list key)
  (let ((result (get-alias alias-name (all))))
    (case result
      ((tuple 'error _)
        result)
      (_
        (msh-cmd:get-command
          (proplists:get_value key result)
          (msh-cmd:all))))))

(defun get-command (alias-name prop-list)
  (let ((result (get-value alias-name prop-list 'command)))
    (case result
      ((tuple 'error _) result)
      (_
        (lists:sort
          (merge-args
            result
            (get-args alias-name prop-list)))))))

(defun get-args (alias-name prop-list)
  (orddict:filter
    (lambda (x _)
      (== x 'args))
    (get-alias alias-name prop-list)))

(defun merge-args (command-data alias-args)
  "The 'command-data' parameter should have the form that every entry in the
  command data structure has. The 'alias-args' parameter should have a similar
  proplist form, but with only one key, 'args', and a value that is a list (can
  be empty)."
  (orddict:merge
    (lambda (k v1 v2)
      (lists:merge (list v1 v2)))
    (orddict:from_list command-data)
    (orddict:from_list alias-args)))
