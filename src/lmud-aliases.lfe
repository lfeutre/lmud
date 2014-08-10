(defmodule lmud-aliases
  (export all))

; %% Below are general aliases/shortcuts for commands.
; parse_cmd("?", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("h", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("get", Args, Line, Req) ->
;   parse_cmd("take", Args, Line, Req);
(defun base ()
  'noop)

; %% Below are WoW aliases/shortcuts for commands.
; % parse_cmd("?", Args, Line, Req) ->
; %   parse_cmd("say", Args, Line, Req);
(defun aliases-wow ()
  'noop)

; %% Below are IRC aliases/shortcuts for commands.
; parse_cmd("/?", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("/h", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("/help", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("/quit", Args, Line, Req) ->
;   parse_cmd("quit", Args, Line, Req);
; parse_cmd("/q", Args, Line, Req) ->
;   parse_cmd("quit", Args, Line, Req);
(defun aliases-irc ()
  'noop)

; %% Below are TinyMUSH aliases/shortcuts for commands.
; parse_cmd("\"", Args, Line, Req) ->
;   parse_cmd("say", Args, Line, Req);
; parse_cmd("/me", Args, Line, Req) ->
;   parse_cmd("emote", Args, Line, Req);
; parse_cmd(":", Args, Line, Req) ->
;   parse_cmd("emote", Args, Line, Req);
; parse_cmd(";", Args, Line, Req) ->
;   parse_cmd("emote_ns", Args, Line, Req);
; parse_cmd("'", Args=[_,_|_], Line, Req) ->
;   parse_cmd("tell", Args, Line, Req);
; parse_cmd("\\\\", Args=[_,_|_], Line, Req) ->
;   parse_cmd("tell", Args, Line, Req);
(defun aliases-tinymud ()
  'noop)
