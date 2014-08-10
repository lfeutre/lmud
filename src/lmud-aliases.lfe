(defmodule lmud-aliases
  (export all))

; %% Below are general aliases/shortcuts for commands.
; parse_cmd("?", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; parse_cmd("h", Args, Line, Req) ->
;   parse_cmd("help", Args, Line, Req);
; get -> take
; tell -> whisper
; north -> go north
; east -> go east
; west -> go west
; south -> go south
(defun base ()
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
; /me -> emote
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
; pose -> emote
; page -> whisper
(defun aliases-tinymud ()
  'noop)

; %% Below are WoW aliases/shortcuts for commands.
; % parse_cmd("?", Args, Line, Req) ->
; %   parse_cmd("say", Args, Line, Req);
; /emote -> emote
; /em -> emote
(defun aliases-wow ()
  'noop)
