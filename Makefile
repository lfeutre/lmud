GAME_HOST ?= 127.0.0.1
GAME_PORT ?= 1203

default: build

build:
	@rebar3 do lfe compile,compile,release

clean:
	@rm -rf _build

start:
	@_build/default/rel/lmud/bin/lmud foreground

connect:
	@echo ">> Connecting to game server ..."
	@echo
	@echo "(use environment variebles GAME_HOST and"
	@echo "GAME_PORT to override defaults)"
	@echo
	@rlwrap telnet $(GAME_HOST) $(GAME_PORT)
