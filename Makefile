GAME_HOST ?= 127.0.0.1
GAME_PORT ?= 1203
REL_ERL_LIBS ?= $(ERL_LIBS):./_build/default/lib

default: build

build:
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 compile
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 lfe compile
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 release

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
