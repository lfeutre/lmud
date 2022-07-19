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

clean-lock:
	@rm rebar.lock

clean-all: clean clean-lock

start:
	@_build/default/rel/lmud/bin/lmud foreground

fresh-start: clean-all build start

connect:
	@echo ">> Connecting to game server ..."
	@echo
	@echo "(use environment variebles GAME_HOST and"
	@echo "GAME_PORT to override defaults)"
	@echo
	@rlwrap telnet $(GAME_HOST) $(GAME_PORT)

dockerize: BRANCH ?= main
dockerize:
	docker build --build-arg branch=$(BRANCH) .
