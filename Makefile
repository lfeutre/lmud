GAME_HOST ?= 127.0.0.1
GAME_PORT ?= 1203
REL_ERL_LIBS ?= $(ERL_LIBS):./_build/default/lib
ERLANG_COOKIE = $(shell grep setcookie _build/default/rel/lmud/releases/0.5.0-dev/vm.args | awk {'print $$2'} | sed s/\'//g)

default: build

build:
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 compile
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 lfe compile
	@ERL_LIBS=$(REL_ERL_LIBS) rebar3 release

clean:
	@rm -rf _build/default/lib/lmud/ebin/*

clean-all:
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
	@echo "(use environment variables GAME_HOST and"
	@echo "GAME_PORT to override default connection"
	@echo "options)"
	@echo
	@rlwrap telnet $(GAME_HOST) $(GAME_PORT)

dockerise: BRANCH ?= main
dockerise:
	@docker build -t lfex/lmud --build-arg branch=$(BRANCH) .

docker-push:
	@docker push lfex/lmud 

docker-run:
	@mkdir -p game-data
	@cp -r data/* game-data/
	@docker rm -f lmud
	@docker run -it \
		-p 1203:1203 \
		--name lmud \
		--mount type=bind,source=`pwd`/game-data,target=/lmud/_build/default/rel/data \
		lfex/lmud

docker-bash:
	@docker exec -it lmud bash

docker-stop:
	@docker exec -it -e RELX_COOKIE=$(ERLANG_COOKIE) lmud _build/default/rel/lmud/bin/lmud stop
