ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif
EBIN=./ebin
LFE_DEPS=./deps/lfe/ebin
LUTIL_DEPS=./deps/lutil/ebin
LFEC=./deps/lfe/bin/lfec -pa $(LFE_DEPS) -o $(EBIN)
LFE_LIB=$(LFE_DEPS)/lfe_lib.beam
CUSTOM_BEHAVES=$(EBIN)/lmud-event-listener.beam
LMUD_UTIL=$(EBIN)/lmud-util.beam

ERL_START=erl -pa $(EBIN) -pa $(LFE_DEPS) -noshell -eval
ERL_END=-s erlang halt

GET_NAME_CMD=$(ERL_START) "'lmud-util':'print-name'()." $(ERL_END)
GET_PORT_CMD=$(ERL_START) "'lmud-util':'print-port'()." $(ERL_END)
GET_VERSION_CMD=$(ERL_START) "'lmud-util':'print-version'()." $(ERL_END)
REL_CONTENT_CMD=$(ERL_START) "'lmud-util':'print-release-data'()." $(ERL_END)


$(LFE_LIB):
	@cp src/*.app.src ./ebin/ && mv ./ebin/*.app.src \
	`ls -1 src/*.app.src|sed -e 's/\.src//g'|sed -e 's/src\//ebin\//g'`
	cd deps/lfe && make compile

$(LMUD_UTIL): $(LFE_LIB)
	$(LFEC) src/{lmud-util,lmud-const}.lfe

$(CUSTOM_BEHAVES): $(LFE_LIB)
	$(LFEC) src/{lmud-event-source,lmud-event-listener}.lfe

compile: $(CUSTOM_BEHAVES)
	rebar get-deps
	rebar compile

rel: $(LMUD_UTIL)
rel: REL=$(shell $(GET_NAME_CMD))-$(shell $(GET_VERSION_CMD))
rel: compile
	$(REL_CONTENT_CMD) > $(REL).rel
	$(ERL_START) "systools:make_script(\"$(REL)\", [local])." $(ERL_END)

run: $(LMUD_UTIL)
run: REL=$(shell $(GET_NAME_CMD))-$(shell $(GET_VERSION_CMD))
run:
	erl -pa ./deps/color/ebin -pa $(LUTIL_DEPS) -boot ./$(REL)

clean: $(LMUD_UTIL)
clean: REL=$(shell $(GET_NAME_CMD))-$(shell $(GET_VERSION_CMD))
clean:
	-rm -v $(REL).rel $(REL).boot $(REL).script erl_crash.dump $(EBIN)/*.beam

clean-run: clean rel run

connect:
	-@rlwrap telnet localhost $(shell $(GET_PORT_CMD))

repl:
	./deps/lfe/bin/lfe -pa $(LFE_DEPS) -pa $(EBIN) -pa $(LUTIL_DEPS)

clean-repl: clean rel repl
