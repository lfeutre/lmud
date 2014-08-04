ERL_START=erl -pa ebin -noshell -eval
ERL_END=-s erlang halt
GET_NAME_CMD=$(ERL_START) "'lmud-util':'print-name'()." $(ERL_END)
GET_VERSION_CMD=$(ERL_START) "'lmud-util':'print-version'()." $(ERL_END)
REL_CONTENT_CMD=$(ERL_START) "'lmud-util':'print-release-data'()." $(ERL_END)

PROJ_NAME=$(shell $(GET_NAME_CMD))
VERSION=$(shell $(GET_VERSION_CMD))
REL=$(PROJ_NAME)-$(VERSION)
REL_FILE=$(REL).rel

MK_REL_CMD=$(ERL_START) "systools:make_script(\"$(REL)\", [local])." $(ERL_END)

compile:
	rebar get-deps
	rebar compile

$(REL_FILE):
	$(REL_CONTENT_CMD) > $(REL_FILE)

rel: compile clean $(REL_FILE)
	$(MK_REL_CMD)

run:
	erl -pa ./deps/color/ebin -boot ./$(REL)

clean:
	-rm $(REL_FILE) $(REL).boot $(REL).script
