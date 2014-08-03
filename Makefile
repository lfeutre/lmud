compile:
	rebar get-deps
	rebar compile

rel: compile
	erl -pa ebin/ \
	-noshell \
	-eval "systools:make_script(\"lmud-0.4.0\", [local])." \
	-s erlang halt

run:
	erl -boot ./lmud-0.4.0
