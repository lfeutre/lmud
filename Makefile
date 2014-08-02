compile:
	rebar get-deps
	rebar compile

rel: compile
	erl -pa lib/erlymud/ebin/ \
	-noshell \
	-eval "systools:make_script(\"erlymud-0.3.5\", [local])." \
	-s erlang halt

run:
	erl -boot ./erlymud-0.3.5
