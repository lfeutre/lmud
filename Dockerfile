FROM erlang:25

ARG branch=main

EXPOSE 1203

RUN git clone https://github.com/lfex/lmud.git
RUN cd lmud && rebar3 compile && rebar3 lfe compile && rebar3 release

WORKDIR ./lmud

VOLUME ["/lmud/_build/default/rel/lmud/data"]

CMD ["_build/default/rel/lmud/bin/lmud", "foreground"]
#CMD ["bash"]
#CMD ["rebar3", "lfe", "repl", "--", "-sname", "lmud"]
#CMD ["rebar3", "shell", "--", "-sname", "lmud"]

#ENTRYPOINT ["_build/default/rel/lmud/bin/lmud"]
