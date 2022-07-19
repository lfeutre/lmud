FROM erlang:25

ARG branch=main

EXPOSE 1203

RUN git clone https://github.com/lfex/lmud.git
RUN cd lmud && make build
WORKDIR ./lmud

CMD ["make", "start"]
#CMD ["bash"]
