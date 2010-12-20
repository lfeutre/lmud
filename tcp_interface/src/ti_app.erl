-module(ti_app).

-behaviour(application).

-export([start/2, stop/1, default_handler/2]).

-define(DEFAULT_PORT, 1155).
-define(DEFAULT_HANDLER, {ti_app, default_handler, {}}).

default_handler(Data, State) ->
  case Data of
    "quit" -> 
      {done, "Quitting..\n"};
    "" ->
      {ok, Data, State};
    _Other -> 
      Response = "ECHO: " ++ Data ++ "\n",
      {ok, Response, State}
  end.

start(_StartType, _StartArgs) ->
  Port = case application:get_env(tcp_interface, port) of
           {ok, P} -> P;
           undefined -> ?DEFAULT_PORT
         end,
  Handler = case application:get_env(tcp_interface, handler) of
           {ok, H} -> H;
           undefined -> ?DEFAULT_HANDLER
         end,
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}, 
                                      {nodelay, true}, 
                                      {reuseaddr, true}]),
  case ti_sup:start_link(LSock, Handler) of
    {ok, Pid} ->
      ti_sup:start_child(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.
