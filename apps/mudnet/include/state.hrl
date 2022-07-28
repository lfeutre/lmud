-record(state_acceptor, {
         lsock :: port()
      }).

-record(state_conn, {
         socket :: port(),
         session :: pid(),
         telnet_session
      }).

-record(state_listener, {
         lsock :: port(),
         port :: 0..65535,
         acceptors :: non_neg_integer()
      }).
