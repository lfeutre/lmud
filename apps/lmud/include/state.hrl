%% Game state

-record(state_character, {
         name="noname" :: em_character:name_type(),
         room :: em_room:room_pid(),
         client :: any(),
         desc="" :: string(),
         objects=[] :: [em_object:object()]
      }).

-record(state_game, {
         users=[] :: em_game:users()
      }).

-record(state_room, {
         name :: any(),
         title :: any(),
         brief :: any(),
         desc :: any(),
         people=[],
         exits=[],
         objects=[],
         resets=[],
         event_listeners=[]
      }).

-record(state_speller, {
         spell_mod :: atom(),
         spell_state :: any(),
         caster :: any(),
         room :: any()
      }).

-record(state_uaer, {
         name,
         conn,
         privileges=ordsets:new()
      }).

-record(object, {
         ids=[] :: em_object:id_list(),
         plurals=[] :: em_object:id_list(),
         adjs=[] :: em_object:adj_list(),
         primary_id="" :: em_object:id(),
         primary_adj="" :: em_object:adj(),
         short="nondescript thing" :: string(),
         desc="" :: string(),
         show_in_room="" :: string(),
         proper_name="" :: string(),
         quantity=0 :: em_object:count(),
         is_attached=false :: boolean(),
         is_plural=false :: boolean(),
         is_unique=false :: boolean(),
         template="dummy" :: string()
      }).

%% System state

-record(state_acceptor, {
         lsock::socket()
      }).

-record(state_conn, {
         socket::socket(),
         session::pid(),
         telnet_session
      }).
