%% Game state

-record(state_character, {
         name="noname" :: lmud_character:name_type(),
         room :: lmud_room:room_pid(),
         client :: any(),
         desc="" :: string(),
         objects=[] :: [lmud_object:object()]
      }).

-record(state_game, {
         users=[] :: lmud_game:users()
      }).

-record(state_room, {
         name :: string(),
         title="" :: string(),
         brief="" :: string(),
         desc="" :: string(),
         people=[], % TODO: rename this to characters; also add npcs
         exits=[] :: lmud_room:exits(),
         objects=[] :: [lmud_object:object()],
         resets=[] :: [lmud_object:object()],
         event_listeners=[]
      }).

-record(state_speller, {
         spell_mod :: atom(),
         spell_state :: any(),
         caster :: any(),
         room :: any()
      }).

-record(state_user, {
         name,
         conn,
         password="" :: string(),
         privileges=ordsets:new()
      }).

-record(object, {
         ids=[] :: lmud_object:id_list(),
         plurals=[] :: lmud_object:id_list(),
         adjs=[] :: lmud_object:adj_list(),
         primary_id="" :: lmud_object:id(),
         primary_adj="" :: lmud_object:adj(),
         short="nondescript thing" :: string(),
         desc="" :: string(),
         show_in_room="" :: string(),
         proper_name="" :: string(),
         quantity=0 :: lmud_object:count(),
         is_attached=false :: boolean(),
         is_plural=false :: boolean(),
         is_unique=false :: boolean(),
         template="dummy" :: string()
      }).

%% System state

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
