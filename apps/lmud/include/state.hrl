%% Game state

-record(state_character, {
         id=lmud:id() :: string(),
         name :: lmud_character:name_type(),
         desc="" :: string(),
         level :: integer(),
         type=undefined :: atom(),
         subtype=undefined :: atom(),
         species=undefined :: atom(),
         room :: lmud_room:room_pid(),
         objects=[] :: [lmud_object:object()],
         client :: any(),
         'connected-since'
      }).

-record(state_game, {
         id=lmud:id() :: string(),
         users=[] :: lmud_game:users(),
         characters=[] :: pid()
      }).

-record(state_room, {
         id=lmud:id() :: string(),
         name :: string(),
         title="" :: string(),
         brief="" :: string(),
         desc="" :: string(),
         people=[], % TODO: rename this to pcs; also add npcs
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
         id=lmud:id() :: string(),
         name,
         email,
         password="" :: string(),
         privileges=ordsets:new(),
         'member-since'=binary_to_list(iso8601:format(erlang:timestamp())),
         conn,
         character :: pid()
      }).

-record(object, {
         id=lmud:id() :: string(),
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
