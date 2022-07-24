-record(state_character, {
          name="noname" :: em_character:name_type(),
          room :: em_room:room_pid(),
          client,
          desc="" :: string(),
          objects=[] :: [em_object:object()]
        }).

-record(object, {ids=[]::em_object:id_list(),
                 plurals=[]::em_object:id_list(),
                 adjs=[]::em_object:adj_list(),
                 primary_id=""::em_object:id(),
                 primary_adj=""::em_object:adj(),
                 short="nondescript thing"::string(),
                 desc=""::string(),
                 show_in_room=""::string(),
                 proper_name=""::string(),
                 quantity=0::em_object:count(),
                 is_attached=false::boolean(),
                 is_plural=false::boolean(),
                 is_unique=false::boolean(),
                 template="dummy"::string()
        }).
