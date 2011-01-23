%% Basic types
-type count()           :: non_neg_integer().
-type file_path()       :: string().
-type orddict(Key, Val) :: [{Key, Val}].
-type proplist()        :: [{atom(), any()}].
-type mfargs()          :: {atom(), atom(), list()}.

%% Network stuff
-type socket() :: port().
-type inet_port() :: 0..65535.

