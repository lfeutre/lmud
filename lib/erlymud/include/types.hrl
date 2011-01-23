%% Basic types
-type count()           :: non_neg_integer().
-type orddict(Key, Val) :: [{Key, Val}].

%% Network stuff
-type socket() :: port().
-type inet_port() :: 0..65535.

