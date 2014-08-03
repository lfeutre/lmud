% request.hrl
-record(req, {conn::em_conn:conn_pid(), user::em_user:user_pid(), 
              living::em_living:living_pid(), 
              queue=[]::[init | {input, string()}], 
              handlers=[]::[req_handler()]}).

-define(req_done, {done, Req}).
-define(req_next(Fun), {ok, {?MODULE, Fun, []}, Req}).
-define(req_next(Fun, Args), {ok, {?MODULE, Fun, Args}, Req}).
-define(req_next(Fun, Args, State), {ok, {?MODULE, Fun, Args}, State}).
-define(req_next_and_link(Fun, Args, State), {link, {?MODULE, Fun, Args}, State}).
-define(req_next_and_link(Module, Fun, Args, State), {link, {Module, Fun, Args}, State}).

-type req_handler() :: {atom(), atom(), list()}.

-type req()      :: #req{}.
-type req_done() :: {done, req()}.
-type req_ok()   :: {ok, req_handler(), req()}.
-type req_link() :: {link, req_handler(), req()}.
-type req_any()  :: req_done() | req_ok().

