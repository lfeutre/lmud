% request.hrl
-record(req, {conn, user, living, queue=[], handlers=[]}).

-define(req_done, {done, Req}).
-define(req_next(Fun), {ok, {?MODULE, Fun, []}, Req}).
-define(req_next(Fun, Args), {ok, {?MODULE, Fun, Args}, Req}).
-define(req_next(Fun, Args, State), {ok, {?MODULE, Fun, Args}, State}).
-define(req_next_and_link(Fun, Args, State), {link, {?MODULE, Fun, Args}, State}).
-define(req_next_and_link(Module, Fun, Args, State), {link, {Module, Fun, Args}, State}).

