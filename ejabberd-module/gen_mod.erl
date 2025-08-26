%%%----------------------------------------------------------------------
%%% Mock gen_mod module for compilation testing
%%%----------------------------------------------------------------------

-module(gen_mod).
-export([get_opt/2, get_module_opt/4]).

get_opt(iqdisc, _Opts) -> one_queue;
get_opt(_, _) -> undefined.

get_module_opt(_Host, _Module, hmac_server_url, Default) -> Default;
get_module_opt(_Host, _Module, hmac_shared_secret, Default) -> Default;
get_module_opt(_Host, _Module, max_size, Default) -> Default;
get_module_opt(_Host, _Module, quota_per_user, Default) -> Default;
get_module_opt(_Host, _Module, token_expiry, Default) -> Default;
get_module_opt(_Host, _Module, allowed_extensions, Default) -> Default;
get_module_opt(_Host, _Module, _, Default) -> Default.
