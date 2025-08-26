%%%----------------------------------------------------------------------
%%% Mock ejabberd.hrl for compilation testing
%%%----------------------------------------------------------------------

% Mock logging macros
-define(INFO_MSG(Msg, Args), io:format("INFO: " ++ Msg ++ "~n", Args)).
-define(WARNING_MSG(Msg, Args), io:format("WARNING: " ++ Msg ++ "~n", Args)).
-define(DEBUG_MSG(Msg, Args), io:format("DEBUG: " ++ Msg ++ "~n", Args)).
-define(ERROR_MSG(Msg, Args), io:format("ERROR: " ++ Msg ++ "~n", Args)).

% Mock translation macro
-define(T(Text), Text).

% Mock gen_mod functions
-define(gen_mod, gen_mod_mock).

% Mock exports that would normally come from ejabberd
-export([get_opt/2, get_module_opt/4]).

% Mock implementations
get_opt(iqdisc, _Opts) -> one_queue;
get_opt(_, _) -> undefined.

get_module_opt(_Host, _Module, hmac_server_url, Default) -> Default;
get_module_opt(_Host, _Module, hmac_shared_secret, Default) -> Default;
get_module_opt(_Host, _Module, max_size, Default) -> Default;
get_module_opt(_Host, _Module, quota_per_user, Default) -> Default;
get_module_opt(_Host, _Module, token_expiry, Default) -> Default;
get_module_opt(_Host, _Module, allowed_extensions, Default) -> Default;
get_module_opt(_Host, _Module, _, Default) -> Default.
