%%%----------------------------------------------------------------------
%%% Mock gen_iq_handler module for compilation testing
%%%----------------------------------------------------------------------

-module(gen_iq_handler).
-export([add_iq_handler/6, remove_iq_handler/3]).

add_iq_handler(_Type, _Host, _NS, _Module, _Function, _Disc) -> ok.
remove_iq_handler(_Type, _Host, _NS) -> ok.
