%%%----------------------------------------------------------------------
%%% Mock jid module for compilation testing
%%%----------------------------------------------------------------------

-module(jid).
-export([user/1, server/1]).

user({jid, User, _Server, _Resource}) -> User;
user(_) -> <<"mockuser">>.

server({jid, _User, Server, _Resource}) -> Server;
server(_) -> <<"mockserver">>.
