%%%----------------------------------------------------------------------
%%% Mock xmpp.hrl for compilation testing
%%%----------------------------------------------------------------------

% Mock XMPP record definitions
-record(iq, {type, from, to, sub_els}).
-record(upload_request, {filename, size, 'content-type'}).
-record(upload_slot, {get, put, headers}).
-record(upload_header, {name, value}).
