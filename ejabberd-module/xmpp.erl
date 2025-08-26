%%%----------------------------------------------------------------------
%%% Mock xmpp module for compilation testing
%%%----------------------------------------------------------------------

-module(xmpp).
-include("xmpp.hrl").

% Export mock functions that are called in the main module
-export([make_error/2, err_internal_server_error/0, err_resource_constraint/0, 
         err_not_acceptable/0, err_forbidden/0, err_bad_request/0, err_not_allowed/0]).

make_error(IQ, Error) -> 
    IQ#iq{type = error, sub_els = [Error]}.

err_internal_server_error() -> {error, internal_server_error}.
err_resource_constraint() -> {error, resource_constraint}.
err_not_acceptable() -> {error, not_acceptable}.
err_forbidden() -> {error, forbidden}.
err_bad_request() -> {error, bad_request}.
err_not_allowed() -> {error, not_allowed}.
