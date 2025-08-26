%%%----------------------------------------------------------------------
%%% File    : mod_http_upload_hmac.erl
%%% Author  : HMAC File Server Team
%%% Purpose : XEP-0363 HTTP File Upload with HMAC File Server Integration
%%% Created : 25 Aug 2025
%%%----------------------------------------------------------------------

-module(mod_http_upload_hmac).
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_options/1, mod_doc/0]).
-export([process_iq/1, get_url/3, get_slot/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload:0">>).
-define(DEFAULT_MAX_SIZE, 104857600). % 100MB
-define(DEFAULT_TOKEN_EXPIRY, 3600).  % 1 hour

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_http_upload_hmac for ~s", [Host]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_HTTP_UPLOAD,
                                  ?MODULE, process_iq, IQDisc),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_http_upload_hmac for ~s", [Host]),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_HTTP_UPLOAD),
    ok.

reload(Host, NewOpts, OldOpts) ->
    ?INFO_MSG("Reloading mod_http_upload_hmac for ~s", [Host]),
    ok.

%%%----------------------------------------------------------------------
%%% IQ Processing
%%%----------------------------------------------------------------------

process_iq(#iq{type = get, from = From, to = To, 
               sub_els = [#upload_request{filename = Filename,
                                         size = Size,
                                         'content-type' = ContentType}]} = IQ) ->
    User = jid:user(From),
    Server = jid:server(From),
    Host = jid:server(To),
    
    case check_upload_permission(User, Server, Host, Size) of
        ok ->
            case generate_upload_slot(User, Server, Host, Filename, Size, ContentType) of
                {ok, PutURL, GetURL, Headers} ->
                    Slot = #upload_slot{get = GetURL, put = PutURL, headers = Headers},
                    IQ#iq{type = result, sub_els = [Slot]};
                {error, Reason} ->
                    ?WARNING_MSG("Upload slot generation failed: ~p", [Reason]),
                    xmpp:make_error(IQ, xmpp:err_internal_server_error())
            end;
        {error, quota_exceeded} ->
            ?INFO_MSG("Upload denied for ~s@~s: quota exceeded", [User, Server]),
            xmpp:make_error(IQ, xmpp:err_resource_constraint());
        {error, file_too_large} ->
            ?INFO_MSG("Upload denied for ~s@~s: file too large (~B bytes)", [User, Server, Size]),
            xmpp:make_error(IQ, xmpp:err_not_acceptable());
        {error, forbidden_extension} ->
            ?INFO_MSG("Upload denied for ~s@~s: forbidden file extension", [User, Server]),
            xmpp:make_error(IQ, xmpp:err_not_acceptable());
        {error, Reason} ->
            ?WARNING_MSG("Upload permission check failed: ~p", [Reason]),
            xmpp:make_error(IQ, xmpp:err_forbidden())
    end;

process_iq(#iq{type = get} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_bad_request());

process_iq(#iq{type = set} = IQ) ->
    xmpp:make_error(IQ, xmpp:err_not_allowed()).

%%%----------------------------------------------------------------------
%%% Permission Checking
%%%----------------------------------------------------------------------

check_upload_permission(User, Server, Host, Size) ->
    MaxSize = get_max_size(Host),
    if Size > MaxSize ->
           {error, file_too_large};
       true ->
           case check_user_quota(User, Server, Host, Size) of
               ok ->
                   check_extension_allowed(Host, "");
               Error ->
                   Error
           end
    end.

check_user_quota(User, Server, Host, Size) ->
    MaxQuota = get_user_quota(Host),
    case get_user_usage(User, Server, Host) of
        {ok, CurrentUsage} when CurrentUsage + Size =< MaxQuota ->
            ok;
        {ok, _} ->
            {error, quota_exceeded};
        {error, _} ->
            ok  % If we can't check usage, allow upload
    end.

check_extension_allowed(_Host, _Extension) ->
    % TODO: Implement extension filtering
    ok.

%%%----------------------------------------------------------------------
%%% Upload Slot Generation
%%%----------------------------------------------------------------------

generate_upload_slot(User, Server, Host, Filename, Size, ContentType) ->
    UUID = generate_uuid(),
    Timestamp = unix_timestamp(),
    Expiry = Timestamp + get_token_expiry(Host),
    
    case generate_upload_token(User, Server, Filename, Size, Timestamp, Host) of
        {ok, Token} ->
            BaseURL = get_hmac_server_url(Host),
            PutURL = iolist_to_binary([BaseURL, "/upload/", UUID, "/", 
                                     binary_to_list(Filename),
                                     "?token=", Token, 
                                     "&user=", User, "@", Server,
                                     "&expiry=", integer_to_binary(Expiry)]),
            GetURL = iolist_to_binary([BaseURL, "/download/", UUID, "/",
                                     binary_to_list(Filename)]),
            
            Headers = [#upload_header{name = <<"Authorization">>,
                                    value = <<"Bearer ", Token/binary>>},
                      #upload_header{name = <<"Content-Type">>,
                                    value = ContentType}],
            
            {ok, PutURL, GetURL, Headers};
        {error, Reason} ->
            {error, Reason}
    end.

generate_upload_token(User, Server, Filename, Size, Timestamp, Host) ->
    Secret = get_hmac_secret(Host),
    UserJID = iolist_to_binary([User, "@", Server]),
    Payload = iolist_to_binary([UserJID, "\0", Filename, "\0", 
                               integer_to_binary(Size), "\0",
                               integer_to_binary(Timestamp)]),
    
    case crypto:mac(hmac, sha256, Secret, Payload) of
        Mac when is_binary(Mac) ->
            Token = base64:encode(Mac),
            {ok, Token};
        _ ->
            {error, token_generation_failed}
    end.

%%%----------------------------------------------------------------------
%%% Helper Functions
%%%----------------------------------------------------------------------

generate_uuid() ->
    % Simple UUID generation
    Now = os:timestamp(),
    {MegaSecs, Secs, MicroSecs} = Now,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b", 
                                [MegaSecs, Secs, MicroSecs])).

unix_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_url(Host, UUID, Filename) ->
    BaseURL = get_hmac_server_url(Host),
    iolist_to_binary([BaseURL, "/download/", UUID, "/",
                     binary_to_list(Filename)]).

get_slot(User, Server, Host, Filename) ->
    % External API for getting upload slots
    Size = 0,  % Size will be determined during upload
    ContentType = <<"application/octet-stream">>,
    generate_upload_slot(User, Server, Host, Filename, Size, ContentType).

%%%----------------------------------------------------------------------
%%% Configuration Helpers
%%%----------------------------------------------------------------------

get_hmac_server_url(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, hmac_server_url,
                          <<"http://localhost:8080">>).

get_hmac_secret(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, hmac_shared_secret,
                          <<"default-secret-change-me">>).

get_max_size(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, max_size, ?DEFAULT_MAX_SIZE).

get_user_quota(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, quota_per_user, 1073741824). % 1GB

get_token_expiry(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, token_expiry, ?DEFAULT_TOKEN_EXPIRY).

get_user_usage(User, Server, Host) ->
    % TODO: Implement user quota tracking
    {ok, 0}.

%%%----------------------------------------------------------------------
%%% Module Options
%%%----------------------------------------------------------------------

mod_options(Host) ->
    [{hmac_server_url, <<"http://localhost:8080">>},
     {hmac_shared_secret, <<"default-secret-change-me">>},
     {max_size, ?DEFAULT_MAX_SIZE},
     {quota_per_user, 1073741824},  % 1GB
     {token_expiry, ?DEFAULT_TOKEN_EXPIRY},
     {allowed_extensions, []},
     {iqdisc, one_queue}].

mod_doc() ->
    #{desc =>
          ?T("This module implements XEP-0363 HTTP File Upload "
             "with HMAC File Server integration. It provides "
             "seamless authentication using XMPP credentials "
             "and automatic token generation for secure uploads."),
      opts =>
          [{hmac_server_url,
            #{value => ?T("URL"),
              desc => ?T("Base URL of the HMAC File Server")}},
           {hmac_shared_secret,
            #{value => ?T("Secret"),
              desc => ?T("Shared secret for HMAC token generation")}},
           {iqdisc,
            #{value => ?T("Discipline"),
              desc => ?T("IQ processing discipline")}}],
      example =>
          [?T("modules:"), ?T("  mod_http_upload_hmac:"),
           ?T("    hmac_server_url: \"http://localhost:8080\""),
           ?T("    hmac_shared_secret: \"your-secure-secret\"")]}.
