%%%----------------------------------------------------------------------
%%% File    : mod_http_upload_hmac_network_resilient.erl  
%%% Author  : HMAC File Server Team
%%% Purpose : Network-Resilient XEP-0363 HTTP File Upload with HMAC Integration
%%% Version : 3.3.0 Network Resilience Edition
%%% Created : 26 Aug 2025
%%%----------------------------------------------------------------------

-module(mod_http_upload_hmac_network_resilient).
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, mod_options/1, mod_doc/0]).
-export([process_iq/1, get_url/3, get_slot/4, refresh_token/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload:0">>).
-define(DEFAULT_MAX_SIZE, 104857600). % 100MB
-define(DEFAULT_TOKEN_EXPIRY, 14400). % 4 hours for network resilience
-define(DEFAULT_EXTENDED_EXPIRY, 86400). % 24 hours for mobile scenarios
-define(DEFAULT_GRACE_PERIOD, 7200). % 2 hours grace period

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_http_upload_hmac_network_resilient for ~s", [Host]),
    IQDisc = gen_mod:get_opt(iqdisc, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_HTTP_UPLOAD,
                                  ?MODULE, process_iq, IQDisc),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_http_upload_hmac_network_resilient for ~s", [Host]),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_HTTP_UPLOAD),
    ok.

reload(Host, NewOpts, OldOpts) ->
    ?INFO_MSG("Reloading mod_http_upload_hmac_network_resilient for ~s", [Host]),
    ok.

%%%----------------------------------------------------------------------
%%% IQ Processing with Network Resilience
%%%----------------------------------------------------------------------

process_iq(#iq{type = get, from = From, to = To, 
               sub_els = [#upload_request{filename = Filename,
                                         size = Size,
                                         'content-type' = ContentType}]} = IQ) ->
    User = jid:user(From),
    Server = jid:server(From),
    Host = jid:server(To),
    
    ?INFO_MSG("Upload request from ~s@~s: ~s (~B bytes)", [User, Server, Filename, Size]),
    
    case check_upload_permission(User, Server, Host, Size) of
        ok ->
            case generate_resilient_upload_slot(User, Server, Host, Filename, Size, ContentType) of
                {ok, PutURL, GetURL, Headers} ->
                    Slot = #upload_slot{get = GetURL, put = PutURL, headers = Headers},
                    ?INFO_MSG("Upload slot created for ~s@~s: resilient token with extended expiry", [User, Server]),
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
%%% Permission Checking (Enhanced for Mobile)
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
%%% Network-Resilient Upload Slot Generation
%%%----------------------------------------------------------------------

generate_resilient_upload_slot(User, Server, Host, Filename, Size, ContentType) ->
    UUID = generate_uuid(),
    Timestamp = unix_timestamp(),
    
    % Determine expiry based on mobile optimization settings
    BaseExpiry = get_token_expiry(Host),
    ExtendedExpiry = case get_mobile_optimizations(Host) of
        true -> 
            % For mobile clients: much longer token validity
            Timestamp + get_extended_expiry(Host);
        false -> 
            % Standard expiry
            Timestamp + BaseExpiry
    end,
    
    % Generate primary token
    case generate_resilient_upload_token(User, Server, Filename, Size, Timestamp, Host, ExtendedExpiry) of
        {ok, Token} ->
            BaseURL = get_hmac_server_url(Host),
            
            % Create resilient URLs with session recovery parameters
            SessionId = generate_session_id(),
            PutURL = iolist_to_binary([BaseURL, "/upload/", UUID, "/", 
                                     http_uri:encode(binary_to_list(Filename)),
                                     "?token=", Token, 
                                     "&user=", User, "@", Server,
                                     "&expiry=", integer_to_binary(ExtendedExpiry),
                                     "&session_id=", SessionId,
                                     "&network_resilience=true",
                                     "&resume_allowed=true"]),
            
            GetURL = iolist_to_binary([BaseURL, "/download/", UUID, "/",
                                     http_uri:encode(binary_to_list(Filename))]),
            
            % Enhanced headers for network resilience
            Headers = [
                #upload_header{name = <<"Authorization">>,
                              value = <<"Bearer ", Token/binary>>},
                #upload_header{name = <<"Content-Type">>,
                              value = ContentType},
                #upload_header{name = <<"X-Upload-Session-ID">>,
                              value = list_to_binary(SessionId)},
                #upload_header{name = <<"X-Network-Resilience">>,
                              value = <<"enabled">>},
                #upload_header{name = <<"X-Token-Refresh-URL">>,
                              value = iolist_to_binary([BaseURL, "/auth/refresh"])},
                #upload_header{name = <<"X-Extended-Timeout">>,
                              value = integer_to_binary(ExtendedExpiry)}
            ],
            
            ?INFO_MSG("Generated resilient upload slot: session=~s, expiry=~B", [SessionId, ExtendedExpiry]),
            {ok, PutURL, GetURL, Headers};
        {error, Reason} ->
            {error, Reason}
    end.

generate_resilient_upload_token(User, Server, Filename, Size, Timestamp, Host, Expiry) ->
    Secret = get_hmac_secret(Host),
    UserJID = iolist_to_binary([User, "@", Server]),
    
    % Enhanced payload for network resilience with extended context
    Payload = iolist_to_binary([
        UserJID, "\0", 
        Filename, "\0", 
        integer_to_binary(Size), "\0",
        integer_to_binary(Timestamp), "\0",
        integer_to_binary(Expiry), "\0",
        <<"network_resilient">>
    ]),
    
    case crypto:mac(hmac, sha256, Secret, Payload) of
        Mac when is_binary(Mac) ->
            Token = base64:encode(Mac),
            ?DEBUG_MSG("Generated resilient token for ~s: length=~B, expiry=~B", 
                      [UserJID, byte_size(Token), Expiry]),
            {ok, Token};
        _ ->
            {error, token_generation_failed}
    end.

%%%----------------------------------------------------------------------
%%% Token Refresh for Network Changes
%%%----------------------------------------------------------------------

refresh_token(User, Server, Host) ->
    % Generate a new token when client detects network change
    Timestamp = unix_timestamp(),
    Expiry = Timestamp + get_extended_expiry(Host),
    
    case generate_resilient_upload_token(User, Server, <<"refresh">>, 0, Timestamp, Host, Expiry) of
        {ok, Token} ->
            ?INFO_MSG("Token refreshed for ~s@~s due to network change", [User, Server]),
            {ok, Token, Expiry};
        Error ->
            Error
    end.

%%%----------------------------------------------------------------------
%%% Helper Functions (Enhanced for Mobile)
%%%----------------------------------------------------------------------

generate_uuid() ->
    % Enhanced UUID generation with timestamp component
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Random = crypto:strong_rand_bytes(4),
    RandomHex = binary_to_list(binary:encode_hex(Random)),
    lists:flatten(io_lib:format("~8.16.0b-~8.16.0b-~8.16.0b-~s", 
                                [MegaSecs, Secs, MicroSecs, RandomHex])).

generate_session_id() ->
    % Generate unique session ID for tracking across network changes
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Hash = crypto:hash(sha256, term_to_binary({MegaSecs, Secs, MicroSecs, make_ref()})),
    binary_to_list(binary:encode_hex(binary:part(Hash, 0, 8))).

unix_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_url(Host, UUID, Filename) ->
    BaseURL = get_hmac_server_url(Host),
    iolist_to_binary([BaseURL, "/download/", UUID, "/",
                     http_uri:encode(binary_to_list(Filename))]).

get_slot(User, Server, Host, Filename) ->
    % External API for getting upload slots
    Size = 0,  % Size will be determined during upload
    ContentType = <<"application/octet-stream">>,
    generate_resilient_upload_slot(User, Server, Host, Filename, Size, ContentType).

%%%----------------------------------------------------------------------
%%% Configuration Helpers (Enhanced for Network Resilience)
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

get_extended_expiry(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, extended_token_expiry, ?DEFAULT_EXTENDED_EXPIRY).

get_mobile_optimizations(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, mobile_optimizations, true).

get_grace_period(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, grace_period, ?DEFAULT_GRACE_PERIOD).

get_user_usage(User, Server, Host) ->
    % TODO: Implement user quota tracking
    {ok, 0}.

%%%----------------------------------------------------------------------
%%% Module Options (Enhanced for Network Resilience)
%%%----------------------------------------------------------------------

mod_options(Host) ->
    [{hmac_server_url, <<"http://localhost:8080">>},
     {hmac_shared_secret, <<"default-secret-change-me">>},
     {max_size, ?DEFAULT_MAX_SIZE},
     {quota_per_user, 1073741824},  % 1GB
     {token_expiry, ?DEFAULT_TOKEN_EXPIRY},  % 4 hours standard
     {extended_token_expiry, ?DEFAULT_EXTENDED_EXPIRY},  % 24 hours for mobile
     {grace_period, ?DEFAULT_GRACE_PERIOD},  % 2 hours grace period
     {mobile_optimizations, true},  % Enable mobile-friendly features
     {network_resilience, true},    % Enable network change handling
     {session_recovery, true},      % Enable session recovery
     {allowed_extensions, []},
     {iqdisc, one_queue}].

mod_doc() ->
    #{desc =>
          ?T("This module implements XEP-0363 HTTP File Upload "
             "with HMAC File Server integration and network resilience. "
             "It provides seamless authentication using XMPP credentials "
             "and handles WiFi/LTE network switching gracefully."),
      opts =>
          [{hmac_server_url,
            #{value => ?T("URL"),
              desc => ?T("Base URL of the HMAC File Server")}},
           {hmac_shared_secret,
            #{value => ?T("Secret"),
              desc => ?T("Shared secret for HMAC token generation")}},
           {max_size,
            #{value => ?T("Size"),
              desc => ?T("Maximum file size in bytes")}},
           {token_expiry,
            #{value => ?T("Seconds"),
              desc => ?T("Standard upload token expiry time")}},
           {extended_token_expiry,
            #{value => ?T("Seconds"),
              desc => ?T("Extended token expiry for mobile scenarios")}},
           {mobile_optimizations,
            #{value => ?T("Boolean"),
              desc => ?T("Enable mobile network optimizations")}},
           {network_resilience,
            #{value => ?T("Boolean"),
              desc => ?T("Enable network change resilience")}},
           {iqdisc,
            #{value => ?T("Discipline"),
              desc => ?T("IQ processing discipline")}}],
      example =>
          [?T("modules:"), ?T("  mod_http_upload_hmac_network_resilient:"),
           ?T("    hmac_server_url: \"http://localhost:8080\""),
           ?T("    hmac_shared_secret: \"your-secure-secret\""),
           ?T("    token_expiry: 14400  # 4 hours"),
           ?T("    extended_token_expiry: 86400  # 24 hours for mobile"),
           ?T("    mobile_optimizations: true"),
           ?T("    network_resilience: true")]}.
