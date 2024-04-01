-module(holster).

%% @doc
%% req/1/2/3/4 makes the request the process is cleaned up,
%% compared to stay_connected/1/2/3/4 that keeps the connection up for further requests
%%
%%  URI:
%%
%%  The following are two example URIs and their component parts:
%%
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%%
%% scheme = Ex: http, https, ftp, mailto, file, data, irc
%%          https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
%% Authority = Host Name + Port No
%% URI ( query string and/or path )
%%     https://tools.ietf.org/html/rfc3986
%% Fragment: https://tools.ietf.org/html/rfc3986
%% NB: Fragments are included
%% @end


-include_lib("kernel/include/logger.hrl").

-type req_type() :: get | head | options | patch | post | put.
-type conn_type() :: once | stay_connected.
% -type default_ws_timetouts() :: [

% ].

-export_type([
    req_type/0,
    conn_type/0
]).

-export([
    % func/0, func2/0,
    basic_auth_request/6, %% TODO: gen boiler plate /5 /4 /3 /2
    simple_proc_req/2,
    simple_proc_req/3,
    simple_proc_req/4,
    simple_proc_req/5,
    simple_proc_req/6,
    simple_proc_req_timed/6,
    simple_proc_req_timed/7,
    req/2,
    req/3,
    req/4,
    req/5,
    stay_connected_req/2,
    stay_connected_req/3,
    stay_connected_req/4,
    stay_connected_req/5,
    stay_connected_req/6,
    stay_connected_req/7,
    another_request/3,
    another_request/4,
    another_request/5,
    another_request/6,
    ws_connect/2,
    ws_connect/5,
    ws_req/2,
    ws_close/1,
    close_req/1
]).

-export([
    parse_uri/1,
    convert_to_gun_friendly_scheme/1
]).

-ifdef(TEST).
-compile(export_all).
-endif.

% -export([
%     test/0,
%     once_test/0,
%     stay_connected/0
% ]).

%% TODO:
%% - Userinfo not done/tested yet. user:passwd@host.com

% func() ->
%     application:ensure_all_started(cowboy),
%     application:ensure_all_started(holster),
%     Dispatch = cowboy_router:compile([
%         {'_', [
%             {"/auth-endpoint", holster_basic_auth_webserver, []}
%         ]}
%     ]),
%     % ?debugFmt("~p\n", [file:read_file("test/ssl/cert.pem")]),
%     % ?debugFmt("~p\n", [file:read_file("test/ssl/key.pem")]),
%     {ok, _Pid} = cowboy:start_clear(
%         https,
%         _TransOpts=[
%             {port, 8000}
%         ],
%         _ProtoOpts=#{
%             env => #{dispatch => Dispatch}
%         }
%     ).

% func2() ->
%     % http://user:password@domain.com/
%     Url = "http://Alladin:open_sesame@localhost:8000/auth-endpoint",
%     {ok, URIMap} = parse_uri(Url),
%     Headers = 
%         case maps:get(userinfo, URIMap, undefined) of
%             undefined ->
%                 ok;
%             UserInfo ->
%                 EncodedUserInfo = base64:encode(UserInfo, #{ mode => urlsafe }),
%                 [
%                     {
%                         <<"authorization">>,
%                         binary:list_to_bin([<<"Basic ">>, EncodedUserInfo])
%                     }
%                 ]
%         end,
%     application:ensure_all_started(holster),
%     simple_proc_req(
%         get,
%         Url,
%         #{},
%         #{},
%         Headers
%     ).

% func2() ->
%     % holster:simple_proc_req(get, "https://localhost:8443/api", #{tls_opts => [{verify, verify_none}]}).
%     ConnectOpts = #{tls_opts => [{verify, verify_none}]},
%     URI = "https://localhost:8443/api",
%     Timeout=120000,
%     Headers = [],
%     ReqOpts = #{},
%     {ok, URIMap} = parse_uri(URI),
%     {ok, Pid} = holster_request:start_link(
%         maps:get(host, URIMap),
%         maps:get(scheme, URIMap),
%         maps:get(port, URIMap),
%         ConnectOpts,
%         Timeout
%     ),
%     _ = holster_request:req(
%         Pid,
%         get,
%         maps:get(path, URIMap),
%         Headers,
%         ReqOpts,
%         Timeout
%     ).

basic_auth_request(ReqType, URI, ConnectOpts, ReqOpts, Headers, Timeout) ->
    {ok, URIMap} = parse_uri(URI),
    simple_proc_req(
        ReqType,
        URI,
        ConnectOpts,
        ReqOpts,
        _Headers2 = lists:append(
            base64_encoded_user_info_header_field(URIMap),
            Headers
        ),
        Timeout,
        URIMap
    ).

base64_encoded_user_info_header_field(URIMap) when is_map(URIMap) ->
    base64_encoded_user_info_header_field(get_uri_user_info(URIMap));
base64_encoded_user_info_header_field(undefined) ->
    [];
base64_encoded_user_info_header_field(UserInfo) 
        when is_list(UserInfo) orelse is_binary(UserInfo) ->
    [
        {
            <<"authorization">>,
            binary:list_to_bin([
                <<"Basic ">>, 
                base64:encode(UserInfo, #{ mode => urlsafe })
            ])
        }
    ].

get_uri_user_info(URIMap) ->
    maps:get(userinfo, URIMap, undefined).

simple_proc_req(ReqType, URI) ->
    simple_proc_req(ReqType, URI, _ConnectOpts=#{}, _ReqOpts=#{}, _Headers=[], _Timeout=120000).

simple_proc_req(ReqType, URI, ConnectOpts) ->
    simple_proc_req(ReqType, URI, ConnectOpts, _ReqOpts=#{}, _Headers=[], _Timeout=120000).

simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts) ->
    simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts, _Headers=[], _Timeout=120000).

simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts, Headers) ->
    simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts, Headers, 120000).

%% TODO: Add RequestOpts
% FROM gun.erl
% -type req_opts() :: #{
%     flow => pos_integer(),
%     reply_to => pid(),
%     tunnel => stream_ref()
% }.

simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts, Headers, Timeout) ->
    {ok, URIMap} = parse_uri(URI),
    simple_proc_req(ReqType, URI, ConnectOpts, ReqOpts, Headers, Timeout, URIMap).

simple_proc_req(ReqType, _URI, ConnectOpts, ReqOpts, Headers, Timeout, URIMap) ->
    {ok, Pid} = holster_request:start_link(
        maps:get(host, URIMap),
        maps:get(scheme, URIMap),
        maps:get(port, URIMap),
        ConnectOpts,
        Timeout
    ),
    _ = holster_request:req(
        Pid,
        ReqType,
        prep_partial_uri(URIMap),
        Headers,
        ReqOpts,
        Timeout
    ),
    receive
        A ->
            {response, A}
    after
        60000 ->
            {error, {?FUNCTION_NAME, timeout}}
    end.

% returns: {Status, RespHeaders, <<Data/binary>>, TimesMap}
simple_proc_req_timed(ReqType, URI, ConnectOpts, ReqOpts, Headers, Timeout) ->
    {ok, URIMap} = parse_uri(URI),
    {ok, Pid} = holster_request:start_link(
        maps:get(host, URIMap),
        maps:get(scheme, URIMap),
        maps:get(port, URIMap),
        ConnectOpts,
        Timeout,
        timed
    ),
    _ = holster_request:req_timed(
        Pid,
        ReqType,
        prep_partial_uri(URIMap),
        Headers,
        ReqOpts,
        Timeout
    ),
    receive
        A ->
            {response, A}
    end.

% returns: {Status, RespHeaders, <<Data/binary>>, TimesMap}
simple_proc_req_timed(ReqType, URI, ConnectOpts, ReqOpts, Headers, Timeout, Body) ->
    {ok, URIMap} = parse_uri(URI),
    {ok, Pid} = holster_request:start_link(
        maps:get(host, URIMap),
        maps:get(scheme, URIMap),
        maps:get(port, URIMap),
        ConnectOpts,
        Timeout,
        timed
    ),
    _ = holster_request:req_timed(
        Pid,
        ReqType,
        prep_partial_uri(URIMap),
        Headers,
        ReqOpts,
        Timeout,
        Body
    ),
    receive
        A ->
            {response, A}
    end.

%% This exists as gub open takes http://host:port.
%% and subsequent requests only take the parts after host ( query string etc )
%% EX: URL : "foo://user@example.com:8042/over/there?name=ferret#nose"
%% gun open 
prep_partial_uri(URIMap) ->
    uri_string:recompose(
        maps:without([host, scheme, userinfo, port], URIMap)
    ).

% -spec req(req_type(), uri_string:uri_string())
%         -> {response, term()} | {'error','invalid_scheme'}.
req(ReqType, URI) ->
    req_response_only(
        do_req(ReqType, URI, #{}, [], #{}, once)
    ).

% -spec req(req_type(), uri_string:uri_string(), gun:opts())
%         -> {response, term()}.
req(ReqType, URI, ConnectOpts) ->
    req_response_only(
        do_req(ReqType, URI, ConnectOpts, [], #{}, once)
    ).

% -spec req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers())
%         -> {response, term()}.
req(ReqType, URI, ConnectOpts, Headers) ->
    req_response_only(
        do_req(ReqType, URI, ConnectOpts, Headers, #{}, once)
    ).

-spec req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers(), gun:req_opts())
        -> {response, term()}.
req(ReqType, URI, ConnectOpts, Headers, ReqOpts) ->
    req_response_only(
        do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, once)
    ).

req_response_only({response, Response}) ->
    {response, Response};
req_response_only({{ok, _Pid}, R}) ->
    req_response_only(R).

-spec stay_connected_req(req_type(), uri_string:uri_string())
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI) ->
    do_req(ReqType, URI, #{}, [], #{}, stay_connected).

-spec stay_connected_req(req_type(), uri_string:uri_string(), gun:opts())
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI, ConnectOpts) ->
    do_req(ReqType, URI, ConnectOpts, [], #{}, stay_connected).

-spec stay_connected_req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers())
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI, ConnectOpts, Headers) ->
    do_req(ReqType, URI, ConnectOpts, Headers, #{}, stay_connected).

-spec stay_connected_req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers(), gun:req_opts())
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI, ConnectOpts, Headers, ReqOpts) ->
    do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, stay_connected).

-spec stay_connected_req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers(), gun:req_opts(), binary() | undefined)
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, Body) ->
    do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, stay_connected, Body).

-spec stay_connected_req(req_type(), uri_string:uri_string(), gun:opts(), gun:req_headers(), gun:req_opts(), binary() | undefined, undefined | pid())
        -> {{ok, pid()}, {response, term()}}.
stay_connected_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, Body, Pid) ->
    do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, stay_connected, Body, Pid).



-spec another_request(req_type(), uri_string:uri_string(), pid())
        -> {{ok, pid()}, {response, term()}}.
another_request(ReqType, URI, Pid) ->
    do_req(ReqType, URI, _ConnectOpts=#{}, _Header=[], _ReqOpts=#{}, stay_connected, _Body=undefined, Pid).

another_request(ReqType, URI, Pid, Headers) ->
    do_req(ReqType, URI, _ConnectOpts=#{}, Headers, _ReqOpts=#{}, stay_connected, _Body=undefined, Pid).

-spec another_request(req_type(), uri_string:uri_string(), gun:req_headers(), pid())
        -> {{ok, pid()}, {response, term()}}.
another_request(ReqType, URI, Pid, Headers, Body) ->
    do_req(ReqType, URI, _ConnectOpts=#{}, Headers, _ReqOpts=#{}, stay_connected, Body, Pid).

-spec another_request(req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts(), pid())
        -> {{ok, pid()}, {response, term()}}.
another_request(ReqType, URI, Pid, Headers, Body, ReqOpts) ->
    do_req(ReqType, URI, _ConnectOpts=#{}, Headers, ReqOpts, stay_connected, Body, Pid).



-spec ws_connect(uri_string:uri_string(), gun:opts()) -> {ok, pid(), gun:resp_headers()} | {error, {ws_upgrade, timeout}}.
ws_connect(URI, ConnectOpts) ->
    ws_connect(URI, ConnectOpts, default_ws_timeouts(), [], #{}).

ws_connect(URI, ConnectOpts, Timeouts, WsUpgradeHeaders, WsUpgradeOpts) ->
    {ok, URIMap} = parse_uri(URI),
    {ok, Pid} = holster_ws:start_link(
        maps:get(host, URIMap),
        maps:get(scheme, URIMap),
        maps:get(port, URIMap),
        ConnectOpts,
        maps:get(path, URIMap),
        Timeouts,
        WsUpgradeHeaders,
        WsUpgradeOpts
    ),
    case holster_ws:ws_upgrade(Pid) of
        {ws_upgraded, WsHeaders} ->
            {ok, Pid, WsHeaders};
        {error, {ws_upgrade, timeout}} ->
            {error, {ws_upgrade, timeout}}
    end.

% -spec default_ws_timetouts() -> default_ws_timetouts().
default_ws_timeouts() ->
    [
        {ws_await_up_timeout, 5000},
        {connected_idle_timeout, 60 * 60 * 1000},
        {ws_upgrade_timeout, 5000}
    ].

% {ws_response,{close,1011,<<>>}}
ws_req(WsPid, Term) ->
    holster_ws:ws_send(WsPid, Term).

ws_close(WsPid) ->
    holster_ws:close(WsPid).

%%=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, ConnType) ->
    do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, ConnType, undefined).

do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, ConnType, Body) ->
    do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, ConnType, Body, undefined).

do_req(ReqType, URI, ConnectOpts, Headers, ReqOpts, ConnType, Body, PidOrUndef) ->
    case parse_uri(URI) of
        {ok, URIMap} ->
            {ok, Pid} = start_or_get_pid(
                PidOrUndef,
                maps:get(host, URIMap),
                maps:get(scheme, URIMap),
                maps:get(port, URIMap),
                ConnectOpts,
                undefined,
                ConnType
            ),
            {
                {ok, Pid},
                holster_sm:req(
                    Pid,
                    ReqType,
                    uri_string:recompose(URIMap),
                    Headers,
                    ReqOpts,
                    Body
                )
            };
        Error ->
            {response, Error}
    end.

%% This binary case reduces the amount of duplicating ( string & binary in URIMap )
parse_uri(URI) when is_binary(URI) ->
    parse_uri(binary_to_list(URI));
parse_uri(URI) ->
    URIMap = uri_string:parse(URI),
    Scheme = maps:get(scheme, URIMap, undefined),
    case check_gun_friendly_scheme(Scheme) of
        {error, unsupported_scheme} ->
            {error, unsupported_scheme};
        ok ->
            {ok, add_default_port(URIMap)}
    end.

start_or_get_pid(undefined, Host, Scheme, Port, ConnectOpts, Timeout, ConnType) ->
    {ok, _Pid} = holster_sm:start_link(
        Host,
        Scheme,
        Port,
        ConnectOpts,
        Timeout,
        ConnType
    );
start_or_get_pid(Pid, _, _, _, _, _, _) ->
    {ok, Pid}.

add_default_port(URIMap) ->
    Scheme = maps:get(scheme, URIMap, undefined),
    Defaults = #{ port => scheme_defaults(Scheme) },
    maps:merge(Defaults, URIMap).

close_req(Pid) ->
    holster_sm:close(Pid).

scheme_defaults("http") ->
    80;
scheme_defaults("ws") ->
    80;
scheme_defaults("https") ->
    443;
scheme_defaults("wss") ->
    443;
scheme_defaults("ftp") ->
    21;
scheme_defaults("ssh") ->
    22;
scheme_defaults("sftp") ->
    22;
scheme_defaults("tftp") ->
    69.

check_gun_friendly_scheme(Scheme) when is_list(Scheme) ->
    case convert_to_gun_friendly_scheme(Scheme) of
        {error, Reason} ->
            {error, Reason};
        _GunFriendlyScheme ->
            ok
    end.

convert_to_gun_friendly_scheme(Scheme) when is_list(Scheme) ->
    try
        %% Atoms are created in holster_sup
        erlang:list_to_existing_atom(Scheme)
    catch
        _:_:_ ->
            {error, unsupported_scheme}
    end.