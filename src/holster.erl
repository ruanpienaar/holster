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
    simple_proc_req/2,
    simple_proc_req/3,
    simple_proc_req/4,
    simple_proc_req/5,
    simple_proc_req_timed/5,
    simple_proc_req_timed/6,
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
    ws_connect/2,
    ws_connect/5,
    ws_req/2,
    ws_close/1,
    close_req/1
]).

-export([
    parse_uri/1
]).

% -export([
%     test/0,
%     once_test/0,
%     stay_connected/0
% ]).

%% TODO:
%% - Userinfo not done/tested yet. user:passwd@host.com

simple_proc_req(ReqType, URI) ->
    simple_proc_req(ReqType, URI, #{}, [], 120000).

simple_proc_req(ReqType, URI, ConnectOpts) ->
    simple_proc_req(ReqType, URI, ConnectOpts, [], 120000).

simple_proc_req(ReqType, URI, ConnectOpts, Headers) ->
    simple_proc_req(ReqType, URI, ConnectOpts, Headers, 120000).

simple_proc_req(ReqType, URI, ConnectOpts, Headers, Timeout) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query, Fragment}} = parse_uri(URI),
    {ok, Pid} = holster_request:start_link(Host, Scheme, Port, ConnectOpts, Timeout),
    _ = holster_request:req(
        Pid,
        ReqType,
        combine_fragment({Path, Query, Fragment}),
        Headers,
        ConnectOpts,
        Timeout
    ),
    receive
        A ->
            {response, A}
    end.

% returns: {Status, RespHeaders, <<Data/binary>>, TimesMap}
simple_proc_req_timed(ReqType, URI, ConnectOpts, Headers, Timeout) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query, Fragment}} = parse_uri(URI),
    {ok, Pid} = holster_request:start_link(Host, Scheme, Port, ConnectOpts, Timeout, timed),
    _ = holster_request:req_timed(
        Pid,
        ReqType,
        combine_fragment({Path, Query, Fragment}),
        Headers,
        ConnectOpts,
        Timeout
    ),
    receive
        A ->
            {response, A}
    end.

% returns: {Status, RespHeaders, <<Data/binary>>, TimesMap}
simple_proc_req_timed(ReqType, URI, ConnectOpts, Headers, Timeout, Body) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query, Fragment}} = parse_uri(URI),
    {ok, Pid} = holster_request:start_link(Host, Scheme, Port, ConnectOpts, Timeout, timed),
    _ = holster_request:req_timed(
        Pid,
        ReqType,
        combine_fragment({Path, Query, Fragment}),
        Headers,
        ConnectOpts,
        Timeout,
        Body
    ),
    receive
        A ->
            {response, A}
    end.

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
    do_req(ReqType, URI, #{}, [], #{}, stay_connected, Pid).

-spec another_request(req_type(), uri_string:uri_string(), gun:req_headers(), pid())
        -> {{ok, pid()}, {response, term()}}.
another_request(ReqType, URI, Headers, Pid) ->
    do_req(ReqType, URI, #{}, Headers, #{}, stay_connected, Pid).

-spec another_request(req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts(), pid())
        -> {{ok, pid()}, {response, term()}}.
another_request(ReqType, URI, Headers, ReqOpts, Pid) ->
    do_req(ReqType, URI, #{}, Headers, ReqOpts, stay_connected, Pid).

-spec ws_connect(uri_string:uri_string(), gun:opts()) -> {ok, pid(), gun:resp_headers()} | {error, {ws_upgrade, timeout}}.
ws_connect(URI, ConnectOpts) ->
    ws_connect(URI, ConnectOpts, default_ws_timeouts(), [], #{}).

ws_connect(URI, ConnectOpts, Timeouts, WsUpgradeHeaders, WsUpgradeOpts) ->
    {ok, {Proto, _UserInfo, Host, Port, WsPath, _Query, _Fragment}} =
        parse_uri(URI),
    {ok, Pid} = holster_ws:start_link(
        Host, Proto, Port, ConnectOpts, WsPath, Timeouts, WsUpgradeHeaders, WsUpgradeOpts),
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
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query, Fragment}} ->
            {ok, Pid} = start_or_use(PidOrUndef, Host, Scheme, Port, ConnectOpts, undefined, ConnType),
            {
                {ok, Pid},
                holster_sm:req(
                    Pid,
                    ReqType,
                    combine_fragment({Path, Query, Fragment}),
                    Headers,
                    ReqOpts,
                    Body
                )
            };
        Error ->
            {response, Error}
    end.

parse_uri(URI) ->
% 1> uri_string:parse("foo://user@example.com:8042/over/there?name=ferret#nose").
% #{fragment => "nose",host => "example.com",
%   path => "/over/there",port => 8042,query => "name=ferret",
%   scheme => foo,userinfo => "user"}
    URIMap = uri_string:parse(URI),
    Scheme = maps:get(scheme, URIMap, undefined),
    io:format("Scheme ~p\n", [Scheme]),
    case scheme_validation(Scheme) of
        valid ->
            GunFriendlyScheme = gun_friendly_scheme(Scheme),
            URIMapWithDefaults = get_defaults(URIMap),
            UserInfo = maps:get(user, URIMapWithDefaults, ""),
            Host = maps:get(host, URIMapWithDefaults, ""),
            Port = maps:get(port, URIMapWithDefaults, ""),
            Path = maps:get(path, URIMapWithDefaults, ""),
            Query = maps:get(query, URIMapWithDefaults, ""),
            Fragment = maps:get(fragment, URIMapWithDefaults, ""),
            {ok, {GunFriendlyScheme, UserInfo, Host, Port, Path, Query, Fragment}};
        {error, invalid_scheme} ->
            {error, invalid_scheme}
    end.

start_or_use(undefined, Host, Scheme, Port, ConnectOpts, Timeout, ConnType) ->
    {ok, _Pid} = holster_sm:start_link(
        Host, Scheme, Port, ConnectOpts, Timeout, ConnType);
start_or_use(Pid, _, _, _, _, _, _) ->
    {ok, Pid}.

get_defaults(URIMap) ->
    Scheme = maps:get(scheme, URIMap, undefined),
    Defaults = #{
        port => scheme_defaults(Scheme),
        query => ""
    },
    maps:merge(URIMap, Defaults).

scheme_validation(Scheme) when
        Scheme =:= "http" orelse
        Scheme =:= "https" orelse
        Scheme =:= "wss" orelse
        Scheme =:= "ws"
        ->
    valid;
scheme_validation(_) ->
    {error, invalid_scheme}.

gun_friendly_scheme(Scheme) ->
    erlang:list_to_existing_atom(Scheme).

combine_fragment({Path, Query, Fragment}) ->
    Path ++ Query ++ Fragment.

close_req(Pid) ->
    holster_sm:close(Pid).

scheme_defaults("http") ->
    80;
scheme_defaults("https") ->
    443;
scheme_defaults("ftp") ->
    21;
scheme_defaults("ssh") ->
    22;
scheme_defaults("sftp") ->
    22;
scheme_defaults("tftp") ->
    69.