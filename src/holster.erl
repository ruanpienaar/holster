-module(holster).

-include_lib("kernel/include/logger.hrl").

-export([
    once_off_req/1,
    once_off_req/2,
    long_running_req/1,
    long_running_req/2,
    req/1,
    req/2,
    req/3,
    req/4,
    req/6,
    req/7
]).

% -export([
%     test/0,
%     once_off_test/0,
%     long_running_test/0
% ]).

%% @doc Simply use a URL to send a request with the ussual defaults.
%%      The following are two example URIs and their component parts:
%%  ( Copied from inets-7.0.7 http_uri.erl )
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
%%
%% 11> http_uri:parse("http://localhost:8080?q=abc").
%% {ok,{http,[],"localhost",8080,"/","?q=abc"}}
%%
%% 12> http_uri:parse("https://localhost:8080/page/1?q=abc").
%% {ok,{https,[],"localhost",8080,"/page/1","?q=abc"}}
%%
%% 13> http_uri:parse("https://user:password@localhost:8080/page/1?q=abc").
%% {ok,{https,"user:password","localhost",8080,"/page/1","?q=abc"}}
%%
%% @end

%% TODO: Userinfo not done/tested yet.
%% user:passwd@host.com

-spec once_off_req(http_uri:uri()) -> {response, term()}.
once_off_req(Url) ->
    once_off_req(Url, #{}).

-spec once_off_req(http_uri:uri(), gun:opts()) -> {response, term()}.
once_off_req(Url, Opts) ->
    do_req(Url, Opts, 1000, once_off).

-spec long_running_req(http_uri:uri()) -> {{ok, pid()}, {response, term()}}.
long_running_req(Url) ->
    long_running_req(Url, #{}).

-spec long_running_req(http_uri:uri(), gun:opts() | pid()) -> {{ok, pid()}, {response, term()}}.
long_running_req(Url, Opts) when is_map(Opts) ->
    do_req(Url, Opts, 1000, long_running);
long_running_req(Url, Pid) when is_pid(Pid) ->
    do_req(Url, #{}, 1000, long_running, Pid).

do_req(Url, OptsMap, Timeout, ConnType) ->
    do_req(Url, OptsMap, Timeout, ConnType, undefined).

do_req(Url, OptsMap, Timeout, ConnType, undefined) ->
    case http_uri:parse(Url) of
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} ->
            req(Host, Path ++ Query, Scheme, Port, OptsMap, Timeout, ConnType);
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query, _Fragment}} ->
            req(Host, Path ++ Query, Scheme, Port, OptsMap, Timeout, ConnType);
        {error, Reason} ->
            {error, Reason}
    end;
do_req(Url, _OptsMap, _Timeout, _ConnType, Pid) ->
    case http_uri:parse(Url) of
        {ok, {_Scheme, _UserInfo, _Host, _Port, Path, Query}} ->
            holster_sm:req(Pid, Path ++ Query);
        {ok, {_Scheme, _UserInfo, _Host, _Port, Path, Query, _Fragment}} ->
            holster_sm:req(Pid, Path ++ Query);
        {error, Reason} ->
            {error, Reason}
    end.

req(Host) ->
    req(Host, "/").

req(Host, URI) ->
    req(Host, URI, http).

req(Host, URI, http) ->
    req(Host, URI, http, 80);
req(Host, URI, https) ->
    req(Host, URI, https, 443).

req(Host, URI, https, Port) when Port /= 443 ->
    req(Host, URI, https, Port, #{transport => tls}, 1000);
req(Host, URI, Proto, Port) ->
    req(Host, URI, Proto, Port, #{}, 1000).

req(Host, URI, Proto, Port, OptsMap, Timeout) ->
    req(Host, URI, Proto, Port, OptsMap, Timeout, once_off).

req(Host, URI, Proto, Port, OptsMap, Timeout, once_off = ConnType) ->
    {ok, Pid} = holster_sm:start_link(Host, Proto, Port, OptsMap, Timeout, ConnType),
    holster_sm:req(Pid, URI);
req(Host, URI, Proto, Port, OptsMap, Timeout, long_running = ConnType) ->
    {ok, Pid} = holster_conn_sup:start_child(
        Host, Proto, Port, OptsMap, Timeout, ConnType),
    erlang:monitor(process, Pid),
    {{ok, Pid}, holster_sm:req(Pid, URI)}.







% test() ->

%     {ok, _} = application:ensure_all_started(holster),

%     {ok, _} = dbg:tracer(),
%     {ok, _} = dbg:p(all, call),
%     % {ok, _} = dbg:tpl(holster_sm, open, cx),
%     % {ok, _} = dbg:tpl(holster_sm, connected, cx),
%     % {ok, _} = dbg:tpl(holster_sm, in_request, cx),

%     % {ok, _} = dbg:tpl(gun_tcp, cx),
%     % {ok, _} = dbg:tpl(gun, cx),

%     timer:sleep(100),

%     % {ok, Pid} =
%     %     holster_sm:start_link(
%     %         "localhost",
%     %          http,
%     %          54321,
%     %          #{
%     %             retry => 1,
%     %             retry_timeout => 60000,
%     %             http_opts => #{
%     %                 closing_timeout => 60000,
%     %                 keepalive => 60000
%     %             }
%     %          },
%     %          60000,
%     %          long_running
%     %          % once_off
%     %     ),


%     holster:long_running_req(
%         "http://localhost:54321",
%         #{
%             retry => 10000,
%             retry_timeout => 0,
%             domain_lookup_timeout => 250,
%             connect_timeout => 250,
%             http_opts => #{
%                 closing_timeout => 250
%             }
%         }).

%     % timer:apply_interval(1000, holster_sm, req, [Pid, "/docs/en/gun/2.0/guide/http/"]).

%     % holster_sm:req(Pid, "/docs/en/gun/2.0/guide/http/"),
%     % holster_sm:req(Pid, "/"),



% once_off_test() ->
%     {ok, _} = application:ensure_all_started(holster),
%     % {ok, _} = dbg:tracer(),
%     % {ok, _} = dbg:p(all, call),
%     % {ok, _} = dbg:tpl(gun, cx),
%     run_x_times(10, 10, 0, fun() -> do_once_off_test() end).

% long_running_test() ->
%     {ok, _} = application:ensure_all_started(holster),
%     % {ok, _} = dbg:tracer(),
%     % {ok, _} = dbg:p(all, call),
%     % {ok, _} = dbg:tpl(gun, cx),
%     % run_x_times(10, 10, 0, fun() -> do_long_running_test() end).
%     [ spawn(
%         fun() ->
%             do_long_running_test()
%         end
%     ) || _ <- lists:seq(1, 10) ].

% run_x_times(0, X, Total, _F) ->
%     Total / X;
% run_x_times(X, Y, Total, F) ->
%     {MicroS, _} = timer:tc(F),
%     run_x_times(X-1, Y, Total+MicroS, F).

% do_once_off_test() ->
%     lists:foreach(fun(_) ->
%         % timer:sleep(5),
%         %% Create 404 requests
%         R = once_off_req("http://localhost:54321/api"),
%         % ?LOG_WARNING("once_off RESPONSE ~p\n", [R])
%         ok
%     end, lists:seq(1, 10000)).

% do_long_running_test() ->
%     {{ok, Pid}, _} = long_running_req("http://localhost:54321"),
%     lists:foreach(fun(_) ->
%         % timer:sleep(5),
%         %% Create 404 requests
%         R = long_running_req("http://localhost:54321/api", Pid),
%         % ?LOG_WARNING("long_running RESPONSE ~p\n", [R])
%         ok
%     end, lists:seq(1, 10000)).
