-module(holster).

-include_lib("kernel/include/logger.hrl").

-export([
    once_off_req/1,
    long_running_req/1,
    long_running_req/2,
    req/1,
    req/2,
    req/3,
    req/4,
    req/6,
    req/7
]).

-export([
    test/0,
    once_off_test/0,
    long_running_test/0
]).

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
    do_req(Url, #{}, 1000, once_off).

-spec long_running_req(http_uri:uri()) -> {{ok, pid()}, {response, term()}}.
long_running_req(Url) ->
    do_req(Url, #{}, 1000, long_running).

-spec long_running_req(http_uri:uri(), pid()) -> {{ok, pid()}, {response, term()}}.
long_running_req(Url, Pid) ->
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
do_req(Url, OptsMap, Timeout, ConnType, Pid) ->
    case http_uri:parse(Url) of
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} ->
            holster_sm:req(Pid, Path ++ Query);
        {ok, {Scheme, _UserInfo, Host, Port, Path, Query, _Fragment}} ->
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
    {{ok, Pid}, holster_sm:req(Pid, URI)}.







    %     when ConnType == once_off orelse
    %          ConnType == long_running ->
    % ClientPid = self(),
    % Pid = spawn_link(fun() ->
    %     init(Host, Proto, Port, OptsMap, Timeout, ClientPid, ConnType)
    % end),
    % receive
    %     up ->
    %         Pid ! {client_request, URI, ClientPid},
    %         receive
    %             {reply, X} ->
    %                 {ok, X}
    %         after
    %             Timeout ->
    %                 {error, {request, timeout}}
    %         end;
    %     not_up ->
    %         {error, not_up};
    %     not_connected ->
    %         {error, not_connected}
    % after
    %     Timeout ->
    %         {error, {internal, timeout}}
    % end.

% consec_req(URI, Pid) ->
%     ok.

% init(Host, Proto, Port, OptsMap, Timeout, ClientPid, ConnType) ->
%     case gun:open(Host, Port, OptsMap) of
%         {ok, ConnPid} ->
%             case gun:await_up(ConnPid, Timeout) of
%                 {ok, _UpProtocol} ->
%                     ClientPid ! up,
%                     loop(ConnPid, Timeout, ConnType);
%                 {error, AwaitUpError} ->
%                     ClientPid ! not_up,
%                     ?LOG_ERROR({error, open, AwaitUpError}),
%                     {error, AwaitUpError}
%             end;
%         {error, OpenError} ->
%             ClientPid ! not_connected,
%             ?LOG_ERROR({error, await_up, OpenError}),
%             {error, OpenError}
%     end.

% loop(ConnPid, Timeout, ConnType) ->
%     receive
%         {client_request, URI, ClientPid} ->
%             handle_request(ConnPid, URI, Timeout, ClientPid, ConnType);
%         {gun_up, ServerPid, Proto} ->
%             ?LOG_INFO("Gun up ~p ~p\n", [ServerPid, Proto]),
%             loop(ConnPid, Timeout, ConnType);
%         {gun_down, _ServerPid, _Proto, Reason, _ , _} ->
%             ?LOG_INFO("Connection Down! ~p\n", [Reason]),
%             do_close(ConnPid);
%         % {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
%         %     ?LOG_INFO(
%         %         "fin - Response Headers ~p\nStatus ~p",
%         %         [Headers, Status]
%         %     ),
%         %     no_data;
%         % {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
%         %     ?LOG_INFO(
%         %         "nofin - Response Headers ~p\nStatus ~p",
%         %         [Headers, Status]
%         %     ),
%         %     receive_data(ConnPid, StreamRef, Timeout),
%         %     loop(ConnPid, Timeout, ConnType);
%         {'DOWN', _Mref, process, ConnPid, Reason} ->
%             ?LOG_INFO("Connection Down!"),
%             exit(Reason);
%         close ->
%             ?LOG_INFO("Connection Closed! 'close' msg"),
%             do_close(ConnPid),
%             exit(close);
%         UnhandledMsg ->
%             ?LOG_INFO("Received UnhandledMsg ~p\n\n", [UnhandledMsg]),
%             loop(ConnPid, Timeout, ConnType)
% end.

% handle_request(ConnPid, URI, Timeout, ClientPid, ConnType) ->
%     StreamRef = gun:get(ConnPid, URI),
%     receive
%         {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
%             ?LOG_INFO(
%                 "fin - Response Headers ~p\nStatus ~p",
%                 [Headers, Status]
%             ),
%             ClientPid ! {reply, #{
%                 status => Status,
%                 headers => Headers
%             }},
%             loop(ConnPid, Timeout, ConnType);
%         {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
%             ?LOG_INFO(
%                 "nofin - Response Headers ~p\nStatus ~p",
%                 [Headers, Status]
%             ),
%             ResponseData = receive_data(ConnPid, StreamRef, Timeout),
%             ClientPid ! {reply, #{
%                 status => Status,
%                 headers => Headers,
%                 response => ResponseData
%             }}
%     end.


% do_close(ConnPid) ->
%     % ok = gun:cancel(ConnPid, StreamRef),
%     % ok = gun:close(ConnPid).
%     ok = gun:shutdown(ConnPid).

% receive_data(ConnPid, StreamRef, Timeout) ->
%     receive
%         {gun_data, ConnPid, StreamRef, nofin, Data} ->
%             ?LOG_INFO("~s~n", [Data]),
%             receive_data(ConnPid, StreamRef, Timeout);
%         {gun_data, ConnPid, StreamRef, fin, Data} ->
%             ?LOG_INFO("~s~n", [Data]),
%             Data;
%         {'DOWN', _MRef, process, ConnPid, Reason} ->
%             ?LOG_ERROR("receive data {}"),
%             exit(Reason)
%     after
%         Timeout ->
%             exit(timeout)
%     end.










test() ->

    {ok, _} = application:ensure_all_started(holster),

    {ok, _} = dbg:tracer(),
    {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(holster_sm, open, cx),
    % {ok, _} = dbg:tpl(holster_sm, connected, cx),
    % {ok, _} = dbg:tpl(holster_sm, in_request, cx),

    {ok, _} = dbg:tpl(gun_tcp, cx),
    {ok, _} = dbg:tpl(gun, cx),

    timer:sleep(100),

    {ok, Pid} =
        holster_sm:start_link(
            "localhost",
             http,
             54321,
             #{
                retry => 1,
                retry_timeout => 60000,
                http_opts => #{
                    closing_timeout => 60000,
                    keepalive => 60000
                }
             },
             60000,
             long_running
             % once_off
        ),

    % timer:apply_interval(1000, holster_sm, req, [Pid, "/docs/en/gun/2.0/guide/http/"]).

    % holster_sm:req(Pid, "/docs/en/gun/2.0/guide/http/"),
    holster_sm:req(Pid, "/"),
    Pid.


once_off_test() ->
    {ok, _} = application:ensure_all_started(holster),
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(gun, cx),
    run_x_times(10, 10, 0, fun() -> do_once_off_test() end).

long_running_test() ->
    {ok, _} = application:ensure_all_started(holster),
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(gun, cx),
    % run_x_times(10, 10, 0, fun() -> do_long_running_test() end).
    [ spawn(
        fun() ->
            do_long_running_test()
        end
    ) || _ <- lists:seq(1, 10) ].

run_x_times(0, X, Total, _F) ->
    Total / X;
run_x_times(X, Y, Total, F) ->
    {MicroS, _} = timer:tc(F),
    run_x_times(X-1, Y, Total+MicroS, F).

do_once_off_test() ->
    lists:foreach(fun(_) ->
        % timer:sleep(5),
        %% Create 404 requests
        R = once_off_req("http://localhost:54321/api"),
        % ?LOG_WARNING("once_off RESPONSE ~p\n", [R])
        ok
    end, lists:seq(1, 10000)).

do_long_running_test() ->
    {{ok, Pid}, _} = long_running_req("http://localhost:54321"),
    lists:foreach(fun(_) ->
        % timer:sleep(5),
        %% Create 404 requests
        R = long_running_req("http://localhost:54321/api", Pid),
        % ?LOG_WARNING("long_running RESPONSE ~p\n", [R])
        ok
    end, lists:seq(1, 10000)).
