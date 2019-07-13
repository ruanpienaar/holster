-module(holster).

-export([
    req/1,
    req/2,
    req/3,
    req/4,
    req/6,
    req/7
]).

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
    Pid = spawn_link(fun() ->
        init(Host, URI, Proto, Port, OptsMap, Timeout)
    end),
    {ok, Host, URI, Proto, Port, OptsMap, Timeout, Pid}.

req(Host, URI, Proto, Port, OptsMap, Timeout, Pid) ->
    Pid ! {client_request, URI},
    {ok, Host, URI, Proto, Port, OptsMap, Timeout, Pid}.

init(Host, URI, _, Port, OptsMap, Timeout) ->
    {ok, ConnPid} = gun:open(Host, Port, OptsMap),
    case gun:await_up(ConnPid, Timeout) of
        {ok, _Protocol} ->
            StreamRef = gun:get(ConnPid, URI),
            loop(ConnPid, Timeout, StreamRef);
        {error, Reason} ->
            {error, Reason}
    end.

loop(ConnPid, Timeout, StreamRef) ->
    receive
        {client_request, NewUri} ->
            NewStreamRef = gun:get(ConnPid, NewUri),
            loop(ConnPid, Timeout, NewStreamRef);
        {gun_up, ServerPid, Proto} ->
            error_logger:error_msg("Gun up ~p ~p\n", [ServerPid, Proto]),
            loop(ConnPid, Timeout, StreamRef);
        {gun_down, _ServerPid, _Proto, Reason, _ , _} ->
            ok = error_logger:error_msg("Connection Down! ~p\n", [Reason]),
            do_close(ConnPid, StreamRef);
        {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
            error_logger:info_msg(
                "fin - Response Headers ~p\nStatus ~p",
                [Headers, Status]
            ),
            no_data;
        {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
            error_logger:info_msg(
                "nofin - Response Headers ~p\nStatus ~p",
                [Headers, Status]
            ),
            receive_data(ConnPid, StreamRef, Timeout),
            loop(ConnPid, Timeout, StreamRef);
        {'DOWN', _Mref, process, ConnPid, Reason} ->
            ok = error_logger:error_msg("Connection Down!"),
            exit(Reason);
        close ->
            ok = error_logger:error_msg("Connection Closed! 'close' msg"),
            do_close(ConnPid, StreamRef),
            exit(close);
        UnhandledMsg ->
            io:format("Received UnhandledMsg ~p\n\n", [UnhandledMsg]),
            loop(ConnPid, Timeout, StreamRef)
end.

do_close(ConnPid, StreamRef) ->
    ok = gun:cancel(ConnPid, StreamRef),
    ok = gun:close(ConnPid).

receive_data(ConnPid, StreamRef, Timeout) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            io:format("~s~n", [Data]),
            receive_data(ConnPid, StreamRef, Timeout);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            io:format("~s~n", [Data]);
        {'DOWN', _MRef, process, ConnPid, Reason} ->
            ok = error_logger:error_msg("receive data {}"),
            exit(Reason)
    after
        Timeout ->
            exit(timeout)
    end.
