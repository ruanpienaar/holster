-module(holster).

-export([
    req/1
]).

req(Url) ->
    req(Url, http).

req(Url, http) ->
    req(Url, http, 80);
req(Url, https) ->
    req(Url, https, 443).

req(Url, https, Port) when Port /= 443 ->
    req(Url, https, Port, #{transport => tls}, 1000);
req(Url, Proto, Port) ->
    req(Url, Proto, Port, #{}, 1000).

req(Url, Proto, Port, OptsMap, Timeout) ->
    Pid = spawn_link(fun() -> init(Url, Proto, Port, OptsMap, Timeout) end),
    Pid ! {self(), Url}.

init(Url, _, Port, OptsMap, Timeout) ->
    {ok, ConnPid} = gun:open(Url, Port, OptsMap),
    case gun:await_up(ConnPid, Timeout) of
        {ok, Protocol} ->
            loop(ConnPid, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

loop(ConnPid, Timeout) ->
    receive
        {ReqPid, Url} ->
            % Resonse = gun:get(ConnPid, Url),
            StreamRef = gun:get(ConnPid, Url),
            Response =
                receive
                    {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
                        no_data;
                    {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
                        receive_data(ConnPid, MRef, StreamRef, Timeout);
                    {'DOWN', MRef, process, ConnPid, Reason} ->
                        error_logger:error_msg("Connection Down!"),
                        exit(Reason)
                after
                    Timeout ->
                        exit(timeout)
                end,
            ReqPid ! {response, Response},
            loop(ConnPid);
        {'DOWN', Mref, process, ConnPid, Reason} ->
            ok = error_logger:error_msg("Connection Down!"),
            exit(Reason);
        close ->
            ok = gun:cancel(ConnPid, StreamRef),
            ok = gun:close(ConnPid);
        Msg ->
            io:format("Received Msg ~p\n\n", [Msg]),
            loop(ConnPid)
end.

receive_data(ConnPid, MRef, StreamRef, Timeout) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            io:format("~s~n", [Data]),
            receive_data(ConnPid, MRef, StreamRef);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            io:format("~s~n", [Data]);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            ok = error_logger:error_msg("Oops!"),
            exit(Reason)
    after
        1000 ->
            exit(timeout)
    end.
