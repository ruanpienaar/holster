-module(holster_request).

%% @doc Shortlived process for making requests then closing
%% connect(open)/waiting-for-up/making-a-request
%% all have a timeout of 60s
%% @end

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/5,
    req/6
]).

%% TODO
%% handle 'DOWN'
%% connecting timeout
%% connected timeout
%% in_request timeout
%% receive_data timeout

start_link(Host, Proto, Port, ConnectOpts, Timeout) ->
    {ok, proc_lib:spawn_link(fun() ->
        connect(#{
            host => Host,
            proto => Proto,
            port => Port,
            connectOpts => ConnectOpts,
            timeout => Timeout
        })
    end)}.

-spec req(pid(), holster:req_type(), http_uri:uri(), gun:req_headers(), gun:req_opts(), timeout()) -> {response, term()}.
req(Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout) when is_map(ReqOpts) ->
    Pid ! {req, self(), ReqType, URI, Headers, ReqOpts, ReqTimeout}.

connect(#{
        host := Host,
        port := Port,
        connectOpts := ConnectOpts
    } = State) ->
    case gun:open(Host, Port, ConnectOpts) of
        {ok, ConnPid} ->
            true = erlang:link(ConnPid),
            MRef = monitor(process, ConnPid),
            connecting(State#{
                conn_pid => ConnPid,
                conn_m_ref => MRef
            });
        {error, OpenError} ->
            {error, OpenError}
    end.

connecting(#{
            conn_pid := ConnPid,
            proto := Proto
        } = State) ->
    receive
        {gun_up, ConnPid, Proto} ->
            connected(State)
    after
        60000 ->
            ok = gun:close(ConnPid),
            exit(self())
    end.

connected(#{
        conn_pid := ConnPid
    } = State) ->
    receive
        {req, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout} ->
            StreamRef = gun:ReqType(ConnPid, URI, Headers, ReqOpts),
            in_request(State#{
                stream_ref => StreamRef,
                client_pid => ClientPid,
                req_timeout => ReqTimeout
            })
    after
        60000 ->
            ok = gun:close(ConnPid),
            exit(self())
    end.

%% TODO: deduct timeout with time spent to get remainder... OR Set timeout with timer:
in_request(#{
        client_pid := ClientPid,
        conn_pid := ConnPid
    } = State) ->
    receive
        {gun_down, ConnPid, _Scheme, Reason, X} ->
            reply(ClientPid, {gun_down, ConnPid, _Scheme, Reason, X});
        {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders} ->
            reply(ClientPid, receive_data(State#{
                response_data => <<>>,
                response_code => Status,
                resonse_headers => RespHeaders
            }));
        {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders} ->
            reply(ClientPid, {Status, RespHeaders, <<>>})
    after
        60000 ->
            ok = gun:close(ConnPid),
            exit(self())
    end.

receive_data(#{
        conn_pid := ConnPid,
        stream_ref := StreamRef,
        conn_m_ref := MRef,
        response_data := ExistingResponsedata,
        req_timeout := ReqTimeout,
        response_code := Status,
        resonse_headers := RespHeaders
    } = State) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            % io:format("~s~n", [Data]),
            receive_data(State#{
                response_data => <<ExistingResponsedata/binary, Data/binary>>
            });
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            {Status, RespHeaders, <<ExistingResponsedata/binary, Data/binary>>};
        {'DOWN', MRef, process, ConnPid, Reason} ->
            % error_logger:error_msg("Oops!"),
            % exit(Reason)
            {'DOWN', MRef, process, ConnPid, Reason}
    after ReqTimeout ->
        exit(timeout)
    end.

reply(ClientPid, Response) ->
    ClientPid ! Response.
