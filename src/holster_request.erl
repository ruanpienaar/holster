-module(holster_request).

%% @doc Shortlived process for making requests then closing
%% connect(open)/waiting-for-up/making-a-request
%% all have a timeout of 60s
%% @end

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/5,
    start_link/6,
    req/6,
    req_timed/6,
    req_timed/7
]).

%% TODO
%% handle 'DOWN'
%% connecting timeout
%% connected timeout
%% in_request timeout
%% receive_data timeout
%% await_up_timeout

%% TODO: allow for response timeout, instead of hardcoded 60s

start_link(Host, Proto, Port, ConnectOpts, Timeout) ->
    ConnectOpts2 = case maps:get(transport, ConnectOpts, undefined) of 
        undefined ->
            case Proto of 
                "https" ->
                    maps:put(transport, tls, ConnectOpts);
                _ ->
                    ConnectOpts
            end;
        _ ->
            ConnectOpts
    end,
    {ok, proc_lib:spawn_link(fun() ->
        connect(#{
            timed => false,
            host => Host,
            proto => holster:convert_to_gun_friendly_scheme(Proto),
            port => Port,
            connectOpts => ConnectOpts2,
            timeout => Timeout
        })
    end)}.

start_link(Host, Proto, Port, ConnectOpts, Timeout, timed) ->
    ConnectOpts2 = case maps:get(transport, ConnectOpts, undefined) of 
        undefined ->
            case Proto of 
                "https" ->
                    maps:put(transport, tls, ConnectOpts);
                _ ->
                    ConnectOpts
            end;
        _ ->
            ConnectOpts
    end,
    {ok, proc_lib:spawn_link(fun() ->
        connect(#{
            timed => true,
            host => Host,
            proto => Proto,
            port => Port,
            connectOpts => ConnectOpts2,
            timeout => Timeout
        })
    end)}.

-spec req(pid(), holster:req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts(), timeout()) -> {response, term()}.
req(Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout) when is_map(ReqOpts) ->
    ?LOG_NOTICE(#{ req => {Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout} }),
    Pid ! {req, self(), ReqType, URI, Headers, ReqOpts, ReqTimeout}.

-spec req_timed(pid(), holster:req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts(), timeout()) -> {response, term()}.
req_timed(Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout) when is_map(ReqOpts) ->
    ?LOG_NOTICE(#{ req => {Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout} }),
    Pid ! {req_timed, self(), ReqType, URI, Headers, ReqOpts, ReqTimeout}.

% -spec req_timed(pid(), holster:req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts(), timeout()) -> {response, term()}.
req_timed(Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout, Body) when is_map(ReqOpts) ->
    ?LOG_NOTICE(#{ req => {Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout, Body} }),
    Pid ! {req_timed, self(), ReqType, URI, Headers, ReqOpts, ReqTimeout, Body}.

connect(#{
        timed := true,
        host := Host,
        port := Port,
        connectOpts := ConnectOpts
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    case timer:tc( fun() -> gun:open(Host, Port, ConnectOpts) end ) of
        {OpenMicSec, {ok, ConnPid}} ->
            ?LOG_NOTICE(#{ debug => {gun, open, {ok, ConnPid}}}),
            BefConnectingMicSec = erlang:system_time(microsecond),
            true = erlang:link(ConnPid),
            MRef = monitor(process, ConnPid),
            connecting(State#{
                open_micsec => OpenMicSec,
                before_opening_micsec => BefConnectingMicSec,
                conn_pid => ConnPid,
                conn_m_ref => MRef
            });
        {OpenMicSec, {error, OpenError}} ->
            {OpenMicSec, {error, OpenError}}
    end;
connect(#{
        host := Host,
        port := Port,
        connectOpts := ConnectOpts
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    case gun:open(Host, Port, ConnectOpts) of
        {ok, ConnPid} ->
            ?LOG_NOTICE(#{ debug => {gun, open, {ok, ConnPid}}}),
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
            timed := true,
            conn_pid := ConnPid,
            proto := Proto,
            conn_m_ref := MRef
        } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    {ok, Proto} = gun:await_up(ConnPid, 60000, MRef),
    AfterConnectingMicSec = erlang:system_time(microsecond),
    connected(State#{
        after_opening_micsec => AfterConnectingMicSec
    });
    % receive
    %     {gun_up, ConnPid, _} -> %% 3rd could be http2, not https :(
    %         AfterConnectingMicSec = erlang:system_time(microsecond),
    %         connected(State#{
    %             after_opening_micsec => AfterConnectingMicSec
    %         })
    % after
    %     60000 ->
    %         ok = gun:close(ConnPid),
    %         exit(self())
    % end;
connecting(#{
            conn_pid := ConnPid,
            proto := _Proto,
            conn_m_ref := MRef
        } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    %% TODO: await up timeout
    case gun:await_up(ConnPid, 60000, MRef) of
        {ok, _GunProto} ->
            ?LOG_NOTICE(#{ debug => gun_up }),
            connected(State);
        {error, timeout} ->
            ?LOG_NOTICE(#{ debug => {await_up, {error, timeout}} }),
            {error, timeout}
    end.
    % receive
    %     {gun_up, ConnPid, Proto} ->
    %         connected(State)
    %     % Other ->
    %     %     io:format("Recv Other ~p\n", [Other]),
    %     %     connecting(State)
    % after
    %     60000 ->
    %         ok = gun:close(ConnPid),
    %         exit(self())
    % end.

connected(#{
        conn_pid := ConnPid
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    receive
        {req, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout} ->
            ?LOG_NOTICE(#{ req6 => {req, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout} }),
            StreamRef = gun:ReqType(ConnPid, URI, Headers, ReqOpts),
            in_request(State#{
                stream_ref => StreamRef,
                client_pid => ClientPid,
                req_timeout => ReqTimeout
            });
        {req_timed, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout} ->
            ?LOG_NOTICE(#{ req_timed => {req_timed, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout} }),
            RecvReqMicSec = erlang:system_time(microsecond),
            StreamRef = gun:ReqType(ConnPid, URI, Headers, ReqOpts),
            in_request(State#{
                recv_req_micsec => RecvReqMicSec,
                stream_ref => StreamRef,
                client_pid => ClientPid,
                req_timeout => ReqTimeout
            });
        {req_timed, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout, Body} ->
            ?LOG_NOTICE(#{ req_timed_body => {req_timed, ClientPid, ReqType, URI, Headers, ReqOpts, ReqTimeout, Body} }),
            RecvReqMicSec = erlang:system_time(microsecond),
            StreamRef = gun:post(ConnPid, URI, Headers, Body, ReqOpts),
            in_request(State#{
                recv_req_micsec => RecvReqMicSec,
                stream_ref => StreamRef,
                client_pid => ClientPid,
                req_timeout => ReqTimeout
            })
    after
        60000 ->
            ?LOG_NOTICE(#{timeout => {?MODULE, never_received_req}}),
            ok = gun:close(ConnPid),
            exit(self())
    end.

%% TODO: deduct timeout with time spent to get remainder... OR Set timeout with timer:
%% TODO: correlate StreamRef with State.StreamRef
in_request(#{
        timed := true,
        client_pid := ClientPid,
        conn_pid := ConnPid
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    receive
        {gun_down, ConnPid, Scheme, Reason, X} ->
            ?LOG_NOTICE(#{debug => {gun_down, ConnPid, Scheme, Reason, X}}),
            reply(ClientPid, {gun_down, ConnPid, Scheme, Reason, X});
        {gun_response, ConnPid, StreamRef, nofin, Status, RespHeaders} ->
            ?LOG_NOTICE(#{debug => {gun_response, ConnPid, StreamRef, nofin, Status, RespHeaders}}),
            reply(ClientPid, receive_data(State#{
                response_data => <<>>,
                response_code => Status,
                resonse_headers => RespHeaders
            }));
        {gun_response, ConnPid, StreamRef, fin, Status, RespHeaders} ->
            ?LOG_NOTICE(#{debug => {gun_response, ConnPid, StreamRef, fin, Status, RespHeaders}}),
            RecvFinalRespMicSec = erlang:system_time(microsecond),
            reply(ClientPid, {Status, RespHeaders, <<>>, times_map(State#{
                recv_final_response => RecvFinalRespMicSec
            })})
    after
        60000 ->
            ?LOG_NOTICE(#{debug => timeout}),
            ok = gun:close(ConnPid),
            exit(self())
    end;
in_request(#{
        client_pid := ClientPid,
        conn_pid := ConnPid
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    receive
        {gun_down, ConnPid, _Scheme, Reason, X} ->
            ?LOG_NOTICE(#{debug => {gun_down, ConnPid, _Scheme, Reason, X}}),
            reply(ClientPid, {gun_down, ConnPid, _Scheme, Reason, X});
        {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders} ->
            ?LOG_NOTICE(#{debug => {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders}}),
            reply(ClientPid, receive_data(State#{
                response_data => <<>>,
                response_code => Status,
                resonse_headers => RespHeaders
            }));
        {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders} ->
            ?LOG_NOTICE(#{debug => {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders}}),
            reply(ClientPid, {Status, RespHeaders, <<>>})
    after
        60000 ->
            ok = gun:close(ConnPid),
            exit(self())
    end.

times_map(State) ->
    maps:with(
        [
            before_opening_micsec,
            after_opening_micsec,
            recv_req_micsec,
            recv_final_response
        ],
        State
    ).

receive_data(#{
        timed := TimedBool,
        conn_pid := ConnPid,
        stream_ref := StreamRef,
        % conn_m_ref := MRef,
        response_data := ExistingResponsedata,
        req_timeout := ReqTimeout,
        response_code := Status,
        resonse_headers := RespHeaders
    } = State) ->
    ?LOG_NOTICE(#{ state => State }),
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            ?LOG_NOTICE(#{ nofin_data => Data }),
            receive_data(State#{
                response_data => <<ExistingResponsedata/binary, Data/binary>>
            });
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            ?LOG_NOTICE(#{ fin_data => Data }),
            case TimedBool of
                true ->
                    RecvFinalRespMicSec = erlang:system_time(microsecond),
                    {Status, RespHeaders, <<ExistingResponsedata/binary, Data/binary>>, times_map(State#{
                        recv_final_response => RecvFinalRespMicSec
                    })};
                false ->
                    {Status, RespHeaders, <<ExistingResponsedata/binary, Data/binary>>}
            end;
        {'DOWN', MRef, process, ConnPid, Reason} ->
            % error_logger:error_msg("Oops!"),
            % exit(Reason)
            ?LOG_NOTICE(#{ down => {'DOWN', MRef, process, ConnPid, Reason} }),
            {'DOWN', MRef, process, ConnPid, Reason}
    after ReqTimeout ->
        exit(timeout)
    end.

reply(ClientPid, Response) ->
    ?LOG_NOTICE(#{ response => Response }),
    ClientPid ! Response.
