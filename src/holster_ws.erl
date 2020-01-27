-module(holster_ws).

%% @doc Websocket handling process
%% connect(open)/waiting-for-up/ws_upgrade/making-a-request
%% all have a timeout of 5s
%% @end

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/7,
    ws_upgrade/1,
    ws_send/2
]).

%% TODO
%% handle 'DOWN'
%% connecting timeout
%% connected timeout
%% in_request timeout
%% receive_data timeout
%% monitor client_pid - if 'DOWN', close request too
%% handle/test when websocket closes

start_link(Host, Proto, Port, ConnectOpts, WsOpts, WsPath, Timeout) ->
    {ok, proc_lib:spawn_link(fun() ->
        connect(#{
            host => Host,
            proto => Proto,
            port => Port,
            connectOpts => ConnectOpts,
            ws_path => WsPath,
            ws_opts => WsOpts, % TODO
            timeout => Timeout
        })
    end)}.

ws_upgrade(Pid) ->
    Pid ! {ws_upgrade, self()},
    receive
        {ws_upgraded, WsHeaders} ->
            {ws_upgraded, WsHeaders}
    after
        5000 ->
            {error, {ws_upgrade, timeout}}
    end.

%% TODO: do timeout example

ws_send(Pid, Term) ->
    Pid ! {ws_send, Term},
    receive
        {ws_response, Response} ->
            {ws_response, Response}
    after
        5000 ->
            {error, {ws_send, timeout}}
    end.

% -spec req(pid(), holster:req_type(), http_uri:uri(), gun:req_headers(), gun:req_opts(), timeout()) -> {response, term()}.
% req(Pid, ReqType, URI, Headers, ReqOpts, ReqTimeout) when is_map(ReqOpts) ->
%     Pid ! {req, self(), ReqType, URI, Headers, ReqOpts, ReqTimeout}.

connect(#{
        host := Host,
        port := Port,
        connectOpts := _ConnectOpts
    } = State) ->
    erlang:display({?FUNCTION_NAME, State}),
    case gun:open(Host, Port) of
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
    erlang:display({?FUNCTION_NAME, State}),
    receive
        {gun_up, ConnPid, Proto} ->
            connected(State)
    after
        5000 ->
            ok = gun:close(ConnPid)
    end.

connected(#{
        conn_pid := ConnPid
    } = State) ->
    % erlang:display({?FUNCTION_NAME, State}),
    receive
        {ws_upgrade, ClientPid} ->
            ws_upgrading(State#{ client_pid => ClientPid });
        {gun_down, ConnPid, _Proto} ->
            % erlang:display({?FUNCTION_NAME, msg, {gun_down, ConnPid, _Proto}}),
            connect(State);
        {gun_up, ConnPid, _Proto} ->
            % erlang:display({?FUNCTION_NAME, msg, {gun_up, ConnPid, _Proto}}),
            connected(State)
    % after
    %     5000 ->
    %         ok = gun:close(ConnPid)
    end.

ws_upgrading(#{
        client_pid := ClientPid,
        conn_pid := ConnPid,
        ws_path := WsPath
    } = State) ->
    erlang:display({?FUNCTION_NAME, State}),
    WsRef = gun:ws_upgrade(ConnPid, WsPath),
    receive
        {gun_upgrade, ConnPid, WsRef, [<<"websocket">>], WsHeaders} ->
            % erlang:display(WsHeaders),
            % [{<<"connection">>,<<"Upgrade">>},{<<"upgrade">>,<<"websocket">>},{<<"sec-websocket-accept">>,<<"vbjPYJDFpXziWzWTq0rGScsIRMo=">>}]
            _ = reply(ClientPid, {ws_upgraded, WsHeaders}),
            ws_connected(State#{
                ws_ref => WsRef
            });
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after
        5000 ->
            ok = gun:close(ConnPid)
    end.

ws_connected(#{
        conn_pid := ConnPid
    } = State) ->
    % erlang:display({?FUNCTION_NAME, State}),
    receive
        % duplication really just to showcase usage
        {ws_send, {text, Json}} ->
            ok = gun:ws_send(ConnPid, {text, Json}),
            in_ws_request(State);
        {ws_send, Term} ->
            ok = gun:ws_send(ConnPid, Term),
            in_ws_request(State);
        {gun_down, ConnPid, _Proto} ->
            % erlang:display({?FUNCTION_NAME, msg, Other}),
            connect(State);
        {gun_up, ConnPid, _Proto} ->
            % erlang:display({?FUNCTION_NAME, msg, Other}),
            ws_connected(State)
    % CAN stay connected for longer than 1 minute.. for now
    % after
    %     5000 ->
    %         ok = gun:close(ConnPid),
    end.

in_ws_request(#{
        conn_pid := ConnPid,
        ws_ref := WsRef,
        client_pid := ClientPid
    } = State) ->
    erlang:display({?FUNCTION_NAME, State}),
    receive
        {gun_ws, ConnPid, WsRef, {close, 1011, <<>>}} ->
            % just close for now ...
            ok = gun:close(ConnPid);
            % connect(State);
        {gun_ws, ConnPid, WsRef, Frame} ->
            _ = reply(ClientPid, {ws_response, Frame}),
            ws_connected(State)
    % CAN wait for response for longer than 1 minute.. for now
    % after
    %     5000 ->
    %         ok = gun:close(ConnPid),
    end.

%% TODO: deduct timeout with time spent to get remainder... OR Set timeout with timer:
% in_request(#{
%         client_pid := ClientPid,
%         conn_pid := ConnPid
%     } = State) ->
%     receive
%         {gun_down, ConnPid, _Scheme, Reason, X} ->
%             reply(ClientPid, {gun_down, ConnPid, _Scheme, Reason, X});
%         {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders} ->
%             reply(ClientPid, receive_data(State#{
%                 response_data => <<>>,
%                 response_code => Status,
%                 resonse_headers => RespHeaders
%             }));
%         {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders} ->
%             reply(ClientPid, {Status, RespHeaders, <<>>})
%     after
%         5000 ->
%             ok = gun:close(ConnPid),
%             exit(self())
%     end.

% receive_data(#{
%         conn_pid := ConnPid,
%         stream_ref := StreamRef,
%         conn_m_ref := MRef,
%         response_data := ExistingResponsedata,
%         req_timeout := ReqTimeout,
%         response_code := Status,
%         resonse_headers := RespHeaders
%     } = State) ->
%     receive
%         {gun_data, ConnPid, StreamRef, nofin, Data} ->
%             % io:format("~s~n", [Data]),
%             receive_data(State#{
%                 response_data => <<ExistingResponsedata/binary, Data/binary>>
%             });
%         {gun_data, ConnPid, StreamRef, fin, Data} ->
%             {Status, RespHeaders, <<ExistingResponsedata/binary, Data/binary>>};
%         {'DOWN', MRef, process, ConnPid, Reason} ->
%             % error_logger:error_msg("Oops!"),
%             % exit(Reason)
%             {'DOWN', MRef, process, ConnPid, Reason}
%     after ReqTimeout ->
%         exit(timeout)
%     end.

reply(ClientPid, Response) ->
    ClientPid ! Response.
