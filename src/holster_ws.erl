-module(holster_ws).

%% @doc Websocket handling process
%% connect(open)/waiting-for-up/ws_upgrade/making-a-request
%% all have a timeout of 5s
%% @end

-include_lib("kernel/include/logger.hrl").

-export([
    start_link/8,
    ws_upgrade/1,
    ws_upgrade/2,
    ws_send/2,
    close/1
]).

%% TODO
%% handle 'DOWN'
%% connecting timeout
%% connected timeout
%% in_request timeout
%% receive_data timeout
%% monitor client_pid - if 'DOWN', close request too
%% handle/test when websocket closes

start_link(Host, Proto, Port, ConnectOpts, WsPath, Timeouts, WsUpgradeHeaders, WsUpgradeOpts) ->
    {ok, proc_lib:spawn_link(fun() ->
        connect(#{
            host => Host,
            proto => Proto,
            port => Port,
            connectOpts => ConnectOpts,
            ws_path => WsPath,
            timeouts => Timeouts,
            ws_upgrade_headers => WsUpgradeHeaders,
            ws_upgrade_opts => WsUpgradeOpts
        })
    end)}.

ws_upgrade(Pid) ->
    ws_upgrade(Pid, 5000).

ws_upgrade(Pid, Timeout) ->
    Pid ! {ws_upgrade, self()},
    receive
        {ws_upgraded, WsHeaders} ->
            {ws_upgraded, WsHeaders}
    after
        Timeout ->
            {error, {ws_upgrade, timeout}}
    end.

%% TODO: store multile clients later

ws_send(Pid, Term) ->
    ws_send(Pid, Term, 5000).

ws_send(Pid, Term, Timeout) ->
    Pid ! {ws_send, self(), Term},
    receive
        ws_sent ->
            ok
    after
        Timeout ->
            {error, {ws_send, timeout}}
    end.

close(Pid) ->
    _ = Pid ! close,
    ok.

%% ---
%% Internals

connect(#{
        host := Host,
        port := Port,
        connectOpts := ConnectOpts
    } = State) ->
    ?LOG_DEBUG("~p ~p\n",[?MODULE, ?FUNCTION_NAME]),
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
            timeouts := Timeouts
        } = State) ->
    ?LOG_DEBUG("~p ~p Conn:~p\n",[?MODULE, ?FUNCTION_NAME, ConnPid]),
    AwaitUpTimeout =
        default(lists:keyfind(ws_await_up_timeout, 1, Timeouts), 5000),
    receive
        {gun_up, ConnPid, _Proto} ->
            connected(State);
        close ->
            ok = gun:close(ConnPid)
    after
        AwaitUpTimeout ->
            ?LOG_DEBUG("[~p] (~p) AwaitUpTimeout", [?MODULE, ConnPid]),
            ok = gun:close(ConnPid)
    end.

connected(#{
        conn_pid := ConnPid,
        timeouts := Timeouts
    } = State) ->
    ?LOG_DEBUG("~p ~p Conn:~p\n",[?MODULE, ?FUNCTION_NAME, ConnPid]),
    ConnectedIdleTimeout =
        default(lists:keyfind(connected_idle_timeout, 1, Timeouts), 60 * 60 * 1000),
    receive
        {ws_upgrade, ClientPid} ->
            ws_upgrading(State#{ client_pid => ClientPid });
        {gun_down, ConnPid, _Proto} ->
            ?LOG_DEBUG("[~p] (~p) gun_down in connected", [?MODULE, ConnPid]),
            connect(State);
        {gun_up, ConnPid, _Proto} ->
            connected(State);
        close ->
            ok = gun:close(ConnPid)
    after
        ConnectedIdleTimeout ->
            ?LOG_DEBUG("[~p] (~p) ConnectedIdleTimeout", [?MODULE, ConnPid]),
            ok = gun:close(ConnPid)
    end.

ws_upgrading(#{
        client_pid := ClientPid,
        conn_pid := ConnPid,
        timeouts := Timeouts,
        ws_path := WsPath
    } = State) ->
    ?LOG_DEBUG("~p ~p Conn:~p\n",[?MODULE, ?FUNCTION_NAME, ConnPid]),
    WsUpgradeTimeout =
        default(lists:keyfind(ws_upgrade_timeout, 1, Timeouts), 5000),
    StreamRef = gun:ws_upgrade(ConnPid, WsPath, [], #{}),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], WsHeaders} ->
            _ = reply(ClientPid, {ws_upgraded, WsHeaders}),
            ws_connected(State#{
                stream_ref => StreamRef
            });
        {gun_response, ConnPid, _, _, Status, Headers} ->
            ?LOG_DEBUG("[~p] (~p) ~p gun_response in ws_upgrading", [?MODULE, ConnPid, Status]),
            ok = gun:close(ConnPid),
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, _StreamRef, Reason} ->
            ?LOG_DEBUG("[~p] (~p) ~p gun_error in ws_upgrading", [?MODULE, ConnPid, Reason]),
            ok = gun:close(ConnPid),
            exit({ws_upgrade_failed, Reason});
        close ->
            ok = gun:close(ConnPid)
    after
        WsUpgradeTimeout ->
            ?LOG_DEBUG("[~p] (~p) WsUpgradeTimeout", [?MODULE, ConnPid]),
            ok = gun:close(ConnPid)
    end.

ws_connected(#{
        client_pid := ClientPid,
        conn_pid := ConnPid,
        timeouts := Timeouts,
        stream_ref := StreamRef
    } = State) ->
    ?LOG_DEBUG("~p ~p Conn:~p\n",[?MODULE, ?FUNCTION_NAME, ConnPid]),
    ConnectedIdleTimeout =
        default(lists:keyfind(connected_idle_timeout, 1, Timeouts), 60 * 60 * 1000),
    receive
        %% Sending data
        {ws_send, NewClientPid, {text, Json}} ->
            ok = gun:ws_send(ConnPid, StreamRef, {text, Json}),
            NewClientPid ! ws_sent,
            ws_connected(State#{ client_pid => NewClientPid });
        {ws_send, NewClientPid, Term} ->
            ok = gun:ws_send(ConnPid, StreamRef, Term),
            NewClientPid ! ws_sent,
            ws_connected(State#{ client_pid => NewClientPid });
        %% Receiving data
        {gun_ws, ConnPid, _StreamRef, {close, 1011, <<>>}} ->
            ?LOG_DEBUG("[~p] (~p) ~p gun_ws {close, 1011, <<>>} in ws_connected", [?MODULE, ConnPid]),
            ok = gun:close(ConnPid);
        {gun_ws, ConnPid, StreamRef, Frame} ->
            ?LOG_DEBUG("WS ~p ~p ~p", [ConnPid, StreamRef, Frame]),
            _ = reply(ClientPid, {ws_response, Frame}),
            ws_connected(State);
        {gun_down, ConnPid, _Proto} ->
            ?LOG_DEBUG("[~p] (~p) gun_down in ws_connected", [?MODULE, ConnPid]),
            connect(State);
        {gun_up, ConnPid, _Proto} ->
            ?LOG_DEBUG("[~p] (~p) gun_up in ws_connected", [?MODULE, ConnPid]),
            ws_connected(State);
        close ->
            ok = gun:close(ConnPid)
    after
        ConnectedIdleTimeout ->
            ?LOG_DEBUG("[~p] (~p) ws_connected IdleTimeout", [?MODULE, ConnPid]),
            ok = gun:close(ConnPid)
    end.

reply(ClientPid, Response) ->
    ClientPid ! Response.

default(false, Default) ->
    Default;
default({_, V}, _) ->
    V.