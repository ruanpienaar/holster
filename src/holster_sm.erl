-module(holster_sm).

-include_lib("kernel/include/logger.hrl").
-include("holster.hrl").

%% API
-export([
    start_link/6,
    req/6,
    close/1
]).

-behaviour(gen_statem).

-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4
]).
-export([
    open/3,
    connected/3,
    in_request/3
]).

%% TODO:
%% - scheme - handle and check that once connected it matches up.
%% - Check if gun is using a better spec than iodata() for url's later
%% - implement timeout functionality
%% - fix other request types at ( included in the bottom of this file )

-spec start_link(
    inet:hostname() | inet:ip_address(),
    http | https,
    inet:port_number(),
    gun:opts(),
    timeout(),
    holster:conn_type()
) -> {ok, pid()} | ignore | {error, term()}.
start_link(Host, Proto, Port, ConnectOpts, Timeout, ConnType) when is_map(ConnectOpts) ->
    gen_statem:start_link(?MODULE, {Host, Proto, Port, ConnectOpts, Timeout, ConnType}, []).

% -spec req(pid(), holster:req_type(), uri_string:uri_string()) -> {response, term()}.
% req(Pid, ReqType, URI) ->
%     req(Pid, ReqType, URI, [], #{}).

% -spec req(pid(), holster:req_type(), uri_string:uri_string(), gun:req_headers()) -> {response, term()}.
% req(Pid, ReqType, URI, Headers) ->
%     req(Pid, ReqType, URI, Headers, #{}).

% -spec req(pid(), holster:req_type(), uri_string:uri_string(), gun:req_headers(), gun:req_opts()) -> {response, term()}.
% req(Pid, ReqType, URI, Headers, ReqOpts) when is_map(ReqOpts) ->
%     gen_statem:call(Pid, {req, ReqType, URI, Headers, ReqOpts}).

-spec req(
        pid(),
        holster:req_type(),
        uri_string:uri_string(),
        gun:req_headers(),
        gun:req_opts(),
        undefined | binary()
    ) -> {response, term()}.
req(Pid, ReqType, URI, Headers, ReqOpts, Body)
        when is_map(ReqOpts) andalso
        (Body == undefined orelse is_binary(Body)) ->
    gen_statem:call(Pid, {req, ReqType, URI, Headers, ReqOpts, Body}).

close(Pid) ->
    gen_statem:stop(Pid, client_closed, 5000).

%% ============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

-spec init(Args :: term()) -> gen_statem:init_result(term()).
init({Host, Proto, Port, ConnectOpts, Timeout, ConnType}) ->
    _ = process_flag(trap_exit, true),
    {ok, open, initial_sm_data(Host, Proto, Port, ConnectOpts, Timeout, ConnType)}.

initial_sm_data(Host, Proto, Port, ConnectOpts, Timeout, ConnType) ->
    (initial_sm_data_defaults())#{
        conn_type => ConnType,
        connect_opts => ConnectOpts,
        host => Host,
        port => Port,
        proto => Proto,
        timeout => Timeout
    }.

initial_sm_data_defaults() ->
    #{
        body => undefined,
        client_from => undefined,
        conn_m_ref => undefined,
        conn_pid => undefined,
        req_headers => [],
        req_opts => #{},
        req_type => undefined,
        req_uri => undefined,
        response_data => <<>>,
        response_headers => undefined,
        response_status => undefined,
        scheme => undefined,
        stop_reason => undefined,
        stream_ref => undefined
    }.

% -spec open
%     ('enter', OldState :: atom(), Data :: term())
%         -> gen_statem:state_enter_result('open');
%     (gen_statem:event_type(), Msg :: term(), Data :: term())
%         -> gen_statem:event_handler_result(atom()).
open(enter, connected = PrevState, _Data) ->
    %% TODO: Something is making long running connetions disconnect and reconnect.
    keep_state_and_data;
open(enter, open = PrevState, #{
        scheme := undefined,
        host := Host,
        port := Port,
        connect_opts := ConnectOpts,
        conn_pid := undefined } = Data
    ) ->
    case gun:open(Host, Port, ConnectOpts) of
        {ok, ConnPid} ->
            MRef = monitor(process, ConnPid),
            {keep_state, Data#{
                conn_pid => ConnPid,
                conn_m_ref => MRef
            }};
        {error, OpenError} ->
            {stop, normal, Data#{ stop_reason => {error, OpenError}}}
    end;
open({call, From}, {req, ReqType, URI, Headers, ReqOpts, Body}, Data) ->
    %% NB! postpone until connected
    ?LOG_DEBUG(#{
        state => ?FUNCTION_NAME,
        postponing_request => {req, ReqType, URI, Headers, ReqOpts, Body}
    }),
    {keep_state, Data#{ client_from => From, body => Body }, [postpone]};
%% When we connect first time
open(info, {gun_up, ConnPid, Scheme}, #{
        conn_pid := ConnPid,
        scheme := _Scheme } = Data) ->
    {next_state, connected, Data#{ scheme => Scheme }};
%% When gun reconnects in the background
open(info, {gun_up, ConnPid, Scheme}, #{
        conn_pid := undefined,
        scheme := undefined } = Data) ->
    {next_state, connected, Data#{
        conn_pid => ConnPid,
        scheme => Scheme
    }};
open(info, {'DOWN', MRef, process, ConnPid, Reason}, #{
        conn_m_ref := MRef,
        client_from := undefined } = Data) ->
    {stop, normal, Data#{ stop_reason => {'DOWN', ConnPid}}};
open(info, {'DOWN', MRef, process, ConnPid, Reason}, #{
        conn_m_ref := MRef,
        client_from := From } = Data) when From =/= undefined -> %% TODO: check what type is From
    {stop_and_reply, normal, [{reply, From, {response, Reason}}]}.

connected(enter, PrevState, #{
        conn_pid := ConnPid,
        scheme := Scheme } = _Data)
        when (ConnPid =/= undefined andalso Scheme =/= undefined) andalso
             (PrevState =/= open orelse PrevState =/= in_request) ->
    keep_state_and_data;
connected({call, From},
          {req, ReqType, URI, Headers, ReqOpts, Body},
          #{ conn_type := ConnType } = Data) ->
    {next_state, in_request, Data#{
        body => Body,
        req_uri => URI,
        req_type => ReqType,
        req_headers => Headers,
        req_opts => ReqOpts,
        client_from => From
    }};
connected(info, {gun_down, ConnPid, _Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        conn_type := once,
        stream_ref := StreamRef,
        client_from := undefined } = Data) ->
    ok = cancel_stream_ref_if_present(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid),
    {stop, normal, Data#{
        conn_pid => undefined,
        stream_ref := undefined,
        scheme => undefined
    }};
% where there's been requests sent out
connected(info, {gun_down, ConnPid, _Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        conn_type := stay_connected,
        stream_ref := StreamRef,
        client_from := undefined } = Data) ->
    ?LOG_DEBUG("~p -> gun_down ~p", [?FUNCTION_NAME, Reason]),
    case StreamRef of
        undefined ->
            ok;
        _ ->
            ok = gun:cancel(ConnPid, StreamRef)
    end,
    {next_state, open, Data#{
        conn_pid => undefined,
        stream_ref := undefined,
        scheme => undefined
    }}.

% -spec in_request('enter',
%          OldState :: atom(),
%          Data :: term()) ->
%             gen_statem:state_enter_result('in_request');
%         (gen_statem:event_type(),
%          Msg :: term(),
%          Data :: term()) ->
%             gen_statem:event_handler_result(atom()).
in_request(enter, connected = PrevState, #{
        conn_pid := ConnPid,
        req_uri := URI,
        req_type := ReqType,
        req_headers := Headers,
        req_opts := ReqOpts,
        body := Body } = Data) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    StreamRef =
        case Body of
            undefined ->
                gun:ReqType(ConnPid, URI, Headers, ReqOpts);
            _ ->
                %% Gun doesn't allow for get(... , Body)
                gun:post(ConnPid, URI, Headers, Body)
        end,
    {keep_state, Data#{ stream_ref => StreamRef }};
in_request(info, {gun_down, ConnPid, _Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        stream_ref := StreamRef,
        client_from := From } = Data) when From =/= undefind ->
    ok = gun:cancel(ConnPid, StreamRef),
    {next_state, open, Data#{
        conn_pid => undefined,
        stream_ref := undefined,
        scheme => undefined,
        response_headers => <<>>,
        response_data => <<>>
    }};
in_request(info, {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders}, #{
        conn_pid := ConnPid } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    %% After {gun_response, nofin} we get the actual `{gun_data, fin/nofin}` resonse
    {keep_state, Data#{
        response_status => Status,
        response_headers => RespHeaders
    }};
% Handle once request response by closing/ending the connection and gen_statem
in_request(info, {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders}, #{
        conn_type := once,
        client_from := From,
        conn_pid := ConnPid } = _Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    %% response likely non 200 or error
    ReqResp = {Status, RespHeaders, _ResponseData = <<>>},
    ok = gun:shutdown(ConnPid),
    {stop_and_reply, normal, [{reply, From, {response, ReqResp}}]};
% Handle stay_connected request response by responding and staying open
in_request(info, {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders}, #{
        conn_type := stay_connected,
        client_from := From,
        conn_pid := ConnPid } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    ReqResp = {Status, RespHeaders, _ResponseData = <<>>},
    {next_state, connected, Data#{
        req_uri => undefined,
        req_type => undefined,
        client_from => undefined,
        response_status => undefined,
        response_headers => <<>>,
        response_data => <<>>,
        stream_ref => undefined
    }, [{reply, From, {response, ReqResp}}]};
%% stay in in_request, all `gun_data` calls received, next will be `gun_response`
in_request(info, {gun_data, ConnPid, _StreamRef, fin, ResponseData}, #{
        conn_pid := ConnPid,
        client_from := From,
        response_status := Status,
        response_headers := RespHeaders,
        response_data := ExistingResponsedata } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    ReqResp = {Status, RespHeaders, <<ExistingResponsedata/binary, ResponseData/binary>>},
    {next_state, connected, Data#{
        req_uri => undefined,
        req_type => undefined,
        client_from => undefined,
        response_status => undefined,
        response_headers => <<>>,
        response_data => <<>>,
        stream_ref => undefined
    }, [{reply, From, {response, ReqResp}}]};
in_request(info, {gun_data, ConnPid, _StreamRef, nofin, ResponseData}, #{
        conn_pid := ConnPid,
        response_data := ExistingResponsedata } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    {keep_state, Data#{
        response_data => <<ExistingResponsedata/binary, ResponseData/binary>>
    }}.

terminate(_Reason, _StateName, #{
        conn_pid := _ConnPid,
        stream_ref := _StreamRef,
        client_from := _From,
        stop_reason := StopReason } = _Data
    ) ->
    ?LOG_INFO(#{
        stopping => ?MODULE,
        stop_reason => StopReason
    }),
    %% TODO: should we cleanup ? is it not done elsewhere ?
    %% TODO: cancel andor shutdown/close connection
    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

cancel_stream_ref_if_present(_ConnPid, undefined) ->
    ok;
cancel_stream_ref_if_present(ConnPid, StreamRef) ->
    ok = gun:cancel(ConnPid, StreamRef).