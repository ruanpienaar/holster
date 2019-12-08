-module(holster_sm).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/6,
    req/2
]).

-behaviour(gen_statem).

%% gen_statem callbacks
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

%% ============================================================================


% -spec start_link() ->
%             {ok, Pid :: pid()} |
%             ignore |
%             {error, Error :: term()}.

%% -spec start_link(host, proto, port, gun:opts(), timeout, get | post)

start_link(Host, Proto, Port, OptsMap, Timeout, ConnType) ->
    gen_statem:start_link(?MODULE, {Host, Proto, Port, OptsMap, Timeout, ConnType}, []).

-spec req(pid(), http_uri:uri()) -> {response, term()}.
req(Pid, URI) ->
    req(Pid, URI, get).

-spec req(pid(), http_uri:uri(), get | post) -> {response, term()}.
req(Pid, URI, ReqType) ->
    gen_statem:call(Pid, {req, URI, ReqType}).

%% ============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

-spec init(Args :: term()) -> gen_statem:init_result(term()).
init({Host, Proto, Port, OptsMap, Timeout, ConnType}) ->
    process_flag(trap_exit, true),
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(gun, cx),
    {ok, open, #{
        scheme => undefined,
        host => Host,
        req_uri => undefined,
        req_type => undefined, %% get | post
        proto => Proto,
        port => Port,
        opts_map => OptsMap,
        timeout => Timeout,
        conn_type => ConnType,
        conn_pid => undefined,
        stream_ref => undefined,
        client_from => undefined,
        response_status => undefined,
        response_headers => undefined,
        response_data => <<>>,
        conn_m_ref => undefined,
        stop_reason => undefined
    }}.

% -spec open
%     ('enter', OldState :: atom(), Data :: term())
%         -> gen_statem:state_enter_result('open');
%     (gen_statem:event_type(), Msg :: term(), Data :: term())
%         -> gen_statem:event_handler_result(atom()).

open(enter, connected = PrevState, Data) ->
    %% For some reason GUN considers it nice to try and reconnect in the background
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    keep_state_and_data;
open(enter, open = PrevState, #{
        scheme := undefined,
        host := Host,
        port := Port,
        opts_map := OptsMap,
        conn_pid := undefined } = Data) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    case gun:open(Host, Port, OptsMap) of
        {ok, ConnPid} ->
            MRef = monitor(process, ConnPid),
            {keep_state, Data#{
                conn_pid => ConnPid,
                conn_m_ref => MRef
            }};
        {error, OpenError} ->
            {stop, normal, Data#{ stop_reason => {error, OpenError}}}
    end;
open({call, From}, {req, URI, ReqType}, Data) ->
    %% NB! postpone until connected
    ?LOG_DEBUG("~p({call, ~p}, ~p, POSTPONE!", [?FUNCTION_NAME, From, {req, URI, ReqType}]),
    {keep_state, Data#{ client_from => From }, [postpone]};
% open(cast, Msg, Data) ->
%     ?LOG_INFO("~p(cast, ~p, ", [?FUNCTION_NAME, Msg]),
%     {next_state, open, Data};
%% When we connect first time
open(info, {gun_up, ConnPid, Scheme}, #{
        conn_pid := ConnPid,
        scheme := _Scheme } = Data) ->
    ?LOG_DEBUG("~p -> gun_up(first time)", [?FUNCTION_NAME]),
    {next_state, connected, Data#{ scheme => Scheme }};
%% When gun reconnects in the background
open(info, {gun_up, ConnPid, Scheme}, #{
        conn_pid := undefined,
        scheme := undefined } = Data) ->
    ?LOG_DEBUG("~p -> gun_up(reconnect)", [?FUNCTION_NAME]),
    {next_state, connected, Data#{
        conn_pid => ConnPid,
        scheme => Scheme
    }};
open(info, {'DOWN', MRef, process, ConnPid, Reason}, #{
        conn_m_ref := MRef,
        client_from := From } = Data) ->
    case From of
        undefined ->
            {stop, normal, Data#{ stop_reason => {'DOWN', ConnPid}}};
        _ ->
            {stop_and_reply, normal, [{reply, From, {response, Reason}}]}
    end.
% open(info, Msg, Data) ->
%     ?LOG_INFO("~p(info, ~p, ", [?FUNCTION_NAME, Msg]),
%     {next_state, open, Data}.

connected(enter, PrevState, #{
        conn_pid := ConnPid,
        scheme := Scheme
        } = Data) when (ConnPid =/= undefined andalso
                        Scheme =/= undefined)
                       andalso
                       (PrevState =/= open orelse
                        PrevState =/= in_request) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    keep_state_and_data;
% Once Off call
connected({call, From}, {req, URI, ReqType}, #{
        conn_type := once_off = ConnType,
        conn_pid := ConnPid
        } = Data) ->
    ?LOG_DEBUG("~p({call, ~p}, ~p, ", [?FUNCTION_NAME, From, {req, URI, ReqType}]),
    {next_state, in_request, Data#{
        req_uri => URI,
        req_type => ReqType,
        client_from => From
    }};
% Long Running call
connected({call, From}, {req, URI, ReqType}, #{ conn_type := long_running } = Data) ->
    ?LOG_DEBUG("~p({call, ~p}, ~p, ", [?FUNCTION_NAME, From, {req, URI, ReqType}]),
    {next_state, in_request, Data#{
        req_uri => URI,
        req_type => ReqType,
        client_from => From
    }};
% connected(cast, Msg, Data) ->
%     ?LOG_INFO("~p(cast, ~p, ", [?FUNCTION_NAME, Msg]),
%     {next_state, connected, Data};
connected(info, {gun_down, ConnPid, Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        conn_type := once_off,
        stream_ref := StreamRef,
        client_from := undefined } = Data) ->
    ?LOG_DEBUG("~p -> gun_down ~p", [?FUNCTION_NAME, Reason]),
    case StreamRef of
        undefined ->
            ok;
        _ ->
            ok = gun:cancel(ConnPid, StreamRef)
    end,
    ok = gun:shutdown(ConnPid),
    {stop, normal, Data#{
        conn_pid => undefined,
        stream_ref := undefined,
        scheme => undefined
    }};
% where there's been requests sent out
connected(info, {gun_down, ConnPid, Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        conn_type := long_running,
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
% connected(info, {gun_up, ConnPid, Scheme},
%         #{ conn_pid := ConnPid } = Data) ->
%     ?LOG_DEBUG("~p -> gun_up", [?FUNCTION_NAME]),
%     {next_state, connected, Data#{ scheme => Scheme }};
% connected(info, Msg, Data) ->
%     ?LOG_INFO("~p(info, ~p, ", [?FUNCTION_NAME, Msg]),
%     {next_state, connected, Data}.

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
        req_type := ReqType } = Data) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    StreamRef = gun:ReqType(ConnPid, URI),
    {keep_state, Data#{ stream_ref => StreamRef }};
% in_request({call, From}, _Msg, State) ->
%     {next_state, in_request, State, [{reply, From, ok}]};
% in_request(cast, _Msg, State) ->
%     {next_state, in_request, State};

in_request(info, {gun_down, ConnPid, Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        stream_ref := StreamRef,
        client_from := From } = Data) when From =/= undefind ->
    ?LOG_DEBUG("~p -> gun_down ~p", [?FUNCTION_NAME, Reason]),
    ok = gun:cancel(ConnPid, StreamRef),
    {next_state, open, Data#{
        conn_pid => undefined,
        stream_ref := undefined,
        scheme => undefined,
        response_headers => <<>>,
        response_data => <<>>
    }};
in_request(info, {gun_response, ConnPid, StreamRef, nofin, Status, RespHeaders}, #{
        conn_pid := ConnPid } = Data) ->
    ?LOG_DEBUG("~p -> gun_response ~p", [?FUNCTION_NAME, nofin]),
    %% After {gun_response, nofin} we get the actual `{gun_data, fin/nofin}` resonse
    {keep_state, Data#{
        response_status => Status,
        response_headers => RespHeaders
    }};
% Handle once_off request response by closing/ending the connection and gen_statem
in_request(info, {gun_response, ConnPid, StreamRef, fin, Status, RespHeaders}, #{
        conn_type := once_off,
        client_from := From,
        conn_pid := ConnPid } = Data) ->
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, once_off]),
    %% response likely non 200 or error
    ReqResp = {Status, RespHeaders, _Data = <<>>},
    ok = gun:shutdown(ConnPid),
    ?LOG_DEBUG("stop once_off request", []),
    {stop_and_reply, normal, [{reply, From, {response, ReqResp}}]};
% Handle long_running request response by responding and staying open
in_request(info, {gun_response, ConnPid, StreamRef, fin, Status, RespHeaders}, #{
        conn_type := long_running,
        client_from := From,
        conn_pid := ConnPid } = Data) ->
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, long_running]),
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
in_request(info, {gun_data, ConnPid, StreamRef, fin, ResponseData}, #{
        conn_pid := ConnPid,
        client_from := From,
        response_status := Status,
        response_headers := RespHeaders,
        response_data := ExistingResponsedata } = Data) ->
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, long_running]),
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
in_request(info, {gun_data, ConnPid, StreamRef, nofin, ResponseData}, #{
        conn_pid := ConnPid,
        response_data := ExistingResponsedata } = Data) ->
    {keep_state, Data#{
        response_data => <<ExistingResponsedata/binary, ResponseData/binary>>
    }}.
% in_request(info, _Msg, State) ->
%     {next_state, in_request, State}.

terminate(_Reason, _StateName, #{
        conn_pid := _ConnPid,
        stream_ref := _StreamRef,
        client_from := _From } = _Data) ->

    %% TODO: should we cleanup ? is it not done elsewhere ?

    %% TODO: cancel andor shutdown/close connection

    void.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.





% -spec open('enter',
%          OldState :: atom(),
%          Data :: term()) ->
%             gen_statem:state_enter_result('open');
%         (gen_statem:event_type(),
%          Msg :: term(),
%          Data :: term()) ->
%             gen_statem:event_handler_result(atom()).
% open({call, From}, _Msg, State) ->
%     {next_state, open, State, [{reply, From, ok}]};
% open(cast, _Msg, State) ->
%     {next_state, open, State};
% open(info, _Msg, State) ->
%     {next_state, open, State}.
