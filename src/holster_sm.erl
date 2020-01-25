-module(holster_sm).

-include_lib("kernel/include/logger.hrl").
-include("holster.hrl").

%% API
-export([
    start_link/6,
    req/5,
    close/1
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

% -spec req(pid(), holster:req_type(), http_uri:uri()) -> {response, term()}.
% req(Pid, ReqType, URI) ->
%     req(Pid, ReqType, URI, [], #{}).

% -spec req(pid(), holster:req_type(), http_uri:uri(), gun:req_headers()) -> {response, term()}.
% req(Pid, ReqType, URI, Headers) ->
%     req(Pid, ReqType, URI, Headers, #{}).

-spec req(pid(), holster:req_type(), http_uri:uri(), gun:req_headers(), gun:req_opts()) -> {response, term()}.
req(Pid, ReqType, URI, Headers, ReqOpts) when is_map(ReqOpts) ->
    gen_statem:call(Pid, {req, ReqType, URI, Headers, ReqOpts}).

close(Pid) ->
    gen_statem:stop(Pid, client_closed, 5000).

%% ============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

-spec init(Args :: term()) -> gen_statem:init_result(term()).
init({Host, Proto, Port, ConnectOpts, Timeout, ConnType}) ->
    process_flag(trap_exit, true),
    % {ok, _} = dbg:tracer(),
    % {ok, _} = dbg:p(all, call),
    % {ok, _} = dbg:tpl(gun, cx),
    {ok, open, #{
        scheme => undefined,
        host => Host,
        req_uri => undefined,
        req_type => undefined,
        req_headers => [],
        req_opts => #{},
        proto => Proto,
        port => Port,
        connect_opts => ConnectOpts,
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

open(enter, connected = PrevState, _Data) ->
    %% For some reason GUN considers it nice to try and reconnect in the background
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    keep_state_and_data;
open(enter, open = PrevState, #{
        scheme := undefined,
        host := Host,
        port := Port,
        connect_opts := ConnectOpts,
        conn_pid := undefined } = Data) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
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
open({call, From}, {req, ReqType, URI, Headers, ReqOpts}, Data) ->
    %% NB! postpone until connected
    ?LOG_DEBUG("~p({call, ~p}, ~p, POSTPONE!", [?FUNCTION_NAME, From, {req, ReqType, URI, Headers, ReqOpts}]),
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
        scheme := Scheme } = _Data)
        when (ConnPid =/= undefined andalso Scheme =/= undefined) andalso
             (PrevState =/= open orelse PrevState =/= in_request) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    keep_state_and_data;
connected({call, From},
          {req, ReqType, URI, Headers, ReqOpts},
          #{ conn_type := ConnType } = Data) ->
    ?LOG_DEBUG("~p({call, ~p}, ~p ~p, ",
        [?FUNCTION_NAME, From, ConnType, {req, ReqType, URI, Headers, ReqOpts}]),
    {next_state, in_request, Data#{
        req_uri => URI,
        req_type => ReqType,
        req_headers => Headers,
        req_opts => ReqOpts,
        client_from => From
    }};
% connected(cast, Msg, Data) ->
%     ?LOG_INFO("~p(cast, ~p, ", [?FUNCTION_NAME, Msg]),
%     {next_state, connected, Data};
connected(info, {gun_down, ConnPid, _Scheme, Reason, _}, #{
        conn_pid := ConnPid,
        conn_type := once,
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
        req_type := ReqType,
        req_headers := Headers,
        req_opts := ReqOpts} = Data) ->
    ?LOG_DEBUG("~p -> ENTER PrevState ~p", [?FUNCTION_NAME, PrevState]),
    StreamRef = gun:ReqType(ConnPid, URI, Headers, ReqOpts),
    {keep_state, Data#{ stream_ref => StreamRef }};
% in_request({call, From}, _Msg, State) ->
%     {next_state, in_request, State, [{reply, From, ok}]};
% in_request(cast, _Msg, State) ->
%     {next_state, in_request, State};
in_request(info, {gun_down, ConnPid, _Scheme, Reason, _}, #{
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
in_request(info, {gun_response, ConnPid, _StreamRef, nofin, Status, RespHeaders}, #{
        conn_pid := ConnPid } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    ?LOG_DEBUG("~p -> gun_response ~p", [?FUNCTION_NAME, nofin]),
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
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, once]),
    %% response likely non 200 or error
    ReqResp = {Status, RespHeaders, _ResponseData = <<>>},
    ok = gun:shutdown(ConnPid),
    ?LOG_DEBUG("stop once request", []),
    {stop_and_reply, normal, [{reply, From, {response, ReqResp}}]};
% Handle stay_connected request response by responding and staying open
in_request(info, {gun_response, ConnPid, _StreamRef, fin, Status, RespHeaders}, #{
        conn_type := stay_connected,
        client_from := From,
        conn_pid := ConnPid } = Data) ->
    %% TODO: should we re-set StreamRef or check it ??
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, stay_connected]),
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
    ?LOG_DEBUG("~p -> gun_response ~p Type ~p", [?FUNCTION_NAME, fin, stay_connected]),
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












%% failures
% 10> holster:req(get, "http://www.google.com/").
% {response,{200,
%            [{<<"date">>,<<"Sat, 25 Jan 2020 19:07:15 GMT">>},
%             {<<"expires">>,<<"-1">>},
%             {<<"cache-control">>,<<"private, max-age=0">>},
%             {<<"content-type">>,<<"text/html; charset=ISO-8859-1">>},
%             {<<"p3p">>,
%              <<"CP=\"This is not a P3P policy! See g.co/p3phelp for more info.\"">>},
%             {<<"server">>,<<"gws">>},
%             {<<"x-xss-protection">>,<<"0">>},
%             {<<"x-frame-options">>,<<"SAMEORIGIN">>},
%             {<<"set-cookie">>,
%              <<"1P_JAR=2020-01-25-19; expires=Mon, 24-Feb-2020 19:07:15 "...>>},
%             {<<"set-cookie">>,
%              <<"NID=196=DPnv5eAHRW2OfX58KBBcmI6ngemm34FYo7eahquHOxY2"...>>},
%             {<<"accept-ranges">>,<<"none">>},
%             {<<"vary">>,<<"Accept-Encoding">>},
%             {<<"transfer-encoding">>,<<"chunked">>}],
%            <<"<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org/WebPage\" lang=\"en-GB\"><head><meta "...>>}}
% 11> holster:req(head, "http://www.google.com/").
% {response,{200,
%            [{<<"date">>,<<"Sat, 25 Jan 2020 19:07:21 GMT">>},
%             {<<"expires">>,<<"-1">>},
%             {<<"cache-control">>,<<"private, max-age=0">>},
%             {<<"content-type">>,<<"text/html; charset=ISO-8859-1">>},
%             {<<"p3p">>,
%              <<"CP=\"This is not a P3P policy! See g.co/p3phelp for more info.\"">>},
%             {<<"server">>,<<"gws">>},
%             {<<"x-xss-protection">>,<<"0">>},
%             {<<"x-frame-options">>,<<"SAMEORIGIN">>},
%             {<<"set-cookie">>,
%              <<"1P_JAR=2020-01-25-19; expires=Mon, 24-Feb-2020 19:07:21 "...>>},
%             {<<"set-cookie">>,
%              <<"NID=196=WA1GCig1FEK4Zdvr_LGf6J8stpavs1QS8NKEK7WfRMn5"...>>},
%             {<<"transfer-encoding">>,<<"chunked">>},
%             {<<"accept-ranges">>,<<"none">>},
%             {<<"vary">>,<<"Accept-Encoding">>}],
%            <<>>}}
% 12> holster:req(options, "http://www.google.com/").
% {response,{405,
%            [{<<"allow">>,<<"GET, HEAD">>},
%             {<<"date">>,<<"Sat, 25 Jan 2020 19:07:24 GMT">>},
%             {<<"content-type">>,<<"text/html; charset=UTF-8">>},
%             {<<"server">>,<<"gws">>},
%             {<<"content-length">>,<<"1592">>},
%             {<<"x-xss-protection">>,<<"0">>},
%             {<<"x-frame-options">>,<<"SAMEORIGIN">>}],
%            <<"<!DOCTYPE html>\n<html lang=en>\n  <meta charset=utf-8>\n  <meta name=viewport content=\"initial-sca"...>>}}
% 13> holster:req(patch, "http://www.google.com/").
% 2020-01-25T19:07:28.328343+00:00 error: ** State machine <0.209.0> terminating, ** Last event = {{call,{<0.198.0>,#Ref<0.3536720746.4088397827.39810>}},{req,patch,"/",[],#{}}}, ** When server state  = {connected,#{client_from => {<0.198.0>,#Ref<0.3536720746.4088397827.39810>},conn_m_ref => #Ref<0.3536720746.4088397832.41281>,conn_pid => <0.210.0>,conn_type => once,connect_opts => #{},host => "www.google.com",port => 80,proto => http,req_type => undefined,req_uri => undefined,response_data => <<>>,response_headers => undefined,response_status => undefined,scheme => http,stop_reason => undefined,stream_ref => undefined,timeout => undefined}}, ** Reason for termination = error:{'function not exported',{gun,patch,2}}, ** Callback mode = [state_functions,state_enter], ** Stacktrace =, **  [{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]
% 2020-01-25T19:07:28.329848+00:00 error: crasher: initial call: holster_sm:init/1, pid: <0.209.0>, registered_name: [], error: {undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [<0.198.0>], message_queue_len: 0, messages: [], links: [<0.198.0>], dictionary: [], trap_exit: true, status: running, heap_size: 6772, stack_size: 27, reductions: 10086; neighbours: neighbour: pid: <0.198.0>, registered_name: [], initial_call: {erlang,apply,2}, current_function: {gen,do_call,4}, ancestors: [], message_queue_len: 0, links: [<0.200.0>,<0.206.0>,<0.209.0>,<0.149.0>], trap_exit: false, status: waiting, heap_size: 4185, stack_size: 37, reductions: 20206, current_stacktrace: [{gen,do_call,4,[{file,"gen.erl"},{line,169}]},{gen_statem,call_dirty,4,[{file,"gen_statem.erl"},{line,592}]},{holster,do_req,7,[{file,"/home/rp/code/holster/src/holster.erl"},{line,144}]},{holster,req,2,[{file,"/home/rp/code/holster/src/holster.erl"},{line,72}]},{erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},{shell,exprs,7,[{file,"shell.erl"},{line,686}]},{shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},{shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]
% ** exception exit: undef
%      in function  gun:patch/2
%         called as gun:patch(<0.210.0>,"/")
%      in call from holster_sm:in_request/3 (/home/rp/code/holster/src/holster_sm.erl, line 233)
%      in call from gen_statem:call_state_function/5 (gen_statem.erl, line 1586)
%      in call from gen_statem:loop_event_state_enter/8 (gen_statem.erl, line 982)
%      in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 249)
% 14> 2020-01-25T19:07:28.332398+00:00 error: ** State machine <0.206.0> terminating, ** Last event = {'EXIT',<0.198.0>,{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}}, ** When server state  = {connected,#{client_from => undefined,conn_m_ref => #Ref<0.3536720746.4088397832.41271>,conn_pid => <0.207.0>,conn_type => once,connect_opts => #{},host => "www.google.com",port => 80,proto => http,req_type => undefined,req_uri => undefined,response_data => <<>>,response_headers => <<>>,response_status => undefined,scheme => http,stop_reason => undefined,stream_ref => undefined,timeout => undefined}}, ** Reason for termination = exit:{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ** Callback mode = [state_functions,state_enter], ** Stacktrace =, **  [{gen_statem,loop_receive,3,[{file,"gen_statem.erl"},{line,888}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]
% 2020-01-25T19:07:28.331949+00:00 error: ** State machine <0.200.0> terminating, ** Last event = {'EXIT',<0.198.0>,{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}}, ** When server state  = {connected,#{client_from => undefined,conn_m_ref => #Ref<0.3536720746.4088397827.39788>,conn_pid => <0.201.0>,conn_type => once,connect_opts => #{},host => "www.google.com",port => 80,proto => http,req_type => undefined,req_uri => undefined,response_data => <<>>,response_headers => <<>>,response_status => undefined,scheme => http,stop_reason => undefined,stream_ref => undefined,timeout => undefined}}, ** Reason for termination = exit:{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ** Callback mode = [state_functions,state_enter], ** Stacktrace =, **  [{gen_statem,loop_receive,3,[{file,"gen_statem.erl"},{line,888}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]
% 2020-01-25T19:07:28.334280+00:00 error: crasher: initial call: holster_sm:init/1, pid: <0.206.0>, registered_name: [], exit: {{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]},[{gen_statem,loop_receive,3,[{file,"gen_statem.erl"},{line,888}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [<0.198.0>], message_queue_len: 0, messages: [], links: [], dictionary: [], trap_exit: true, status: running, heap_size: 10958, stack_size: 27, reductions: 14296; neighbours:
% 2020-01-25T19:07:28.334672+00:00 error: crasher: initial call: holster_sm:init/1, pid: <0.200.0>, registered_name: [], exit: {{undef,[{gun,patch,[<0.210.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]},[{gen_statem,loop_receive,3,[{file,"gen_statem.erl"},{line,888}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [<0.198.0>], message_queue_len: 0, messages: [], links: [], dictionary: [], trap_exit: true, status: running, heap_size: 10958, stack_size: 27, reductions: 15033; neighbours:

% 14> holster:req(post, "http://www.google.com/").
% 2020-01-25T19:07:34.882395+00:00 error: ** State machine <0.213.0> terminating, ** Last event = {{call,{<0.211.0>,#Ref<0.3536720746.4088397827.39833>}},{req,post,"/",[],#{}}}, ** When server state  = {connected,#{client_from => {<0.211.0>,#Ref<0.3536720746.4088397827.39833>},conn_m_ref => #Ref<0.3536720746.4088397832.41297>,conn_pid => <0.214.0>,conn_type => once,connect_opts => #{},host => "www.google.com",port => 80,proto => http,req_type => undefined,req_uri => undefined,response_data => <<>>,response_headers => undefined,response_status => undefined,scheme => http,stop_reason => undefined,stream_ref => undefined,timeout => undefined}}, ** Reason for termination = error:{'function not exported',{gun,post,2}}, ** Callback mode = [state_functions,state_enter], ** Stacktrace =, **  [{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]
% 2020-01-25T19:07:34.883904+00:00 error: crasher: initial call: holster_sm:init/1, pid: <0.213.0>, registered_name: [], error: {undef,[{gun,post,[<0.214.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [<0.211.0>], message_queue_len: 0, messages: [], links: [<0.211.0>], dictionary: [], trap_exit: true, status: running, heap_size: 6772, stack_size: 27, reductions: 10071; neighbours: neighbour: pid: <0.211.0>, registered_name: [], initial_call: {erlang,apply,2}, current_function: {gen,do_call,4}, ancestors: [], message_queue_len: 0, links: [<0.149.0>,<0.213.0>], trap_exit: false, status: waiting, heap_size: 610, stack_size: 37, reductions: 1737, current_stacktrace: [{gen,do_call,4,[{file,"gen.erl"},{line,169}]},{gen_statem,call_dirty,4,[{file,"gen_statem.erl"},{line,592}]},{holster,do_req,7,[{file,"/home/rp/code/holster/src/holster.erl"},{line,144}]},{holster,req,2,[{file,"/home/rp/code/holster/src/holster.erl"},{line,72}]},{erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},{shell,exprs,7,[{file,"shell.erl"},{line,686}]},{shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},{shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]
% ** exception exit: undef
%      in function  gun:post/2
%         called as gun:post(<0.214.0>,"/")
%      in call from holster_sm:in_request/3 (/home/rp/code/holster/src/holster_sm.erl, line 233)
%      in call from gen_statem:call_state_function/5 (gen_statem.erl, line 1586)
%      in call from gen_statem:loop_event_state_enter/8 (gen_statem.erl, line 982)
%      in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 249)
% 15>
% 15>
% 15> holster:req(put, "http://www.google.com/").
% 2020-01-25T19:07:38.451282+00:00 error: ** State machine <0.217.0> terminating, ** Last event = {{call,{<0.215.0>,#Ref<0.3536720746.4088397827.39851>}},{req,put,"/",[],#{}}}, ** When server state  = {connected,#{client_from => {<0.215.0>,#Ref<0.3536720746.4088397827.39851>},conn_m_ref => #Ref<0.3536720746.4088397827.39854>,conn_pid => <0.218.0>,conn_type => once,connect_opts => #{},host => "www.google.com",port => 80,proto => http,req_type => undefined,req_uri => undefined,response_data => <<>>,response_headers => undefined,response_status => undefined,scheme => http,stop_reason => undefined,stream_ref => undefined,timeout => undefined}}, ** Reason for termination = error:{'function not exported',{gun,put,2}}, ** Callback mode = [state_functions,state_enter], ** Stacktrace =, **  [{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]
% 2020-01-25T19:07:38.453145+00:00 error: crasher: initial call: holster_sm:init/1, pid: <0.217.0>, registered_name: [], error: {undef,[{gun,put,[<0.218.0>,"/"],[]},{holster_sm,in_request,3,[{file,"/home/rp/code/holster/src/holster_sm.erl"},{line,233}]},{gen_statem,call_state_function,5,[{file,"gen_statem.erl"},{line,1586}]},{gen_statem,loop_event_state_enter,8,[{file,"gen_statem.erl"},{line,982}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [<0.215.0>], message_queue_len: 0, messages: [], links: [<0.215.0>], dictionary: [], trap_exit: true, status: running, heap_size: 6772, stack_size: 27, reductions: 10035; neighbours: neighbour: pid: <0.215.0>, registered_name: [], initial_call: {erlang,apply,2}, current_function: {gen,do_call,4}, ancestors: [], message_queue_len: 0, links: [<0.149.0>,<0.217.0>], trap_exit: false, status: waiting, heap_size: 610, stack_size: 37, reductions: 1737, current_stacktrace: [{gen,do_call,4,[{file,"gen.erl"},{line,169}]},{gen_statem,call_dirty,4,[{file,"gen_statem.erl"},{line,592}]},{holster,do_req,7,[{file,"/home/rp/code/holster/src/holster.erl"},{line,144}]},{holster,req,2,[{file,"/home/rp/code/holster/src/holster.erl"},{line,72}]},{erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},{shell,exprs,7,[{file,"shell.erl"},{line,686}]},{shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},{shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]
% ** exception exit: undef
%      in function  gun:put/2
%         called as gun:put(<0.218.0>,"/")
%      in call from holster_sm:in_request/3 (/home/rp/code/holster/src/holster_sm.erl, line 233)
%      in call from gen_statem:call_state_function/5 (gen_statem.erl, line 1586)
%      in call from gen_statem:loop_event_state_enter/8 (gen_statem.erl, line 982)
%      in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 249)
% 16>
