-module(holster_tests).

-include_lib("eunit/include/eunit.hrl").

%% NB: list should be sorted
-define(MECK_MODS, [

]).

unit_test_() ->
    {
        setup,
        fun() ->
            _ = error_logger:tty(false),
            AppsStruct1 = application:ensure_all_started(cowboy),
            ?assertEqual(
                {ok,[cowlib,ranch,cowboy]},
                AppsStruct1
            ),
            AppsStruct2 = application:ensure_all_started(holster),
            ?debugFmt("AppsStruct2 ~p\n", [AppsStruct2]),
            ?assertEqual(
                {ok,[gun,jsx,holster]},
                AppsStruct2
            ),
            {ok, Apps} = AppsStruct1,
            {ok, Apps2} = AppsStruct2,
            _ = error_logger:tty(true),
            Dispatch = cowboy_router:compile([
                {'_', [
                    {"/", holster_webserver_h, []}
                ]}
            ]),
            {ok, Pid} = cowboy:start_clear(
                http,
                [{port, 80}],
                #{
                    env => #{dispatch => Dispatch}
                }
            ),
            ListenPortInfo = io_lib:format("~s",[os:cmd("netstat -an | grep LISTEN | grep 80")]),
            ?debugFmt("Listening port ~p\n", [ListenPortInfo]),
            ?assert(
                [[]]  =/= ListenPortInfo
            ),

            ?debugFmt("~s", [io_lib:format("~s",[os:cmd("curl localhost")])]),

            true = erlang:link(Pid),

            % {ok, _} = dbg:tracer(),
            % {ok, _} = dbg:p(all, call),
            % {ok, _} = dbg:tpl(holster, cx),
            % {ok, _} = dbg:tpl(holster_request, cx),
            % {ok, _} = dbg:tpl(gun, cx),

            {{ok, Pid}, {ok, Apps ++ Apps2}}
        end,
        fun({{ok, Pid}, {ok, Apps}}) ->
            true = erlang:unlink(Pid),
            true = erlang:exit(Pid, shutdown),
            _ = error_logger:tty(false),
            [ ok = application:stop(App) || App <- Apps ],
            _ = error_logger:tty(true),
            ok = dbg:stop_clear(),
            ?assertEqual(
                ?MECK_MODS,
                lists:sort(meck:unload())
            )
        end,
        [
            {timeout, 50000000, {"simple_proc_req", fun simple_proc_req/0}},
            {timeout, 50000000, {"req", fun req/0}},
            {timeout, 50000000, {"stay_connected_req", fun stay_connected_req/0}}
        ]
    }.

simple_proc_req() ->
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:simple_proc_req(get, <<"http://localhost:8080/">>)
    ),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:simple_proc_req(get, "http://localhost:8080/")
    ).

req() ->
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:req(get, <<"http://localhost:8080/">>)
    ),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:req(get, "http://localhost:8080/")
    ).

stay_connected_req() ->
    {{ok, Pid}, Response} = holster:stay_connected_req(get, "http://localhost:8080/"),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    {{ok, Pid}, Response} = holster:another_request(get, "http://localhost:8080/", Pid),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    {{ok, Pid}, Response} = holster:another_request(get, "http://localhost:8080/", Pid),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ).