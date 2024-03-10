-module(holster_tests).

-include_lib("eunit/include/eunit.hrl").

%% NB: list should be sorted
-define(MECK_MODS, [

]).

parse_uri_test() ->
    Url1 = <<"http://localhost">>,
    {ok, Parse1} = holster:parse_uri(Url1),
    ?assertEqual(
        #{host => "localhost",path => [],scheme => "http", port => 80},
        Parse1
    ),
    ?assertEqual(<<"http://localhost:80">>, list_to_binary(uri_string:recompose(Parse1))),
    Url2 = <<"http://localhost:8080/">>,
    {ok, Parse2} = holster:parse_uri(Url2),
    ?assertEqual(
        #{host => "localhost",path => "/",port => 8080, scheme => "http"},
        Parse2
    ),
    ?assertEqual(Url2, list_to_binary(uri_string:recompose(Parse2))),
    Url3 = <<"https://localhost:8080/">>,
    {ok, Parse3} = holster:parse_uri(Url3),
    ?assertEqual(
        #{host => "localhost",path => "/",port => 8080, scheme => "https"},
        Parse3
    ),
    ?assertEqual(Url3, list_to_binary(uri_string:recompose(Parse3))),
    Url4 = <<"http://localhost:8080?abc=test&another=bla">>,
    {ok, Parse4} = holster:parse_uri(Url4),
    ?assertEqual(
        #{host => "localhost",path => [],port => 8080, scheme => "http", query => "abc=test&another=bla"},
        Parse4
    ),
    ?assertEqual(Url4, list_to_binary(uri_string:recompose(Parse4))),
    Url5 = <<"http://user@example.com:8042/over/there?name=ferret#nose">>,
    {ok, Parse5} = holster:parse_uri(Url5),
    ?assertEqual(
        #{fragment => "nose",host => "example.com", path => "/over/there",port => 8042, query => "name=ferret",scheme => "http", userinfo => "user"},
        Parse5
    ),
    ?assertEqual(Url5, list_to_binary(uri_string:recompose(Parse5))),
    Url6 = <<"ws://mywebsockethost">>,
    {ok, Parse6} = holster:parse_uri(Url6),
    ?assertEqual(
        #{host => "mywebsockethost",path => [],port => 80, scheme => "ws"},
        Parse6
    ),
    ?assertEqual(<<"ws://mywebsockethost:80">>, list_to_binary(uri_string:recompose(Parse6))),
    Url7 = <<"ws://mywebsockethost:12345">>,
    {ok, Parse7} = holster:parse_uri(Url7),
    ?assertEqual(
        #{host => "mywebsockethost",path => [],port => 12345,scheme => "ws"},
        Parse7
    ),
    ?assertEqual(Url7, list_to_binary(uri_string:recompose(Parse7))),
    Url8 = <<"wss://mywebsockethost">>,
    {ok, Parse8} = holster:parse_uri(Url8),
    ?assertEqual(
        #{host => "mywebsockethost",path => [],port => 443,scheme => "wss"},
        Parse8
    ),
    ?assertEqual(<<"wss://mywebsockethost:443">>, list_to_binary(uri_string:recompose(Parse8))),
    Url9 = <<"wss://mywebsockethost:12345">>,
    {ok, Parse9} = holster:parse_uri(Url9),
    ?assertEqual(
        #{host => "mywebsockethost",path => [],port => 12345,scheme => "wss"},
        Parse9
    ),
    ?assertEqual(Url9, list_to_binary(uri_string:recompose(Parse9))),
    ok.

% Should be common test :()
unit_test_() ->
    {
        setup,
        fun() ->
            ets:new(test_table, [named_table, public, bag]),
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
                    {"/[...]", holster_webserver_h, []}
                ]}
            ]),
            {ok, Pid} = cowboy:start_clear(
                http,
                [{port, 8080}],
                #{
                    env => #{dispatch => Dispatch}
                }
            ),
            ListenPortInfo = io_lib:format("~s",[os:cmd("netstat -an | grep LISTEN | grep 8080")]),
            ?debugFmt("Listening port ~p\n", [ListenPortInfo]),
            ?assert(
                [[]]  =/= ListenPortInfo
            ),
            ?debugFmt("~s", [io_lib:format("~s",[os:cmd("curl -vvv localhost:8080")])]),
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
            {timeout, 5000, {"simple_proc_req", fun simple_proc_req/0}}
            % ,{timeout, 5000, {"req", fun req/0}}
            % ,{timeout, 5000, {"stay_connected_req", fun stay_connected_req/0}}
        ]
    }.

simple_proc_req() ->
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>,<<"11">>},
              {<<"date">>,_},
              {<<"server">>,<<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:simple_proc_req(get, <<"http://localhost:8080/insert?table=test_table&entry=somevalue">>)
    ).
    % ?assertMatch(
    %     {response,{200,
    %          [{<<"content-length">>,<<"11">>},
    %           {<<"date">>,_},
    %           {<<"server">>,<<"Cowboy">>}],
    %          <<"dummy reply">>}},
    %     holster:simple_proc_req(get, "http://localhost:8080/")
    % ).

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