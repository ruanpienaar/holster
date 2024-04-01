-module(holster_tests).

-include_lib("eunit/include/eunit.hrl").

%% NB: list should be sorted
-define(MECK_MODS, [

]).

base64_encoded_user_info_header_field_test() ->
    ?assertEqual(
        [{<<"authorization">>, <<"Basic QWxsYWRpbjpvcGVuX3Nlc2FtZQ==">>}],
        holster:base64_encoded_user_info_header_field("Alladin:open_sesame")
    ),
    ?assertEqual(
        [{<<"authorization">>, <<"Basic QWxsYWRpbjpvcGVuX3Nlc2FtZQ==">>}],
        holster:base64_encoded_user_info_header_field(<<"Alladin:open_sesame">>)
    ),
    ?assertEqual(
        [],
        holster:base64_encoded_user_info_header_field(#{})
    ),
    ?assertEqual(
        [{<<"authorization">>, <<"Basic QWxsYWRpbjpvcGVuX3Nlc2FtZQ==">>}],
        holster:base64_encoded_user_info_header_field(#{userinfo => "Alladin:open_sesame"})
    ),
    ?assertEqual(
        [{<<"authorization">>, <<"Basic QWxsYWRpbjpvcGVuX3Nlc2FtZQ==">>}],
        holster:base64_encoded_user_info_header_field(
            element(
                2,
                holster:parse_uri(
                    "http://Alladin:open_sesame@localhost:8000/auth-endpoint"
                )
            )
        )
    ).

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
    ?assertEqual(Url9, list_to_binary(uri_string:recompose(Parse9))).

http_unit_test_() ->
    {
        setup,
        fun() ->
            _ = ets:new(test_table, [named_table, public, bag]),
            _ = error_logger:tty(false),
            AppsStruct1 = application:ensure_all_started(cowboy),
            ?assertEqual(
                {ok,[cowlib,ranch,cowboy]},
                AppsStruct1
            ),
            AppsStruct2 = application:ensure_all_started(holster),
            ?assertEqual(
                {ok,[gun,jsx,holster]},
                AppsStruct2
            ),
            {ok, Apps} = AppsStruct1,
            {ok, Apps2} = AppsStruct2,
            _ = error_logger:tty(true),
            Dispatch = cowboy_router:compile([
                {'_', [
                    {"/auth-endpoint", holster_basic_auth_webserver, []},
                    {"/[...]", holster_webserver_h, []}
                ]}
            ]),
            {ok, Pid} = cowboy:start_clear(
                http,
                _TransOpts = [{port, 8080}],
                _ProtoOpts=#{
                    env => #{dispatch => Dispatch}
                }
            ),
            NetstatCmd = "netstat -an | grep LISTEN | grep 8080",
            ListenPortInfo = io_lib:format("~s",[os:cmd(NetstatCmd)]),
            ?debugFmt("CMD: ~p\n~s", [NetstatCmd, ListenPortInfo]),
            ?assert(
                [[]]  =/= ListenPortInfo
            ),
            CurlCmd = "curl -vvv localhost:8080",
            ?debugFmt("CMD: ~p\n~s", [CurlCmd, io_lib:format("~s",[os:cmd(CurlCmd)])]),
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
            {
                foreach,
                fun() ->
                    ok
                end,
                fun(_) ->
                    true = ets:delete_all_objects(test_table)
                end,
                [
                    {timeout, 5000, {"simple_proc_req", fun simple_proc_req/0}}
                    ,{timeout, 5000, {"req", fun req/0}}
                    ,{timeout, 5000, {"stay_connected_req", fun stay_connected_req/0}}
                    ,{timeout, 5000, {"stay_connected_req_many_req", fun stay_connected_req_many_req/0}}
                    ,{timeout, 5000, {"test user auth", fun simple_proc_req_user_auth/0}}
                ]
            }
        ]
    }.

simple_proc_req() ->
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:simple_proc_req(get, <<"http://localhost:8080/insert?table=test_table&entry=somevalue">>)
    ),
    ?assertEqual(
        [{<<"somevalue">>}],
        ets:tab2list(test_table)
    ),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:simple_proc_req(get, "http://localhost:8080/insert?table=test_table&entry=somevalue2")
    ),
    ?assertEqual(
        [{<<"somevalue">>},{<<"somevalue2">>}],
        ets:tab2list(test_table)
    ).

req() ->
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:req(get, <<"http://localhost:8080/insert?table=test_table&entry=somevalue3">>)
    ),
    ?assertEqual(
        [{<<"somevalue3">>}],
        ets:tab2list(test_table)
    ),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        holster:req(get, "http://localhost:8080/insert?table=test_table&entry=somevalue4")
    ),
    ?assertEqual(
        [{<<"somevalue3">>}, {<<"somevalue4">>}],
        ets:tab2list(test_table)
    ).

stay_connected_req() ->
    {{ok, Pid}, Response} = holster:stay_connected_req(get, "http://localhost:8080/insert?table=test_table&entry=somevalue5"),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    ?assertEqual(
        [{<<"somevalue5">>}],
        ets:tab2list(test_table)
    ),
    {{ok, Pid}, Response} = holster:another_request(get, "http://localhost:8080/insert?table=test_table&entry=somevalue6", Pid),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    ?assertEqual(
        [{<<"somevalue6">>}, {<<"somevalue5">>}],
        ets:tab2list(test_table)
    ),
    {{ok, Pid}, Response} = holster:another_request(get, "http://localhost:8080/insert?table=test_table&entry=somevalue7", Pid),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    ?assertEqual(
        [{<<"somevalue7">>}, {<<"somevalue6">>}, {<<"somevalue5">>}],
        ets:tab2list(test_table)
    ).

stay_connected_req_many_req() ->
    {{ok, Pid}, Response} = holster:stay_connected_req(
        get,
        "http://localhost:8080/insert?table=test_table&entry=1"
    ),
    ?assertMatch(
        {response,{200,
             [{<<"content-length">>, <<"11">>},
              {<<"date">>, _},
              {<<"server">>, <<"Cowboy">>}],
             <<"dummy reply">>}},
        Response
    ),
    ?debugFmt("Pid ~p\n", [Pid]),
    lists:foreach(
        fun(X) ->
            %% Check response PID matches
            {{ok, Pid}, Response2} = 
                holster:another_request(
                    get,
                    "http://localhost:8080/insert?table=test_table&entry="++integer_to_list(X),
                    Pid
                ),
            {response, {HttpResponseCode, Proplist, Reply}} = Response2,
            ?assertMatch(
                {
                    {ok, Pid},
                    {
                        response,
                        {
                            200,
                            %% Map here, to allow for selective matching ( Proplist does not allow selective matching )
                            #{
                                <<"content-length">> := <<"11">>,
                                <<"date">> := _,
                                <<"server">> := <<"Cowboy">>
                            },
                            <<"dummy reply">>
                        }
                    }
                },
                %% Turning headers into map, to deal with server closed: {<<"connection">>,<<"close">>}. 
                %% happens at times.
                {{ok, Pid}, {response, {HttpResponseCode, maps:from_list(Proplist), Reply}}}
            )
        end,
        lists:seq(1, 1000)
    ),
    All = lists:sort(ets:tab2list(test_table)),
    ?assertEqual(
        1000,
        length(All)
    ),
    ?assertEqual(
        [
            {<<"1">>}, {<<"10">>}, {<<"100">>}, {<<"1000">>}, {<<"101">>}, {<<"102">>}, {<<"103">>}, {<<"104">>}, {<<"105">>}, {<<"106">>}, {<<"107">>}, {<<"108">>}, {<<"109">>}, {<<"11">>}, {<<"110">>}, {<<"111">>}, {<<"112">>}, {<<"113">>}, {<<"114">>}, {<<"115">>}, {<<"116">>}, {<<"117">>}, {<<"118">>}, {<<"119">>}, {<<"12">>}, {<<"120">>}, {<<"121">>}, {<<"122">>}, {<<"123">>}, {<<"124">>}, {<<"125">>}, {<<"126">>}, {<<"127">>}, {<<"128">>}, {<<"129">>}, {<<"13">>}, {<<"130">>}, {<<"131">>}, {<<"132">>}, {<<"133">>}, {<<"134">>}, {<<"135">>}, {<<"136">>}, {<<"137">>}, {<<"138">>}, {<<"139">>}, {<<"14">>}, {<<"140">>}, {<<"141">>}, {<<"142">>}, {<<"143">>}, {<<"144">>}, {<<"145">>}, {<<"146">>}, {<<"147">>}, {<<"148">>}, {<<"149">>}, {<<"15">>}, {<<"150">>}, {<<"151">>}, {<<"152">>}, {<<"153">>}, {<<"154">>}, {<<"155">>}, {<<"156">>}, {<<"157">>}, {<<"158">>}, {<<"159">>}, {<<"16">>}, {<<"160">>}, {<<"161">>}, {<<"162">>}, {<<"163">>}, {<<"164">>}, {<<"165">>}, {<<"166">>}, {<<"167">>}, {<<"168">>}, {<<"169">>}, {<<"17">>}, {<<"170">>}, {<<"171">>}, {<<"172">>}, {<<"173">>}, {<<"174">>}, {<<"175">>}, {<<"176">>}, {<<"177">>}, {<<"178">>}, {<<"179">>}, {<<"18">>}, {<<"180">>}, {<<"181">>}, {<<"182">>}, {<<"183">>}, {<<"184">>}, {<<"185">>}, {<<"186">>}, {<<"187">>}, {<<"188">>}, {<<"189">>}, {<<"19">>}, {<<"190">>}, {<<"191">>}, {<<"192">>}, {<<"193">>}, {<<"194">>}, {<<"195">>}, {<<"196">>}, {<<"197">>}, {<<"198">>}, {<<"199">>}, {<<"2">>}, {<<"20">>}, {<<"200">>}, {<<"201">>}, {<<"202">>}, {<<"203">>}, {<<"204">>}, {<<"205">>}, {<<"206">>}, {<<"207">>}, {<<"208">>}, {<<"209">>}, {<<"21">>}, {<<"210">>}, {<<"211">>}, {<<"212">>}, {<<"213">>}, {<<"214">>}, {<<"215">>}, {<<"216">>}, {<<"217">>}, {<<"218">>}, {<<"219">>}, {<<"22">>}, {<<"220">>}, {<<"221">>}, {<<"222">>}, {<<"223">>}, {<<"224">>}, {<<"225">>}, {<<"226">>}, {<<"227">>}, {<<"228">>}, {<<"229">>}, {<<"23">>}, {<<"230">>}, {<<"231">>}, {<<"232">>}, {<<"233">>}, {<<"234">>}, {<<"235">>}, {<<"236">>}, {<<"237">>}, {<<"238">>}, {<<"239">>}, {<<"24">>}, {<<"240">>}, {<<"241">>}, {<<"242">>}, {<<"243">>}, {<<"244">>}, {<<"245">>}, {<<"246">>}, {<<"247">>}, {<<"248">>}, {<<"249">>}, {<<"25">>},
            {<<"250">>}, {<<"251">>}, {<<"252">>}, {<<"253">>}, {<<"254">>}, {<<"255">>}, {<<"256">>}, {<<"257">>}, {<<"258">>}, {<<"259">>}, {<<"26">>}, {<<"260">>}, {<<"261">>}, {<<"262">>}, {<<"263">>}, {<<"264">>}, {<<"265">>}, {<<"266">>}, {<<"267">>}, {<<"268">>}, {<<"269">>}, {<<"27">>}, {<<"270">>}, {<<"271">>}, {<<"272">>}, {<<"273">>}, {<<"274">>}, {<<"275">>}, {<<"276">>}, {<<"277">>}, {<<"278">>}, {<<"279">>}, {<<"28">>}, {<<"280">>}, {<<"281">>}, {<<"282">>}, {<<"283">>}, {<<"284">>}, {<<"285">>}, {<<"286">>}, {<<"287">>}, {<<"288">>}, {<<"289">>}, {<<"29">>}, {<<"290">>}, {<<"291">>}, {<<"292">>}, {<<"293">>}, {<<"294">>}, {<<"295">>}, {<<"296">>}, {<<"297">>}, {<<"298">>}, {<<"299">>}, {<<"3">>}, {<<"30">>}, {<<"300">>}, {<<"301">>}, {<<"302">>}, {<<"303">>}, {<<"304">>}, {<<"305">>}, {<<"306">>}, {<<"307">>}, {<<"308">>}, {<<"309">>}, {<<"31">>}, {<<"310">>}, {<<"311">>}, {<<"312">>}, {<<"313">>}, {<<"314">>}, {<<"315">>}, {<<"316">>}, {<<"317">>}, {<<"318">>}, {<<"319">>}, {<<"32">>}, {<<"320">>}, {<<"321">>}, {<<"322">>}, {<<"323">>}, {<<"324">>}, {<<"325">>}, {<<"326">>}, {<<"327">>}, {<<"328">>}, {<<"329">>}, {<<"33">>}, {<<"330">>}, {<<"331">>}, {<<"332">>}, {<<"333">>}, {<<"334">>}, {<<"335">>}, {<<"336">>}, {<<"337">>}, {<<"338">>}, {<<"339">>}, {<<"34">>}, {<<"340">>}, {<<"341">>}, {<<"342">>}, {<<"343">>}, {<<"344">>}, {<<"345">>}, {<<"346">>}, {<<"347">>}, {<<"348">>}, {<<"349">>}, {<<"35">>}, {<<"350">>}, {<<"351">>}, {<<"352">>}, {<<"353">>}, {<<"354">>}, {<<"355">>}, {<<"356">>}, {<<"357">>}, {<<"358">>}, {<<"359">>}, {<<"36">>}, {<<"360">>}, {<<"361">>}, {<<"362">>}, {<<"363">>}, {<<"364">>}, {<<"365">>}, {<<"366">>}, {<<"367">>}, {<<"368">>}, {<<"369">>}, {<<"37">>}, {<<"370">>}, {<<"371">>}, {<<"372">>}, {<<"373">>}, {<<"374">>}, {<<"375">>}, {<<"376">>}, {<<"377">>}, {<<"378">>}, {<<"379">>}, {<<"38">>}, {<<"380">>}, {<<"381">>}, {<<"382">>}, {<<"383">>}, {<<"384">>}, {<<"385">>}, {<<"386">>}, {<<"387">>}, {<<"388">>}, {<<"389">>}, {<<"39">>}, {<<"390">>}, {<<"391">>}, {<<"392">>}, {<<"393">>}, {<<"394">>}, {<<"395">>}, {<<"396">>}, {<<"397">>}, {<<"398">>}, {<<"399">>}, {<<"4">>}, {<<"40">>}, {<<"400">>}, {<<"401">>}, {<<"402">>}, {<<"403">>}, {<<"404">>}, {<<"405">>}, {<<"406">>}, {<<"407">>}, {<<"408">>}, {<<"409">>}, {<<"41">>}, {<<"410">>}, {<<"411">>}, {<<"412">>}, {<<"413">>}, {<<"414">>}, {<<"415">>}, {<<"416">>}, {<<"417">>}, {<<"418">>}, {<<"419">>}, {<<"42">>}, {<<"420">>}, {<<"421">>}, {<<"422">>}, {<<"423">>}, {<<"424">>}, {<<"425">>}, {<<"426">>}, {<<"427">>}, {<<"428">>}, {<<"429">>}, {<<"43">>}, {<<"430">>}, {<<"431">>}, {<<"432">>}, {<<"433">>}, {<<"434">>}, {<<"435">>}, {<<"436">>}, {<<"437">>}, {<<"438">>}, {<<"439">>}, {<<"44">>}, {<<"440">>}, {<<"441">>}, {<<"442">>}, {<<"443">>}, {<<"444">>}, {<<"445">>}, {<<"446">>}, {<<"447">>}, {<<"448">>}, {<<"449">>}, {<<"45">>}, {<<"450">>}, {<<"451">>}, {<<"452">>}, {<<"453">>}, {<<"454">>}, {<<"455">>}, {<<"456">>}, {<<"457">>}, {<<"458">>}, {<<"459">>}, {<<"46">>}, {<<"460">>}, {<<"461">>}, {<<"462">>}, {<<"463">>}, {<<"464">>}, {<<"465">>}, {<<"466">>}, {<<"467">>}, {<<"468">>}, {<<"469">>}, {<<"47">>}, {<<"470">>}, {<<"471">>}, {<<"472">>}, {<<"473">>}, {<<"474">>}, {<<"475">>}, {<<"476">>}, {<<"477">>}, {<<"478">>}, {<<"479">>}, {<<"48">>}, {<<"480">>}, {<<"481">>}, {<<"482">>}, {<<"483">>}, {<<"484">>}, {<<"485">>}, {<<"486">>}, {<<"487">>}, {<<"488">>}, {<<"489">>}, {<<"49">>}, {<<"490">>}, {<<"491">>}, {<<"492">>}, {<<"493">>}, {<<"494">>}, {<<"495">>},
            {<<"496">>},
            {<<"497">>},{<<"498">>},{<<"499">>},{<<"5">>},{<<"50">>},{<<"500">>},{<<"501">>},{<<"502">>},{<<"503">>},{<<"504">>},{<<"505">>},{<<"506">>},{<<"507">>},{<<"508">>},{<<"509">>},{<<"51">>},{<<"510">>},{<<"511">>},{<<"512">>},{<<"513">>},{<<"514">>},{<<"515">>},{<<"516">>},{<<"517">>},{<<"518">>},{<<"519">>},{<<"52">>},{<<"520">>},{<<"521">>},{<<"522">>},{<<"523">>},{<<"524">>},{<<"525">>},{<<"526">>},{<<"527">>},{<<"528">>},{<<"529">>},{<<"53">>},{<<"530">>},{<<"531">>},{<<"532">>},{<<"533">>},{<<"534">>},{<<"535">>},{<<"536">>},{<<"537">>},{<<"538">>},{<<"539">>},{<<"54">>},{<<"540">>},{<<"541">>},{<<"542">>},{<<"543">>},{<<"544">>},{<<"545">>},{<<"546">>},{<<"547">>},{<<"548">>},{<<"549">>},{<<"55">>},{<<"550">>},{<<"551">>},{<<"552">>},{<<"553">>},{<<"554">>},{<<"555">>},{<<"556">>},{<<"557">>},{<<"558">>},{<<"559">>},{<<"56">>},{<<"560">>},{<<"561">>},{<<"562">>},{<<"563">>},{<<"564">>},{<<"565">>},{<<"566">>},{<<"567">>},{<<"568">>},{<<"569">>},{<<"57">>},{<<"570">>},{<<"571">>},{<<"572">>},{<<"573">>},{<<"574">>},{<<"575">>},{<<"576">>},{<<"577">>},{<<"578">>},{<<"579">>},{<<"58">>},{<<"580">>},{<<"581">>},{<<"582">>},{<<"583">>},{<<"584">>},{<<"585">>},{<<"586">>},{<<"587">>},{<<"588">>},{<<"589">>},{<<"59">>},{<<"590">>},{<<"591">>},{<<"592">>},{<<"593">>},{<<"594">>},{<<"595">>},{<<"596">>},{<<"597">>},{<<"598">>},{<<"599">>},{<<"6">>},{<<"60">>},{<<"600">>},{<<"601">>},{<<"602">>},{<<"603">>},{<<"604">>},{<<"605">>},{<<"606">>},{<<"607">>},{<<"608">>},{<<"609">>},{<<"61">>},{<<"610">>},{<<"611">>},{<<"612">>},{<<"613">>},{<<"614">>},{<<"615">>},{<<"616">>},{<<"617">>},{<<"618">>},{<<"619">>},{<<"62">>},{<<"620">>},{<<"621">>},{<<"622">>},{<<"623">>},{<<"624">>},{<<"625">>},{<<"626">>},{<<"627">>},{<<"628">>},{<<"629">>},{<<"63">>},{<<"630">>},{<<"631">>},{<<"632">>},{<<"633">>},{<<"634">>},{<<"635">>},{<<"636">>},{<<"637">>},{<<"638">>},{<<"639">>},{<<"64">>},{<<"640">>},{<<"641">>},{<<"642">>},{<<"643">>},{<<"644">>},{<<"645">>},{<<"646">>},{<<"647">>},{<<"648">>},{<<"649">>},{<<"65">>},{<<"650">>},{<<"651">>},{<<"652">>},{<<"653">>},{<<"654">>},{<<"655">>},{<<"656">>},{<<"657">>},{<<"658">>},{<<"659">>},{<<"66">>},{<<"660">>},{<<"661">>},{<<"662">>},{<<"663">>},{<<"664">>},{<<"665">>},{<<"666">>},{<<"667">>},{<<"668">>},{<<"669">>},{<<"67">>},{<<"670">>},{<<"671">>},{<<"672">>},{<<"673">>},{<<"674">>},{<<"675">>},{<<"676">>},{<<"677">>},{<<"678">>},{<<"679">>},{<<"68">>},{<<"680">>},{<<"681">>},{<<"682">>},{<<"683">>},{<<"684">>},{<<"685">>},{<<"686">>},{<<"687">>},{<<"688">>},{<<"689">>},{<<"69">>},{<<"690">>},{<<"691">>},{<<"692">>},{<<"693">>},{<<"694">>},{<<"695">>},{<<"696">>},{<<"697">>},{<<"698">>},{<<"699">>},{<<"7">>},{<<"70">>},{<<"700">>},{<<"701">>},{<<"702">>},{<<"703">>},{<<"704">>},{<<"705">>},{<<"706">>},{<<"707">>},{<<"708">>},{<<"709">>},{<<"71">>},{<<"710">>},{<<"711">>},{<<"712">>},{<<"713">>},{<<"714">>},{<<"715">>},{<<"716">>},{<<"717">>},{<<"718">>},{<<"719">>},{<<"72">>},{<<"720">>},{<<"721">>},{<<"722">>},{<<"723">>},{<<"724">>},{<<"725">>},{<<"726">>},{<<"727">>},{<<"728">>},{<<"729">>},{<<"73">>},{<<"730">>},{<<"731">>},{<<"732">>},{<<"733">>},{<<"734">>},{<<"735">>},{<<"736">>},{<<"737">>},{<<"738">>},{<<"739">>},{<<"74">>},{<<"740">>},{<<"741">>},{<<"742">>},{<<"743">>},{<<"744">>},{<<"745">>},{<<"746">>},{<<"747">>},{<<"748">>},{<<"749">>},{<<"75">>},
            {<<"750">>},{<<"751">>},{<<"752">>},{<<"753">>},{<<"754">>},{<<"755">>},{<<"756">>},{<<"757">>},{<<"758">>},{<<"759">>},{<<"76">>},{<<"760">>},{<<"761">>},{<<"762">>},{<<"763">>},{<<"764">>},{<<"765">>},{<<"766">>},{<<"767">>},{<<"768">>},{<<"769">>},{<<"77">>},{<<"770">>},{<<"771">>},{<<"772">>},{<<"773">>},{<<"774">>},{<<"775">>},{<<"776">>},{<<"777">>},{<<"778">>},{<<"779">>},{<<"78">>},{<<"780">>},{<<"781">>},{<<"782">>},{<<"783">>},{<<"784">>},{<<"785">>},{<<"786">>},{<<"787">>},{<<"788">>},{<<"789">>},{<<"79">>},{<<"790">>},{<<"791">>},{<<"792">>},{<<"793">>},{<<"794">>},{<<"795">>},{<<"796">>},{<<"797">>},{<<"798">>},{<<"799">>},{<<"8">>},{<<"80">>},{<<"800">>},{<<"801">>},{<<"802">>},{<<"803">>},{<<"804">>},{<<"805">>},{<<"806">>},{<<"807">>},{<<"808">>},{<<"809">>},{<<"81">>},{<<"810">>},{<<"811">>},{<<"812">>},{<<"813">>},{<<"814">>},{<<"815">>},{<<"816">>},{<<"817">>},{<<"818">>},{<<"819">>},{<<"82">>},{<<"820">>},{<<"821">>},{<<"822">>},{<<"823">>},{<<"824">>},{<<"825">>},{<<"826">>},{<<"827">>},{<<"828">>},{<<"829">>},{<<"83">>},{<<"830">>},{<<"831">>},{<<"832">>},{<<"833">>},{<<"834">>},{<<"835">>},{<<"836">>},{<<"837">>},{<<"838">>},{<<"839">>},{<<"84">>},{<<"840">>},{<<"841">>},{<<"842">>},{<<"843">>},{<<"844">>},{<<"845">>},{<<"846">>},{<<"847">>},{<<"848">>},{<<"849">>},{<<"85">>},{<<"850">>},{<<"851">>},{<<"852">>},{<<"853">>},{<<"854">>},{<<"855">>},{<<"856">>},{<<"857">>},{<<"858">>},{<<"859">>},{<<"86">>},{<<"860">>},{<<"861">>},{<<"862">>},{<<"863">>},{<<"864">>},{<<"865">>},{<<"866">>},{<<"867">>},{<<"868">>},{<<"869">>},{<<"87">>},{<<"870">>},{<<"871">>},{<<"872">>},{<<"873">>},{<<"874">>},{<<"875">>},{<<"876">>},{<<"877">>},{<<"878">>},{<<"879">>},{<<"88">>},{<<"880">>},{<<"881">>},{<<"882">>},{<<"883">>},{<<"884">>},{<<"885">>},{<<"886">>},{<<"887">>},{<<"888">>},{<<"889">>},{<<"89">>},{<<"890">>},{<<"891">>},{<<"892">>},{<<"893">>},{<<"894">>},{<<"895">>},{<<"896">>},{<<"897">>},{<<"898">>},{<<"899">>},{<<"9">>},{<<"90">>},{<<"900">>},{<<"901">>},{<<"902">>},{<<"903">>},{<<"904">>},{<<"905">>},{<<"906">>},{<<"907">>},{<<"908">>},{<<"909">>},{<<"91">>},{<<"910">>},{<<"911">>},{<<"912">>},{<<"913">>},{<<"914">>},{<<"915">>},{<<"916">>},{<<"917">>},{<<"918">>},{<<"919">>},{<<"92">>},{<<"920">>},{<<"921">>},{<<"922">>},{<<"923">>},{<<"924">>},{<<"925">>},{<<"926">>},{<<"927">>},{<<"928">>},{<<"929">>},{<<"93">>},{<<"930">>},{<<"931">>},{<<"932">>},{<<"933">>},{<<"934">>},{<<"935">>},{<<"936">>},{<<"937">>},{<<"938">>},{<<"939">>},{<<"94">>},{<<"940">>},{<<"941">>},{<<"942">>},{<<"943">>},{<<"944">>},{<<"945">>},{<<"946">>},{<<"947">>},{<<"948">>},{<<"949">>},{<<"95">>},{<<"950">>},{<<"951">>},{<<"952">>},{<<"953">>},{<<"954">>},{<<"955">>},{<<"956">>},{<<"957">>},{<<"958">>},{<<"959">>},{<<"96">>},{<<"960">>},{<<"961">>},{<<"962">>},{<<"963">>},{<<"964">>},{<<"965">>},{<<"966">>},{<<"967">>},{<<"968">>},{<<"969">>},{<<"97">>},{<<"970">>},{<<"971">>},{<<"972">>},{<<"973">>},{<<"974">>},{<<"975">>},{<<"976">>},{<<"977">>},{<<"978">>},{<<"979">>},{<<"98">>},{<<"980">>},{<<"981">>},{<<"982">>},{<<"983">>},{<<"984">>},{<<"985">>},{<<"986">>},{<<"987">>},{<<"988">>},{<<"989">>},{<<"99">>},{<<"990">>},{<<"991">>},{<<"992">>},{<<"993">>},{<<"994">>},{<<"995">>},{<<"996">>},{<<"997">>},{<<"998">>},{<<"999">>}
        ],
        All
    ).

simple_proc_req_user_auth() ->
    %% Invalid userinfo ( username only )
    ?assertEqual(
        {
            response,
            {
                400,
                [
                    {<<"content-length">>, <<"0">>}
                ],
                <<>>
            }
        },
        holster:basic_auth_request(
            _ReqType = get,
            _URI = <<"http://dummy@localhost:8080/auth-endpoint">>,
            _ConnectOpts = #{},
            _ReqOpts = #{},
            _Headers=[], %% TODO Add existing header
            5000
        )
    ),
    CC = holster:basic_auth_request(
            _ReqType = get,
            _URI = <<"http://dummy:passwd@localhost:8080/auth-endpoint">>,
            _ConnectOpts = #{},
            _ReqOpts = #{},
            _Headers=[], %% TODO Add existing header
            5000
        ),
    ?debugFmt("~p", [CC]),
    ?assertMatch(
        {
                response,
                {
                    401,
                    [{<<"content-length">>, <<"0">>},
                     {<<"date">>, _},
                     {<<"server">>, <<"Cowboy">>},
                     {<<"www-authenticate">>, <<"Basic realm=\"cowboy\"">>}],
                    <<>>
                }
            },
        CC
    ).

https_unit_test_() ->
    {
        setup,
        fun() ->
            _ = error_logger:tty(false),
            AppsStruct1 = application:ensure_all_started(cowboy),
            ?assertEqual(
                {ok,[cowlib,ranch,cowboy]},
                AppsStruct1
            ),
            % AppsStruct2 = application:ensure_all_started(ssl),
            % ?assertEqual(
            %     aaa,
            %     AppsStruct2
            % ),

            ?assert(
                is_list(ssl:cipher_suites(all,'tlsv1.3'))
            ),

            AppsStruct3 = application:ensure_all_started(holster),
            ?assertEqual(
                {ok,[gun,jsx,holster]},
                AppsStruct3
            ),
            _ = error_logger:tty(true),
            {ok, Apps} = AppsStruct1,
            {ok, Apps3} = AppsStruct3,
            Dispatch = cowboy_router:compile([
                {'_', [
                    {"/api", holster_ssl_webserver_h, []}
                ]}
            ]),
            % ?debugFmt("~p\n", [file:read_file("test/ssl/cert.pem")]),
            % ?debugFmt("~p\n", [file:read_file("test/ssl/key.pem")]),
            {ok, Pid} = cowboy:start_tls(
                https,
                _TransOpts=[
                    {port, 8443},
                    {certfile, "test/ssl/cert.pem"},
                    {keyfile, "test/ssl/key.pem"}
                ],
                _ProtoOpts=#{
                    env => #{dispatch => Dispatch}
                }
            ),
            NetstatCmd = "netstat -an | grep LISTEN | grep 8443",
            ListenPortInfo = io_lib:format("~s",[os:cmd(NetstatCmd)]),
            ?debugFmt("CMD: ~p\n~s", [NetstatCmd, ListenPortInfo]),
            ?assert(
                [[]]  =/= ListenPortInfo
            ),
            % {ok, _} = dbg:tracer(),
            % {ok, _} = dbg:p(all, call),
            % {ok, _} = dbg:tpl(tls_record, validate_tls_records_type, cx),
            % {ok, _} = dbg:tpl(tls_record, decode_tls_records, cx),
            % {ok, _} = dbg:tpl(gun, open, cx),
            % {ok, _} = dbg:tpl(gun, await_up, cx),
            % {ok, _} = dbg:tpl(gun, get, cx),
            % {ok, _} = dbg:tpl(holster, cx),
            % {ok, _} = dbg:tpl(holster_request, cx),
            % {ok, _} = dbg:tpl(holster_ssl_webserver_h, cx),
            CurlCmd = "curl -XGET -vvv --insecure https://localhost:8443/api",
            ?debugFmt("CMD: ~p\n~s", [CurlCmd, io_lib:format("~s",[os:cmd(CurlCmd)])]),
            true = erlang:link(Pid),
            {{ok, Pid}, {ok, Apps ++ Apps3}}
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
            {"simple_proc_req/3 test ", fun https_simple_proc_req_3/0}
        ]
    }.

https_simple_proc_req_3() ->
    ?assertMatch(
        {response, {
            200,
            [
                {<<"content-length">>, <<"12">>},
                {<<"content-type">>, <<"text/plain">>},
                {<<"date">>, _},
                {<<"server">>, <<"Cowboy">>}
            ],
            <<"Hello world!">>
        }},
        holster:simple_proc_req(
            get,
            "https://localhost:8443/api",
            #{
                tls_opts => [{verify, verify_none}],
                transport => tls
            }
        )
    ).