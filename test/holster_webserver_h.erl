-module(holster_webserver_h).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).

init(Req0, Opts) ->
    ?LOG_DEBUG(#{}),
    Req = handle(
        cowboy_req:path(Req0),
        maps:from_list(cowboy_req:parse_qs(Req0)),
        cowboy_req:method(Req0),
        Req0
    ),
    {ok, Req, Opts}.

handle(<<"/insert">>, #{<<"entry">> := Entry, <<"table">> := Table}, <<"GET">>, Req) ->
    ?LOG_DEBUG(#{}),
    true = ets:insert(binary_to_atom(Table), {Entry}),
    cowboy_req:reply(200, #{}, <<"dummy reply">>, Req);
handle(<<"/">>, _, <<"GET">>, Req) ->
    ?LOG_DEBUG(#{}),
    cowboy_req:reply(200, Req);
handle(Path, Qs, Method, Req) ->
    ?LOG_DEBUG(#{}),
    io:format("~p ~p ~p\n", [Path, Qs, Method]),
    cowboy_req:reply(404, Req).
