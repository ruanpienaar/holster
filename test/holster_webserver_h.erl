-module(holster_webserver_h).

-export([init/2]).

init(Req0, Opts) ->
    Req = handle(cowboy_req:method(Req0), Req0),
    {ok, Req, Opts}.

handle(<<"GET">>, Req) ->
    cowboy_req:reply(200, #{}, <<"dummy reply">>, Req);
handle(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).