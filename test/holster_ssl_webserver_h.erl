-module(holster_ssl_webserver_h).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).

init(Req0, Opts) ->
    ?LOG_NOTICE(#{ req => Req0, opts => Opts }),
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>
        },
        <<"Hello world!">>,
        Req0
    ),
    {ok, Req, Opts}.