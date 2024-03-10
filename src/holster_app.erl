-module(holster_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

-include("holster.hrl").

start(_StartType, _StartArgs) ->
    {ok, _SupPid} = holster_sup:start_link().

stop(_State) ->
    ok.
