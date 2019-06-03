-module(holster_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("holster.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    holster_sup:start_link().

stop(_State) ->
    ok.
