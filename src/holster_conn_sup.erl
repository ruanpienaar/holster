-module(holster_conn_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/6
]).

-export([init/1]).

-include("holster.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Host, Proto, Port, OptsMap, Timeout, ConnType) ->
    supervisor:start_child(
        ?MODULE, [Host, Proto, Port, OptsMap, Timeout, ConnType]).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        #{
            id       => holster_sm,                   % mandatory
            start    => {holster_sm, start_link, []}, % mandatory
            restart  => temporary,                    % optional
            shutdown => brutal_kill,                  % optional
            type     => worker,                       % optional
            modules  => [holster_sm]                  % optional
        }
    ]} }.
