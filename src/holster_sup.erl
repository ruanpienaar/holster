-module(holster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-include("holster.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    create_connection_proto_atoms(),
    {ok, { {one_for_one, 5, 10}, [
                #{
                    id       => holster_conn_sup,                   % mandatory
                    start    => {holster_conn_sup, start_link, []}, % mandatory
                    restart  => permanent,                          % optional
                    shutdown => brutal_kill,                        % optional
                    type     => supervisor,                         % optional
                    modules  => [holster_conn_sup]                  % optional
                }
            ]
        }
    }.

create_connection_proto_atoms() ->
    [
        http,
        https,
        ftp,
        ssh,
        sftp,
        tftp
    ].