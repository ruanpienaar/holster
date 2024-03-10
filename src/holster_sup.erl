-module(holster_sup).

-include("holster.hrl").

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    _ = create_connection_proto_atoms(),
    {ok, {#{}, []}}.

create_connection_proto_atoms() ->
    [
        http,
        https,
        ftp,
        ssh,
        sftp,
        tftp
    ].