%% Association :: Type := Type  %% denotes a mandatory association
%%              | Type => Type  %% denotes an optional association
-type holster_sm_state() :: #{
    scheme := undefined | http | https,
    host := string()
    % req_uri => undefined,
    % req_type => undefined, %% get | post
    % proto => Proto,
    % port => Port,
    % opts_map => OptsMap,
    % timeout => Timeout,
    % conn_type => ConnType,
    % conn_pid => undefined,
    % stream_ref => undefined,
    % client_from => undefined,
    % response_status => undefined,
    % response_headers => undefined,
    % response_data => <<>>,
    % conn_m_ref => undefined,
    % stop_reason => undefined
}.
