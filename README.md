# holster
A [Gun](https://github.com/ninenines/gun) holster

## Quickstart

```
$ make
$ ./start-dev.sh
erl> {ok, _} = application:ensure_all_started(holster).
```

## Example requests:

You can either have a once-off request, or a long-running request.
a once-off request connects to the given host, makes the call, and tears
down the connection, whereas a long-running connection will keep the
connection open for consecutive requests made.

### Once off:
```Erlang
holster:once_off_req(Url :: string()) =
  {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         resp_headers() :: proplists:proplist(),
                         response() :: binary()} | 
  {response, ErrResponse :: term()}
```

### Long running:
```Erlang
holster:long_running_req(Url :: string()) =
  {
     {ok, pid()}, {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         resp_headers() :: proplists:proplist(),
                         response() :: binary()}}
     {
                  {response, ErrResponse :: term()}
  }

holster:long_running_req(Url :: string(), PidOrOpts :: pid() | gun:opts()) =
  {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         resp_headers() :: proplists:proplist(),
                         response() :: binary()} | 
  {response, ErrResponse :: term()}
```


TODO: reply with pid on long_running_req/2 when Opts passed in, or just both
cases.
