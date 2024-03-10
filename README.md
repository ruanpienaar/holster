# holster
A [Gun](https://github.com/ninenines/gun) holster

## Quickstart

```
$ make
$ ./rebar3 shell
erl> {ok, _} = application:ensure_all_started(holster).
```

## Description:

holster makes the gun interaction simpler, by hiding out some of the fancy
footwork required.
Support for long running connections (http2), and simple requests.
connection get's teared down for simple requests after each request.

### [simple request](https://github.com/ruanpienaar/holster/blob/master/src/holster.erl#L92):

( req/2/3/4/5 )

```Erlang
holster:req(req_type(), Uri :: string()) =
  {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         gun:resp_headers(),
                         response() :: binary()} |
  {response, ErrResponse :: term()}
```

### Example simple request

```Erlang
holster:req(get, "http://www.google.com").
```

### Stay connected request:
```Erlang
holster:stay_connected_req(Uri :: string()) =
  {
     {ok, pid()}, {response, Resonse :: {resp_code() :: non_neg_integer(), 
                                         resp_headers(),
                                         response() :: binary()}} |
     {response, ErrResponse :: term()}
  }
```

and then all other subsequent requests made with the pid:

```Erlang
holster:another_request(get, Uri :: string(), PidOrOpts :: pid()) =
  {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         resp_headers() :: proplists:proplist(),
                         response() :: binary()} | 
  {response, ErrResponse :: term()}
```

### Basic erlang process approach

```Erlang
holster:simple_proc_req(req_type(), Uri :: string()) =
  {response, Resonse :: {resp_code() :: non_neg_integer(), 
                         resp_headers() :: proplists:proplist(),
                         response() :: binary()} | 
  {response, ErrResponse :: term()}
```

### Websocket request:
```Erlang
ws_connect(uri_string:uri_string(), gun:opts()) = 
  {ok, pid(), gun:resp_headers()} | 
  {error, {ws_upgrade, timeout}}.
```

### Websocket request example:
```Erlang
{ok, <0.167.0>,
    [{<<"upgrade">>, <<"websocket">>},
     {<<"connection">>, <<"Upgrade">>},
     {<<"sec-websocket-accept">>, <<"nRurElfkCM9Jf2Obkkcm5rqfpy8=">>},
     {<<"date">>, <<"Sun, 10 Mar 2024 13:33:34 GMT">>},
     {<<"server">>, <<"Python/3.12 websockets/12.0">>}]} = holster:ws_connect("ws:\/\/localhost:8765", #{}).

ok = holster:ws_req(<0.167.0>, {text, <<"test">>}).

```