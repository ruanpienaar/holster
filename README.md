# holster
A holster for [Gun](https://github.com/ninenines/gun)

[Gun User Guide](https://ninenines.eu/docs/en/gun/1.3/guide/)

## Quickstart

```
$ make
$ rebar3 shell
```

## Example:

First request:
```Erlang
{ok, Host, URI, Proto, Port, OptsMap, Timeout, Pid} = holster:req("www.erlang.org", "/").
```

Consequent requests:
```Erlang
holster:req(Host, "/news", Proto, Port, OptsMap, Timeout, Pid).
holster:req(Host, "/downloads", Proto, Port, OptsMap, Timeout, Pid).
```
