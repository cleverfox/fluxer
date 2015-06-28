# fluxer - Erlang client for InfluxDB 0.9 #

Copyright (c) 2015 Vladimir Goncharov
Copyright (c) 2014 Sergey Abramyan

__Version:__ 0.3.0

    Adaptation for influx 0.9, throw out all except write_series and query. Switched from hackney to
    httpc, because of problems with persistent connections. Added parameters for HTTP client (for
    example: you can specify SSL certificates or SSL verify options).

## API

**Results**

*ok_result:*

- ok
- {ok, map()}

*error_result:*

- {error, integer(), binary()}
- {error, db_not_set}
- {error, database_already_exists}
- {error, influxdb_unavailable}

**Init flux object**

Init with default settings

```erlang

1> Flux = fluxer:init()
{flux,undefined,<<"127.0.0.1">>,8086,<<"root">>,<<"root">>,false}
```

If need other settings, call fluxer:init/1. Example:

```erlang

1> Config = #{
    host => <<"influx_server.example.net">>,
    db => <<"test">>,
    port => 8086,
    user => <<"myuser">>,
    password => <<"mypassword">>,
    ssl => true,
    http_opts => [
    {ssl, [
        {certfile,"client_cert.pem"},
        {cacertfile,"ca.pem"}
    ]}
    ]
}.
2> Flux = fluxer:init(Config).
{flux,<<"test">>,<<"influx_server.example.net">>,8086,
    <<"myuser">>,<<"mypassword">>,true,
    [{ssl,[{certfile,"client_cert.pem"},
        {cacertfile,"ca.pem"}]}]}
```

### Reading & Writing Data

**Write series**

```erlang

1> Flux = fluxer:init(Config).
...
2> Data = #{database => mydb, points => [#{fields => #{ value => 1000 },name => <<"nodemem">>,
time => fun() -> {MSec,Sec,_USec}=now(), MSec*1000000+Sec end()} ], retentionPolicy => default, 
tags => #{ node => <<"abc">> }}.
#{database => mydb,
    points => [#{fields => #{value => 1000},name => <<"nodemem">>,time => 1435503197}],
    retentionPolicy => default,
    tags => #{node => <<"abc">>}}
3> fluxer:write_series(Flux, Data).
ok
```

**Query**

```erlang

1> Flux = fluxer:init(Config).
...
2> fluxer:query(Flux,<<"SHOW measurements">>).
{ok,#{<<"results">> => [#{<<"series">> => [#{<<"columns">> => [<<"name">>],
    <<"name">> => <<"measurements">>,
    <<"values">> => [[<<"conns">>],
    [<<"cpu_load_short">>],
    [<<"mem">>],
    [<<"nodemem">>],
    [<<"nodestatus">>],
    [<<"nstatus">>],
    [<<"status">>]]}]}]}}
3> fluxer:query(Flux, <<"select node,value from mem where node='node31' limit 5">>).
{ok,#{<<"results">> => [#{<<"series">> => [#{<<"columns">> => [<<"time">>,<<"value">>],
    <<"name">> => <<"mem">>,
    <<"tags">> => #{<<"node">> => <<"node31">>},
    <<"values">> => [[<<"2015-06-05T20:19:26Z">>,1354596904.0],
    [<<"2015-06-05T20:19:59Z">>,1356222384.0],
    [<<"2015-06-05T20:20:32Z">>,1355411128.0],
    [<<"2015-06-05T20:21:04Z">>,1355631568.0],
    [<<"2015-06-05T20:21:37Z">>,1355001048.0]]}]}]}}
```


