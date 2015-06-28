-module(fluxer).

-export([init/0]).
-export([init/1]).
-export([set_db/2]).

-export([write_series/2]).
-export([query/2]).

-record(flux, {db       :: binary(),
               host     :: binary(),
               port     :: non_neg_integer(),
               user     :: binary(),
               password :: binary(),
               ssl      :: boolean(),
               http_opts:: list()
              }).

-type ok_result() :: {ok, map()}.
-type error_result() :: {error, integer(), binary()} |
                        {error, db_not_set} |
                        {error, database_already_exists} |
                        {error, influxdb_unavailable}.

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> #flux{}.
init() ->
    init(#{}).

-spec init(map()) -> #flux{}.
init(Config) ->
    #flux{
         db = maps:get(db, Config, undefined),
         host = maps:get(host, Config, <<"127.0.0.1">>),
         port = maps:get(port, Config, 8086),
         user = maps:get(user, Config, <<"root">>),
         password = maps:get(password, Config, <<"root">>),
         ssl = maps:get(ssl, Config, false),
         http_opts = maps:get(http_opts, Config, [])
        }.

set_db(Name, Flux) ->
    Flux#flux{ db = Name }.

-spec write_series(#flux{}, map()) -> ok | error_result().
write_series(Flux, _Data) when Flux#flux.db == undefined ->
    {error, db_not_set};
write_series(Flux, Data) ->
    URL=make_url(<<"/write">>, Flux),
    case httpc:request(post,
                       {binary_to_list(URL),[],"application/json", json:to_binary(Data) },
                       Flux#flux.http_opts,
                       [{body_format, binary}]) of
        {ok, {{_,Res,_}, _Headers, _RespBody}} when Res >= 200 andalso 204 >= Res ->
            ok;
        {ok, {{_,Res,_}, _Headers, RespBody}} ->
            {error, Res, RespBody};
        {error, _Error} ->
            error_logger:error_msg("Error ~p",[_Error]),
            {error, influxdb_unavailable}
    end.

-spec query(#flux{}, binary()) -> ok_result() | error_result().
query(Flux, _Query) when Flux#flux.db == undefined ->
    {error, db_not_set};
query(Flux, Query) when is_binary(Query) ->
    URL=make_url(<<"/query">>, Flux, [
                                      {<<"q">>,Query},
                                      {<<"db">>,Flux#flux.db}
                                     ]),
    case httpc:request(get,{binary_to_list(URL),[]},Flux#flux.http_opts,[{body_format, binary}]) of
        {ok, {{_,200,_}, _Headers, RespBody}} ->
            {ok, json:from_binary(RespBody)};
        {ok, {StatusCode, _Headers, RespBody}} ->
            {error, StatusCode, RespBody};
        {error, _Error} ->
            error_logger:error_msg("Error ~p",[_Error]),
            {error, influxdb_unavailable}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec make_url(binary() | list(), #flux{}) -> binary().
make_url(Path, Flux) when is_list(Path) ->
    make_url(iolist_to_binary(Path), Flux);
make_url(Path, Flux) ->
    make_url(Path, Flux, []).

-spec make_url(binary() | list(), #flux{}, list()) -> binary().
make_url(Path, Flux, Qs) when is_list(Path) ->
    make_url(iolist_to_binary(Path), Flux, Qs);
make_url(Path, Flux, Qs) ->
    Qs2 = [{<<"u">>, Flux#flux.user}, {<<"p">>, Flux#flux.password} | Qs],
    Protocol = case Flux#flux.ssl of
                   false -> <<"http://">>;
                   true -> <<"https://">>
               end,
    URI = iolist_to_binary([Protocol, Flux#flux.host, ":", integer_to_list(Flux#flux.port)]),
    hmake_url(URI, <<$/, Path/binary>>, Qs2).


%% ***** this part from hackney_lib
%% @doc  construct an url from a base url, a path and a list of
%% properties to give to the url.
hmake_url(Url, Path, Query) when is_list(Query) ->
    %% a list of properties has been passed
    hmake_url(Url, Path, qs(Query));
hmake_url(Url, Path, Query) when is_binary(Path) ->
    hmake_url(Url, [Path], Query);
hmake_url(Url, PathParts, Query) when is_binary(Query) ->
    %% create path
    PathParts1 = [fix_path(P) || P <- PathParts, P /= "", P /= "/" orelse P /= <<"/">>],
    Path = join([<<>> | PathParts1], <<"/">>),

    %% initialise the query
    Query1 = case Query of
        <<>> -> <<>>;
        _ -> << "?", Query/binary >>
    end,

    %% make the final uri
    iolist_to_binary([fix_path(Url), Path, Query1]).

qs(KVs) ->
   qs(KVs, []).

qs([], Acc) ->
    join(lists:reverse(Acc), <<"&">>);
qs([{K, V}|R], Acc) ->
    K1 = urlencode(K),
    V1 = urlencode(V),
    Line = << K1/binary, "=", V1/binary >>,
    qs(R, [Line | Acc]).

fix_path(Path) when is_list(Path) ->
    fix_path(list_to_binary(Path));
fix_path(<<>>) ->
    <<>>;
fix_path(<<"/", Path/binary>>) ->
    fix_path(Path);
fix_path(Path) ->
    case binary:part(Path, {size(Path), -1}) of
        <<"/">> -> binary:part(Path, {0, size(Path) - 1});
        _ -> Path
    end.

urlencode(Bin) ->
	urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
-spec urlencode(binary() | string(), [noplus|upper]) -> binary().
urlencode(Bin, Opts) ->
	Plus = not proplists:get_value(noplus, Opts, false),
	Upper = proplists:get_value(upper, Opts, false),
	urlencode(to_binary(Bin), <<>>, Plus, Upper).

-spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
	if	C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
		urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $ , Plus ->
		urlencode(Rest, <<Acc/binary, $+>>, P, U);
		true ->
		H = C band 16#F0 bsr 4, L = C band 16#0F,
		H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
		L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
		urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
	end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
	Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 16 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 16 -> $a + C - 10.


join([], _Separator) ->
    <<>>;
join([S], _separator) ->
    S;
join(L, Separator) ->
    iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
    Acc;
join([S | Rest], Separator, []) ->
    join(Rest, Separator, [S]);
join([S | Rest], Separator, Acc) ->
    join(Rest, Separator, [S, Separator | Acc]).

to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
    V.


