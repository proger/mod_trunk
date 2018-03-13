-module(mod_trunk).
-behaviour(gen_mod).

-compile(export_all).

-export([start/2, stop/1, depends/2, mod_opt_type/1,
         read_config/1,
         process/2,
         health/0]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_http.hrl").

-type bare_jid() :: {binary(), binary()}.

-record(trunk_alias, {alias :: binary(), bare_jid :: bare_jid()}).

-record(config, {host :: binary(),
                 src_host :: binary(),
                 token :: binary()}).

%%% routing

sms_to_xmpp({Src, Dst, Text, Timestamp}, #config{src_host=SrcHost}) ->
    case alias_to_jid(Dst) of
        {ok, To} ->
            Pkt0 = #message{from=jid:make(Src, SrcHost),
                            to=To,
                            body=xmpp:mk_text(Text)},
            Pkt1 = xmpp:put_meta(Pkt0, sms, true),
            Pkt2 =
                try
                    xmpp_util:add_delay_info(Pkt1, jid:make(SrcHost), Timestamp, <<"SMS Trunk">>)
                catch
                    error:function_clause ->
                        ?ERROR_MSG("sms_to_xmpp: bad timestamp: ~p~n", [{Src, Dst, Timestamp}]),
                        Pkt1
                end,
            {ok, Pkt2};
        unregistered ->
            throw(unregistered);
        {error, E} ->
            throw({error, E})
    end.

%%% config

config(Host) ->
    read_config(
      gen_mod:get_module_opt(Host, ?MODULE, backends,
                             fun(O) when is_list(O) -> O end,
                             []),
      Host).

read_config(Proplist) ->
    #config{src_host=proplists:get_value(src_host, Proplist, <<"sms">>),
            token=proplists:get_value(token, Proplist, <<"token">>)}.

read_config(Proplist, Host) ->
    Config = read_config(Proplist),
    Config#config{host=Host}.

%%% mnesia

-define(RECORD(X), {X, record_info(fields, X)}).

mnesia_set_from_record({Name, Fields}) ->
    mnesia:create_table(Name,
                        [{disc_copies, [node()]},
                         {type, set},
                         {attributes, Fields}]),

    case mnesia:table_info(Name, attributes) of
        Fields -> ok;
        _ -> mnesia:transform_table(Name, ignore, Fields)
    end.

register_alias(Alias, #jid{luser = LUser, lserver = LServer}) ->
    F = fun() ->
                mnesia:write(#trunk_alias{alias=Alias, bare_jid={LUser, LServer}})
        end,
    mnesia:transaction(F).

unregister_alias(Alias) ->
    F = fun() ->
                mnesia:delete({trunk_alias, Alias})
        end,
    mnesia:transaction(F).

alias_to_jid(Alias) ->
    case mnesia:transaction(fun() -> mnesia:read({trunk_alias, Alias}) end) of
        {atomic, [#trunk_alias{bare_jid={LUser, LServer}}]} ->
            {ok, jid:make(LUser, LServer)};
        {atomic, []} ->
            unregistered;
        E ->
            {error, E}
    end.

%%% gen_mod

mod_opt_type(config) -> fun ?MODULE:read_config/1;
mod_opt_type(_) -> [config].

start(_Host, _Opts) -> mnesia_set_from_record(?RECORD(trunk_alias)), ok.
stop(_Host) -> ok.
depends(_Host, _Opts) -> [].

%%% ejabberd_http

process([] , #request{method='POST', data=Data, host=Host, ip=_ClientIp}) ->
    try
        Config = config(Host),
        M1 = read_input(Data),
        M2 = check_strip_token(M1, Config),
        {ok, Pkt} = sms_to_xmpp(M2, Config),
        ok = ejabberd_router:route(Pkt),
        {200, [], <<"">>}
    catch
        throw:{error,{_,invalid_json}} = R ->
            ?DEBUG("invalid_json: ~p~n", [[{data, Data}, {error, R}]]),
            {400, [], <<"Malformed JSON">>};
        throw:{bad_json, _} = R ->
            ?DEBUG("bad_json: ~p~n", [[{data, Data}, {error, R}]]),
            {400, [], <<"Unexpected JSON">>};
        throw:{token_mismatch, _} = R ->
            ?DEBUG("token_mismatch: ~p~n", [[{data, Data}, {error, R}]]),
            {401, [], <<"Token mismatch">>};
        throw:unregistered ->
            ?ERROR_MSG("unregistered alias: ~p~n", [[{data, Data}]]);
        E:R ->
            ?ERROR_MSG("failed to route trunk: ~p~n", [[{data, Data}, {error, {E, R}}]]),
            {500, [], <<"">>}
    end;
process(Path, Request) ->
    ?DEBUG("stray request: ~p~n", [{Path, Request}]),
    {405, [], <<"">>}.

read_input(Data) ->
    case jiffy:decode(Data, [return_maps]) of
        #{<<"src">> := Src,
          <<"dst">> := Dst,
          <<"text">> := Text,
          <<"ts">> := Timestamp,
          <<"token">> := Token} ->
            {Src, Dst, Text, Timestamp, Token};
        Other -> throw({bad_json, Other})
    end.

check_strip_token({Src, Dst, Text, Timestamp, Token}, #config{token=Token}) ->
    {Src, Dst, Text, Timestamp};
check_strip_token({_, _, _, _, T1}, #config{token=T2}) ->
    throw({token_mismatch, {T1, T2}}).

%%% misc

health() ->
    Hosts = ejabberd_config:get_myhosts(),
    {[config(H) || H <- Hosts]}.

test() ->
    ejabberd_router:route(#message{from=jid:make(<<"alice">>, <<"localhost">>),
                                   to=jid:make(<<"bob">>, <<"localhost">>),
                                   body=[#text{data= <<"hello">>}]}),
    ok.
