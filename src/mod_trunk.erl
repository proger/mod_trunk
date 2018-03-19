-module(mod_trunk).
-behaviour(gen_mod).

-compile(export_all).

-export([start/2, stop/1, depends/2, mod_opt_type/1,
         read_config/1,
         process/2,
         health/0,
         register_alias/2, unregister_alias/1, all_aliases/0]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_http.hrl").

-type bare_jid() :: {binary(), binary()}.

-record(trunk_alias, {alias :: binary(), bare_jid :: bare_jid()}).

-record(config, {src_host :: binary(),
                 token :: binary()}).

%%% routing

sms_to_xmpp({Src, Dst, Text, _Timestamp} = M, #config{src_host=SrcHost}) ->
    case alias_to_jid(Dst) of
        {ok, To} ->
            ?DEBUG("mod_trunk wants to route: ~p to ~p~n", [M, To]),
            Pkt0 = #message{from=jid:make(Src, SrcHost),
                            to=To,
                            body=xmpp:mk_text(Text)},
            {ok, Pkt0};
            %% Pkt1 =
            %%     try
            %%         xmpp_util:add_delay_info(Pkt0, jid:make(SrcHost), Timestamp, <<"SMS Trunk">>)
            %%     catch
            %%         error:function_clause ->
            %%             ?ERROR_MSG("sms_to_xmpp: bad timestamp: ~p~n", [{Src, Dst, Timestamp}]),
            %%             Pkt0
            %%     end,
            %% {ok, Pkt1};
        unregistered ->
            throw(unregistered);
        {error, E} ->
            throw({error, E})
    end.

%%% config

config(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, config).

read_config(Proplist) ->
    #config{src_host=proplists:get_value(src_host, Proplist, <<"sms">>),
            token=proplists:get_value(token, Proplist, <<"token">>)}.

%%% mnesia

-define(RECORD(X), {X, record_info(fields, X)}).

mnesia_set_from_record({Name, Fields}) ->
    TabDef = [{disc_copies, [node()]}, {type, set}, {attributes, Fields}],
    ejabberd_mnesia:create(?MODULE, Name, TabDef).

all_aliases() ->
    mnesia:dirty_select(trunk_alias, [{'_',[],['$_']}]).

register_alias(Alias, #jid{luser = LUser, lserver = LServer}) ->
    F = fun() -> mnesia:write(#trunk_alias{alias=Alias, bare_jid={LUser, LServer}}) end,
    mnesia:transaction(F);
register_alias(Alias, BinaryJID) ->
    register_alias(Alias, jid:decode(BinaryJID)).

unregister_alias(Alias) ->
    F = fun() -> mnesia:delete({trunk_alias, Alias}) end,
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

process([] , #request{method='POST', data=Data, host=_Host, ip=_ClientIp}) ->
    try
        Config = config(global),
        M1 = read_input(Data),
        M2 = check_strip_token(M1, Config),
        {ok, Pkt} = sms_to_xmpp(M2, Config),
        ok = ejabberd_router:route(Pkt),
        {200, [], <<"">>}
    catch
        throw:{error,{_,invalid_json}} = R ->
            ?DEBUG("invalid_json: ~p~n", [[{binary, Data}, {error, R}]]),
            {400, [], <<"Malformed JSON\n">>};
        throw:{bad_json, _} = R ->
            ?DEBUG("bad_json: ~p~n", [[try_read_input(Data), {error, R}]]),
            {400, [], <<"Unexpected JSON\n">>};
        throw:{token_mismatch, _} = R ->
            ?DEBUG("token_mismatch: ~p~n", [[try_read_input(Data), {error, R}]]),
            {401, [], <<"Token mismatch\n">>};
        throw:unregistered ->
            case try_read_input(Data) of
                {json, #{<<"dst">> := Dst} = M} ->
                    ?INFO_MSG("mod_trunk: unknown dst alias. use `~p:register_alias(~p, <<\"jid@localhost\">>).` to register. original message: ~p~n", [?MODULE, Dst, M]);
                Other ->
                    ?ERROR_MSG("mod_trunk: unregistered alias: ~p~n", [Other])
            end,
            {404, [], <<"">>};
        E:R ->
            ?ERROR_MSG("mod_trunk error: ~p~n", [[{error, {E, R}}, try_read_input(Data)]]),
            {500, [], <<"">>}
    end;
process(Path, Request) ->
    ?DEBUG("stray request: ~p~n", [{Path, Request}]),
    {405, [], <<"">>}.

try_read_input(Data) ->
    try
        {json, jiffy:decode(Data, [return_maps])}
    catch
        _:_ -> {binary, Data}
    end.

read_input(Data) ->
    case jiffy:decode(Data, [return_maps]) of
        #{<<"src">> := Src,
          <<"dst">> := Dst,
          <<"text">> := PreText,
          <<"ts">> := Timestamp,
          <<"token">> := Token} ->
            Text = case catch base64:decode(PreText) of
                {'EXIT', _} -> PreText;
                T -> T
            end,
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
    [{config, [{H, config(H)} || H <- Hosts]},
     {aliases, mnesia:table_info(trunk_alias, size)}].

test() ->
    ejabberd_router:route(#message{from=jid:make(<<"alice">>, <<"localhost">>),
                                   to=jid:make(<<"bob">>, <<"localhost">>),
                                   body=[#text{data= <<"hello">>}]}),
    ok.
