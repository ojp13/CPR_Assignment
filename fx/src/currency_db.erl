-module(currency_db).

-export([new/0, write/4, read/3, find_matches/2, delete/2, find_paired_currencies/2, find_paired_and_defined_currencies/2, find_all_pairs/1]).

-include("pair_rate.hrl").

new() -> 
    TabId = ets:new(?MODULE, [set, named_table, {keypos, #pair_rate.pair}]),
    TabId.

write(Source_Currency, Target_Currency, Rate, TabId) -> 
    ets:insert(TabId, #pair_rate{pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, rate=Rate}).

read(Source_Currency, Target_Currency, TabId) ->
    case ets:lookup(TabId, #pair{source_currency=Source_Currency, target_currency=Target_Currency}) of 
        [] -> {error, instance};
        [#pair_rate{rate=undefined}] -> 
            undefined;
        [#pair_rate{rate=Rate}] -> Rate
    end.


find_matches(Currency, TabId) ->
    % The fun2ms used to generate the MS is left in here. It doesn't work well at compile time
    % MatchSpec = ets:fun2ms(fun({_, {_, Source, Target}, _}) when (Source == usd) or (Target == Currency) -> [{Source, Target}] end).
    % MatchSpec = ets:fun2ms(fun({_, {_, Source, Target}, Rate}) when (Source == usd) and (Rate =/= undefined) -> [Target] end).
    Pairs = ets:select(TabId, [{{'_',{'_','$1','$2'},'_'},[{'or',{'==','$1',Currency},{'==','$2',Currency}}],[[{{'$1','$2'}}]]}]),
    lists:flatten(Pairs).


find_paired_currencies(Currency, TabId) ->
    Where_Source_Currency = ets:select(TabId, [{{'_',{'_','$1','$2'},'_'},[{'==','$1',Currency}],[['$2']]}]),
    io:format("Found Sources: ~p ~n", [Where_Source_Currency]),
    Where_Target_Currency = ets:select(TabId, [{{'_',{'_','$1','$2'},'_'},[{'==','$2',Currency}],[['$1']]}]),
    io:format("Found Targets: ~p ~n", [Where_Target_Currency]),
    lists:flatten([Where_Source_Currency | Where_Target_Currency]).

find_paired_and_defined_currencies(Currency, TabId) ->
    Where_Source_Currency = ets:select(TabId, [{{'_',{'_','$1','$2'},'$3'},[{'and',{'==','$1',Currency},{'=/=','$3',undefined}}],[['$2']]}]),
    io:format("Found Sources: ~p ~n", [Where_Source_Currency]),
    Where_Target_Currency = ets:select(TabId, [{{'_',{'_','$1','$2'},'$3'},[{'and',{'==','$2',Currency},{'=/=','$3',undefined}}],[['$1']]}]),
    io:format("Found Targets: ~p ~n", [Where_Target_Currency]),
    lists:flatten([Where_Source_Currency | Where_Target_Currency]).

find_all_pairs(TabId) ->
    % Pairs = ets:select(TabId, [{{'_',{'_','$1','$2'},'$3'},[],[[{{{'$1','$2'}, '$3'}}]]}]),
    Pairs = ets:match(TabId, '$1'),
    lists:flatten(Pairs).


delete(Exotic_Currency, TabId) ->
    ets:select_delete(TabId, [{{'_',{'_','$1','$2'},'_'},[{'or',{'==','$1',Exotic_Currency},{'==','$2',Exotic_Currency}}],[true]}]),
    {ok, deleted}.



