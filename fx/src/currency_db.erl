-module(currency_db).

-export([new/0, write/4, read/3, match/3, find_matches/2, delete/2]).

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

match(Source_Currency, Target_Currency, TabId) ->
    lists:flatten(ets:match(TabId, #pair_rate{pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, rate='$0'})).

% find_matches(Currency, TabId) -> 
%     ets:select(TabId, [{{'_', }}])

find_matches(Currency, TabId) ->
    % The fun2ms used to generate the MS is left in here. It doesn't work well at compile time
    % MatchSpec = ets:fun2ms(fun({pair_rate, {pair, Source, Target}, _}) when (Source == Currency) or (Target == Currency) -> [{Source, Target}] end),
    Pairs = ets:select(TabId, [{{'_',{'_','$1','$2'},'_'},[{'or',{'==','$1',Currency},{'==','$2',Currency}}],[[{{'$1','$2'}}]]}]),
    lists:flatten(Pairs).


delete(Exotic_Currency, TabId) ->
    ets:select_delete(TabId, [{{'_',{'_','$1','$2'},'_'},[{'or',{'==','$1',Exotic_Currency},{'==','$2',Exotic_Currency}}],[true]}]),
    {ok, deleted}.
