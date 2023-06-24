-module(currency_db).

-export([new/0, write/4, read/3, match/3, find_matches/2]).

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

find_matches(TabId, Currency) -> 
    Where_Source_Currency = ets:match(TabId, {'_', {'_', Currency, '$1'}, '_'}),
    Where_Target_Currency = ets:match(TabId, {'_', {'_', '$1', Currency}, '_'}),
    lists:flatten(Where_Source_Currency ++ Where_Target_Currency).


        
