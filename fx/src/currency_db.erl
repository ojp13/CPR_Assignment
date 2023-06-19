-module(currency_db).

-export([new/0, write/4, delete/2, destroy/1, read/3, match/3]).

-include("pair_rate.hrl").

new() -> 
    TabId = ets:new(?MODULE, [set, named_table, {keypos, #pair_rate.pair}]),
    TabId.

destroy(_DbRef) -> 
    ets:delete(?MODULE).

write(Source_Currency, Target_Currency, Rate, TabId) -> 
    ets:insert(TabId, #pair_rate{pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, rate=Rate}),
    TabId.

delete(Source_Currency, TabId) ->
    ets:delete(TabId, Source_Currency),
    TabId.

read(Source_Currency, Target_Currency, TabId) ->
    case ets:lookup(TabId, #pair{source_currency=Source_Currency, target_currency=Target_Currency}) of 
        [] -> {error, instance};
        [#pair_rate{rate=Rate}] -> Rate
    end.

match(Source_Currency, Target_Currency, TabId) ->
    lists:flatten(ets:match(TabId, #pair_rate{pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, rate='$0'})).

        
