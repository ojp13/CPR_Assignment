-module(fx_db).

-export([new/0, write/6, read_all/1]).

-include("pair_rate.hrl").

new() -> 
    Fx_Tab_Id = ets:new(?MODULE, [bag, named_table, {keypos, #transaction.transaction_id}]),
    Fx_Tab_Id.

write(Transaction_Id, {Source_Currency, Target_Currency}, Volume, Bid_Rate, Client, TabId) -> 
    ets:insert(TabId, #transaction{transaction_id = Transaction_Id, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Bid_Rate, client_id=Client}).


read_all(TabId) -> 
    io:format("Reading all"),
    % Transactions = ets:match(TabId, {'_', '$1', {'_', '_', '_'}, '_', '_', '_'}),
    Transaction = ets:first(TabId),
    Transaction.

% ets:select(fx_db, [{{'_', '$1', {'_', '_', '_'}, '_', '_', '_'}, [], [['$1']]}]).