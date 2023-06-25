-module(fx_db).

-export([new/0, write/7, read_all/1, read_by_id/2]).

-include("pair_rate.hrl").

new() -> 
    Fx_Tab_Id = ets:new(?MODULE, [bag, named_table, {keypos, #transaction.transaction_id}]),
    Fx_Tab_Id.

write(Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client, TabId) -> 
    ets:insert(TabId, #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client}).


read_all(TabId) -> 
    Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
    form_transaction_from_select_result(Transactions).

form_transaction_from_select_result([[Transaction_Id, Type, Source_Currency, Target_Currency, Volume, Rate, Client]|T]) ->
    Transaction = #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client},
    [Transaction | form_transaction_from_select_result(T)];
form_transaction_from_select_result([]) ->
    [].

read_by_id(TabId, Transaction_Id) -> 
    Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
    form_transaction_from_select_result(Transactions).