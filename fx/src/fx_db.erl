-module(fx_db).

-export([new/0, write/6, read_all/0, read_by_id/1, init/0]).

-include("pair_rate.hrl").

new() -> 
    Db = spawn(?MODULE, init, []),
    register(transaction_server, Db),
    {ok, db}.

init() ->
    Fx_Tab_Id = ets:new(?MODULE, [bag, named_table, {keypos, #transaction.transaction_id}]),
    db_server(Fx_Tab_Id).

write(Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client) -> 
    transaction_server ! {write, Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client, self()},
    receive
        Response -> Response
    end.

read_all() -> 
    transaction_server ! {read_all, self()},
    receive
        Response -> Response
    end.

read_by_id(Transaction_Id) -> 
    transaction_server ! {read_by_id, Transaction_Id, self()},
    receive
        Response -> Response
    end.

db_server(TabId) ->
    receive
        {write, Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client, Pid} -> 
            ets:insert(TabId, #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client}),
            Pid ! ok,
            db_server(TabId);
        {read_all, Pid} -> 
            Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Transactions = form_transaction_from_select_result(Transactions),
            Pid ! Processed_Transactions,
            db_server(TabId);
        {read_by_id, Transaction_Id, Pid} -> 
            Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Transactions = form_transaction_from_select_result(Transactions),
            Pid ! Processed_Transactions,
            db_server(TabId)
    end.

form_transaction_from_select_result([[Transaction_Id, Type, Source_Currency, Target_Currency, Volume, Rate, Client]|T]) ->
    Transaction = #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client},
    [Transaction | form_transaction_from_select_result(T)];
form_transaction_from_select_result([]) ->
    [].

