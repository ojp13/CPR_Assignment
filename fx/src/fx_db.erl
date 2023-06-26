-module(fx_db).

-export([new/0, write/6, read_all/0, read_by_id/1, init/0, find_asks/3]).

-include("pair_rate.hrl").

new() -> 
    Db = spawn(?MODULE, init, []),
    register(transaction_server, Db),
    {ok, db}.

init() ->
    % Use an ordered set so that results are always returned in first-last insertion order
    Fx_Tab_Id = ets:new(?MODULE, [ordered_set, named_table, {keypos, #transaction.transaction_id}]),
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

find_asks(Transaction_Id, Ask_Pair, Ask_Rate) -> 
    transaction_server ! {asks, Transaction_Id, Ask_Pair, Ask_Rate, self()},
    receive
        {matches, Transaction_Id, Response} -> Response
    end.
    

db_server(TabId) ->
    receive
        {write, Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client, Pid} -> 
            ets:insert(TabId, #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client}),
            Pid ! ok,
            db_server(TabId);
        {read_all, Pid} -> 
            Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Transactions = form_transactions_from_select_result(Transactions),
            Pid ! Processed_Transactions,
            db_server(TabId);
        {read_by_id, Transaction_Id, Pid} -> 
            Transaction = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            [Processed_Transaction | _] = form_transactions_from_select_result(Transaction),
            Pid ! Processed_Transaction,
            db_server(TabId);
        {asks, Transaction_Id, Ask_Pair, Ask_Rate, Pid} ->
            % We look for asks where the rate is greater than or equal to the calculated ask rate
            Asks = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, 
            [{'and',{'==', '$2', 'ask'},{'==', '$3', Ask_Pair#pair.source_currency},{'==', '$4', Ask_Pair#pair.target_currency}, {'>=', '$6', Ask_Rate}}], 
            [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Asks = form_transactions_from_select_result(Asks),
            Pid ! {matches, Transaction_Id, Processed_Asks},
            db_server(TabId)
    end.


form_transactions_from_select_result([[Transaction_Id, Type, Source_Currency, Target_Currency, Volume, Rate, Client]|T]) ->
    Transaction = #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client},
    [Transaction | form_transactions_from_select_result(T)];
form_transactions_from_select_result([]) ->
    [].
