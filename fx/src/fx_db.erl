-module(fx_db).

-export([new/0, write/6, read_all/0, read_by_id/1, init/0, find_asks/3, find_bids/3, update_volume/2, delete_transaction/1, wait/1, signal/1, delete_for_client/2, read_by_pair/1]).

-include("pair_rate.hrl").

new() -> 
    Db = spawn(?MODULE, init, []),
    register(transaction_server, Db),
    {ok, db}.

init() ->
    % Use an ordered set so that results are always returned in first-last insertion order
    Fx_Tab_Id = ets:new(?MODULE, [ordered_set, named_table, {keypos, #transaction.transaction_id}]),
    free(Fx_Tab_Id).

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
        {read_by_id_result, Response} -> Response
    end.

read_by_pair({Source_Currency, Target_Currency}) -> 
    transaction_server ! {read_by_pair, {Source_Currency, Target_Currency}, self()},
    receive
        {read_by_pair_result, Response} -> Response
    end.

find_asks(Transaction_Id, Ask_Pair, Ask_Rate) -> 
    transaction_server ! {asks, Transaction_Id, Ask_Pair, Ask_Rate, self()},
    receive
        {matches, Transaction_Id, Response} -> Response
    end.

find_bids(Transaction_Id, Bid_Pair, Bid_Rate) -> 
    transaction_server ! {bids, Transaction_Id, Bid_Pair, Bid_Rate, self()},
    receive
        {matches, Transaction_Id, Response} -> Response
    end.
    
update_volume(Transaction_Id, New_Volume) -> 
    transaction_server ! {update_volume, Transaction_Id, New_Volume, self()},
    receive
        Response -> Response
    end.

delete_transaction(Transaction_Id) -> 
    transaction_server ! {delete, Transaction_Id, self()},
    receive
        Response -> Response
    end.

delete_for_client(Transaction_Id, Pid) ->
    transaction_server ! {delete_for_client, Transaction_Id, Pid, self()},
    receive
        Response -> Response
    end.

wait(RequestingPid) ->
    transaction_server ! {wait, RequestingPid}.

signal(RequestingPid) ->
    transaction_server ! {signal, RequestingPid}.

free(TabId) ->
    receive
        {wait, RequestingPid} ->
            catch link(RequestingPid),
            RequestingPid ! lock_achieved,
            busy(RequestingPid, TabId)
    end.

busy(Pid, TabId) ->
    receive
        {write, Transaction_Id, Type, {Source_Currency, Target_Currency}, Volume, Rate, Client, Pid} -> 
            ets:insert(TabId, #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client}),
            Pid ! ok,
            busy(Pid,TabId);
        {read_all, Pid} -> 
            Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Transactions = form_transactions_from_select_result(Transactions),
            Pid ! Processed_Transactions,
            busy(Pid,TabId);
        {read_by_id, Transaction_Id, Pid} -> 
            Transaction = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            [Processed_Transaction | _] = form_transactions_from_select_result(Transaction),
            Pid ! {read_by_id_result, Processed_Transaction},
            busy(Pid,TabId);
        {read_by_pair, {Source_Currency, Target_Currency}, Pid} -> 
            Transactions = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, 
            [{'or',
                {'and',{'==', '$2', bid},{'==', '$3', Source_Currency},{'==', '$4', Target_Currency}},
                {'and',{'==', '$2', ask},{'==', '$3', Target_Currency},{'==', '$4', Source_Currency}}
            }], 
            [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Transactions = form_transactions_from_select_result(Transactions),
            Pid ! {read_by_pair_result, Processed_Transactions},
            busy(Pid,TabId);
        {update_volume, Transaction_Id, New_Volume, Pid} ->
            ets:update_element(TabId, Transaction_Id, {#transaction.volume, New_Volume}),
            Transaction = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            [New_Bid | _] = form_transactions_from_select_result(Transaction),
            Pid ! New_Bid,
            busy(Pid,TabId);
        {delete, Transaction_Id, Pid} ->
            Transaction = ets:select_delete(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [true]}]),
            Pid ! Transaction,
            busy(Pid,TabId);
        {asks, Transaction_Id, Ask_Pair, Ask_Rate, Pid} ->
            % We look for asks where the rate is greater than or equal to the calculated ask rate
            Asks = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, 
            [{'and',{'==', '$2', 'ask'},{'==', '$3', Ask_Pair#pair.source_currency},{'==', '$4', Ask_Pair#pair.target_currency}, {'>=', '$6', Ask_Rate}}], 
            [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Asks = form_transactions_from_select_result(Asks),
            Pid ! {matches, Transaction_Id, Processed_Asks},
            busy(Pid,TabId);
        {bids, Transaction_Id, Bid_Pair, Bid_Rate, Pid} ->
            % We look for asks where the rate is greater than or equal to the calculated ask rate
            Bids = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, 
            [{'and',{'==', '$2', 'bid'},{'==', '$3', Bid_Pair#pair.source_currency},{'==', '$4', Bid_Pair#pair.target_currency}, {'>=', '$6', Bid_Rate}}], 
            [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            Processed_Bids = form_transactions_from_select_result(Bids),
            Pid ! {matches, Transaction_Id, Processed_Bids},
            busy(Pid,TabId);
        {delete_for_client, Transaction_Id, Client_Pid, Pid} ->
            Transaction = ets:select(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, [{'==', '$1', Transaction_Id}], [['$1', '$2', '$3', '$4', '$5', '$6', '$7']]}]),
            [Processed_Transaction | _] = form_transactions_from_select_result(Transaction),
            ets:select_delete(TabId, [{{'_', '$1', '$2', {'_', '$3', '$4'}, '$5', '$6', '$7'}, 
            [{'and',{'==', '$1', Transaction_Id},{'==', '$7', Client_Pid}}], 
            [true]}]),
            Pid ! Processed_Transaction,
            busy(Pid,TabId);
        {signal, Pid} ->
            unlink(Pid),
            Pid ! lock_removed,
            free(TabId);
        {'EXIT', Pid, _} ->
            free(TabId)
    end.

form_transactions_from_select_result([[Transaction_Id, Type, Source_Currency, Target_Currency, Volume, Rate, Client]|T]) ->
    Transaction = #transaction{transaction_id = Transaction_Id, type=Type, pair=#pair{source_currency=Source_Currency, target_currency=Target_Currency}, volume=Volume, rate=Rate, client_id=Client},
    [Transaction | form_transactions_from_select_result(T)];
form_transactions_from_select_result([]) ->
    [].
