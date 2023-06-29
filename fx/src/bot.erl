-module(bot).

-export([start_bot/3, init/4, loop/6]).



start_bot(Currencies, Amount, Fx_Node) -> 
    spawn_link(?MODULE, init, [Currencies, Amount, Fx_Node,  node()]),
    timer:sleep(4000),
    management_loop_transact().

management_loop_transact() ->
    node() ! transact,
    timer:sleep(8000),
    management_loop_wait().

management_loop_wait() ->
    node() ! cancel_all,
    timer:sleep(4000),
    management_loop_transact().


init(Currencies, Amount, Fx_Node, Name) ->
    register(Name, self()),
    Balances = create_initial_balance(Currencies, Amount),
    io:format("Started ~p ~n", [Name]),
    loop(Balances, Amount*rand:uniform(), [], rand:uniform(), rand:uniform(), Fx_Node).

loop(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node) ->
    Live_Transactions = get_live_transactions(Transactions),
    io:format("Current Live Transactions: ~p ~n", [Live_Transactions]),
    io:format("Current Balances: ~p ~n", [Balances]),
    Current_Usd_Balance = get_usd_balance(Balances, Fx_Node),
    io:format("Current usd balance: ~p ~n", [Current_Usd_Balance]),
    make_bids(Balances, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node),
    Usd_Balance = get_virtual_balance_for_currency(Balances, usd),
    sell_usd(Balances, Usd_Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node),
    waiting(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node).

waiting(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node) ->
    receive 
        {update_virtual_balances, Currency, Delta} ->
            Updated_Balances = update_virtual_balances(Balances, Currency, Delta),
            waiting(Updated_Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        {update_actual_balances, Currency, Delta} ->
            Updated_Balances = update_actual_balances(Balances, Currency, Delta),
            waiting(Updated_Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        {add_to_transactions, Transaction_Id, Type, Pair, Volume} ->
            Updated_Transactions = add_transaction(Transaction_Id, Type, Pair, Volume, Transactions, live),
            waiting(Balances, Lowest_Balance, Updated_Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        {ask, Id, Volume, Amount} ->
            % Bizzarely the notification doesn't include the pair, so we have to work it out
            {Source_Currency, Target_Currency} = find_pair_from_transactions(Id, Transactions),
            % We lost the source currency and gained the target currency.
            self() ! {update_actual_balances, Source_Currency, -Volume}, 
            self() ! {update_actual_balances, Target_Currency, Amount},
            waiting(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        {bid, Id, Volume, Amount} ->
            {Source_Currency, Target_Currency} = find_pair_from_transactions(Id, Transactions),
            % We lost the source currency and gained the target currency.
            self() ! {update_actual_balances, Source_Currency, -Volume}, 
            self() ! {update_actual_balances, Target_Currency, Amount},
            waiting(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        cancel_all ->
            io:format("Cancelling all transactions ~n"),
            Updated_Transactions = cancel_transactions(Transactions, Fx_Node),
            loop(Balances, Lowest_Balance, Updated_Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        transact ->
            loop(Balances, Lowest_Balance, Transactions, Diversification_Factor, Risk_Appetite, Fx_Node);
        stop ->
            io:format("Stopping at request of spawner~n")
    end.



create_initial_balance([Currency | T], Amount) ->
    [{Currency, Amount, Amount} | create_initial_balance(T, Amount)];
create_initial_balance([], _) ->
    [].

make_bids([Currency_Balance | T], Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node) ->
    {Currency, _, Virtual_Balance} = Currency_Balance,
    % Determine how much to put towards a bid at any one time
    Amount_To_Bid = math:floor(Virtual_Balance * (Diversification_Factor * rand:uniform())),
    % io:format("Considering a bid for currency ~p, with current balance ~p and amount to bid of ~p ~n", [Currency, Virtual_Balance, Amount_To_Bid]),
    case (Virtual_Balance - Amount_To_Bid) < Lowest_Balance of
        % If we don't want to bid anymore, move on
        true -> make_bids(T, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
        false ->
            % Amount we want to be close to the current market rate
            % io:format("About to make a bid ~n"),
            Risk_Factor = Risk_Appetite * rand:uniform() * 0.05,
            Current_Rate = rpc:call(Fx_Node, currency, get, [{Currency, usd}]),
            % io:format("Current Rate for ~p of ~p ~n", [{Currency, usd}, Current_Rate]),
            case Current_Rate of
                {error, _} ->
                    make_bids(T, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
                {ok, Rate} ->
                    Bid_Success = make_bid({Currency, usd}, Amount_To_Bid, (1/Rate) * (1-Risk_Factor), Fx_Node),
                    % io:format("Did the bid succeed: ~p ~n", [Bid_Success]),
                    case Bid_Success of
                        {ok, bid_successful} -> 
                            self() ! {update_virtual_balances, Currency, -Amount_To_Bid},
                            make_bids(T, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
                        _ ->
                            make_bids(T, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node)
                    end
            end
    end;
make_bids([], _, _, _, _) ->
    ok. 

make_bid(Pair, Volume, Rate, Fx_Node) ->
    rpc:call(Fx_Node, fx, bid, [Pair, Volume, Rate, self()]),
    % io:format("Waiting for bid response ~n"),
    receive
        {ok, Transaction_Id} ->
            self() ! {add_to_transactions, Transaction_Id, bid, Pair, Volume},
            {ok, bid_successful};
        {error, range} -> {error, range};
        {error, bid_failed} -> {error, bid_failed}
    end.



% Sell my usd for other currencies
sell_usd([Currency_Balance | T], Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node) ->
    {Currency, _, _} = Currency_Balance,
    % Determine how much to put towards a ask at any one time
    Amount_To_Ask = math:floor(Balance * (Diversification_Factor * rand:uniform())),
    % io:format("Considering selling usd for currency ~p, with current balance ~p and amount to ask of ~p ~n", [Currency, Balance, Amount_To_Ask]),
    case (Balance - Amount_To_Ask) < Lowest_Balance of
        % If we don't want to ask anymore, move on
        true -> sell_usd(T, Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
        false ->
            % Amount we want to be close to the current market rate
            % io:format("About to make an ask ~n"),
            Risk_Factor = Risk_Appetite * rand:uniform() * 0.05,
            Current_Rate = rpc:call(Fx_Node, currency, get, [{usd, Currency}]),
            % io:format("Current Rate for ~p of ~p ~n", [{usd, Currency}, Current_Rate]),
            case Current_Rate of
                {error, _} ->
                    sell_usd(T, Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
                {ok, Rate} ->
                    Ask_Success = make_ask({usd, Currency}, Amount_To_Ask, (1/Rate) * (1-Risk_Factor), Fx_Node),
                    % io:format("Did the ask succeed: ~p ~n", [Ask_Success]),
                    case Ask_Success of
                        {ok, ask_successful} -> 
                            self() ! {update_virtual_balances, usd, -Amount_To_Ask},
                            sell_usd(T, Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node);
                        _ ->
                            sell_usd(T, Balance, Lowest_Balance, Diversification_Factor, Risk_Appetite, Fx_Node)
                    end
            end
    end;
sell_usd([], _, _, _, _, _) ->
    ok. 


make_ask(Pair, Volume, Rate, Fx_Node) ->
    rpc:call(Fx_Node, fx, ask, [Pair, Volume, Rate, self()]),
    % io:format("Waiting for ask response ~n"),
    receive
        {ok, Transaction_Id} ->
            self() ! {add_to_transactions, Transaction_Id, ask, Pair, Volume},
            {ok, ask_successful};
        {error, range} -> {error, range};
        {error, ask_failed} -> {error, ask_failed}
    end.


update_virtual_balances([Balance | T], Currency, Delta) ->
    {Balance_Currency, Actual_Balance, Virtual_Balance} = Balance,
    case Balance_Currency of
        Currency ->
            [{Currency, Actual_Balance, Virtual_Balance + Delta} | update_virtual_balances(T, Currency, Delta)];
        _ ->
            [{Balance_Currency, Actual_Balance, Virtual_Balance} | update_virtual_balances(T, Currency, Delta)]
    end;
update_virtual_balances([], _, _) ->
    [].

update_actual_balances([Balance | T], Currency, Delta) ->
    {Balance_Currency, Actual_Balance, Virtual_Balance} = Balance,
    case Balance_Currency of
        Currency ->
            [{Currency, Actual_Balance + Delta, Virtual_Balance } | update_actual_balances(T, Currency, Delta)];
        _ ->
            [{Balance_Currency, Actual_Balance, Virtual_Balance} | update_actual_balances(T, Currency, Delta)]
    end;
update_actual_balances([], _, _) ->
    [].

add_transaction(Transaction_Id, Type, Pair, Volume, Transactions, State) ->
    [{Transaction_Id, Type, Pair, Volume, State} | add_transaction(Transactions)].

add_transaction([H|T]) ->
    [H | add_transaction(T)];
add_transaction([]) ->
    [].

get_virtual_balance_for_currency([Balance | T], Currency) ->
    {Balance_Currency, _, Virtual_Balance} = Balance,
    case Balance_Currency of
        Currency ->
            Virtual_Balance;
        _ ->
            get_virtual_balance_for_currency(T, Currency)
    end;
get_virtual_balance_for_currency([], _) ->
    0.

find_pair_from_transactions(Transaction_Id, [Head_Transaction | T]) ->
    {Id, _, Pair, _, _} = Head_Transaction,
    case Id of
        Transaction_Id -> Pair;
        _ -> find_pair_from_transactions(Transaction_Id, T)
    end;
find_pair_from_transactions(_, []) ->
    error.

cancel_transactions([Head_Transaction | T], Fx_Node) -> 
    {Id, Type, Pair, LocalStateVolume, State} = Head_Transaction,
    % We want to keep track of whether transactions are live or inactive
    case State of
        live -> 
            Response = rpc:call(Fx_Node, fx, cancel, [Id]),
            case Response of
                {error, unknown} ->
                    % The transaction is done and we'll have notifications for it
                    [{Id, Type, Pair, LocalStateVolume, inactive} | cancel_transactions(T, Fx_Node)];
                {ok, {_, _, _, {_, Response_Source_Currency, _}, Volume, _, _}} ->
                    % We need to update the virtual balances to make sure the cancel gets reflected
                    self() ! {update_virtual_balances, Response_Source_Currency, Volume},
                    [{Id, Type, Pair, LocalStateVolume, inactive} | cancel_transactions(T, Fx_Node)]
            end;
        _ ->
            [Head_Transaction | cancel_transactions(T, Fx_Node)]
    end;
cancel_transactions([], _) ->
    [].

get_live_transactions([Head_Transaction | T]) ->
    {_, _, _, _, State} = Head_Transaction,
    case State of
        live ->
            [Head_Transaction | get_live_transactions(T)];
        _ ->
            get_live_transactions(T)
    end;
get_live_transactions([]) ->
    [].

get_usd_balance([Currency_Balance | T], Fx_Node) ->
    {Currency, Balance, _} = Currency_Balance,
    case Currency of 
        usd -> Balance +  get_usd_balance(T, Fx_Node);
        _ -> 
            Current_Rate = rpc:call(Fx_Node, currency, get, [{usd, Currency}]),
            case Current_Rate of
                {error, _} ->
                    get_usd_balance(T, Fx_Node);
                {ok, Rate} ->
                    Usd_Value = Balance * Rate,
                    Usd_Value + get_usd_balance(T, Fx_Node)
            end
    end;
get_usd_balance([], _) ->
    0.