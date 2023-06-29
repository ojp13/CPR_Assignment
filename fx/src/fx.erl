-module(fx).

-include("pair_rate.hrl").

-compile(export_all).
-compile({no_auto_import,[get/1]}).

-type currency() :: eur|usd|cad|gbp|chf|jpy|aud|nzd.
-type exotic_currency() :: atom().
-type pair() :: {currency(),currency()} | {currency(),exotic_currency()} |
          {exotic_currency(),currency()}.
-type exchange_rate() :: float() | undefined.
-type fx() :: pid().
-type id() :: ref().
-type ref() :: atom().
-type client() :: pid().
% -type client() :: pid() | {alias(), node()}.
% -type alias() :: atom().

-spec start_link(Currencies :: [currency()]) -> {ok,fx()}.
start_link(Currencies) ->
    FX = spawn(?MODULE, init, [Currencies]),
    register(fx, FX),
    {ok, fx}.

init(Currencies) ->
    currency:start_link(Currencies),
    fx_db:new(),
    currency:start_rate_setting_server(),
    Matching_Server = spawn(?MODULE, matching_server, []),
    register(matching_server, Matching_Server),
    fx_server(1).


-spec bid(pair(), Volume :: number(), exchange_rate(), client()) -> {ok,id()} | {error, unknown_pair | undefined | range}.
bid(Pair, Volume, Bid_Rate, Client) ->
    Actual_Rate = currency:get(Pair),
    case Actual_Rate of
        {error, Reason} ->
            Client ! {error, Reason};
        {ok, Rate} ->
            % It has to be 1 / Bid Rate as rates are specified as their inverse
            case (abs((1/Bid_Rate) - Rate) / Rate) < 0.05 of
                false -> Client ! {error, range};
                true -> 
                    % We've performed all necessary checks, now we can add the bid
                    % E.g. bid pair={usd, eur} volume=1000 rate=4 | Buying X eur with 1000 usd at 4 usd per eur. X >= 250
                    fx ! {bid, Pair, Volume, Bid_Rate, Client, self()},
                    receive
                        {ok, Transaction_Id} ->
                            Client ! {ok, Transaction_Id},
                            io:format("Received Confirmation of Bid ~p ~n", [Transaction_Id]),
                            matching_server ! {match_bid_to_asks, Transaction_Id}
                    end
            end
    end.


ask(Pair, Volume, Ask_Rate, Client) ->
    Actual_Rate = currency:get(Pair),
    case Actual_Rate of
        {error, Reason} ->
            Client ! {error, Reason};
        {ok, Rate} ->
            io:format("Found rate: ~p ~n", [Rate]),
            case (abs((1/Ask_Rate) - Rate) / Rate) < 0.05 of
                false -> Client ! {error, range};
                true -> 
                    % We've performed all necessary checks, now we can add the bid
                    % It has to be 1 / Ask Rate as rates are specified as their inverse
                    % E.g. ask pair={eur, usd} volume=250 rate=0.25 | Selling 250 eur for usd at 0.25 eur per usd. At least 1000 USD
                    fx ! {ask, Pair, Volume, Ask_Rate, Client, self()},
                    receive
                        {ok, Transaction_Id} ->
                            Client ! {ok, Transaction_Id},
                            io:format("Received Confirmation of Ask ~p ~n", [Transaction_Id]),
                            matching_server ! {match_ask_to_bids, Transaction_Id}
                    end
            end
    end.

read_all_transactions(Client) ->
    fx ! {read_all, self()},
    receive 
        {ok, Response} ->
            Client ! Response
    end.

-spec cancel(id()) -> {ok, {ask | get, id(), pair(), Volume :: integer(), exchange_rate()}} | {error, unknown}.
cancel(Transaction_Id) ->
    fx ! {cancel_transaction, Transaction_Id, self()},
    receive
        [] -> {error, unknown_pair};
        Response -> {ok, Response}
    end.
        



fx_server(Transaction_Id_Counter) ->
    receive
        {bid, Pair, Volume, Bid_Rate, Client, Pid} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            fx_db:write(Transaction_Id_Counter, bid, Pair, Volume, Bid_Rate, Client),
            Pid ! {ok, Transaction_Id_Counter},
            fx_db:signal(self()),
            receive lock_removed -> ok end,
            fx_server(Transaction_Id_Counter + 1);
        {ask, Pair, Volume, Ask_Rate, Client, Pid} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            fx_db:write(Transaction_Id_Counter, ask, Pair, Volume, Ask_Rate, Client),
            Pid ! {ok, Transaction_Id_Counter},
            fx_db:signal(self()),
            receive lock_removed -> ok end,
            fx_server(Transaction_Id_Counter + 1);
        {read_all, Pid} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            All_Transactions = fx_db:read_all(),
            io:format("All Transactions Found: ~n ~p ~n", [All_Transactions]),
            Pid ! {ok, All_Transactions},
            fx_db:signal(self()),
            receive lock_removed -> ok end,
            fx_server(Transaction_Id_Counter);
        {cancel_transaction, Transaction_Id, Pid} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            Deleted_Transaction = fx_db:delete_for_client(Transaction_Id, Pid),
            case Deleted_Transaction of
                [] -> Pid ! {error, unknown};
                Response -> Pid ! Response
            end,
            fx_db:signal(self()),
            receive lock_removed -> ok end,
            fx_server(Transaction_Id_Counter)
    end.

matching_server() ->
    receive
        {match_bid_to_asks, Transaction_Id} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            io:format("Attempting to match bid with Transaction Id: ~p ~n", [Transaction_Id]),
            Transaction = fx_db:read_by_id(Transaction_Id),
            case Transaction of 
                [] ->
                    fx_db:signal(self()),
                    receive lock_removed -> ok end,
                    matching_server();
                Bid ->
                    io:format("Transaction Found: ~p ~n", [Bid]),
                    Bid_Pair = Bid#transaction.pair,
                    Bid_Rate = Bid#transaction.rate,
                    find_matching_asks(Transaction_Id, Bid_Pair, Bid_Rate),
                    receive
                        % Have to specify a special pattern here otherwise the match_bids_to_asks were 
                        % processed in this receive
                        {matching_asks, Transaction_Id, Matching_Asks} ->
                            io:format("Matching Transactions Found: ~n ~p ~n", [Matching_Asks]),
                            case Matching_Asks of 
                                [] -> 
                                    fx_db:signal(self()),
                                    receive lock_removed -> ok end,
                                    matching_server();
                                Matches ->
                                    Result = process_asks_against_bid(Bid, Matches),
                                    io:format("Result of consuming asks: ~p ~n", [Result]),
                                    fx_db:signal(self()),
                                    receive lock_removed -> ok end,
                                    matching_server()
                            end
                    end
            end;
        {match_ask_to_bids, Transaction_Id} ->
            fx_db:wait(self()),
            receive lock_achieved -> ok end,
            io:format("Attempting to match ask with Transaction Id: ~p ~n", [Transaction_Id]),
            Transaction = fx_db:read_by_id(Transaction_Id),
            case Transaction of 
                [] ->
                    fx_db:signal(self()),
                    receive lock_removed -> ok end,
                    matching_server();
                Ask ->
                    io:format("Transaction Found: ~p ~n", [Ask]),
                    Ask_Pair = Ask#transaction.pair,
                    Ask_Rate = Ask#transaction.rate,
                    find_matching_bids(Transaction_Id, Ask_Pair, Ask_Rate),
                    receive
                        % Have to specify a special pattern here otherwise the match_bids_to_asks were 
                        % processed in this receive
                        {matching_bids, Transaction_Id, Matching_Bids} ->
                            io:format("Matching Transactions Found: ~n ~p ~n", [Matching_Bids]),
                            case Matching_Bids of 
                                [] -> 
                                    fx_db:signal(self()),
                                    receive lock_removed -> ok end,
                                    matching_server();
                                Matches ->
                                    Result = process_bids_against_ask(Ask, Matches),
                                    io:format("Result of consuming bids: ~p ~n", [Result]),
                                    fx_db:signal(self()),
                                    io:format("Attempting to unlock ~n"),
                                    receive lock_removed -> ok end,
                                    matching_server()
                            end
                    end
            end
    end.

find_matching_asks(Transaction_Id, Bid_Pair, Bid_Rate) ->
    Ask_Pair = #pair{source_currency=Bid_Pair#pair.target_currency, target_currency=Bid_Pair#pair.source_currency},
    Ask_Rate = 1 / Bid_Rate,
    Matching_Asks = fx_db:find_asks(Transaction_Id, Ask_Pair, Ask_Rate),
    matching_server ! {matching_asks, Transaction_Id, Matching_Asks}.

process_asks_against_bid(Bid, [Ask | T]) ->
    % Now we need to work out how much volume to consume from matching asks
    % E.g.
    % bid pair={usd, gbp} volume=200 rate=2 means I am buying gbp with 200 usd at a rate of 2 usd per gbp, meaning I get at least 100 gbp
    % ask pair={gbp, usd} volume=100 rate=0.5 means I am selling gbp for usd as a rate of 0.5 usd per gbp, meaning I get at least 200 usd
    % Bid_Volume = 200, Matching_Ask_Volume = Ask#transaction.volume / Ask#transaction.rate
    % The above transactions match perfectly
    % Next example: 
    % bid pair={usd, gbp} volume=200 rate=2 means I am buying gbp with 200 usd at a rate of 2 usd per gbp, meaning I get at least 100 gbp
    % ask pair={gbp, usd} volume=100 rate=0.52 means I am selling gbp for usd as a rate of 0.52 usd per gbp, meaning I get at least 192 usd
    % At the Ask Rate of 0.52, I would spend (100 * 1/0.52)=192.3 usd to buy 100 gbp. Leaving me with (200-192.3) = 7.7 usd to bid
    %   At this point, the first ask is fully delivered and should be closed. The bid will stay open and I should move to the second ask
    
    % If my bid is {usd, gbp} volume 200, I have 200 usd to spend on gbp 
        % The rate is irrelevant as this point as the Asks passed in will be filtered to a rate better than the bid rate
    % This is the volume of bid currency we have to consume. E.g. 200 usd
    Bid_Volume = Bid#transaction.volume,
    
    % If my ask is {gbp, usd} volume 100 rate 0.5, I am selling 100 gbp, expecting at least 200 usd
    % There are 3 cases: (Assume Ask_Volume of 100, Rate of 0.5)
    % 1. Bid fully consumed - If Bid_Volume = 100, then 50 of my ask volume is consumed
    %       The ask remains but the bid is finished
    % 2. Ask fully consumed - If Bid_Volume = 300, then 100 of my ask volume is consumed and a Bid_Volume of 100 is left
    % 3. Both bid and ask consumed - If Bid_Volume = 200, then my whole ask volume of 100 is consumed
    Starting_Ask_Volume = Ask#transaction.volume,
    Ask_Rate = Ask#transaction.rate,
    Consumed_Ask_Volume = Bid_Volume * Ask_Rate,
    Remaining_Ask_Volume = Starting_Ask_Volume - Consumed_Ask_Volume,
    case Remaining_Ask_Volume > 0 of
        true ->
            % Case 1. Close the bid. Reduce the volume of the ask
            io:format("Case 1 ~n"),
            fx_db:update_volume(Ask#transaction.transaction_id, Remaining_Ask_Volume),
            fx_db:delete_transaction(Bid#transaction.transaction_id),
            Asker_Notification = {ask, Ask#transaction.transaction_id, Consumed_Ask_Volume, Bid_Volume},
            io:format("Asker client id: ~p. Notification: ~p ~n", [Ask#transaction.client_id, Asker_Notification]),
            notification(Ask#transaction.client_id, Asker_Notification),
            Bidder_Notification = {bid, Bid#transaction.transaction_id, Bid_Volume, Consumed_Ask_Volume},
            notification(Bid#transaction.client_id, Bidder_Notification),
            ok;
        false ->
            case Remaining_Ask_Volume =/= 0 of
                true ->
                    % Case 2. Close the ask. Reduce the volume of the bid.
                    % Ensure the volume consumed is the Ask Volume
                    io:format("Case 2 ~n"),
                    Updated_Bid = fx_db:update_volume(Bid#transaction.transaction_id, Bid_Volume - (Starting_Ask_Volume / Ask_Rate)),
                    fx_db:delete_transaction(Ask#transaction.transaction_id),
                    Asker_Notification = {ask, Ask#transaction.transaction_id, Starting_Ask_Volume, Starting_Ask_Volume / Ask_Rate},
                    notification(Ask#transaction.client_id, Asker_Notification),
                    Bidder_Notification = {bid, Bid#transaction.transaction_id, Starting_Ask_Volume / Ask_Rate, Starting_Ask_Volume},
                    notification(Bid#transaction.client_id, Bidder_Notification),
                    io:format("Reprocessing the bid: ~p ~n", [Updated_Bid]),
                    process_asks_against_bid(Updated_Bid, T);
                false ->
                    % Case 3. Close both orders
                    io:format("Case 3 ~n"),
                    fx_db:delete_transaction(Bid#transaction.transaction_id),
                    fx_db:delete_transaction(Ask#transaction.transaction_id),
                    Asker_Notification = {ask, Ask#transaction.transaction_id, Starting_Ask_Volume, Bid_Volume},
                    notification(Ask#transaction.client_id, Asker_Notification),
                    Bidder_Notification = {bid, Bid#transaction.transaction_id, Bid_Volume, Starting_Ask_Volume},
                    notification(Bid#transaction.client_id, Bidder_Notification),
                    ok
            end
    end;
process_asks_against_bid(_, []) ->
    ok.


find_matching_bids(Transaction_Id, Ask_Pair, Ask_Rate) ->
    io:format("Ask Pair: ~p ~n", [Ask_Pair]),
    io:format("Ask Rate: ~p ~n", [Ask_Rate]),
    Bid_Pair = #pair{source_currency=Ask_Pair#pair.target_currency, target_currency=Ask_Pair#pair.source_currency},
    Bid_Rate = 1 / Ask_Rate,
    Matching_Bids = fx_db:find_bids(Transaction_Id, Bid_Pair, Bid_Rate),
    io:format("Matching Bids: ~p ~n", [Matching_Bids]),
    matching_server ! {matching_bids, Transaction_Id, Matching_Bids}.

process_bids_against_ask(Ask, [Bid | T]) ->
    Ask_Volume = Ask#transaction.volume,
    
    Starting_Bid_Volume = Bid#transaction.volume,
    Bid_Rate = Bid#transaction.rate,
    Consumed_Bid_Volume = Ask_Volume * Bid_Rate,
    Remaining_Bid_Volume = Starting_Bid_Volume - Consumed_Bid_Volume,
    case Remaining_Bid_Volume > 0 of
        true ->
            % Case 1. Close the ask. Reduce the volume of the bid
            io:format("Case 1 ~n"),
            fx_db:update_volume(Bid#transaction.transaction_id, Remaining_Bid_Volume),
            fx_db:delete_transaction(Ask#transaction.transaction_id),
            Asker_Notification = {ask, Ask#transaction.transaction_id, Ask_Volume, Consumed_Bid_Volume},
            io:format("Asker client id: ~p. Notification: ~p ~n", [Ask#transaction.client_id, Asker_Notification]),
            notification(Ask#transaction.client_id, Asker_Notification),
            Bidder_Notification = {bid, Bid#transaction.transaction_id, Consumed_Bid_Volume, Ask_Volume},
            notification(Bid#transaction.client_id, Bidder_Notification),
            ok;
        false ->
            case Remaining_Bid_Volume =/= 0 of
                true ->
                    % Case 2. Close the bid. Reduce the volume of the ask.
                    % Ensure the volume consumed is calculated from the Bid Volume
                    io:format("Case 2 ~n"),
                    Updated_Ask = fx_db:update_volume(Ask#transaction.transaction_id, Ask_Volume - (Starting_Bid_Volume / Bid_Rate)),
                    fx_db:delete_transaction(Bid#transaction.transaction_id),
                    Asker_Notification = {ask, Ask#transaction.transaction_id, (Starting_Bid_Volume / Bid_Rate), Starting_Bid_Volume},
                    notification(Ask#transaction.client_id, Asker_Notification),
                    Bidder_Notification = {bid, Bid#transaction.transaction_id, Starting_Bid_Volume, (Starting_Bid_Volume / Bid_Rate)},
                    notification(Bid#transaction.client_id, Bidder_Notification),
                    io:format("Reprocessing the ask: ~p ~n", [Updated_Ask]),
                    process_asks_against_bid(Updated_Ask, T);
                false ->
                    % Case 3. Close both orders
                    io:format("Case 3 ~n"),
                    fx_db:delete_transaction(Bid#transaction.transaction_id),
                    fx_db:delete_transaction(Ask#transaction.transaction_id),
                    Asker_Notification = {ask, Ask#transaction.transaction_id, Ask_Volume, Ask_Volume*Bid_Rate},
                    notification(Ask#transaction.client_id, Asker_Notification),
                    Bidder_Notification = {bid, Bid#transaction.transaction_id, Starting_Bid_Volume, Ask_Volume},
                    notification(Bid#transaction.client_id, Bidder_Notification),
                    ok
            end
    end;
process_bids_against_ask(_, []) ->
    ok.


-spec notification(client(), Notification :: {ask | get, id(), Volume :: integer(), Amount :: float()}) -> ok.
notification(Client, Notification) -> 
    Client ! Notification.
        
