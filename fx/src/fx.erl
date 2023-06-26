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
            io:format("Found rate: ~p ~n", [Rate]),
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
                            matching_server ! {match_bids_to_asks, Transaction_Id}
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
                            Client ! {ok, Transaction_Id}
                    end
            end
    end.

read_all_transactions(Client) ->
    fx ! {read_all, self()},
    receive 
        {ok, Response} ->
            Client ! Response
    end.

-spec notification(client(), Notification :: {ask | get, id(), Volume :: integer(), Amount :: float()}) -> ok.
notification(Client, Notification) -> 
    ok.

fx_server(Transcation_Id_Counter) ->
    receive
        {bid, Pair, Volume, Bid_Rate, Client, Pid} ->
            Transcation_Id_Counter,
            fx_db:write(Transcation_Id_Counter, bid, Pair, Volume, Bid_Rate, Client),
            Pid ! {ok, Transcation_Id_Counter},
            fx_server(Transcation_Id_Counter + 1);
        {ask, Pair, Volume, Ask_Rate, Client, Pid} ->
            Transcation_Id_Counter,
            fx_db:write(Transcation_Id_Counter, ask, Pair, Volume, Ask_Rate, Client),
            Pid ! {ok, Transcation_Id_Counter},
            fx_server(Transcation_Id_Counter + 1);
        {read_all, Pid} ->
            All_Transactions = fx_db:read_all(),
            io:format("All Transactions Found: ~p ~n", [All_Transactions]),
            Pid ! {ok, All_Transactions},
            fx_server(Transcation_Id_Counter)
    end.

matching_server() ->
    receive
        {match_bids_to_asks, Transaction_Id} ->
            io:format("Attempting to match bid with Transaction Id: ~p ~n", [Transaction_Id]),
            Transaction = fx_db:read_by_id(Transaction_Id),
            case Transaction of 
                [] ->
                    matching_server();
                Result ->
                    io:format("Transaction Found: ~p ~n", [Result]),
                    Bid_Pair = Result#transaction.pair,
                    Bid_Rate = Result#transaction.rate,
                    Matching_Asks = find_matching_asks(Transaction_Id, Bid_Pair, Bid_Rate),
                    io:format("Matching Transactions Found: ~n ~p ~n", [Matching_Asks]),
                    matching_server()
            end
    end.

find_matching_asks(Transaction_Id, Bid_Pair, Bid_Rate) ->
    Ask_Pair = #pair{source_currency=Bid_Pair#pair.target_currency, target_currency=Bid_Pair#pair.source_currency},
    Ask_Rate = 1 / Bid_Rate,
    Matching_Asks = fx_db:find_asks(Transaction_Id, Ask_Pair, Ask_Rate)
    case Matching_Asks of 
        [] -> {ok, no_matches_found};
        Matches ->
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
    .

    

% free(TabId, Transcation_Id_Counter) ->
%     receive
%         {wait, RequestingPid} ->
%             catch link(RequestingPid),
%             RequestingPid ! ok,
%             busy(RequestingPid, TabId, Transcation_Id_Counter)
%     end.

% busy(RequestingPid, TabId, Transcation_Id_Counter) ->
%     receive
%         {bid, RequestingPid, Pair, Volume, Bid_Rate, Client, Pid} ->
%             Transaction_Id = Transcation_Id_Counter,
%             fx_db:write(Transaction_Id, bid, Pair, Volume, Bid_Rate, Client, TabId),
%             Pid ! {ok, Transaction_Id},
%             busy(RequestingPid, TabId, Transcation_Id_Counter + 1);
%         {read_by_id, RequestingPid, Transaction_Id} ->
%             Transaction = fx_db:read_by_id(TabId, Transaction_Id),
%             RequestingPid ! {ok, Transaction},
%             busy(RequestingPid, TabId, Transcation_Id_Counter);
%         {read_all, RequestingPid, Pid} ->
%             Response = fx_db:read_all(TabId),
%             Pid ! Response,
%             busy(RequestingPid, TabId, Transcation_Id_Counter);
%         {signal, RequestingPid} ->
%             unlink(RequestingPid),
%             RequestingPid ! ok,
%             free(TabId, Transcation_Id_Counter);
%         {'EXIT', RequestingPid, _} ->
%             free(TabId, Transcation_Id_Counter)
%     end.