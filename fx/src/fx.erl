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
            % E.g. {eur, usd} rate of 4 gives 4 usd per eur
            % A bid of {{eur, usd}, volume = 1, rate = 4} means I want to buy at a rate of 4 eur per usd
            case (abs((1/Bid_Rate) - Rate) / Rate) < 0.05 of
                false -> Client ! {error, range};
                true -> 
                    % We've performed all necessary checks, now we can add the bid
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
                    fx ! {ask, Pair, Volume, Ask_Rate, Client, self()},
                    receive
                        {ok, Transaction_Id} ->
                            Client ! {ok, Transaction_Id},
                            matching_server ! {match_bids_to_asks, Transaction_Id}
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
    io:format("Starting Matching Server ~n"),
    receive
        {match_bids_to_asks, Transaction_Id} ->
            Transaction = fx_db:read_by_id(Transaction_Id),
            io:format("Transaction Found: ~p ~n", [Transaction]),
            matching_server()
    end.


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