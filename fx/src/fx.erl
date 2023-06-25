-module(fx).

-include("pair_rate.hrl").

-compile(export_all).
-compile({no_auto_import,[get/1]}).

-type currency() :: eur|usd|cad|gbp|chf|jpy|aud|nzd.
-type exotic_currency() :: atom().
-type pair() :: {currency(),currency()} | {currency(),exotic_currency()} |
          {exotic_currency(),currency()}.
-type exchange_rate() :: float() | undefined.
-type currency_converter() :: pid().
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
    TabId = fx_db:new(),
    fx_server(TabId).


-spec bid(pair(), Volume :: number(), exchange_rate(), client()) -> {ok,id()} | {error, unknown_pair | undefined | range}.
bid(Pair, Volume, Bid_Rate, Client) ->
    Actual_Rate = currency:get(Pair),
    case Actual_Rate of
        {error, Reason} ->
            {error, Reason};
        {ok, Rate} ->
            case (abs(Bid_Rate - Rate) / Rate) < 0.05 of
                false -> {error, range};
                true -> 
                    fx ! {bid, Pair, Volume, Bid_Rate, Client, self()},
                    receive
                        Response ->
                            io:format("Response: ~p ~n", [Response]),
                            ok
                    end
            end
    end.

-spec notification(client(), Notification :: {ask | get, id(), Volume :: integer(), Amount :: float()}) -> ok.
notification(Client, Notification) -> 
    ok.



fx_server(TabId) ->
    receive
        {bid, Pair, Volume, Bid_Rate, Client, Pid} ->
            Transaction_Id = 1,
            Response = fx_db:write(Transaction_Id, Pair, Volume, Bid_Rate, Client, TabId),
            Pid ! Response,
            fx_server(TabId);
        {read_all, Pid} ->
            Response = fx_db:read_all(TabId),
            Pid ! Response,
            fx_server(TabId)
    end.


