-module(currency).

-include("pair_rate.hrl").

-compile(export_all).

-type currency() :: usd|eur|usd|cad|gbp|chf|jpy|aud|nzd.
-type exotic_currency() :: atom().
-type pair() :: {currency(),currency()} | {currency(),exotic_currency()} |
          {exotic_currency(),currency()}.
-type exchange_rate() :: float() | undefined.
-type currency_converter() :: pid().
% -type id() :: ref().
% -type client() :: pid() | {alias(), node()}.
% -type alias() :: atom().


start_link(Currencies) ->
    CurrencyConverter = spawn(?MODULE, init, [Currencies]),
    register(currency_converter, CurrencyConverter).

init(Currencies) ->
    TabId = currency_db:new(),
    generate_pair_rates(Currencies, TabId),
    currency_server(TabId).

generate_pair_rates([H|T], TabId) when H =/= usd ->
    currency_db:write(H, usd, undefined, TabId),
    generate_pair_rates(T, TabId);
generate_pair_rates([H|T], TabId) when H == usd ->
    generate_pair_rates(T, TabId);
generate_pair_rates([], _) ->
    [].

print() ->
    currency_converter ! print.

read(Source_Currency, Target_Currency) ->
    currency_converter ! {read, Source_Currency, Target_Currency}.

set({Source_Currency, Target_Currency}, Rate) when (Source_Currency == usd) and (Target_Currency =/= usd) ->
    set({Target_Currency, Source_Currency}, Rate);
set({Source_Currency, Target_Currency}, Rate) ->
    currency_converter ! {set, Source_Currency, Target_Currency, Rate}.



currency_server(TabId) ->
    receive 
        {read, Source_Currency, Target_Currency} ->
            Rate = currency_db:read(Source_Currency, Target_Currency, TabId),
            io:format("Rate received ~p ~n", [Rate]),
            currency_server(TabId);
        {set, Source_Currency, Target_Currency, Rate} -> 
            currency_db:write(Source_Currency, Target_Currency, Rate, TabId),
            currency_server(TabId)
    end.
