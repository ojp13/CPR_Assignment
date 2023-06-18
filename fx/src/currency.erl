-module(currency).

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
    Pairs = generate_pairs(Currencies),
    currency_server(Pairs).

generate_pairs([H|T]) when H =/= usd ->
    [{H, usd} | generate_pairs(T)];
generate_pairs([H|T]) when H == usd ->
    generate_pairs(T);
generate_pairs([]) ->
    [].

print() ->
    currency_converter ! print.

currency_server(State) ->
    io:format("New currency_converter started with pairs ~p ~n", [State]),
    receive 
        print -> 
            io:format("currency_converter with pairs ~p ~n", [State]),
            currency_server(State)
    end.
