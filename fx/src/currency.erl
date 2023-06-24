-module(currency).

-include("pair_rate.hrl").

-compile(export_all).
-compile({no_auto_import,[get/1]}).

-type currency() :: usd|eur|usd|cad|gbp|chf|jpy|aud|nzd.
-type exotic_currency() :: atom().
-type pair() :: {currency(),currency()} | {currency(),exotic_currency()} |
          {exotic_currency(),currency()}.
-type exchange_rate() :: float() | undefined.
-type currency_converter() :: pid().
% -type id() :: ref().
% -type client() :: pid() | {alias(), node()}.
% -type alias() :: atom().


-spec start_link(Currencies :: [currency()]) -> {ok,currency_converter()}.
start_link(Currencies) ->
    CurrencyConverter = spawn(?MODULE, init, [Currencies]),
    register(currency_converter, CurrencyConverter),
    {ok, currency_converter}.

init(Currencies) ->
    TabId = currency_db:new(),
    generate_pair_rates(Currencies, TabId),
    currency_server(TabId).

generate_pair_rates([H|T], TabId) when H =/= usd ->
    currency_db:write(usd, H, undefined, TabId),
    generate_pair_rates(T, TabId);
generate_pair_rates([H|T], TabId) when H == usd ->
    generate_pair_rates(T, TabId);
generate_pair_rates([], _) ->
    [].

-spec get(pair()) -> {error,unknown_pair|undefined}|{ok,exchange_rate()}.
get({Source_Currency, Target_Currency}) when (Source_Currency =/= usd) and (Target_Currency =/= usd) ->
    First_Rate_Response = get({Source_Currency, usd}),
    Second_Rate_Response = get({Target_Currency, usd}),
    case {First_Rate_Response, Second_Rate_Response} of
        {{error, _}, {error, _}} -> 
            {error, "Neither Component Rate Defined"};
        {{error, _}, _} -> 
            {error, "Source Currency Rate Not Defined"};
        {_, {error, _}} -> 
            {error, "Target Currency Source Rate Defined"};
        {{_, First_Rate}, {_, Second_Rate}} -> 
            First_Rate / Second_Rate
    end;
get({Source_Currency, Target_Currency}) ->
    currency_converter ! {read, Source_Currency, Target_Currency, self()},
    receive
        {_, {error, instance}} -> 
            get(reverse, {Source_Currency, Target_Currency});
        {_, undefined} ->
            {error, undefined};
        {_, Rate} ->
            {ok, Rate}
    end.

get(reverse, {Source_Currency, Target_Currency}) ->
    currency_converter ! {read, Target_Currency, Source_Currency, self()},
    receive
        {_, {error, instance}} -> 
            {error, instance};
        {_, undefined} ->
            {error, undefined};
        {_, Rate} ->
            {ok, 1 / Rate}
    end.

-spec set(pair(),exchange_rate()) -> {error,unknown_pair|not_usd}|ok.
set({Source_Currency, Target_Currency}, _) when (Source_Currency =/= usd) and (Target_Currency =/= usd) ->
    {error, not_usd};
set({Source_Currency, Target_Currency}, Rate) when (Source_Currency =/= usd) and (Target_Currency == usd) ->
    set({Target_Currency, Source_Currency}, 1 / Rate);
set({Source_Currency, Target_Currency}, Rate) ->
    % Check if the rate exists
    case get({Source_Currency, Target_Currency}) of
        {error, instance} -> 
            {error, unknown_pair};
        _ ->      
            currency_converter ! {set, Source_Currency, Target_Currency, Rate, self()},
            receive
                _ -> 
                    ok
            end
    end.

currency_server(TabId) ->
    receive 
        {read, Source_Currency, Target_Currency, Pid} ->
            Rate = currency_db:read(Source_Currency, Target_Currency, TabId),
            Message = {{Source_Currency, Target_Currency}, Rate},
            Pid ! Message,
            currency_server(TabId);
        {set, Source_Currency, Target_Currency, Rate, Pid} -> 
            Response = currency_db:write(Source_Currency, Target_Currency, Rate, TabId),
            Pid ! Response,
            currency_server(TabId)
    end.
