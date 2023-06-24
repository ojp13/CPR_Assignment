-module(currency).

-include("pair_rate.hrl").

-compile(export_all).
-compile({no_auto_import,[get/1]}).

-type currency() :: eur|usd|cad|gbp|chf|jpy|aud|nzd.
-type exotic_currency() :: atom().
-type pair() :: {currency(),currency()} | {currency(),exotic_currency()} |
          {exotic_currency(),currency()}.
-type exchange_rate() :: float() | undefined.
-type currency_converter() :: pid().
% -type id() :: ref().
% -type client() :: pid() | {alias(), node()}.
% -type alias() :: atom().

% Type Guards
-spec is_usd(X :: atom()) -> boolean().
is_usd(X) ->
    X == usd.

-spec is_currency(X :: atom()) -> boolean().
is_currency(X) ->
    Currency_List = [usd,eur,usd,cad,gbp,chf,jpy,aud,nzd],
    lists:member(X, Currency_List).

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
set({Source_Currency, Target_Currency}, Rate) -> 
    case is_currency(Source_Currency) and is_currency(Target_Currency) of 
        true -> set(standard, {Source_Currency, Target_Currency}, Rate);
        false -> set(exotic, {Source_Currency, Target_Currency}, Rate)
    end.

set(standard, {Source_Currency, Target_Currency}, _) when (Source_Currency =/= usd) and (Target_Currency =/= usd) ->
    {error, not_usd};
set(standard, {Source_Currency, Target_Currency}, Rate) when (Source_Currency =/= usd) and (Target_Currency == usd) ->
    set(standard, {Target_Currency, Source_Currency}, 1 / Rate);
set(standard, {Source_Currency, Target_Currency}, Rate) ->
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
    end;
set(exotic, {Source_Currency, Target_Currency}, Rate) -> 
    % Check if we have any pairs for these exotic currencies
    % If we can't find a match for either of them, then one of them doesn't exist
    Source_Check = get_matching_currencies(Source_Currency),
    Target_Check = get_matching_currencies(Target_Currency),

    % Exotic pairs are initialised against the usd
    case Source_Check == [] of
        true ->
            {error, source_not_known};
        false ->
            case Target_Check == [] of
                true -> {error, target_not_known};
                false -> 
                    currency_converter ! {set, Source_Currency, Target_Currency, Rate, self()},
                    receive
                        _ -> 
                            ok
                    end
            end
    end.

currency_server(TabId) ->
    receive 
        {write, Source_Currency, Target_Currency, Rate, Pid} -> 
            Response = currency_db:write(Source_Currency, Target_Currency, Rate, TabId),
            Pid ! Response,
            currency_server(TabId);
        {read, Source_Currency, Target_Currency, Pid} ->
            Rate = currency_db:read(Source_Currency, Target_Currency, TabId),
            Message = {{Source_Currency, Target_Currency}, Rate},
            Pid ! Message,
            currency_server(TabId);
        {set, Source_Currency, Target_Currency, Rate, Pid} -> 
            Response = currency_db:write(Source_Currency, Target_Currency, Rate, TabId),
            Pid ! Response,
            currency_server(TabId);
        {read_matches, Currency, Pid} -> 
            Response = currency_db:find_matches(Currency, TabId),
            Pid ! Response,
            currency_server(TabId);
        {remove_exotic, Exotic_Currency, Pid} ->
            Response = currency_db:delete(Exotic_Currency, TabId),
            Pid ! Response,
            currency_server(TabId)
    end.


start_test_server() ->
        TestServer = spawn(?MODULE, test_data_process, []),
        register(test_server, TestServer),
        {ok, currency_converter}.
test_data_process() ->
        Major_Currencies = get_matching_currencies(usd),
        io:format("Found Major Pairs: ~p ~n", [Major_Currencies]),
        Result = generate_major_pair_tests(Major_Currencies),
        io:format("Result of testing: ~p ~n", [Result]),
        timer:sleep(5000),
        test_data_process().

get_matching_currencies(Currency) ->
    currency_converter ! {read_matches, Currency, self()},
    receive
        Response ->
            Response
    end.
    
generate_major_pair_tests([H|T]) -> 
        case generate_test(H) of
            {ok, _} -> 
                generate_major_pair_tests(T);
            Error -> 
                Error
            end;
    generate_major_pair_tests([]) ->
        {ok, testing_complete}.

generate_test(Currency) -> 
    io:format("Testing Major Pair for Currency ~p ~n", [Currency]),
    Major_Rate = rand:uniform(),
    io:format("Major rate for ~p set to ~p ~n", [Currency, Major_Rate]),
    set({usd, Currency}, Major_Rate),
    {_, Set_Rate} = get({usd, Currency}),
    io:format("Major rate for ~p got at ~p ~n", [Currency, Set_Rate]),
    {_, Reverse_Rate} = get({Currency, usd}),
    io:format("Reverse rate for ~p got at ~p ~n", [Currency, Reverse_Rate]),
    case ((abs((1 / Set_Rate) - Reverse_Rate)) < 0.0001) and (abs(Major_Rate - Set_Rate) < 0.0001) of
        true ->
            io:format("Major and Reverse Rate set correctly for ~p ~n", [Currency]),
            {ok, pass};
        false -> 
            io:format("Error in setting rates for ~p ~n", [Currency]),
            {error, failed}
    end.

-spec add(Currency :: exotic_currency()) -> ok.
add(Exotic_Currency) ->
    case is_currency(Exotic_Currency) of
        true -> {error, major_currency};
        false -> 
            currency_converter ! {write, usd, Exotic_Currency, undefined, self()},
            receive
                Response ->
                    Response
            end
    end.

-spec remove(Currency :: exotic_currency()) -> ok.
remove(Exotic_Currency) ->
    case is_currency(Exotic_Currency) of
        true -> {error, major_currency};
        false -> 
            currency_converter ! {remove_exotic, Exotic_Currency, self()},
            receive
                Response ->
                    Response
            end
    end.

