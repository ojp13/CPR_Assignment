$ chmod u+x currency_test
%% -*- erlang -*-

main([String]) ->
    compile:file(currency),
    compile:file(currency_db),
    io:format("Inputs: ~p ~n", [String]),
    currency:start_link([gbp, eur, cad, chf]),
    currency:add(gld),
    currency:add(gld),
    currency:add(bnm),
    currency:set({gld, bnm}, 4),
    C1 = currency:get_currency_pairs(gld),
    io:format("Found ~p Pairs: ~p ~n", [gld, C1]),
    Rate1 = currency:get({bnm, gld}),
    io:format("Found Rate: ~p ~n", [Rate1]),
    Pairs = currency:get_paired_and_defined_currencies(usd),
    io:format("USD Pairs: ~p ~n", [Pairs]),
    Path1 = currency:find_path({bnm, gld}),
    io:format("Path1: ~p ~n", [Path1]),
    currency:add(sgp),
    currency:set({sgp, bnm}, 10),
    Path2 = currency:find_path({gld, sgp}),
    io:format("Path 2: ~p ~n", [Path2]),
    Rate2 = currency:get({gld, sgp}),
    io:format("Rate 2: ~p ~n", [Rate2]),
    currency:start_test_server();
main(_) ->
    usage().

usage() ->
    io:format("usage: testing currency functionality").


