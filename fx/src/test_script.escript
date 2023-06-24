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
    C1 = currency:get_matching_currencies(gld),
    io:format("Found ~p Pairs: ~p ~n", [gld, C1]),
    Rate1 = currency:get({bnm, gld}),
    io:format("Found Rate: ~p ~n", [Rate1]);
main(_) ->
    usage().

usage() ->
    io:format("usage: testing currency functionality").


