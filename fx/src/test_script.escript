$ chmod u+x currency_test
%% -*- erlang -*-

main([String]) ->
    compile:file(currency),
    compile:file(currency_db),
    io:format("Inputs: ~p ~n", [String]),
    currency:start_link([gbp, eur, cad, chf]),
    currency:start_test_server();
main(_) ->
    usage().

usage() ->
    io:format("usage: testing currency functionality").


