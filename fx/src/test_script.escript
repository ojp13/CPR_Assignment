$ chmod u+x currency_test
%% -*- erlang -*-

main([String]) ->
    [Arg1 | _] = [String],
    io:format("First Argument: ~p ~n", [Arg1]),
    case Arg1 of
        "currency" -> 
            compile:file(currency),
            compile:file(currency_db),
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
        "fx" -> 
            compile:file(currency),
            compile:file(currency_db),
            compile:file(fx),
            compile:file(fx_db),
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            currency:set({gbp, usd}, 2),
            timer:sleep(200),
            fx:ask({usd, gbp}, 200, 2.05, self()),
            receive _ -> ok end,
            fx:ask({usd, gbp}, 200, 2.04, self()),
            receive _ -> ok end,
            fx:ask({usd, gbp}, 200, 2.03, self()),
            receive _ -> ok end,
            fx:ask({usd, gbp}, 200, 1.99, self()),
            receive _ -> ok end,
            fx:ask({usd, gbp}, 200, 1.98, self()),
            receive
                ResponseAsk -> 
                    io:format("Response Ask: ~p ~n", [ResponseAsk])
            end,
            fx:bid({gbp, usd}, 100, 0.5, self()),
            receive
                Response1 -> 
                    io:format("Response 1: ~p ~n", [Response1])
            end,
            fx:read_all_transactions(self()),
            timer:sleep(2000)
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: testing currency functionality").


