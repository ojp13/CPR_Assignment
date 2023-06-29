$ chmod u+x currency_test
%% -*- erlang -*-

main([String]) ->
    [Arg1 | _] = [String],
    io:format("First Argument: ~p ~n", [Arg1]),
    compile:file(currency),
    compile:file(currency_db),
    compile:file(fx),
    compile:file(fx_db),
    compile:file(bot),
    case Arg1 of
        "currency_1" ->
            currency:start_link([gbp, eur, cad, chf]),
            currency:add(gld),
            currency:add(bnm),
            currency:set({gld, bnm}, 4),
            C1 = currency:get_all_pairs(),
            io:format("All Currencies: ~p ~n", [C1]);
        "currency_rate_setting" ->
            currency:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            currency:start_rate_setting_server(),
            io:format("My Pid is: ~p ~n", [self()]),
            receive
                {endme, ok} ->
                    ok
            end;
        "currency_test_server" -> 
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
        "fx_bid_case_1" -> 
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            currency:set({eur, usd}, 0.91),
            timer:sleep(200),
            fx:ask({eur, usd}, 1000, 1.1, self()),
            receive _ -> ok end,
            fx:bid({usd, eur}, 500*0.91, 0.91, self()),
            receive
                Response1 -> 
                    io:format("Response 1: ~p ~n", [Response1])
            end,
            fx:read_all_transactions(self()),
            timer:sleep(2000),
            fx:read_all_transactions(self()),
            wait_for_notifications();
        "fx_bid_case_3" -> 
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            currency:set({eur, usd}, 1),
            timer:sleep(200),
            fx:ask({eur, usd}, 100, 1, self()),
            receive {ok, _} -> ok end,
            fx:bid({usd, eur}, 50, 1, self()),
            receive {ok, _} -> ok end,
            fx:read_all_transactions(self()),
            fx:bid({usd, eur}, 50, 1, self()),
            receive {ok, _} -> ok end,
            fx:read_all_transactions(self()),
            timer:sleep(2000),
            fx:read_all_transactions(self()),
            wait_for_notifications();
        "fx_ask_case_3" -> 
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            currency:set({eur, usd}, 1),
            timer:sleep(200),
            fx:bid({eur, usd}, 100, 1, self()),
            receive {ok, _} -> ok end,
            fx:ask({usd, eur}, 50, 1, self()),
            receive {ok, _} -> ok end,
            fx:read_all_transactions(self()),
            fx:ask({usd, eur}, 50, 1, self()),
            receive {ok, _} -> ok end,
            fx:read_all_transactions(self()),
            timer:sleep(2000),
            fx:read_all_transactions(self()),
            wait_for_notifications();
        "fx_cancel" -> 
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            {ok, Rate} = currency:get({usd, eur}),
            io:format("Found Rate: ~p ~n", [Rate]),
            fx:bid({eur, usd}, 100, Rate, self()),
            receive {ok, _} -> ok end,
            Cancel = fx:cancel(1),
            io:format("Transcation Cancelled: ~p ~n", [Cancel]),
            wait_for_notifications();
        "fx_orders" -> 
            fx:start_link([gbp, eur, cad, chf]),
            timer:sleep(200),
            {ok, EurRate} = currency:get({usd, eur}),
            io:format("Found eur Rate: ~p ~n", [EurRate]),
            fx:bid({eur, usd}, 100, EurRate*0.97, self()),
            receive {ok, _} -> ok end,
            fx:ask({usd, eur}, 100, (1/EurRate)*1.03, self()),
            receive {ok, _} -> ok end,
            Orders = fx:orders({eur, usd}),
            io:format("Found Orders: ~p ~n", [Orders]),
            wait_for_notifications();
        "bots_1" ->
            fx:start_link([eur, usd, cad, gbp, chf, jpy, aud, nzd]),
            timer:sleep(2000),
            bot:start_bot([eur, usd, cad, gbp, chf, jpy, aud, nzd], 1000000, node()),
            % bot:start_bot([eur, usd, cad, gbp, chf, jpy, aud, nzd], 1000000, 'fx@DESKTOP-LIMML5C').,
            wait_for_notifications()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: testing currency functionality").

wait_for_notifications() ->
    receive 
        {ask, Transaction_id, Volume, Amount} ->
            io:format("Ask Notification: Transaction: ~p, Volume: ~p, Amount: ~p ~n", [Transaction_id, Volume, Amount]),
            wait_for_notifications();
        {bid, Transaction_id, Volume, Amount} ->
            io:format("Bid Notification: Transaction: ~p, Volume: ~p, Amount: ~p ~n", [Transaction_id, Volume, Amount]),
            wait_for_notifications()
    end.


