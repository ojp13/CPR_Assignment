-record(pair_rate, {pair, rate}).
-record(pair, {source_currency, target_currency}).

-record(transaction, {transaction_id, type, pair, volume, rate, client_id}).