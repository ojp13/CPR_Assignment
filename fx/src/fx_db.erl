-module(fx_db).

-export([new/0]).

-include("pair_rate.hrl").

new() -> 
    Fx_Tab_Id = ets:new(?MODULE, [bag, named_table, {keypos, #transaction.transaction_id}]),
    Fx_Tab_Id.