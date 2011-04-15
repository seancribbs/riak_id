-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, {idx :: partition(),
                machine_id :: binary(),
                sequence = 0 :: non_neg_integer(),
                last_timestamp :: tuple() }).
