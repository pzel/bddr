-module(bddr_test).
-export([test/3, test/4]).

test(Given, When, Then) ->
    test(Given, When, Then, fun null_teardown/1).

test(Given, When, Then, Teardown) ->
    Environment = run_given(Given),
    Result = run_test(Environment, When, Then),
    _ = (catch Teardown(Environment)),
    return(Result).

%% Internal

null_teardown(_) -> ok.

run_given(G) when is_function(G, 0) -> G();
run_given(G) -> G.

run_test(E, W, T) ->
    try T(W(E))
    catch E:R -> {bddr_failure, {E, R}}
    end.

return({bddr_failure, {exit, E}}) -> exit(E);
return({bddr_failure, {throw, T}}) -> throw(T);
return({bddr_failure, {error, E}}) -> error(E);
return(Result) -> Result.

