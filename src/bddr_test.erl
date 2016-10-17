-module(bddr_test).
-export([test/3, test/4]).

test(Given, When, Then) ->
    test(Given, When, Then, fun null_teardown/1).

test(Given, When, Then, Teardown) ->
    Environment = run_given(Given),
    Result = run_test(Environment, When, Then),
    Teardown(Environment),
    return(Result).

%% Internal

null_teardown(_) -> ok.

run_given(G) when is_function(G, 0) -> G();
run_given(G) -> G.

run_test(Env, W, T) ->
    try T(W(Env))
    catch Error:R -> {bddr_failure, {Error, R, erlang:get_stacktrace()}}
    end.

return({bddr_failure, Failure}) -> exit(Failure);
return(Result) -> Result.
