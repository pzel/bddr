-module(bddr).
-export([run_suite/1]).
-export([test/3, test/4]).

run_suite(FileName) ->
    bddr_suite:run_suite(FileName).

test(Given, When, Then) ->
    bddr_test:test(Given, When, Then).

test(Given, When, Then, Teardown) ->
    bddr_test:test(Given, When, Then, Teardown).
