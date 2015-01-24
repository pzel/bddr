-module(bddr).
-export([test/3, test/4]).

test(Given, When, Then) ->
    bddr_test:test(Given, When, Then).

test(Given, When, Then, Teardown) ->
    bddr_test:test(Given, When, Then, Teardown).
