-module(bddr).
-export([test/3]).

test(Given, When, Then) when is_function(Given, 0) ->
    Then(When(Given()));
test(Given, When, Then) ->
    Then(When(Given)).
