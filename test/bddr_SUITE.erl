-module(bddr_SUITE).
-compile([export_all]).

-define(raises_function_clause(Expr),
               try begin Expr, false end
               catch error:function_clause -> true;
                     E:R -> error({not_function_clause, E, R})
               end).

all() -> [givens_can_be_a_lambda,
          givens_can_be_a_term,

          when_clause_can_access_givens,
          when_cannot_access_givens_bindings,
          when_with_unexpected_givens_raises_function_clause,

          then_clause_can_access_action_result,
          then_clause_matches_on_action_result,
          then_with_unexpected_result_raises_function_clause
         ].

givens_can_be_a_lambda(_) ->
    bddr:test(
      fun() -> A = 1,
               B = A + 1,
               C = B + 1,
               [A,B,C]
      end,
      fun([1,2,3]) -> ok end,
      fun(ok) -> ok end).

givens_can_be_a_term(_) ->
    bddr:test({good, vibrations, 9.99},
              fun({good,_,_}) -> ok end,
              fun(ok) -> ok end).

when_clause_can_access_givens(_) ->
    bddr:test(
      [1, hello],
      fun([1,hello] = _Givens) -> ok end,
      fun(_) -> ok end).

when_cannot_access_givens_bindings(_) ->
    %% The BadTest code will not compile, producing a compiler error like:
    %% your_SUITE.erl:99: variable 'AGiven' is unbound
    %% your_SUITE.erl:98: Warning: variable 'AGiven' is unused
    %% We prove it below, using dynamic_compile.
    true = compiles(
        "-module(good_test).\n"
        "-export([good_test/0]).\n"
        "good_test() ->\n"
        "   bddr:test([],fun(_)->1 end,fun(R)-> 1 = R end).\n"),
    false = compiles(
        "-module(bad_test).\n"
        "-export([bad_test/0]).\n"
        "bad_test() ->\n"
        "   bddr:test([AGiven = 1],fun(_)-> AGiven end,fun(R)-> 1 = R end).\n").

when_with_unexpected_givens_raises_function_clause(_) ->
    true = ?raises_function_clause(
              bddr:test([1,2,3],
                        fun([wrong,args]) -> nay end,
                        fun(_) -> ok end)).

then_clause_can_access_action_result(_) ->
    bddr:test(
      [1],
      fun([Number]) -> Number end,
      fun(Result) -> 1 = Result end).

then_clause_matches_on_action_result(_) ->
    bddr:test(
      [],
      fun(_Givens) -> expected_action_result end,
      fun(expected_action_result) -> ok end).

then_with_unexpected_result_raises_function_clause(_) ->
    true = ?raises_function_clause(
              bddr:test([],
                        fun(_) -> nay end,
                        fun(yay) -> this_will_fail end)).


%% Non-test functions


compiles(Source) ->
    try {ModName, Bin} = dynamic_compile:from_string(Source),
         is_atom(ModName) andalso is_binary(Bin)
    catch throw:_ -> false
    end.
