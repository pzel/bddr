-module(bddr_test_SUITE).
-compile([export_all]).
-include_lib("bddr/include/bddr.hrl").

-define(raises_function_clause(Expr),
               try begin Expr, false end
               catch error:function_clause -> true;
                     E:R -> error({not_function_clause, E, R})
               end).

all() -> [ givens_can_be_a_lambda,
           givens_can_be_a_term,

           when_clause_can_access_givens,
           when_cannot_access_givens_bindings,
           when_with_unexpected_givens_raises_function_clause,

           then_clause_can_access_action_result,
           then_clause_matches_on_action_result,
           then_with_unexpected_result_raises_function_clause,

           test_provides_teardown_option,
           teardown_runs_despite_crash_in_when,
           teardown_runs_despite_crash_in_then,
           failed_teardown_is_failed_test,

           given_when_then_as_macros
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

test_provides_teardown_option(_) ->
    bddr:test(spawn(fun echo/0),
              fun(_) -> some_user_action end,
              fun(_) -> ok end,

              fun(Pid) -> Pid ! self(),
                          receive ok -> ok end,
                          false = is_process_alive(Pid) end).

teardown_runs_despite_crash_in_when(_) ->
    Self = self(),
    {'EXIT', _} =
        (catch bddr:test([],
                         fun(_) -> error(when_error) end,
                         const(ok),
                         fun(_) -> Self ! got_to_teardown end)),
    receive got_to_teardown -> ok
    after 100 -> error(teardown_failed) end.

teardown_runs_despite_crash_in_then(_) ->
    Self = self(),
    {'EXIT', _} =
        (catch bddr:test([],
                         const(ok),
                         fun(_) -> error(then_error) end,
                         fun(_) -> Self ! got_to_teardown end)),
    receive got_to_teardown -> ok
    after 100 -> error(teardown_failed) end.

failed_teardown_is_failed_test(_) ->
    {'EXIT', {broken_teardown, _}} =
        (catch bddr:test([], const(ok), const(ok),
                         fun(_) -> error(broken_teardown) end)),
    ok.

given_when_then_as_macros(_) ->
    bddr:test(?Given() -> one end,
              ?When(one) -> two end,
              ?Then(two) -> ok end,
              ?Teardown(one) -> ok end).

%% Non-test functions
echo() ->
    receive From -> From ! ok end.

compiles(Source) ->
    try {ModName, Bin} = dynamic_compile:from_string(Source),
         is_atom(ModName) andalso is_binary(Bin)
    catch throw:_ -> false
    end.

const(Val) -> fun(_) -> Val end.
