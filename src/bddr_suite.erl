-module(bddr_suite).
-export([run_suite/1]).

run_suite(SuiteName) ->
    {ok, Mod} = fold_error({ok, SuiteName},
                           [fun load_beam/1, fun load_module/1]),
    run_all(Mod).

load_beam(SuiteName) ->
    FileName = erlang:atom_to_list(SuiteName),
    case erl_prim_loader:get_file(FileName ++ ".beam") of
        {ok, Bin, _} -> {ok, {SuiteName, Bin}};
        Error        -> {error, Error}
    end.

load_module({SuiteName, Bin}) ->
    case erlang:load_module(SuiteName, Bin) of
        {ok, Module} -> {ok, Module};
        {module, Module} -> {ok, Module};
        Error        -> {error, Error}
    end.

fold_step(_Fun, {error, E})-> {error, E};
fold_step(Fun, {ok, Acc})->
    case (catch apply(Fun, [Acc])) of
        {ok, NewAcc} -> {ok, NewAcc};
        {error, E}   -> {error, E};
        {'EXIT', E}  -> {error, E}
    end.

fold_error(Acc0, Funs)->
    lists:foldl(fun fold_step/2, Acc0, Funs).

run_all(ModName)->
    EmptyConfig = [],
    Result = [ spawn_test(ModName, Fun, [EmptyConfig])
               || Fun <- apply(ModName, all, []) ],
    exit_with_report(Result).

exit_with_report(Results)->
    Failures = filter_failures(Results),
    NF = length(Failures),
    N = length(Results),
    %% io:format("all ~p~n", [Results]),
    case (NF == 0 andalso N =/= 0) of
        true -> io:format(user, "All ~p tests passed.~n", [N]),
                halt(0);
        false -> print_failures(Failures),
                 halt(NF)
    end.

filter_failures(Results)->
    lists:filter(fun(R)-> not is_passed(R) end, Results).

is_passed({passed, _})-> true;
is_passed({failed, _})-> false.

print_failures(Failures) ->
    [ print_one(F) || F <- Failures ].
print_one({failed, {E,Trace}}) ->
    io:format(user, "Failed:~n  ~p~n  ~p~n", [E, prune(Trace)]);
print_one({failed, E}) ->
    io:format(user, "failed: ~p~n", [E]).


prune(Trace) -> prune(Trace, []).

prune([], Acc) -> lists:reverse(Acc);
prune([{bddr_test, _, _, _}|Rest], Acc) -> prune(Rest, Acc);
prune([{bddr_suite, _, _, _}|Rest], Acc) -> prune(Rest, Acc);
prune([{erl_eval, _, _, _}|Rest], Acc) -> prune(Rest, Acc);
prune([H|T], Acc) -> prune(T, [H|Acc]);
prune(Term, Acc) -> prune([], [Term|Acc]).

spawn_test(M, F, A) ->
    {Pid, Ref} = spawn_monitor(fun() -> exit({ok, apply(M, F, A)}) end),
    receive
        {'DOWN', Ref, _, Pid, {ok, R}} -> {passed, R};
        {'DOWN', Ref, _, Pid, X}       -> {failed, X}
    after 500 -> erlang:exit(Pid, kill),
                 {failed, bddr_timeout}
    end.
