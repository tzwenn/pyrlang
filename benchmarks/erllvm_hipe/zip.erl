%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(zip).
-export([test/0, zip/1, run_benchmark/1]).

zip(N) ->
    lists:sum(lists:map(fun (X) -> X bsl 1 end,
                        (lists:zipwith(fun (X, Y) -> X * Y end,
                                       lists:seq(1, N),
                                       lists:duplicate(N, 42))))).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,zip(10000000)).

test() ->
    loop(1,0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
