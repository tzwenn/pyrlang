%% From: Smoking fast Haskell code using GHC's new LLVM codegen
%% by donsbot

-module(mean).
-export([test/0, mean/1, run_benchmark/1]).

mean(N) ->
    lists:sum(lists:duplicate(N, math:pi()))/N.

loop(0,R) -> R;
loop(N,_) -> loop(N-1,mean(10000000)).

test() ->
    loop(1,0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
