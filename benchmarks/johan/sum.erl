%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(sum).
-export([test/0, run_benchmark/1]).

sum(N) ->
    lists:sum(lists:seq(1, N)).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,sum(10000000)).

test() ->
    loop(1,0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
