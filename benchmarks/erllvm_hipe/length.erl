%% file: "length.erl"

-module(length).
-export([test/0, len/1, run_benchmark/1]).

len(L) -> len(0,L).
len(X,[_|T]) -> len(X+1,T);
len(X,[]) -> X.

make_list(X) -> make_list(X,[]).
make_list(0,L) -> L;
make_list(X,L) -> make_list(X-1,[0|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,len(L)).

test() ->
    L = make_list(20000),
    loop(50000,L,0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
