% file: "nrev.erl"

-module(nrev).
-export([test/0, run_benchmark/1]).

nrev([H|T]) -> app(nrev(T),[H]);
nrev([])    -> [].

app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

iota(N) -> iota(N,[]).
iota(0,L) -> L;
iota(N,L) -> iota(N-1,[N|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,nrev(L)).

test() ->
    L = iota(1000),
    loop(1500,L,0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
