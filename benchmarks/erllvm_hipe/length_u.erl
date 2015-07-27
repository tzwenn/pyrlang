%% file: "length_u.erl"

-module(length_u).
-export([test/0, run_benchmark/1]).

len(L) -> len(0,L).

len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]]]) -> len(X+11,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]]) -> len(X+10,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]) -> len(X+9,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]) -> len(X+8,T);
len(X,[_|[_|[_|[_|[_|[_|[_|T]]]]]]]) -> len(X+7,T);
len(X,[_|[_|[_|[_|[_|[_|T]]]]]]) -> len(X+6,T);
len(X,[_|[_|[_|[_|[_|T]]]]]) -> len(X+5,T);
len(X,[_|[_|[_|[_|T]]]]) -> len(X+4,T);
len(X,[_|[_|[_|T]]]) -> len(X+3,T);
len(X,[_|[_|T]]) -> len(X+2,T);
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
