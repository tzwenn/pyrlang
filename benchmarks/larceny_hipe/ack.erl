-module(ack).
-export([test/0,test/2]).

ack(0, N) -> N+1;
ack(M, 0) -> ack(M-1,1);
ack(M, N) -> ack(M-1, ack(M, N-1)).

test() ->
	test(3,9).

test(M,N) -> ack(M,N).

