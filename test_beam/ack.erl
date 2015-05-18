-module(ack).
-export([ack/2, test/1]).

ack(0, N) -> N + 1;
ack(M, 0) -> ack(M-1, 1);
ack(M, N) -> ack(M-1, ack(M, N-1)).

test(N) ->
	ack(2, N).

