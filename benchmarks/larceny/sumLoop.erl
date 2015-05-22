-module(sumLoop).
-export([test/1]).

tail_rec(N) ->
	tail_rec(N, 0).

tail_rec(0, Sum) -> Sum;
tail_rec(N, Sum) ->
	tail_rec(N-1, Sum+1).

test(N) ->
	tail_rec(N).
