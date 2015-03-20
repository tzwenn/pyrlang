-module(bu).
-export([test/1, test_sub/1, test_sub2/1, test_add/1, test_add2/1]).

test(N) -> test(0, N).

test(N, 0) -> N;
test(I, N) -> test(2*I-1, N-1). 

test2(N) -> test2(0, N).

test2(N, 0) -> N;
test2(I, N) -> test2(2*I, N-1).

test_sub(N) ->
	test(N) - test2(N).

test_sub2(N) ->
	test(N) - -1*test2(N).

test_add(N) ->
	test(N) + test2(N).

test_add2(N) ->
	test(N) + -1*test2(N).
