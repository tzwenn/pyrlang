-module(test_timer).
-export([test/0, fib/1]).

test() ->
	timer:tc(?MODULE, fib, [38]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) ->
	fib(N-1)+fib(N-2).
	
