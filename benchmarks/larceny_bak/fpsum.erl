-module(fpsum).
-export([test/0]).

run() -> 
	run(1000000.0, 0.0).

run(I,N) when I < 0.0 -> N;
run(I,N) -> run(I-1.0,N+I).

test() -> run().
