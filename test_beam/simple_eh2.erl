-module(simple_eh2).
-export([test/1, test/0, loop/0]).

test() -> test(10000).
test(N) ->
	Pid = spawn(simple_eh2, loop, []),
	SendFunc = fun SendFunc(0) -> true;
			   	   SendFunc(I) -> 
						Pid ! 1,
						SendFunc(I-1)
			   end,
	SendFunc(N),
	Pid ! stop,
	true.

loop() ->
	loop(0).
loop(N) ->
	receive
		Num when is_integer(Num) ->
			loop(N+Num);
		stop ->
			N
	end.
