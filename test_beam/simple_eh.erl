-module(simple_eh).
-export([test/1, test/0, loop/0]).

test() -> test(10000).
test(0) -> true;
test(N) ->
	Pid = spawn(simple_eh, loop, []),
	Pid ! {self(), 0},
	loop(),
	test(N-1).

loop() ->
	receive
		{From, Num} ->
			case Num == 100000 of
				true ->
					From ! stop;
				false ->
					From ! {self(), Num+1},
					loop()
			end;
		stop ->
			true
	end.
