-module(echo).
-export([go/0, loop/0]).

go() ->
	Pid2 = spawn(echo, loop, []),
	Pid2 ! {self(), hello},
	receive
		{Pid2, Msg} ->
			erlang:display(p1),
			erlang:display(Msg)
	end,
	Pid2 ! stop.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.
