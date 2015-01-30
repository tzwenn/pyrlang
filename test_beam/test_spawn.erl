-module(test_spawn).
-export([say_something/2,sec_start/0, con_start/0]).

say_something(_What, 0) ->
	done;
say_something(What, Times) ->
	erlang:display(What),
	say_something(What, Times - 1).

sec_start() ->
	say_something(hello, 3),
	say_something(goodbye, 3).

con_start() ->
	spawn(test_spawn, say_something, [hello, 3]),
	spawn(test_spawn, say_something, [goodbye, 3]).
