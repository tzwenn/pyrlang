-module(test_apply).
-export([test1/0, 
		 %test2/0, 
		 %test3/1,
		 foo/1]).

foo(N) ->
	erlang:display(N).

test1() ->
	Closure = fun() -> erlang:display("hello this is test1") end,
	apply(Closure, []).

%test2() ->
	%apply(?MODULE, foo, ["hello this is test2"]).

%test3(FunName) ->
	%apply(?MODULE, FunName, ["hello this is test3"]).
