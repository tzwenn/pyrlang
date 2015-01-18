-module(mod_tuple).
-import(test_tuple, [test/0]).
-export([test_call/0]).
-export([test_size/0]).

test_call() ->
	element(1, test_tuple:test()).

test_size() ->
	tuple_size(test_tuple:test()).
