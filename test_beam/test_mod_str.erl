-module(test_mod_str).
-import(test_str, [test/0]).
-export([test_app_num/0, test_app_str/0, test_map/0]).

test_app_str() ->
	test_str:test() ++ "asdhgsadhg".

test_app_num() ->
	test_str:test() ++ [1,2,3,4].

test_map() ->
	lists:map(fun(N) -> N + 1 end, test_str:test()).
