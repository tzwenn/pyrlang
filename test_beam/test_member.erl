-module(test_member).
-export([test/0]).

get_list() -> [1,2,3].
test() -> lists:member(4, get_list()).
