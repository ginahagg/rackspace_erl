-module(all_tests).

-export([run/0]).

run() ->
	parsers_test:test().
