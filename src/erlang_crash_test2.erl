-module(erlang_crash_test2).
-export([start/0, re/1]).

start() ->

	io:format("Stack overrun 2 in pure Erlang ..."),
	
	re(0).
	
	
re(N) -> 

	case N rem 1000 of 0 -> io:format("."); _ -> nil end,

	spawn(?MODULE, re, [N+1]),
	
	re(N+1).
	

	