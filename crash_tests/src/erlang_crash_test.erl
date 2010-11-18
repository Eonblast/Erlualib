-module(erlang_crash_test).
-export([start/0]).

start() ->

	io:format("Stack overrun in pure Erlang ..."),
	
	re(0).
	
	
re(N) -> 

	case N rem 10000 of 0 -> io:format("."); _ -> nil end,

	re(N+1) + re(N+2).
	

	