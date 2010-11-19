-module(lua_crash_test).
-export([start/0]).
-import(lua).

% Brief demo of sure crash of the Erlang virtual machine.
% Pushing to stack until dead.
start() ->

	{ok, L} = lua:new_state(),
	crash_pusher(L,1,100000). 

crash_pusher(L,N,O) ->

	% print progress every Oth time
	(N rem O == 0) andalso io:format("~p pushes.~n", [N]),

	% this kills the stack if done often enough.
	lua:pushnumber(L, 42),

	% loop
	crash_pusher(L,N+1,O).

% Sample output:
%
% 100000 pushes.
% 200000 pushes.
% 300000 pushes.
% 400000 pushes.
% 500000 pushes.
% 600000 pushes.
% 700000 pushes.
% Segmentation fault
% (VM dies, you are back at your bash shell) 
