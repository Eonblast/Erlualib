-module(lua_crash_fast).
-export([start/0]).

% sample on how to crash the Erlang VM with two bad Erlualib calls.

start() ->

	{ok, L} = lua:new_state(),
	lua:call(L, 1, 0).   

