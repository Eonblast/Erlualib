% Samples

% # cd src
% # erlc -I ../include lua_sample.erl 
% # erl -pa ../ebin -noinput -noshell -run lua_sample hello -s init stop 
% # erl -pa ../ebin -noinput -noshell -run lua_sample type -s init stop 


-module(lua_sample).
-export([hello/0, type/0]).

-include("lua.hrl").
-include("lua_api.hrl").

% Print Hello World
hello() ->

	{ok, L} = lua:new_state(),
	lua:getfield(L, global, "print"),
	lua:pushstring(L, "Hello from Lua!"),
	lua:call(L, 1, 0).
	
% Get and print a type of a value.
type() ->

	{ok, L} = lua:new_state(),
	lua:getfield(L, global, "type"),
	lua:pushnumber(L, 23),
	lua:call(L, 1, 1),
	{ok, String} = lua:tolstring(L, 1),
	lua:remove(L, 1), % always rebalance the stack!
	io:format("Type of 23: ~s~n", [String]).