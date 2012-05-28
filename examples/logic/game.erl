% Samples

% COMPILE
% Should have been compiled during the make, if not:
% # cd samples/logic
% # erlc -I ../../include game.erl
%
% RUN
% # cd samples/logic
% # erl -pa ../../ebin -noinput -noshell -run game start -s init stop 
% or from project root
% # erl -pa ebin -pa samples/logic -noinput -noshell -run game start -s init stop


-module(game).
-export([start/0, fail_no_func/0, move/0, test/0]).

-include("lua.hrl").
-include("lua_api.hrl").


%----------------------------------------------------------------------
start() ->

	{ok, L} = lua:new_state(),				    % get the Lua engine
    lua:print(L, "Game Logic ...: "),
    lua:dofile(L, "logic.lua"),
    lua:func(L, "hello"),
    lua:func(L, "foo", "bar!").
	
%----------------------------------------------------------------------
fail_no_func() ->

	{ok, L} = lua:new_state(),				    % get the Lua engine
    lua:dofile(L, "logic.lua"),
    lua:func(L, "not_a_func").
	
	
%----------------------------------------------------------------------
move() ->

	{ok, L} = lua:new_state(),                 % get the Lua engine
	lua:getfield(L, global, "type"),           % put "type" global on top of stack
	lua:pushnumber(L, 23),                     % put 23 on top
	lua:call(L, 1, 1),                         % execute on top 2 values on stack
	{ok, String} = lua:tolstring(L, 1),        % read result at top of stack
	lua:remove(L, 1),                          % Balance the stack: pop the result.
	io:format("Type of 23: ~s~n", [String]).   % ALWAYS BALANCE THE STACK.



%----------------------------------------------------------------------
test() ->

	{ok, L} = lua:new_state(),				    % get the Lua engine
	lua:print(L, "Hello from Lua!").            % execute Lua print function
	
	
