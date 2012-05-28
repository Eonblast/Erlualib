% Samples

% COMPILE
% Should have been compiled during the make, if not:
% # cd samples/hello
% # erlc -I ../../include lua_sample.erl
%
% RUN
% # cd samples/hello
% # erl -pa ../../ebin -noinput -noshell -run lua_sample hello -s init stop 
% # erl -pa ../../ebin -noinput -noshell -run lua_sample type -s init stop 
% # erl -pa ../../ebin -noinput -noshell -run lua_sample hello_high -s init stop 


-module(lua_sample).
-export([hello/0, hello_high/0, type/0]).

-include("lua.hrl").
-include("lua_api.hrl").


% Print Hello World #1 - low level
%----------------------------------------------------------------------
hello() ->

	{ok, L} = lua:new_state(),				    % get the Lua engine
	lua:getfield(L, global, "print"),           % put "print" global on top of stack
	lua:pushstring(L, "Hello from Lua!"),       % put hello on top
	lua:call(L, 1, 0).                          % execute on top 2 values on stack
	
	
	
% Get and print a type of a value.
%----------------------------------------------------------------------
type() ->

	{ok, L} = lua:new_state(),                 % get the Lua engine
	lua:getfield(L, global, "type"),           % put "type" global on top of stack
	lua:pushnumber(L, 23),                     % put 23 on top
	lua:call(L, 1, 1),                         % execute on top 2 values on stack
	{ok, String} = lua:tolstring(L, 1),        % read result at top of stack
	lua:remove(L, 1),                          % Balance the stack: pop the result.
	io:format("Type of 23: ~s~n", [String]).   % ALWAYS BALANCE THE STACK.



% Print Hello World #2 - high level
%----------------------------------------------------------------------
hello_high() ->

	{ok, L} = lua:new_state(),				    % get the Lua engine
	lua:print(L, "Hello from Lua!").            % execute Lua print function
	
	
