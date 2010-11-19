## Optimizing

The less port calls, the faster the code. 

To gain optimal speed, you want to program directly against the Lua C API.
This package can be used as starting point to do this. But you have to touch
a lot of files to implement one new function on the deepest level, to
reach the best performance.

It is easier and faster to use the low level functions exposed by this 
package, which mimick the Lua C API functions on the Erlang side. But it
comes at the price of a certain overhead of one port call per function.
Even if what you intent implies only one change over to Lua and back.

Every stack manipulation using the Lua C API mimick functions
of this library, amounts to a message sent and received between two
Erlang processes and some additional overhead due to the port call.

(Note that 'port' in this context has nothing to do with sockets.)

So let's look at implementing more on the C side and writing more
functionality in pure C, directly against the Lua C API.

To illustrate what you want to happen and where you get there,
the following examples all demonstrate different ways of calling
Lua **print()**. They are progressing from a more port-intensive
step-by-step execution to an implementation entirely in C, requiring
only one port call.


## 1: Step By Step From the Erlang Shell

	$ erl -pa ./ebin

	1> {ok, L} = lua:new_state().
	2> lua:getfield(L, global, "print").
	3> lua:pushstring(L, "Hello from Lua!").
	4> lua:call(L, 1, 0).

This uses the low level functions as exposed by the Lua C API and
mimicked by the low level functions in this package.

The last call prints this into the shell:

	Hello from Lua!

## 2: Step By Step From Code

The example in samples/hello/lua_sample.erl is easier to call but amounts
to the same: three port calls shuttle between the Lua state engine and Erlang.
Three times an Erlang message is sent and received between Erlang main
processes and port process (which are Erlang processes, NOT system 
processes and do not switch system context).

The functions process() and receive_return in c_src/commands.c and 
src/lua.erl are called likewise, three times. 

The sample should have compiled when you did make. You call it

	cd samples/hello
	erl -pa ../../ebin -noinput -noshell -run lua_sample hello -s init stop 


The source executed is the same as in the shell:

	hello() ->

		{ok, L} = lua:new_state(),				    % get the Lua engine
		lua:getfield(L, global, "print"),           % put "print" global on top of stack
		lua:pushstring(L, "Hello from Lua!"),       % put hello on top
		lua:call(L, 1, 0).                          % execute on top 2 values on stack

It should also give you:

	Hello from Lua!


## 3: Yet More Hidden - lua:port_print()

We can also use lua:port_print(), like so:


	$ erl -pa ./ebin
	1> {ok, L} = lua:new_state().
	2> lua:port_print(L, "Hello Moon!").

But again, the way this is implemented, it makes three port calls. 

See src/lua.erl:

	-export([port_print/2]).
	...	
	port_print(L, String) ->
	
		lua:getglobal(L, "print"),          % put "print" global on stack
		lua:pushstring(L, String),   		% put text on top
		lua:call(L, 1, 0).                  % execute using stack top 2


This makes things easier and safer as it hides the lower lever calls inside
the new function port_print(). The implementation of the function had us
extending one main file of the library src/lua.erl.


### 4: Optimized in C against Lua C Api

The only true optimization is the implementation of lua:c_print(), which
in src/lua.erl looks like this:

	c_print_variable(L, Name) ->
	
    	command(L, {?ERL_LUA_HIGH_PRINT_VARIABLE, Name}),
	    receive_return(L).

Which obviously yields the difference of making only **one port call**,
thus one send and one receive.

The implementation is on the C side, directly against the Lua C API, and not,
as all previous samples, against the Erlang functions that mimick it but
at the cost of a port call for every function.

The actual source for this function is in c_src/commands.c. Note that the
previous versions, of course, all got away without touching on C.

c_src/commands.c:

	void
	erl_lua_high_print(lua_drv_t *driver_data, char *buf, int index)
	{
		lua_State *L = driver_data->L;
		char *str	 = decode_string(buf, &index);

		lua_getfield(L, LUA_GLOBALSINDEX, "print"); /* function to call */
		lua_pushstring(L, str);          /* push text to print on stack */
    	lua_call(L, 1, 0);     /* call 'print' w/ 1 arguments, 0 result */

		reply_ok(driver_data);
		free(str);
	}


Not only is all of the above needed - and the additional burden of
possible C errors caused, but you need to extend **five** other files to
make the above call accessible from Erlang:

1. src/lua.erl:

	-export([c_print/2]).
	
	...		
	
	c_print(L, String) ->
	    command(L, {?ERL_LUAC_PRINT, String}),
    	receive_return(L).

2. include/lua_api.hrl

	-define(ERL_LUAC_PRINT,             200).

3. c_src/commands.h

	#define ERL_LUAC_PRINT             200

4. c_src/erlua.h

	void erl_luac_print (lua_drv_t *driver_data, char *buf, int index);

5. c_src/erlua.c

	case ERL_LUAC_PRINT:
		erl_luac_print(driver_data, buf, index);
		break;

6. c_src/commands.c

	see above


In order:

1. The Erlang side exposed function that maps the function to a constant
2. The Erlang constant for identification of the function cross-language
3. The C constant for identification of the function cross-language
4. The C header declaration for that function
5. The port switch that maps the C constant on a C function
6. The implementation of the C function


### Conclusion


-------------------------------------------------------------------
File        : OPTIMIZE.md
Description : On Implementation of Functionality on top of Erlualib
Authors     : Henning Diedrich
Copyright   : (c) 2010 Eonblast Corporation
License     : MIT see LICENSE.md
Created     : 20 Nov 2010 H. Diedrich <hd2010@eonblast.com>
Changed     : 20 Nov 2010 H. Diedrich <hd2010@eonblast.com>
-------------------------------------------------------------------

Formatting: <http://daringfireball.net/projects/markdown/syntax>