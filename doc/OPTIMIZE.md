## Optimizing

The fewer port calls, the faster the code. 

To gain optimal speed, you want to program directly against the Lua C API.
This package can be used as starting point to do this. But you have to touch
a lot of files to implement one new function on the deepest level, for
the minimal number of port calls. As we will see, there is no real in-between.
You either implement on the Erlang side or in C.

It is easier, faster and more secure to use the low level functions exposed by this 
package, which mimick the Lua C API functions on the Erlang side. But it
comes at the price of a certain overhead of one port call *per each* function call.
Even if what you intend to do clearly implies only one cross over to Lua and back.

This is because every stack manipulation using those functions
amounts to a message sent and received between two
Erlang processes and some additional overhead due to the port call.
(Note that '[port](http://www.erlang.org/doc/tutorial/c_portdriver.html)'
in this context has nothing to do with sockets.)

So let's look at implementing more on the C side and writing most of the
desired functionality in pure C, directly against the Lua C API. And
how that is tied into the Erlang side for convenient use. It is more
work but if you can foresee that the number of different functions that
you will want to call is small, it should be worth the effort.

To illustrate what you want to happen and how you get there,
the following examples all demonstrate different ways of calling
Lua **print()**. They are progressing from port-intensive
step-by-step execution to an implementation entirely in C, requiring
only one port call.


## 1: Step By Step From the Erlang Shell

	$ erl -pa ./ebin

	1> {ok, L} = lua:new_state().
	2> lua:getfield(L, global, "print").
	3> lua:pushstring(L, "Hello from Lua!").
	4> lua:call(L, 1, 0).

This uses the low level functions as exposed by the Lua C API, mimicked by the low level functions in this package.

The last call prints this into the shell:

	Hello from Lua!

## 2: Step By Step From Code

The example in `samples/hello/lua_sample.erl` is easier to call than the
above but under the hood amounts to the same: *three port calls* shuttle
back and forth between the Lua state engine and Erlang.
Three times a message is dispatched and received between the main
processes and the port process. Those are, of course, Erlang processes, 
NOT system processes and they do not switch system context. Erlang
processes switch very fast and Erlang messages are sent and received very
fast, but not as fast as Erlang or C function calls.

The functions `process()` and `receive_return()` in **c_src/commands.c** and 
**src/lua.erl** are likewise called three times. Oviously, only one time should
be necessary.

The sample should have compiled when you did make. You could execute it like so:

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

We can also use **lua:port_print()**, like so:


	$ erl -pa ./ebin
	1> {ok, L} = lua:new_state().
	2> lua:port_print(L, "Hello Moon!").

But again, the way this is implemented, it makes three port calls. 

See **src/lua.erl:**

	-export([port_print/2]).
	...	
	port_print(L, String) ->
	
		lua:getglobal(L, "print"),          % put "print" global on stack
		lua:pushstring(L, String),   		% put text on top
		lua:call(L, 1, 0).                  % execute using stack top 2


This makes things easier and safer as it hides the lower lever calls inside
the new function port_print(). 

### 4: Optimized In C Against Lua C API

The only true optimization is an implementation as seen with `lua:c_print()`,
which in **src/lua.erl** looks like this:

	c_print_variable(L, Name) ->
	
    	command(L, {?ERL_LUA_HIGH_PRINT_VARIABLE, Name}),
	    receive_return(L).

It is obvious from the call that this yields the difference we wanted,
of making only **one port call**, resulting in only one send and one receive.

The implementation of the actual print is on the C side, directly against
the Lua C API, and not, as all previous samples, against the Erlang functions
that mimick it.

The actual implementation for this function now is in 'c_src/commands.c`.
It is implemented in C, which is the price we knew we'd had to pay.
The slower versions above of course all got away **without** touching any **C**.

**c_src/commands.c:**

	void
	erl_lua_high_print(lua_drv_t *driver_data, char *buf, int index)
	{
		lua_State *L = driver_data->L;
		char *str	 = decode_string(buf, &index);

		lua_getfield(L, LUA_GLOBALSINDEX, "print");  /* function to call */
		lua_pushstring(L, str);                      /* push text to print on stack */
    	lua_call(L, 1, 0);                           /* call 'print' w/ 1 arguments, 0 result */

		reply_ok(driver_data);                       /* Send 'ok' back to Erlang */
		free(str);
	}


Not only is the not too transparent C source above needed, with its
additional burden of possible C errors, but you also need to extend **five**
other files to make the above C function accessible from Erlang:

1. **src/lua.erl**

		-export([c_print/2]).
	
		...		
	
		c_print(L, String) ->
		    command(L, {?ERL_LUAC_PRINT, String}),
    		receive_return(L).

2. **include/lua_api.hrl**

		-define(ERL_LUAC_PRINT,             200).

3. **c_src/commands.h**

		#define ERL_LUAC_PRINT             200

4. **c_src/erlua.h**

		void erl_luac_print (lua_drv_t *driver_data, char *buf, int index);

5. **c_src/erlua.c**

		case ERL_LUAC_PRINT:
			erl_luac_print(driver_data, buf, index);
			break;

6. **c_src/commands.c**

	as shown above


**These are in order:**

1. The Erlang side exposed function that maps the functionality to a constant
2. The Erlang constant definition that identifies this functionality cross-language
2. The C constant definition that identifies this functionality cross-language
4. The C header declaration for that function
5. The switch that maps the C constant on the right C function
6. The implementation of the C function

It's not a lot that has to be added to each file, once you know your way around 
It


### Conclusion

It's some work to implement a function efficiently but following this 
blueprint it is straight forward. You will change many files of this package 
in the process. Once implemented, the result should be more stable and faster.

It will be worth your time only when you know that your calls to Lua will be
quite frequent. Otherwise, use the low level function that mimick
the Lua C API on the Erlang side and make up the bulk of `src/lua.erl`.


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