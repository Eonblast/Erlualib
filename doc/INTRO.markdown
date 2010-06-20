## Lua C API

### Fast Start into Lua's C API

This is a brief for people who tinkered with languages before.

Save time. Take a deep breath and read in one draw.

#### Docs

To get some perspective - Lua is so <i>for embedding</i> that its C API
is described in the main [Lua Reference](http://www.lua.org/manual/5.1/manual.html),
chapters 3 and 4, <i>before</i> the description of the actual Lua standard library in chapter 5.

In Programming in Lua, [Chapter 24](http://www.lua.org/pil/24.html) Lua's father Roberto 
Ierusalimschy warns that <i>'the API emphasizes flexibility and simplicity, sometimes at the cost
of ease of use. Common tasks may involve several API calls. This may be boring, but it gives you
full control over all details.'</i>


#### Lua State

The first parameter to most Lua C API functions is a pointer to a <b>Lua State</b> object, in 
effect the Lua engine and it's current global state. You can have more than one.

#### Lua Stack

Lua uses a self made, pseudo stack for execution. Most functions the Lua C API exposes are
for manipulating this stack. So 'stack' in this context never means the actual C stack.

#### Stack Functions

Most functions use an index parameter to point into the stack and then operate on whatever
value is found there. E.g. `void lua_replace (lua_State *L, int index)` moves the top 
element into the position `index`, without shifting any element. Or, `lua_pop(L,n)` pops 
n elements from the stack. 

You can crash Lua and the entire Erlang VM if you get things wrong there. 

#### Pseudo Indices

Presumably to not double functionality, the index parameters are habitually and creatively misused.
Instead of pointing into the stack thay can also be used to point completely elsewhere,
into global tables that hold the <i>environment</i> values, the <i>Registry</i> and the
<i>global names' table</i>.

For example, putting the pseudo index `LUA_REGISTRYINDEX` as 'stack index' parameter
instead of a benign 1 or 2, will make your function call manipulate the 
[Lua Registry Table](http://www.lua.org/pil/27.3.1.html), which has nothing to do with the 
stack. So e.g. `lua_rawgeti(L, LUA_REGISTRYINDEX, ref)` will return a value from this Registry 
Table while `lua_rawgeti(L, 1, ref)` will return a value from the table at position 1 on the stack.
In both cases, ref would be the pointer into that table then: so the middle parameter
points to where the <i>table</i> is found, the last parameter ('ref') points to a field <i>in</i>
that table.

#### Calling Functions

This recycling of functions is also used to put function calls on the stack. They are basically 
taken out of the 'global' table that among other variables, holds the function names.
`lua_getfield (L, LUA_GLOBALSINDEX, "print")` pushes the print function on the stack for use.
To execute it you'd push something to print next and then call lua_call(). You would use the very 
same function to access a field in a table on the stack: e.g. field 'tnirp' in the table that
is at stack position 3: `lua_getfield (L, 3, "tnirp")`.


### Library Architecture

The inner structure of this package is quite straight forward. It offers Erlang functions that go over 
an Erlang 'port' into the C parts and the Lua C libraries.

#### Call Path

From Erlang into Lua, a call ventures as follows. Let's look at gettop() for an example:

* (your erlang module) calls function gettop(L)
* lua.erl -          `gettop(L)`
* lua_api.hrl -      `-define(ERL_LUA_GETTOP, ..`
* lua.erl -         `command(L, {?ERL_LUA_GETTOP}) ..`
* (erl_driver) -    `port_command(Port, term_to_binary(Data)) ..`
* -- Entering C land --
* commands.h -      `#define ERL_LUA_GETTOP ..`
* erlua.c -         `process(handle, vector) .. case ERL_LUA_GETTOP: ..`
* commands.c -       `erl_lua_gettop(driver_data, buf, index)`
* (Lua lapi.c) -     `lua_gettop(driver_data->L)`
* (erl_driver) -    `driver_output_term()`
* -- Back in Erlang --
* lua.erl -         `receive ..`
* (your erlang module) gets an {ok} as return

#### Extension Blueprint

As of now, to add a function, the following places need to be touched.

Note that most 'auxiliary' functions from the Lua Aux Lib are not yet implemented so there's quite
some work left to be done. However, all feats that aux functions accomplish can also be done
using only the lower order functions. The Aux Lib is described in
[chapter 4](http://www.lua.org/manual/5.1/manual.html#4) of the Lua Reference.

Let's look at the implementation of lual_ref. Note the 'l' which is the tag for aux functions.

##### 1. lua_api.hrl
Containts all constants you may wish for, also for functions not implemented in this 
package yet. You will not usually have to add anything but find the constant you need
being already defined:

		-define(ERL_LUAL_REF,               128).

##### 2. lua.erl          	
Come up with a sensible name and write a function that takes the Lua State variable
as well as any additional parameter needed. In this case, a stack index. The function 
then uses command/2 to send the work to the port.

		l_ref(L, Index) ->
 			command(L, {?ERL_LUAL_REF, Index}).

##### 3. commands.h
Also containts all constants you may wish for, also for functions not implemented in this 
package yet. You will not usually have to add anything but find the constant you need
being already defined:

		#define ERL_LUAL_REF               128

##### 4. erlua.c
In `process()`, add your new function's case to the `switch(command)`. Give the function
to be called an intelligent name. It's defined in the next step. The function must have
exactly the parameters as shown: `driver_data, buf, index`. Those contain, more or less,
the Lua State, the stack and the index to the top of the stack.

		case ERL_LUAL_REF:
			erl_lual_ref(driver_data, buf, index);
			break;
		
##### 5. commands.c
Finally, the implementation of the functionality. The signature must be 
`void func(lua_drv_t *driver_data, char *buf, int index)`. Return values are rare and
are sent back over the port as seen below.
Most functions simply end with sending 'ok' by calling `reply_ok(driver_data)`. 
This is so as usually the effect of a function is a mutation of the stack rather
than delivering a result.  
  
		void
		erl_lual_ref(lua_drv_t *driver_data, char *buf, int index)
		{
	 		 long i;
	  		int ref;
	  
	 		 // get a value from the Lua stack, return it in i.
	 		 ei_decode_long(buf, &index, &i);
	
	 		 // Finally, call the actual Lua library function!
			  ref = luaL_ref(driver_data->L, i);
	
			  // prepare return value
			  ErlDrvTermData spec[] = {
	   		     ERL_DRV_ATOM,   ATOM_OK,
	 		       ERL_DRV_INT, (ErlDrvTermData) ref,
	  		      ERL_DRV_TUPLE,  2
	  		};
	  
	 		// send return value to port
			driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
		}


- H. Diedrich 21 Jun 2010
		
		
