## The Lua C API

### Fast Start into Lua's C API

This is a brief for people who tinkered with languages before.

**Save time.** Take a deep breath and read this in one draw.

### Docs

To get some perspective - Lua is *so for embedding* that its C API is described in the main
[Lua Reference](http://www.lua.org/manual/5.1/manual.html), chapters 3 and 4, *before* the description of the actual Lua standard library in chapter 5.

In *Programming in Lua*, [Chapter 24](http://www.lua.org/pil/24.html) Lua's father Roberto Ierusalimschy warns that
*'the API emphasizes flexibility and simplicity, sometimes at the cost of ease of use. Common
tasks may involve several API calls. This may be boring, but it gives you full control over
all details.'* May I humbly add that Lua has no less of a fascination than Erlang, if for
different reasons. And both languages insist on being spelt with a capital first letter.


### Lua State

The first parameter to most Lua C API functions is a pointer to a **Lua State** object, in
effect the *Lua engine* and its *current global state*. You can have more than one. Keep
that in mind.

### Lua Stack

Lua uses a **self made pseudo stack** for execution. Most functions the Lua C API exposes are
for manipulating this 'stack'. So, 'stack' in this context never means the actual C stack.  

### Stack Functions

Most functions use an *index* parameter to point *into* the stack and then operate on whatever
value is found there. E.g. `void lua_replace (lua_State *L, int index)` moves the *top
element* into the position `index`, without shifting any element. Or, `lua_pop(L,n)` pops
n elements from that pseudo stack. 

**You can easily crash Lua and the entire Erlang VM if you get things wrong there.**

### Pseudo Indices

Presumably to not double functionality, the index parameters are habitually and creatively
misused. Instead of pointing into the 'stack' thay can also be used to point anywhere
else that makes sense, e.g. into global tables that hold the *environment* values, the
*Registry* and the *global names' table*.

For example, putting the pseudo index `LUA_REGISTRYINDEX` as 'stack index' parameter
instead of a benign 1 or 2, will make your function call manipulate the [Lua Registry Table](http://www.lua.org/pil/27.3.1.html),
which has nothing to do with the stack at all. So e.g. `lua_rawgeti(L, LUA_REGISTRYINDEX, ref)`
will return a value from this *Registry Table*, while `lua_rawgeti(L, 1, ref)` will return
a value from the *table* at position 1 on the *stack*. In both cases, ref would then make
for a pointer into the pinpointed table. So the middle parameter points to where the *table* is
found, the last parameter ('ref') points to a field *in* that table.

Note that the index parameter is ..used to signal this special case. There is no 'stack'
reference parameter that could be replaced; which, well, one could have been tempted to expect.

### Calling Functions

This recycling of functions is also used to *put function calls on the stack*. They are basically
taken out of the *'global'* table, which,  among other variables, holds the function names.
E.g. `lua_getfield (L, LUA_GLOBALSINDEX, "print")` pushes the *print()* function on the stack,
to stage it for immediate use. To execute it, you'd then push something to print right after
 and call lua_call().

However, you'd use the very same function, `lua_getfield()` to access a *field in a table on
the stack*: e.g. field 'tnirp', *in the table that is at stack position 3*: `lua_getfield (L, 3, "tnirp")`. 
The value you wanted, after the call, is on the top of the Lua 'stack.


## Library Architecture

ÃThe inner structure of this package is quite straight forward. It offers Erlang functions
that go over an Erlang 'port' into the C parts and the Lua C libraries.


### Call Path

From Erlang into Lua, a call ventures as follows. Let's look at `gettop()` for an example.
The following describes something like a callstack:

* **Your Erlang module calls function gettop(L)**
* lua.erl          `gettop(L)`
* lua_api.hrl      `-define(ERL_LUA_GETTOP, ..`
* lua.erl          `command(L, {?ERL_LUA_GETTOP}) ..`
* *(erl_driver)*   `port_command(Port, term_to_binary(Data)) ..`
* **-- Entering C land --**
* commands.h       `#define ERL_LUA_GETTOP ..`
* erlua.c          `process(handle, vector) .. case ERL_LUA_GETTOP: ..`
* commands.c       `erl_lua_gettop(driver_data, buf, index)`
* *(Lua lapi.c)*     `lua_gettop(driver_data->L)`
* *(erl_driver)*     `driver_output_term()`
* **-- Back in Erlang --**
* lua.erl          `receive ..`
* **Your Erlang module** gets an {ok} as return

### Extension Sample

**To add a function** that enhances this API, the following places need to be touched.

Note that most 'auxiliary' functions from the *Lua Aux Lib* are not yet implemented in this API so there's 
quite some work left to be done. However, all feats that aux functions accomplish can also be 
done using only the lower order functions. The Aux Lib is described in [chapter 4](http://www.lua.org/manual/5.1/manual.html#4) of the 
Lua Reference.

Let's take a look at a possible implementation of C `luaL_ref()`. 
Note the **'L'** in the name, which marks it as an *aux* function.
The function (see [Lua manual](http://www.lua.org/manual/5.1/manual.html#luaL_ref)) creates and returns a unique *reference*, 
in the table at index t, for the object at the top of the 'stack', and pops the object.

#### 1. lua_api.hrl
Lua C functions are enumerated in this API, to be able to call them out across language barriers.
The Erlang header file `lua_api.hrl` contains all the function enumeration constants you may wish
for, also for functions not yet implemented in this API. You will not usually have to add anything
but find the constant you'd need being already defined. In our case, `ERL_LUAL_REF`:

		-define(ERL_LUAL_REF,               128).

#### 2. lua.erl          	
We're deciding for the name `l_ref` for our function and write the Erlang side function for it
that takes the Lua State variable, `L`, as well as the additional parameter needed. In this case,
a stack index, `Index`. The function then uses `command/2` to send the payload to the 'port'.
It receives it's answer by a call to `receive_valued_response()`. Note that 'port' in this 
context is an Erlang API concept, not a port as in socket programming.  

		l_ref(L, Index) ->
 			command(L, {?ERL_LUAL_REF, Index}),
			receive_valued_response().


#### 3. commands.h
The C header file `commands.h` also already contains all C constants you may wish for, also 
for those functions not yet implemented. You will not usually have to add anything but should 
find the constant you need being already defined. Again, we find `ERL_LUA_REF`, this time in its
incarnation as a C constant. And as it should, with the same actual value as the Erlang
constant we used above.

		#define ERL_LUAL_REF               128

#### 4. erlua.c
In the `process()` function in the C source file `erlua.c`, add your new function's case to
the `switch(command)`. Give the function to be called a meaningful name, in our case let's
use `erl_lual_ref()`. We will define the function body in the next step. These functions must,
in this switch, always have exactly these parameters, as shown: `driver_data, buf, index`.
These contain, more or less, the *Lua State*, the *'stack'* and an *index to the top* of the
stack. Just write them in there, they are guaranteed to exist in the function at this point,
with the right values set. So that bit may look like this now:

		case ERL_LUAL_REF:
			erl_lual_ref(driver_data, buf, index);
			break;
		
#### 5. commands.c
Finally, we write the actual implementation of the functionality. The signature must be 
`void func(lua_drv_t *driver_data, char *buf, int index)`. Return values are rare. If they
are needed, they are sent back over the port as seen below. But most functions simply end 
with sending 'ok', by means of calling `reply_ok(driver_data)`. This is so, since  usually
the effect of a function is a *change to the Lua 'stack'* rather than delivering a result
'back'. After all, where would that 'back' usually be. Inside of Lua, the way to deliver
a result is exactly that: put it on the top of the stack. The functions that work the
virtual machine ops do not call each other.

But in this case, we want values back. They are parsed into the C struct ErlDrvTermData,
by ways of flattened 2-tuples: a type followed by a value, followed by the next tuple.

		void
		erl_lual_ref(lua_drv_t *driver_data, char *buf, int index)
		{
	 		long i;
	  		int ref;
	  
	 		// Get a value from the Lua stack, it is returned in i.
	 		ei_decode_long(buf, &index, &i);
	
	 		// Finally, call the actual Lua library function.
		  	ref = luaL_ref(driver_data->L, i);
	
			// Prepare the return value. (Usually not necessary.)
			ErlDrvTermData spec[] = {
	   			ERL_DRV_ATOM,    ATOM_OK,
				ERL_DRV_INT,     (ErlDrvTermData) ref,
	  			ERL_DRV_TUPLE,   2
	  		};
	  
	 		// Send the return value to the Erlang 'port'.
			driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
		}


Eventually, the call to `driver_output_term()` sends the data over the port to the Erlang function
`receive_valued_response()` in our initial Erlang function `l_ref()`. 

Congrats!  
SIT PRO.  
H. Diedrich   
21 Jun 2010  
		
		

