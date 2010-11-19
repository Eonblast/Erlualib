## Erlualib

This is a port driver library for **embedding** Lua into Erlang.
It provides a simple interface that is very similar to the Lua C API
and executes Lua scripts in Erlang the faster but less secure way.
See below, '[Crashing the VM](#crashing)' and 
'[Comparing Erlang-Lua Packages](#comparing)'.

Read on to learn about using this library, for an intro to the
inner workings and the Lua C API, see
[Intro](https://github.com/Eonblast/Erlualib/blob/master/doc/INTRO.md).

* This is a fork of Ray Morgan's
[erl-lua](http://github.com/raycmorgan/erl-lua) 
Erlang-Lua driver.
* This fork here goes via Darrik Mazey's 
[fork](http://github.com/darrikmazey/erlua-node).
* For a real-world example of Darrik's erl-lua in action,
see [darrikmazey/erlmon](http://github.com/darrikmazey/erlmon).
* Ray is also working on a higher level API to simplify things
further for the original erl-lua.

**WARNING: This is not fully tested. Still a bunch of work to be done.
If you are careful, it should be pretty stable.**

Doing as this library does is somewhat discouraged, unless you have 
good reasons for it. This library is an *embedded driver port*. It is
safer to use a  *c node port*, because that eliminates the danger that
an error in the Lua port can crash the entire Erlang VM.
See below,  
[Comparing Erlang-Lua Packages](#comparing) and
[Crashing the VM](#crashing).


### Example:

	1> {ok, L} = lua:new_state().  
	{ok,{lua,#Port<0.771>}}  
	2> lua:dostring(L, "t = {a=1;b='test';c={a='whatever'}}").  
	ok  
	3> lua:gettable(L, global, "t").  
	[{"a",1},{"c",[{"a","whatever"}]},{"b","test"}]  


## Building

> * Quick
> * Make
> * Pathes
> * Unit Test


### Quick

You will probably have to adjust pathes, but you might be lucky,
just try and execute  

	$ make

If all worked out, see 'Unit Test', below to check health, then 'Samples'.  

If it didn't, read on for the right Make and how to set pathes.


### Make

There are three Makefiles provided  

> * `Linux:        Makefile.Linux`
> * `Mac/Darwin:   Makefile.Darwin`
> * `Mac/Macports: Makefile.Macports`

Calling make will call `uname` to decide to use Makefile.Linux or .Darwin.

* For **Linux** and **plain Mac** build and install do

        $ make

* For a **Macports** Erlang build and install do

        $ make -f Makefile.Macports

Depending on the way you built Erlang and/or Lua, you might have to do  
use sudo, e.g.:

		$ sudo make
or

		$ sudo make -f Makefile.Macports

Very likely, you'll get errors like **error: ei.h: No such file or
directory**, and you need to set the pathes like so:


### Pathes

Pathes in the make files may need to be adjusted for version numbers, 
and non-standard install prefixes.
Do the following edits in the makefile appropriate for you:  

* Check for the **erl_interface version number** of your Erlang
  installation:

        $ ls /usr/lib/erlang/lib
or
        $ ls /opt/local/lib/erlang/lib

* Put that version number in the path in the 
  **first line** of your makefile. (But don't add '/lib') E.g.:

        ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2

* Find erl_driver.h and ei.h

        $ find / -name erl_driver.h
        $ find / -name ei.h
       
        Note that especially with Macports you may find multiple,
        identical versions.

* Add their pathes to the **second line** in your make file. E.g. add
  to the line beginning fith CFLAGS:

        -I/usr/local/lib/erl/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.6.2/include

* Find libei.a and liberl_interface.a

        $ find / -name libei.a
        $ find / -name liberl_interface.a

* Make sure that's **in the path** in your makefile's **first line**,
  but **without** the trailing /lib.  E.g.:

        ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2


Try make again.

If you have Windows instructions, please add them and push.


### Unit Test

	$ erl -pa ./ebin
	1> eunit:test(lua_test). 
	
See <src/lua_test.erl> for the source of these tests, it's quite illuminating.


## Samples

### File

Samples are in <src/lua_sample.erl>

	cd src
	erlc -I ../include lua_sample.erl 
	erl -pa ../ebin -noinput -noshell -run lua_sample hello -s init stop 

This should give you

	Hello from Lua!

and

	erl -pa ../ebin -noinput -noshell -run lua_sample type -s init stop 

should print

	Type of 23: number

**Take a look at <src/lua_sample.erl> to understand more about the usage of this API.**

### Shell

Run the samples from the shell, start the Erlang Shell with a path into ebin

	$ erl -pa ./ebin

Try this to print "Hello from Lua" **from Lua**:

	1> {ok, L} = lua:new_state().
	2> lua:getfield(L, global, "print").
	3> lua:pushstring(L, "Hello from Lua!").
	4> lua:call(L, 1, 0).

These are the low level function calls as exposed by the Lua C API. 
The last call prints this into the shell:

	Hello from Lua!

Try this to execute a Lua type-to-string call: 
	
	5> lua:getfield(L, global, "type").
	6> lua:pushnumber(L, 23).
	7> lua:call(L, 1, 1).
	8> {ok, String} = lua:tolstring(L, 1).
	9> lua:remove(L, 1). % always rebalance the stack!
	10> String.
	
This prints the type of the number 23 into the shell:

	{ok, "number"}



## Comparing Erlang-Lua Packages<a name=comparing></a>

We found two approaches,

* Cyril Romains' c node port [erlua](http://gitorious.org/erlua):   
  *"erlua is aimed to enable seamless interoperability between Erlang and
  the Lua programming language."*
* Ray Morgan's embedded port driver [erl-lua](http://github.com/raycmorgan/erl-lua):   
 *"The aim of this library is to enable a high-level embedded-lua API
 for use in configuration processing and extension coding for Erlang
 projects."*

This package is based on the latter.


#### C Node Port

Cyril's *erlua* implements a more secure 
[C Node](http://www.erlang.org/doc/tutorial/cnode.html)
that accepts and handles the calls for Lua in a separate node,
implemented in C but behaving like an Erlang node,
which cannot crash the Erlang VM when it goes down.

*"From Erlang's point of view, the C node is treated like a
normal Erlang node."*

<http://www.erlang.org/doc/tutorial/cnode.html>

There seems to be be much more high-level source in *erlua* that
helps looking at the Lua side, the Lua Stack, and Lua Variables.
You can easily execute strings as Lua source and call Lua functions
remotely. However, that was not what we needed.


#### Embedded Port Driver

Ray's erl-lua is lower level 
[embedded port driver](http://www.erlang.org/doc/tutorial/c_portdriver.html),
which is faster but less secure.

*"A port driver is a linked in driver, that is accessible as a
port from an Erlang program. It is a shared library (SO in
Unix, DLL in Windows), with special entry points. The Erlang
runtime calls these entry points, when the driver is started
and when data is sent to the port. The port driver can also
send data to Erlang."*

*"Since a port driver is dynamically linked into the emulator
process, this is the fastest way of calling C-code from Erlang.
Calling functions in the port driver requires no context
switches. But it is also the least safe, because a **crash in the
port driver brings the emulator down too**."*

<http://www.erlang.org/doc/tutorial/c_portdriver.html>

erl-lua seemed to be lower level and mimicking the Lua C API,
providing an Erlang function call for most of the functions in
the basic Lua C API. That creates opportunity to corrupt the
Lua 'Stack' (see [Intro](doc/INTRO.md)) and more effort and 
much more attention is needed to write even a simple 
'hello world' call. But for being flatter it may be more 
robust, with less potential for errors in the C parts
when extending it. Plus, we wanted to optimize for performance
and cut out the intra-node communication that results into
a system context switch for every call.

We are introducing a higher level in our fork of erl-lua that
reduces the number of port calls and bundles functionality 
on the C side, programmed directly against the Lua C API,
to gain further speed. For an example, see c_print() in
c_src/commands.c.


#### NIF

Especially with Lua, it might make sense to reflect about using
[NIFs](http://www.erlang.org/doc/tutorial/nif.html). Because you
might find that you always only call a handfull or even only one
Lua-implemented function from Erlang. 

It looks like there is no NIF approach out there today.


*"NIFs where introduced in R13B03 as an experimental feature. It
is a simpler and more efficient way of calling C-code than
using port drivers. NIFs are most suitable for synchronous
functions like foo and bar in the example, that does some
relatively short calculations without side effects and return
the result."*

*"A NIF (Native Implemented Function) is a function that is
implemented in C instead of Erlang. NIFs appear as any other
functions to the callers. They belong to a module and are
called like any other Erlang functions. The NIFs of a module
are compiled and linked into a dynamic loadable shared library
(SO in Unix, DLL in Windows). The NIF library must be loaded in
runtime by the Erlang code of the module."*

*"Since a NIF library is dynamically linked into the emulator
process, this is the fastest way of calling C-code from Erlang
(alongside port drivers). Calling NIFs requires no context
switches. But it is also the least safe, because a **crash in a
NIF will bring the emulator down too**."*

<http://www.erlang.org/doc/tutorial/nif.html>

## Crashing the VM<a name=crashing></a>

You can easily get the entire Erlang VM crashed, both by mistakes in  
the C code of this driver, the Lua VM, Lua extensions, or by errors  
you make in using this API. 

Which is exactly why instead of linking drivers in, like this lib,
the recommended way is a different one: using **c node ports**, 
like [erlua](http://gitorious.org/erlua).

### A Sample

Of course, stack errors are a sure way to get you into trouble.

Here is a fast and sure way to crash the Erlang process, by a Lua stack  
*underrun*. Run this from the Erlang shell and watch it shutting Erlang  
down for good:


	1> {ok, L} = lua:new_state(), lua:call(L, 1, 0).  
	Segmentation fault
	$ |


You can also use

	erlualib$ erl -pa ./ebin ./crash_tests/ebin
	1> lua_crash_fast:start().

Which runs this source:

	-module(lua_crash_fast).
	-export([start/0]).

	start() ->

		{ok, L} = lua:new_state(),
		lua:call(L, 1, 0). 

To test stack *overrun*, here is an abridged snip from src/lua_crash_test.erl:

	start() ->

        	{ok, L} = lua:new_state(),
       		crash_pusher(L,1,100000).

	crash_pusher(L,N,O) ->

        	lua:pushnumber(L, 42),
        	crash_pusher(L,N+1,O).

Which also results in a killed VM. Start the Erlang shell from the project  
root directory to get something like this:

	erlualib$ erl -pa ./ebin ./crash_tests/ebin
	Erlang R13B01 (erts-5.7.2) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.7.2  (abort with ^G)
	1> lua_crash_test:start().
	100000 pushes.
	200000 pushes.
	300000 pushes.
	400000 pushes.
	500000 pushes.
	600000 pushes.
	700000 pushes.
	Bus error
	erlualib$ 


**Again:** both bugs in the C source of this driver, as well as **errors  
when using the API**, as demonstrated above, can kill the entire Erlang VM.  

Risking this is exactly **not** what Erlang is about.


	-------------------------------------------------------------------
	File        : README.md
	Description : Erlang Embedded Lua Port Driver
	Authors     : Ray Morgan, Darrik Mazey, Henning Diedrich
	Copyright   : (c) 2010 Eonblast Corporation for this fork
	License     : MIT for this fork
	Created     : 11 Apr 2009 Ray C. Morgan <@raycmorgan>
	Changed     : 20 Nov 2010 H. Diedrich <hd2010@eonblast.com>
	-------------------------------------------------------------------

Formatting: <http://daringfireball.net/projects/markdown/syntax>