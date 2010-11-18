## Erlualib

Linked in Lua Driver for Erlang.

Execute Lua scripts in Erlang the faster but less secure way.  
See below, 'Crashing the VM'.

This is a library for **embedding** Lua into Erlang. It provides a  
simple interface that is very similar to the Lua C API. 

For an intro to the Lua API and the structure of this package  
see [Intro](https://github.com/Eonblast/Erlualib/blob/master/doc/INTRO.md).

The aim of this library is to enable a high-level embedded-lua API
for use in **configuration processing and extension coding** for Erlang
projects. For a real-world example of Darrik's erl-lua in action,
see [darrikmazey/erlmon](http://github.com/darrikmazey/erlmon).

Ray is also working on a higher level API to simplify things  
further for the original erl-lua.  

This is a fork of Ray Morgan's [erl-lua](http://github.com/raycmorgan/erl-lua) Erlang-Lua driver.  
This fork here goes via Darrik Mazey's [fork](http://github.com/darrikmazey/erlua-node).

**WARNING:** 'This is definitely not fully tested. Still a  
bunch of work to be done. If you are careful, it should be  
pretty stable (no promises though).'  

<div style='border: 2 px solid red; padding: 20px'>
Doing as this driver does is discouraged, unless you have good    
reasons for it. It is safer to use a port, because this eliminates    
the danger that an error in the extension can crash the entire   
Erlang VM.
</div>



#### Example:

	1> {ok, L} = lua:new_state().  
	{ok,{lua,#Port<0.771>}}  
	2> lua:dostring(L, "t = {a=1;b='test';c={a='whatever'}}").  
	ok  
	3> lua:gettable(L, global, "t").  
	[{"a",1},{"c",[{"a","whatever"}]},{"b","test"}]  


## Building

* Quick
* Makefiles
* Pathes
* Make
* Unit Test


### Quick

You might be lucky, just try and execute  

	$ make

There is Linux, Mac/Darwin and Mac/Macports support.  

It's really only pathes that need to be fixed if make doesn't work.  
See 'Pathes', below.  

If all worked out, see 'Unit Test', below to check health, then 'Samples'.  


### Makefiles

There are three Makefiles provided  

	* Linux:        Makefile.Linux
	* Mac/Darwin:   Makefile.Darwin
	* Mac/Macports: Makefile.Macports

Calling make will call `uname` to decide to use Makefile.Linux or .Darwin.


### Make

For **Linux** and **plain Mac** build and install do

        $ make

For a **Macports** Erlang build & install do

        $ make -f Makefile.Macports

Depending on the way you built Erlang and/or Lua, you might have to do  
use sudo, e.g.:

	$ sudo make

If you get errors like '''error: ei.h: No such file or directory''' you   
need to set the pathes, see below


### Pathes

Pathes in the make files may need to be adjusted for version numbers  
and non-standard install prefixes.  
Do the following edits in the appropriate makefile.  

* Check for the erl_interface version number of your Erlang installation:

        $ ls /usr/lib/erlang/lib
        or
        $ ls /opt/local/lib/erlang/lib

* Put that version number in the path in first line of your makefile. 
  (Don't add '/lib')  E.g.:

        ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2

* Find erl_driver.h and ei.h

        $ find / -name erl_driver.h
        $ find / -name ei.h
       
        Note that especially with Macports you may find multiple identical
	 versions.

* Add their pathes to the second line in your make file. E.g. add to the  
 line beginning fith CFLAGS:

        -I/usr/local/lib/erl/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.6.2/include

* Find libei.a and liberl_interface.a

        $ find / -name libei.a
        $ find / -name liberl_interface.a

* Make sure that's in the path in your makefile's first line, but without  
  the trailing /lib.  E.g.:

        ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2


### Unit Test

	$ erl -pa ./ebin
	1> eunit:test(lua_test). 
	
See src/lua_test.erl for the source of these tests.


## Samples

### File

Samples are in src/lua_sample.erl  

	cd src
	erlc -I ../include lua_sample.erl 
	erl -pa ../ebin -noinput -noshell -run lua_sample hello -s init stop 

This should give you

	Hello from Lua!

and

	erl -pa ../ebin -noinput -noshell -run lua_sample type -s init stop 

should print

	Type of 23: number

**Take a look at src/lua_sample.erl to understand more about the usage of this API.**

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


## Crashing the VM

You can easily get the entire Erlang VM crashed, both by mistakes in  
the C code of this driver, the Lua VM, Lua extensions, or by errors  
you make in using this API. 

Which is exactly why instead of linking drivers in, like this lib,
the recommended way is a different one: using **ports**, like erlua,  
<http://gitorious.org/erlua>.

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

