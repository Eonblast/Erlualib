## Erlualib

Linked in Lua Driver for Erlang.

Execute Lua scripts in Erlang the fastest and less secure way. See below, 'Crashing the VM'.

For an intro to the Lua API and the structure of this package see [Intro](http:doc/INTRO.markdown)

This is a fork of Ray Morgan's [erl-lua](http://github.com/raycmorgan/erl-lua) Erlang-Lua driver.
This fork here goes via Darrik Mazey's [fork](http://github.com/darrikmazey/erlua-node).

This is a library for embedding Lua into Erlang. 
It provides a simple interface that is very similar to the Lua C API.
Ray is also working on a higher level API to simplify things further for the original erl-lua.

WARNING: This is definitely not fully tested.
Still a bunch of work to be done.
If you are careful though, it should be pretty stable (no promises though).

<div style='border: 2 px solid red; padding: 20px'>
Doing as this driver does is discouraged, unless you have good reasons for it.
It is safer to use a port, because this eliminates the danger that an
 error in the extension can crash the entire Erlang VM.
</div>

#### Example:

	1> {ok, L} = lua:new_state().
	{ok,{lua,#Port<0.771>}}
	2> lua:dostring(L, "t = {a=1;b='test';c={a='whatever'}}").
	ok
	3> lua:gettable(L, global, "t").
	[{"a",1},{"c",[{"a","whatever"}]},{"b","test"}]

The aim of this library is to enable a high-level embedded-lua API 
for use in configuration processing and extension coding for erlang projects.
For a real-world example Darrik's erl-lua in action, see
[darrikmazey/erlmon](http://github.com/darrikmazey/erlmon).

## Building

* Quick
* Makefiles
* Pathes
* Make
* Unit Test

### Quick

You might be lucky, just try and execute 

	$ make

It's really only pathes that need to be fixed of this doesn't work. See 'Pathes', below.

If all worked out, see 'Unit Test' below to check health, then 'Samples'.


### Makefiles

There are three Makefiles provided

* Linux: Makefile.Linux
* Mac/Darwin: Makefile.Darwin
* Macports: Makefile.Macports

Calling make will call `uname` to decide to use Makefile.Linux or .Darwin.
If you installed Erlang using Macports, see below.

### Pathes

Pathes in the make files may need to be adjusted for version numbers and non-standard install prefixes.
Do the following edits in the appropriate makefile:

* Check for the erl_interface version number of your Erlang installation: 

	$ ls /usr/lib/erlang/lib 
	or
	$ ls /opt/local/lib/erlang/lib

* Put that version number in the path in first line of your makefile. (Don't add '/lib')  E.g.:

	ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2

* Find erl_driver.h and ei.h

	$ find / -name erl_driver.h
	$ find / -name ei.h

* Add their pathes to second line in your make file. E.g. add to the line beginning fith CFLAGS:

	-I/usr/local/lib/erl/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.6.2/include

* Find libei.a and liberl_interface.a

	$ find / -name libei.a
	$ find / -name liberl_interface.a

* Make sure that's in the path in your makefile's first line, but without the trailing /lib.  E.g.:

	ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2


### Make

For Linux and plain Mac/Darwin source install simply do

        $ make

For a Macports Erlang install on Mac do

        $ make -f Makefile.Macports

Depending on the way you built Erlang and/or Lua, you might have to do use sudo, e.g.:

	$ sudo make


### Unit Test

	$ erl -pa ./ebin
	1> eunit:test(lua_test). 


## Samples

Start the Erlang Shell with a path into ebin

	$ erl -pa ./ebin

Try this to print "Hello from Lua" from Lua:

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
	
This prints the type of the pushed 23 into the shell:

	{ok, "number"}

## Crashing the VM

You can <b>easily</b> get the Erlang VM crashed both by mistakes in the C code of this driver,
or by errors you make in using the API. Of course, stack errors are a sure way to get into trouble.

This is a fast way to crash the Erlang process by a Lua stack underrun:

	1> {ok, L} = lua:new_state(), lua:call(L, 1, 0).  
	Segmentation fault
	$ |

Try this bit, running it from the Erlang shell and watch it shutting Erlang down for good. 

To test stack overrun, here is an abridged snip from src/lua_crash_test.erl:

	run() ->

        	{ok, L} = lua:new_state(),
       		crash_pusher(L,1,100000).

	crash_pusher(L,N,O) ->

        	lua:pushnumber(L, 42),
        	crash_pusher(L,N+1,O).

Which also results in a killed VM. Start the Erlang shell from the project root directory
to get something like this:

	$ erl -pa ./ebin
	Erlang R13B01 (erts-5.7.2) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.7.2  (abort with ^G)
 	1> lua_crash_test:crash_test().
	100000 pushes.
	200000 pushes.
	300000 pushes.
	400000 pushes.
	500000 pushes.	
	600000 pushes.
	700000 pushes.
	Segmentation fault
	$ |  

Again: both bugs in the c source of this driver, as well as <b>errors when using the API</b> -
 as demonstrated above - can kill the entire Erlang VM. Risking this is exactly <b>not</b>
 what Erlang is about.
