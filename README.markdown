## Erlilua

Linked in Lua Driver for Erlang. Execute Lua scripts in Erlang the fastest way. 

This is a fork of Ray Morgan's [erl-lua](http://github.com/raycmorgan/erl-lua) Erlang-Lua driver.
This fork here goes via Darrik Mazey's [fork](http://github.com/darrikmazey/erlua-node)

#### Example:

	1> {ok, L} = lua:new_state().
	{ok,{lua,#Port<0.771>}}
	2> lua:dostring(L, "t = {a=1;b='test';c={a='whatever'}}").
	ok
	3> lua:gettable(L, global, "t").
	[{"a",1},{"c",[{"a","whatever"}]},{"b","test"}]

The aim of this library is to enable a high-level embedded-lua API for use in configuration
processing and extension coding for erlang projects.  For a real-world example of this in 
action, see [darrikmazey/erlmon](http://github.com/darrikmazey/erlmon).

## Builing

* Edit makefile pathes
* make

#### Details: Makefiles

There are three Makefiles provided

* Linux: Makefile.Linux
* Mac/Darwin: Makefile.Darwin
* Macports: Makefile.Macports

For Linux and plain Mac/Darwin source install simply do

	make

For a Macports Erlang install on Mac do

	make -f Makefile.Macports

#### Pathes

Pathes in the make files may need to be adjusted for version numbers and non-standard install prefixes.
Do the following edits in the appropriate makefile:

* Check for the erl_interface version number of your Erlang installation: 
	
	ls /usr/lib/erlang/lib 
	- or -
	ls /opt/local/lib/erlang/lib

* Put that version number in the path in first line of your makefile. (Don't add '/lib')  E.g.:

	ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2

* Find erl_driver.h and ei.h

	find / -name erl_driver.h
	find / -name ei.h

* Add their pathes to second line in your make file. E.g. add to the line beginning fith CFLAGS:

	-I/usr/local/lib/erl/usr/include -I/usr/local/lib/erlang/lib/erl_interface-3.6.2/include

* Find libei.a and liberl_interface.a

	find / -name libei.a
	find / -name liberl_interface.a

* Make sure that's in the path in your makefile's first line, but without the trailing /lib.  E.g.:

	ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.2


