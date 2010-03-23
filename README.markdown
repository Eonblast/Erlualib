This is a fork of raycmorgan's erl-lua erlang-lua integration library.  The original is available [here](http://github.com/raycmorgan/erl-lua)

Example:

	1> {ok, L} = lua:new_state().
	{ok,{lua,#Port<0.771>}}
	2> lua:dostring(L, "t = {a=1;b='test';c={a='whatever'}}").
	ok
	3> lua:gettable(L, global, "t").
	[{"a",1},{"c",[{"a","whatever"}]},{"b","test"}]

The aim of this library is to enable a high-level embedded-lua API for use in configuration processing and extension coding for erlang projects.  For a real-world example of this in action, see [darrikmazey/erlmon](http://github.com/darrikmazey/erlmon)

