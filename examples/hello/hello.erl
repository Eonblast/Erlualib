%% File    : hello.erl
%% Purpose : Brief demonstration of lua basics.
%% Author  : H. Diedrich, Eonblast Corporation
%% Use     $ cd ./examples/hello/ 
%%         $ erlc hello.erl 
%%         $ erl -pa ./ebin -s hello run -s init stop -noshell
%% Or      $ make hello

-module(hello).
-export([run/0]).

run() ->

    % execute a string
    lua:do("print(\"Hello, Robert(o)!\")"),

    % execute a file
    lua:dofile("hello.lua"),

    % separately parse, then execute
    {ok, State} = lua:loadstring("print(\"Hello, Chunk!\");"),
    lua:do(State),
    done.
