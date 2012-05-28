%% File    : mini.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/minibench2/minibench2.erl
%% Purpose : Benchmark for frequent calls to small Luerl scripts
%% Author  : Henning Diedrich
%% Use     $ cd ./examples/minibench2 
%%         $ erlc minibench2.erl 
%%         $ erl -pa ../../ebin -s minibench2 run -s init stop -noshell
%% Or      $ make minibench2

-module(minibench2).
-export([run/0]).

run() ->

    io:format("----------------------------------------------------------~n"),
    io:format("This is a benchmark of frequent fast calls into Luerl.~n"),

    % I. eval and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'~n"),
    I1 = 1000,
    {T1,_} = tc(fun() -> [ok=lua:do("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c") || _ <- lists:seq(1,I1)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T1,I1]),
    io:format("Per call: ~p microseconds.~n", [T1/I1]),
    

    % II. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("(Erlualib can't pre-parse w/o a state)~n"),
    

    % III. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("(Erlualib can't re-use kept state)~n"),


    % IV. measure but state initialization
    io:format("----------------------------------------------------------~n"),
    io:format("Pure initialization of Lua state~n"),
    I4 = 10,
    {T4,_State41} = tc(fun() -> [lua:init() || _ <- lists:seq(1,I4)] end),

    io:format("Adding Up: ~p microseconds for ~p x initializing a Lua port.~n", [T4,I4]),
    io:format("Per call: ~p microseconds.~n", [T4/I4]),


    % V. eval and execute, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("(Erlualib can't keep pre-parsed anonymous chunks)~n"),


    % Vb. eval once, then only execute, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed function with 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result~n"),
    I5b = 100000,
    State5b = lua:init(),
    ok = lua:do("function OneAndOne() a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c end", State5b),
    {T5b,_State5b1} = tc(fun() -> [{ok,"9000"} = lua:func(State5b, "OneAndOne")  || _ <- lists:seq(1,I5b)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T5b,I5b]),
    io:format("Per call: ~p microseconds.~n", [T5b/I5b]),

    % Vc. eval once, then only execute, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed empty function, re-using state from last result~n"),
    I5c = 1000,
    State5c = lua:init(),
    ok = lua:do("function EmptyFunc() end", State5c),
    {T5c,_State5c1} = tc(fun() -> [ok = lua:exec(State5c, "EmptyFunc")  || _ <- lists:seq(1,I5c)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T5c,I5c]),
    io:format("Per call: ~p microseconds.~n", [T5c/I5c]),


    % VI. measure but parsing
    io:format("----------------------------------------------------------~n"),
    io:format("Pure parsing~n"),
    I6 = 1000,
    State6 = lua:init(),
    {T6,_State61} = tc(fun() -> [lua:load("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c", State6) || _ <- lists:seq(1,I6)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T6,I6]),
    io:format("Per call: ~p microseconds.~n", [T6/I6]),


    % VII. Parse and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state~n"),
    I7 = 1000,
    State7 = lua:init(),
    {T7,_State71} = tc(fun() -> [lua:do("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c", State7) || _ <- lists:seq(1,I7)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T7,I7]),
    io:format("Per call: ~p microseconds.~n", [T7/I7]),

    done.

tc(Fun) ->
    T1 = erlang:now(),
    R = apply(Fun, []),
    T2 = erlang:now(),
    {timer:now_diff(T2, T1), R}.