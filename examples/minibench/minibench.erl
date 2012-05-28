%% File    : mini.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/minibench/minibench.erl
%% Purpose : Benchmark for frequent calls to small Luerl scripts
%% Author  : Henning Diedrich
%% Use     $ cd ./examples/minibench 
%%         $ erlc minibench.erl 
%%         $ erl -pa ../../ebin -s minibench run -s init stop -noshell
%% Or      $ make minibench

-module(minibench).
-export([run/0]).

run() ->

    io:format("----------------------------------------------------------~n"),
    io:format("This is a benchmark of frequent fast calls into Luerl.~n"),

    % 0. empty measurement loop
    io:format("----------------------------------------------------------~n"),
    io:format("empty measurement loop~n"),
    I0 = 1000,
    {T0,_} = tc(fun() -> [ ok || _ <- lists:seq(1,I0)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling empty loop.~n", [T0,I0]),
    io:format("Per call: ~p microseconds.~n", [T0/I0]),
    

    % I. eval and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, parse and execute '1 + 1'~n"),
    I1 = 1000,
    {T1,_} = tc(fun() -> [lua:do("return 1 + 1") || _ <- lists:seq(1,I1)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T1,I1]),
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
    io:format("Execute pre-parsed function with '1 + 1', re-using state from last result~n"),
    I5b = 1000,
    State = lua:init(),
    lua:do("OneAndOne = function() return 1 + 1 end", State),
    {T5b,_State5b1} = tc(fun() -> lua:func(State, "OneAndOne") end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T5b,I5b]),
    io:format("Per call: ~p microseconds.~n", [T5b/I5b]),


    % VI. measure but parsing
    io:format("----------------------------------------------------------~n"),
    io:format("Pure parsing~n"),
    I6 = 1000,
    State6 = lua:init(),
    {T6,_State61} = tc(fun() -> [lua:load("return 1 + 1", State6) || _ <- lists:seq(1,I6)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T6,I6]),
    io:format("Per call: ~p microseconds.~n", [T6/I6]),


    % VII. Parse and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Parse and execute '1 + 1', re-using state~n"),
    I7 = 1000,
    State7 = lua:init(),
    {T7,_State71} = tc(fun() -> [lua:do("return 1 + 1", State7) || _ <- lists:seq(1,I7)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of 1 + 1.~n", [T7,I7]),
    io:format("Per call: ~p microseconds.~n", [T7/I7]),


    done.

% helper
selffeed(0, State) -> State;
selffeed(I, State) -> 
    lua:load("return 1 + 1", State),
    lua:do(State),
    selffeed(I-1, State).
    
tc(Fun) ->
    T1 = erlang:now(),
    R = apply(Fun, []),
    T2 = erlang:now(),
    {timer:now_diff(T2, T1), R}.