%%%-------------------------------------------------------------------
%%% File        : lua.erl
%%% Description : Linked-in Lua Driver Library
%%% Authors     : Ray Morgan, Darrik Mazey, Henning Diedrich, Robert Virding
%%% Copyright   : (c) 2010 Eonblast Corporation for this fork
%%% License     : MIT for this fork
%%% Created     : 11 Apr 2009 Ray Morgan <@raycmorgan>
%%% Changed     : 27 May 2012 H. Diedrich <hd2012@eonblast.com>
%%%-------------------------------------------------------------------
%%%
%%% This is a library for embedding Lua into Erlang. It provides a
%%% simple interface that is very similar to the Lua C API and
%%% and a higher level functions following the naming pattern of Luerl.
%%%
%%% For explanation of the inner workings, see doc/INTRO.md
%%%
%%% Fork of Ray Morgan's erl-lua, http://github.com/raycmorgan/erl-lua
%%% Via Darrik Mazey's fork, http://github.com/darrikmazey/erlua-node
%%%
%%% Excerpts Lua C API: http://www.lua.org/manual/5.1/manual.html#3
%%%
%%%-------------------------------------------------------------------

-module(lua).

% low level +
-export([new_state/0, init/0,
         close/1,
         call/3,
         concat/2,
         dostring/2,
         dofile/2,
         dump_table/2,
         getfield/3,
         getglobal/2,
         gettable/3,
         gettable/2,
         gettop/1,
         loadstring/1,
         loadstring/2,
         next/2,
         pop/1,
         push/2,
         pushboolean/2,
         pushinteger/2,
         pushstring/2,
         pushnil/1,
         pushnumber/2,
         remove/1,
         remove/2,
         setfield/3,
         setglobal/2,
         toboolean/2,
         tointeger/2,
         tolstring/2,
         tonumber/2,
         type/2,
         type_atom/2
         ]).

% convenience names for Lua C API functions
-export([stacksize/1
         ]).

% verbatim lua language functions
-export([
		c_print/2, port_print/2, print/2,
		c_print_variable/2, print_variable/2
         ]).

% Pragmatic semi-hard wired signature calls
-export([
		exec/2, exec/3, exec/4, exec/5,
		func/2, func/3, func/4, func/5 
         ]).

% Luerl compatibility
-export([evalfile/1,evalfile/2,
        do/1,do/2,dofile/1,
        load/1, load/2,                 % alias of loadstring
        stop/1,
        gc/1
        ]).
%-export([eval/1,eval/2,evalfile/1,evalfile/2,
%        do/1,do/2,dofile/1,dofile/2,
%        loadfile/1,
%        call/2,call/3,
%        stop/1,
%        gc/1,decode/2,encode/2 % all three not implemented.
%        ]).

-include("lua.hrl").
-include("lua_api.hrl").

%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  Low Level (I): Port Driver Functions
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Note that a 'port' in this context has nothing to do with sockets.

%----------------------------------------------------------------------
% @doc Open the Erlang driver port. 

% new_state() -> State.
new_state() ->
    {ok, lua_driver:open()}.

% Luerl compatibility
% init() -> State.
init() ->
    lua_driver:open().
    
%----------------------------------------------------------------------
% @doc Close the Erlang driver port. 

close(L) ->

    lua_driver:close(L).

%----------------------------------------------------------------------
% @doc Send one command to the port. 
% Response needs to be received, e.g. by a receive_* function.
%----------------------------------------------------------------------
%:% Excerpts from:
%:% http://www.erlang.org/doc/man/erlang.html#port_command-2
%:% http://www.erlang.org/doc/man/erlang.html#port_command-3
%:%
%:% port_command(Port, Data) -> true
%:%
%:% Types: 
%:%	Port = port() | atom() 
%:% 	Data = iodata()
%:%
%:% Sends data to a port. Same as Port ! {self(), {command, Data}}
%:% except for the error behaviour. Any process may
%:% send data to a port with port_command/2, not only the port
%:% owner (the connected process).
%:% 
%:% In short: port_command(Port, Data) has a cleaner and more
%:% logical behaviour than Port ! {self(), {command, Data}}.
%:% 
%:% If the port is busy, the calling process will be suspended
%:% until the port is not busy anymore. See below, /3.
%:% 
%:% For Failures, see below.
%:%
%:% port_command(Port, Data, OptionList) -> true|false
%:% 
%:% Types: 
%:% 	Port = port() | atom() 
%:% 	Data = iodata() 
%:% 	OptionList = [Option]
%:%	Option = force Option = nosuspend
%:% 
%:% Sends data to a port. port_command(Port, Data, []) equals
%:% port_command(Port, Data).
%:% 
%:% If the port command is aborted, false is returned; otherwise,
%:% true is returned.
%:% 
%:% If the port is busy, the calling process will be suspended
%:% until the port is not busy anymore.
%:% 
%:% Options:
%:% 
%:% force
%:%	The calling process will not be suspended if the port is
%:% 	busy; instead, the port command is forced through. The call
%:% 	will fail with a notsup exception if the driver of the port
%:% 	does not support this. For more information see the
%:% 	ERL_DRV_FLAG_SOFT_BUSY driver flag. 
%:% nosuspend
%:% 	The calling process will not be suspended if the port is busy;
%:% 	instead, the port command is aborted and false is returned.
%:% 
%:% Failures:
%:% 
%:% badarg
%:%	If Port is not an open port or the registered name of an
%:% 	open port. 
%:% badarg 
%:%	If Data is not a valid io list. 
%:% badarg 
%:%	If OptionList is not a valid option list. 
%:% notsup
%:%	If the force option has been passed, but the driver of the
%:%   port does not allow forcing through a busy port. 
%----------------------------------------------------------------------
    
command(#lua{port=Port}, Data) ->

    port_command(Port, term_to_binary(Data)).

%----------------------------------------------------------------------
% @doc Receive the return from a port call.
% Handles errors gracefully, returning { error, other, Reason, <Msg> }.
% Handles missing port gracefully, returning { error, no_port, Reason, <Msg> }.
% Makes NO ASSUMPTION about return values on the stack.
% If there are return values and you don't pop them, you unbalance the 
% stack, which will crash the Erlang VM over time.
% @param L Lua State used for error diagnosis
% @returns ok | { other, Other} | { error, Type, Reason, Msg } 

receive_return(L) ->
    receive
        ok ->
            ok;
        error ->
            {error, lua_error};
        {error, Reason} ->
            case erlang:port_info(L#lua.port) of
                undefined -> { error, no_port, Reason, 'Erlualib: the Lua state engine may have crashed or has not been started.' };
                _ -> { error, other, Reason, 'Erlualib: error returned from Lua port call.' }
            end;
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.

    
%----------------------------------------------------------------------
% @doc Receive the return from a port call, with a parameter.
% Assumes return to be one string, on the stack.
% If there are other return values and you don't pop them, you unbalance
% the stack, which will crash the Erlang VM over time.
% @returns {ok, Str} | { other, Other} | { error, lua_error }
% TODO: add error propagation. Redundant w/receive_return(L)?

receive_return_values() ->
    receive
        {ok, Str} ->
            {ok, Str};
        error ->
            {error, lua_error};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.



%%%-------------------------------------------------------------------
% @doc get type atom for Lua type integer

lua_type_to_atom(0) -> nil;
lua_type_to_atom(1) -> boolean;
lua_type_to_atom(2) -> light_user_data;
lua_type_to_atom(3) -> number;
lua_type_to_atom(4) -> string;
lua_type_to_atom(5) -> table;
lua_type_to_atom(6) -> function;
lua_type_to_atom(7) -> user_data;
lua_type_to_atom(8) -> thread;
lua_type_to_atom(_) -> unknown.

% @deprecated
% type_int_to_atom(TypeInt) when is_integer(TypeInt) ->
%    case TypeInt of
%        0 ->
%            Atom = nil;
%        1 ->
%            Atom = boolean;
%        2 ->
%            Atom = light_user_data;
%        3 ->
%            Atom = number;
%        4 ->
%            Atom = string;
%        5 ->
%            Atom = table;
%        6 ->
%           Atom = function;
%       7 ->
%            Atom = user_data;
%        8 ->
%            Atom = thread;
%        _ ->
%            Atom = unknown
%    end,
%    Atom.

%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  Low Level (II): Verbatim Lua C API Function Calls
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%

%----------------------------------------------------------------------
% @doc Call a function, exactly with what is found on the stack
% @param Args Int number of arguments (lying on stack)
% @param Results Int number of results (lying on stack)
%

call(L, Args, Results) ->
    command(L, {?ERL_LUA_CALL, Args, Results}),
    receive_return(L).

%----------------------------------------------------------------------
% lua_call: http://www.lua.org/manual/5.1/manual.html#lua_call
%
% [-(nargs + 1), +nresults, e] <- described below.
%
% void lua_call (lua_State *L, int nargs, int nresults);
%
% Calls a function.
% 
% To call a function you must use the following protocol: first, the
% function to be called is pushed onto the stack; then, the
% arguments to the function are pushed in direct order; that is, the
% first argument is pushed first. Finally you call lua_call; nargs
% is the number of arguments that you pushed onto the stack. All
% arguments and the function value are popped from the stack when
% the function is called. The function results are pushed onto the
% stack when the function returns. The number of results is adjusted
% to nresults, unless nresults is LUA_MULTRET. In this case, all
% results from the function are pushed. Lua takes care that the
% returned values fit into the stack space. The function results are
% pushed onto the stack in direct order (the first result is pushed
% first), so that after the call the last result is on the top of
% the stack.
%
% Any error inside the called function is propagated upwards (with a 
% longjmp). 
%----------------------------------------------------------------------

    
concat(L, N) ->
    command(L, {?ERL_LUA_CONCAT, N}).


getfield(L, global, Name) ->
    getglobal(L, Name);
getfield(L, Index, Name) ->
    command(L, {?ERL_LUA_GETFIELD, Index, Name}),
    receive_return(L).
    
getglobal(L, Name) ->
    command(L, {?ERL_LUA_GETGLOBAL, Name}),
    receive_return(L).

gettable(L, global, Name) when is_atom(Name) ->
    gettable(L, global, atom_to_list(Name));
gettable(L, global, Name) ->
    getfield(L, global, Name),
    {ok, T} = gettop(L),
    Table = gettable(L, T),
    lua:remove(L, T),
    Table.

gettable(L, T) ->
    pushnil(L),
    gettablekey(L, T).

gettablekey(L, T) ->
    next(L, T),
    {ok, OT} = gettop(L),
    case OT of
        T ->
            [];
        _ ->
      %% values can be tables or anything else
      %% recurse on tables
            case type_atom(L, -1) of
                {ok, table} ->
          %% keys can be strings or numbers
          case type_atom(L, -2) of 
            {ok, number} -> 
              {ok, Key} = tonumber(L, -2);
            _KT -> 
              {ok, Key} = tolstring(L, -2)
          end,
                    KV = {Key, gettable(L, T + 2)},
                    remove(L, -1),
                    [KV|gettablekey(L, T)];
                _TA ->
                    {ok, _Type, Val} = pop(L),
          %% keys can be strings or numbers
          case type_atom(L, -1) of 
            {ok, number} -> 
              {ok, Key} = tonumber(L, -1);
            _KT -> 
              {ok, Key} = tolstring(L, -1)
          end,
                    [{Key, Val}|gettablekey(L, T)]
        end
    end.

%----------------------------------------------------------------------
% @doc Lua Stack size. 
% Get Number of Elements on the Stack, which
% equals top index of stack, since index count starts at 1.
% Same as stacksize(L). Exists for its name being closer to the Lua C API.
% @param L Lua State
% @return Int number of elements on the stack

gettop(L) ->
    command(L, {?ERL_LUA_GETTOP}),
    receive_return_values().

%----------------------------------------------------------------------
% @doc Lua Stack size. 
% Get Number of Elements on the Stack, which
% equals top index of stack, since index count starts at 1.
% Same as gettop(L). Exists for its name being more self-explanatory.
% @param L Lua State
% @return Int number of elements on the stack

stacksize(L) ->
    command(L, {?ERL_LUA_GETTOP}),
    receive_return_values().

%----------------------------------------------------------------------
% lua_gettop: http://www.lua.org/manual/5.1/manual.html#lua_gettop
% 
% [-0, +0, -]
% 
% int lua_gettop (lua_State *L);
% 
% Returns the index of the top element in the stack. Because indices
% start at 1, this result is equal to the number of elements in the
% stack (and so 0 means an empty stack). 
%----------------------------------------------------------------------
    
next(L, Index) ->
    command(L, {?ERL_LUA_NEXT, Index}),
    receive_return(L).

pop(L) ->
        {ok, R} = gettop(L),
        if
            R < 1 ->
                {ok, empty};
            true ->
                {ok, T} = type_atom(L, R),
                case T of
                    number ->
                        {ok, N} = tonumber(L, R),
                        remove(L, R),
                        {ok, number, N};
                    string ->
                        {ok, N} = tolstring(L, R),
                        remove(L, R),
                        {ok, string, N};
                    boolean ->
                        {other, N} = toboolean(L, R),
                        remove(L, R),
                        {ok, boolean, N};
                    function ->
                        remove(L, R),
                      {ok, function, function};
                    _ ->
                        remove(L, R),
                        {ok, unknown, unknown}
                end
        end.

push(L, Term) when is_number(Term) ->
    pushnumber(L, Term);
push(L, Term) when is_list(Term) ->
    pushstring(L, Term);
push(L, true) ->
    pushboolean(L, true);
push(L, false) ->
    pushboolean(L, false);
push(L, Term) when is_atom(Term) ->
    pushstring(L, atom_to_list(Term)).

pushboolean(L, Bool) ->
    command(L, {?ERL_LUA_PUSHBOOLEAN, Bool}),
    receive_return(L).
    
pushinteger(L, Int) when is_integer(Int) ->
    command(L, {?ERL_LUA_PUSHINTEGER, Int}),
    receive_return(L).

pushstring(L, String) when is_list(String) ->
    command(L, {?ERL_LUA_PUSHSTRING, String}),
    receive_return(L).

pushnil(L) ->
    command(L, {?ERL_LUA_PUSHNIL}),
    receive_return(L).
    
pushnumber(L, Num) when is_number(Num) ->
    command(L, {?ERL_LUA_PUSHNUMBER, Num}),
    receive_return(L).

remove(L) ->
    remove(L, gettop(L)).

remove(L, Index) ->
    command(L, {?ERL_LUA_REMOVE, Index}),
    receive_return(L).
    
setfield(L, global, Name) ->
    setglobal(L, Name);
setfield(L, Index, Name) ->
    command(L, {?ERL_LUA_SETFIELD, Index, Name}),
    receive_return(L).

setglobal(L, Name) ->
    command(L, {?ERL_LUA_SETGLOBAL, Name}),
    receive_return(L).

toboolean(L, Index) ->
    command(L, {?ERL_LUA_TOBOOLEAN, Index}),
    receive_return_values().

tointeger(L, Index) ->
    command(L, {?ERL_LUA_TOINTEGER, Index}),
    receive_return_values().

tolstring(L, Index) ->
    command(L, {?ERL_LUA_TOLSTRING, Index}),
    receive_return_values().

tonumber(L, Index) ->
    command(L, {?ERL_LUA_TONUMBER, Index}),
    {ok, Value} = receive_return_values(),
    Value2 = list_to_binary(Value),
    {ok, binary_to_term(Value2)}.

type(L, Index) ->
    command(L, {?ERL_LUA_TYPE, Index}),
    receive_return_values().

type_atom(L, Index) ->
    command(L, {?ERL_LUA_TYPE, Index}),
    R = receive_return_values(),
    case R of
        {ok, Str} ->
            {ok, lua_type_to_atom(Str)};
        _ ->
            R
    end.

%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  Mid Level (I): Verbatim Lua C API 'Aux' Calls
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Use of low level calls, and replication of source, for performance.

dostring(#lua{port=Port}=L, Code) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOSTRING, Code})),
    receive_return(L).

dofile(#lua{port=Port}=L, Filename) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOFILE, Filename})),
    receive_return(L);

% Erlua compatibility: inverted parameters, state in return.
% (Needs to be written here, grouped with the other dofile/2.)
dofile(Filename, #lua{port=Port}=L) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOFILE, Filename})),
    {receive_return(L), L}.

% parse and compile, leave result at stack top as anonymous function.
loadstring(Chunk) ->
    loadstring(lua:init(), Chunk).
    
loadstring(#lua{port=Port}=L, Chunk) ->
    case port_command(Port, term_to_binary({?ERL_LUAL_LOADSTRING, Chunk})) of
        true ->
            {ok, L};
        false ->
            {error, L}
    end.  

%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  Mid Level (II): Pragmatic Standard Calls
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Use of low level calls, and replication of source, for performance.


 exec(#lua{port=Port}=L, Name) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_0_0, Name})),
    receive_return(L).

 exec(#lua{port=Port}=L, Name, P1) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_1_0, Name, P1})),
    receive_return(L).

 exec(#lua{port=Port}=L, Name, P1, P2) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_2_0, Name, P1, P2})),
    receive_return(L).

 exec(#lua{port=Port}=L, Name, P1, P2, P3) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_3_0, Name, P1, P2, P3})),
    receive_return(L).

 func(#lua{port=Port}, Name) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_0_1, Name})),
    receive_return_values().

 func(#lua{port=Port}, Name, P1) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_1_1, Name, P1})),
    receive_return_values().

 func(#lua{port=Port}, Name, P1, P2) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_2_1, Name, P1, P2})),
    receive_return_values().

 func(#lua{port=Port}, Name, P1, P2, P3) ->
    port_command(Port, term_to_binary({?ERL_LUAC_FUNC_3_1, Name, P1, P2, P3})),
    receive_return_values().


%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  Mid Level (III): Luerl Compatibility
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Use of low level calls, and replication of source, for performance.

%% luerl:eval(String|Binary|Form[, State]) -> Result.
%eval(Chunk) ->
%    eval(Chunk, init()).

% eval(Chunk, St) ->
%    try do(Chunk, St) of
%        {Ret,_} -> {ok,Ret}
%    catch 
%         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
%    end.
%    
%% luerl:evalfile(Path[, State]) -> {ok, Result} | {error,Reason}.
evalfile(Path) ->
    evalfile(Path, init()).

evalfile(Path, St) ->
    try dofile(Path, St) of
        {Ret,_} -> {ok,Ret}
    catch 
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%%% lua:do(String|Binary|Form[, State]) -> {Result, NewState} 
do(#lua{}=St) -> % execute an anon func at top of stack as per load/loadstring()
    command(St, {?ERL_LUA_CALL, 0, 0}),
    receive_return(St);

do(SB) ->
    do(SB, lua:init()).    

do(B, #lua{}=St) when is_binary(B) ->
    do(binary_to_list(B), St);
do(S, #lua{}=St) when is_list(S) -> % same as dostring/2 but inverted parameter order
    port_command(St#lua.port, term_to_binary({?ERL_LUAL_DOSTRING, S})),
    receive_return(St).

%% dofile(Path[, State]) -> {Result, NewState}.
dofile(Path) ->
    dofile(Path, init()).
% for dofile(Path, St) see above.

%%% load(String|Binary) -> {ok,Form}.
% The following is almost identical to loadstring() for Luerl compatibility.
load(Chunk) ->
    load(Chunk, lua:init()).
    
load(Chunk, #lua{}=St) when is_binary(Chunk) ->
    load(binary_to_list(Chunk), St);
load(Chunk, #lua{}=St) when is_list(Chunk) ->
    port_command(St#lua.port, term_to_binary({?ERL_LUAL_LOADSTRING, Chunk})),
    {receive_return(St),St}.

%%% compilefile(Path) -> {ok,Form}.
%loadfile(Path) ->
%    {ok,Bin} = file:read_file(Path),
%    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
%    luerl_parse:chunk(Ts).

%%% call(Form, Terms, State) -> {Result,State}
%
%call(C, Ts) -> call(C, Ts, init()).
%
%call(C, Ts, St0) ->
%    {Lts,St1} = encode_list(Ts, St0),
%    {Lrs,St2} = luerl_eval:chunk(C, Lts, St1),
%    Rs = decode_list(Lrs, St2),
%    {Rs,St2}.

%%% stop(State) -> GCedState.
stop(St) -> 
    luerl_eval:gc(St).

%%% gc(State) -> State.
gc(_St) ->
    exit(not_implemented).

% For function head matching, from luerl.h:
% -record(tref, {i}).				%Table reference, index

%% encode_list([Term], State) -> {[LuerlTerm],State}.
%% encode(Term, State) -> {LuerlTerm,State}.
% encode_list(Ts, St) ->
%    lists:mapfoldl(fun encode/2, St, Ts).
% encode(B, St) when is_binary(B) -> {B,St};
% encode(A, St) when is_atom(A) -> {atom_to_binary(A, latin1),St};
% encode(I, St) when is_integer(I) -> {float(I),St};
% encode(F, St) when is_float(F) -> {F,St};
% encode(B, St) when is_boolean(B) -> {B,St};
% encode(nil, St) -> {nil,St};
% encode(_L, _St0) ->
%     exit(not_implemented).

%% decode_list([LuerlTerm], State) -> [Term].
%% decode(LuerlTerm, State) -> Term.
% decode_list(Lts, St) ->
%     lists:map(fun (Lt) -> decode(Lt, St) end, Lts).
% decode(B, _) when is_binary(B) -> B;
% decode(N, _) when is_number(N) -> N;
% decode(B, _) when is_boolean(B) -> B;
% decode(nil, _) -> nil;
% decode(#tref{i=_N}, _St) ->
%     exit(not_implemented);
% decode({function,Fun}, _) -> {function,Fun}.

%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  High Level (I): Verbatim Lua Language Functions
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Use of low level calls, and replication of source, for performance.


port_print(L, String) ->

	lua:getglobal(L, "print"),          % put "print" global on stack
	lua:pushstring(L, String),   		% put text on top
	lua:call(L, 1, 0).                  % execute using stack top 2


%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  High Level (II): C-Side Implemented Verbatim Lua Language Functions
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% Use of low level calls, and replication of source, for performance.


%%
% @doc Print a String
% @param L lua state
% @param Name string variable name

c_print(L, String) ->

    command(L, {?ERL_LUAC_PRINT, String}),
    receive_return(L).

	% This is faster than port_print, because it handles
	% the entire logic in c, directly against the Lua C API
	% and uses the Erlang port only once.

%%
% @doc Print the Contents of a Variable
% @param L lua state
% @param Name string variable name

c_print_variable(L, Name) ->

    command(L, {?ERL_LUAC_PRINT_VARIABLE, Name}),
    receive_return(L).


%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  High Level (III): Verbatim Lua Language Function Emulation
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%
	
print(L, String) -> c_print(L, String).
print_variable(L, Name) -> c_print_variable(L, Name).
	
%
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
%%%  High Level (IV): Composite Functions
%%
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%
% 
	
dump_table(L, N) ->
    dump_table(L, N, none).

dump_table(L, N, NextKey) ->

    io:format("dump_table(~p, ~p)~n", [N, NextKey]),

    case NextKey of
        none ->
            Str=lists:concat(["tmpkey, tmpval = pairs(", N, ")(", N, ")"]);
        NK ->
            Str=lists:concat(["tmpkey, tmpval = pairs(", N, ")(", N, ", \"", NK, "\")"])
    end,

    io:format("Str == ~p~n", [Str]),
    lual:dostring(L, Str),
    lua:getglobal(L, "tmpkey"),
    {ok, T} = lua:type(L, -1),

    case T of
        ?LUA_TNIL -> [];
        _ ->
            {ok, K} = lua:tolstring(L, -1),
            lua:remove(L, -1),
            lua:getglobal(L, "tmpval"),
            {ok, VT} = lua:type(L, -1),

            io:format("type == ~p~n", [VT]),

            case VT of
                ?LUA_TNUMBER ->
                    {ok, V} = lua:tonumber(L, -1);
                ?LUA_TTABLE ->
                    V = dump_table(L, lists:concat([N, ".", K]));
                _ ->
                    {ok, V} = lua:tolstring(L, -1)
            end,

            lua:remove(L, -1),
            [{list_to_atom(K),V}|dump_table(L, N, K)]
    end.

