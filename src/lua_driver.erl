%%%-------------------------------------------------------------------
%%% File        : lua_driver.erl
%%% Description : Linked-in Lua Driver Library
%%% Authors     : Ray Morgan, Darrik Mazey, Henning Diedrich
%%% Copyright   : (c) 2010 Eonblast Corporation for this fork
%%% License     : MIT for this fork
%%% Created     : 11 Apr 2009 Ray Morgan <@raycmorgan>
%%% Changed     : 03 Mar 2010 Darrik Mazey <darrik@darmasoft.com>
%%%-------------------------------------------------------------------
%%%
%%% This is a library for embedding Lua into Erlang. It provides a
%%% simple interface that is very similar to the Lua C API.
%%% Some mid- and higher level functions have been added.
%%%
%%% For explanation of the inner workings, see doc/INTRO.md
%%%
%%% Fork of Ray Morgan's erl-lua, http://github.com/raycmorgan/erl-lua
%%% Via Darrik Mazey's fork, http://github.com/darrikmazey/erlua-node
%%%
%%%-------------------------------------------------------------------

-module(lua_driver).

-export([open/0, close/1]).

-include("lua.hrl").

open() ->
  {ok, L} = load_driver(),
  #lua{port=L}.
  
close(#lua{port=Port}) ->
  port_close(Port).


%% Private functions
load_driver() ->
  SearchDir = filename:join([filename:dirname(code:which(lua_driver)), "..", "lib"]),
  case erl_ddll:load(SearchDir, liberlua) of
    ok ->
      {ok, open_port({spawn, 'liberlua'}, [binary])};
    Error ->
      Error
  end.
