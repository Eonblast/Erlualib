/**-------------------------------------------------------------------
*** File        : commands.c
*** Description : C Implementation of Commands for Erlualib
*** Authors     : Ray Morgan, Darrik Mazey, Henning Diedrich
*** Copyright   : (c) 2010 Eonblast Corporation for this fork
*** License     : MIT for this fork
*** Created     : 11 Apr 2009 Ray C. Morgan <@raycmorgan>
*** Changed     : 20 Nov 2010 H. Diedrich <hd2010@eonblast.com>
***-------------------------------------------------------------------
***
*** Implentation of access to the Lua C API and higher functions.
***
*** The functions here are called from process() in erlua.c, which is
*** the interface against the Erlang C extension API.
***
*** Extension requires 
*** - Implementing a function here
*** - Adding its declaration to c_src/erl_lua.h
*** - Adding one constant to c_src/commands.h and include/lua_api.hrl
*** - Adding a case for that constant to process() in c_src/erlua.c
*** - Implementing a wrapper Erlang function call for it in lua.erl
*** See INTRO.md
***
***-----------------------------------------------------------------*/

#include <erl_driver.h>
#include <ei.h>
#include <lua.h>
#include <lauxlib.h>
#include <string.h>

#include "erlua.h"
#include "commands.h"

static void reply_ok(lua_drv_t *driver_data);
static void reply_error(lua_drv_t *driver_data);
static char* decode_string(char *buf, int *index);


/* ********************************************************************
 *
 *   General Lua API
 *
 * *******************************************************************/

void
erl_lua_call(lua_drv_t *driver_data, char *buf, int index)
{
  long args, results;
  
  ei_decode_long(buf, &index, &args);
  ei_decode_long(buf, &index, &results);
  
  lua_call(driver_data->L, args, results);
  
  reply_ok(driver_data);
}

void
erl_lua_concat(lua_drv_t *driver_data, char *buf, int index)
{
  long n;
  
  ei_decode_long(buf, &index, &n);
  
  lua_concat(driver_data->L, n);
  
  reply_ok(driver_data);
}

void
erl_lua_getfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_getfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_getglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;

  name = decode_string(buf, &index);
  
  lua_getglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_gettop(lua_drv_t *driver_data, char *buf, int index)
{
  int size;
  
  size = lua_gettop(driver_data->L);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) size,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_next(lua_drv_t *driver_data, char *buf, int index)
{
  long n;
  
  ei_decode_long(buf, &index, &n);
  
	lua_next(driver_data->L, n);

	reply_ok(driver_data);
}

void
erl_lua_pushboolean(lua_drv_t *driver_data, char *buf, int index)
{
  int b;
  
  ei_decode_boolean(buf, &index, &b);
  
  lua_pushboolean(driver_data->L, b);
  
  reply_ok(driver_data);
}

void
erl_lua_pushinteger(lua_drv_t *driver_data, char *buf, int index)
{
  long long num;
  
  ei_decode_longlong(buf, &index, &num);
  
  lua_pushinteger(driver_data->L, num);
  
  reply_ok(driver_data);
}

void
erl_lua_pushstring(lua_drv_t *driver_data, char *buf, int index)
{
  char *str;
  
  str = decode_string(buf, &index);
  
  lua_pushstring(driver_data->L, str);
  
  reply_ok(driver_data);
  free(str);
}

void
erl_lua_pushnil(lua_drv_t *driver_data, char *buf, int index)
{
  lua_pushnil(driver_data->L);
  reply_ok(driver_data);
}

void
erl_lua_pushnumber(lua_drv_t *driver_data, char *buf, int index)
{
  double dnum;
  long long lnum;
  int type, len;
  
  ei_get_type(buf, &index, &type, &len);
  
  switch (type) {
  case ERL_FLOAT_EXT:
    ei_decode_double(buf, &index, &dnum);
    lua_pushnumber(driver_data->L, dnum);
    break;
  default:
    ei_decode_longlong(buf, &index, &lnum);
    lua_pushnumber(driver_data->L, lnum);
    break;
  }
  
  reply_ok(driver_data);
}

void
erl_lua_remove(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  
  ei_decode_long(buf, &index, &i);
  
  lua_remove(driver_data->L, i);
  
  reply_ok(driver_data);
}

void
erl_lua_setfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_setfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_setglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;
  
  name = decode_string(buf, &index);
  
  lua_setglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_toboolean(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int res;
  ErlDrvTermData spec[2];
  spec[0] = ERL_DRV_ATOM;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_toboolean(driver_data->L, i);
  if (res)
    spec[1] = driver_mk_atom("true");
  else
    spec[1] = driver_mk_atom("false");
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tointeger(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  long long res;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_tointeger(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) res,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tolstring(lua_drv_t *driver_data, char *buf, int index)
{
  size_t len;
  long i;
  const char *str;
  
  ei_decode_long(buf, &index, &i);
  
  str = lua_tolstring(driver_data->L, i, &len);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_STRING, (ErlDrvTermData) str, len,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


/* TODO: return a binary instead of a list that is then converted to a binary */
void
erl_lua_tonumber(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  double res;
  int encode_i = 0;
  int size;
  char *eibuf;
    
  ei_decode_long(buf, &index, &i);
  
  res = lua_tonumber(driver_data->L, i);
  
  ei_encode_version(NULL, &encode_i);
  if ((long long) res == res) {
    ei_encode_longlong(NULL, &encode_i, (long long) res);
    size = encode_i;
    encode_i = 0;
    eibuf = malloc(sizeof(char) * (size + 1));
    
    ei_encode_version(eibuf, &encode_i);
    ei_encode_longlong(eibuf, &encode_i, res);
  } else {
    ei_encode_double(NULL, &encode_i, res);
    size = encode_i;
    encode_i = 0;
    eibuf = malloc(sizeof(char) * (size + 1));

    ei_encode_version(eibuf, &encode_i);
    ei_encode_double(eibuf, &encode_i, res);
  }
    
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_STRING, (ErlDrvTermData) eibuf, size,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
  free(eibuf);
  //driver_free_binary(bin);
}

void
erl_lua_type(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int lua_t;
  
  ei_decode_long(buf, &index, &i);
  
  lua_t = lua_type(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) lua_t,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_no_command(lua_drv_t *driver_data)
{  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_ERROR,
        ERL_DRV_STRING, (ErlDrvTermData) "No Command Found", 16,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


/* ********************************************************************
 *
 *   Lua Auxiliary Functions
 *
 * *******************************************************************/


void
erl_lual_dostring(lua_drv_t *driver_data, char *buf, int index)
{
  char *code;
  
  code = decode_string(buf, &index);
  
  if (!luaL_dostring(driver_data->L, code))
    reply_ok(driver_data);
  else
    reply_error(driver_data);
}

void
erl_lual_dofile(lua_drv_t *driver_data, char *buf, int index)
{
	char *code;

	code = decode_string(buf, &index);
	
	if (!luaL_dofile(driver_data->L, code))
		reply_ok(driver_data);
	else
		reply_error(driver_data);
}


/* ********************************************************************
 *
 *   Lua Language Function Emulations
 *
 * *******************************************************************/

/**
 * Lua-like print("text")
 * 
 * Call from Erlang with lua:print(L, Text).
 */
void
erl_lua_high_print(lua_drv_t *driver_data, char *buf, int index)
{
	lua_State *L = driver_data->L;
	char *str	 = decode_string(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, "print"); /* function to call */
	lua_pushstring(L, str);          /* push text to print on stack */
    lua_call(L, 1, 0);     /* call 'print' w/ 1 arguments, 0 result */

	reply_ok(driver_data);
	free(str);
}


/**
 * Lua-like print(var)
 * 
 * Call from Erlang with lua:print_variable(L, VarName).
 */
void
erl_lua_high_print_variable(lua_drv_t *driver_data, char *buf, int index)
{
	lua_State *L = driver_data->L;
	char *str	 = decode_string(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, "print"); /* function to call */
	lua_getfield(L, LUA_GLOBALSINDEX, str); /* push variable on stack */
    lua_call(L, 1, 0);     /* call 'print' w/ 1 arguments, 0 result */

	reply_ok(driver_data);
	free(str);
}


/* Sample from Lua Manual: a = f("how", t.x, 14)
 *
 * lua_getfield(L, LUA_GLOBALSINDEX, "f"); // function to be called 
 * lua_pushstring(L, "how");                        // 1st argument 
 * lua_getfield(L, LUA_GLOBALSINDEX, "t");   // table to be indexed 
 * lua_getfield(L, -1, "x");        // push result of t.x (2nd arg) 
 * lua_remove(L, -2);                  // remove 't' from the stack 
 * lua_pushinteger(L, 14);                          // 3rd argument 
 * lua_call(L, 3, 1);     // call 'f' with 3 arguments and 1 result 
 * lua_setfield(L, LUA_GLOBALSINDEX, "a");        // set global 'a' 
 *
 * http://www.lua.org/manual/5.1/manual.html#lua_call
 */
 

/* ********************************************************************
 *
 *   Utility
 *
 * *******************************************************************/

static void
reply_ok(lua_drv_t *driver_data)
{
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_OK};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

static void
reply_error(lua_drv_t *driver_data)
{ 
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_ERROR};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

static char*
decode_string(char *buf, int *index)
{
  int type, length;
  char *str;
  
  ei_get_type(buf, index, &type, &length);
  str = malloc(sizeof(char) * (length + 1));
  ei_decode_string(buf, index, str);
  return str;
}