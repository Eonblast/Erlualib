/**-------------------------------------------------------------------
*** File        : commands.c
*** Description : C Implementation of Commands for Erlualib
*** Authors     : Ray Morgan, Darrik Mazey, Henning Diedrich
*** Copyright   : (c) 2010 Eonblast Corporation for this fork
*** License     : MIT
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

static void reply_ok(lua_drv_t *drv);
static void reply_error(lua_drv_t *drv);
static void reply_errmem(lua_drv_t *drv);
static void reply(const lua_drv_t *drv, ErlDrvTermData *spec, size_t size);
static void reply_string(lua_drv_t *drv, const char *str, size_t len);
// new: static void reply_error_atom(lua_drv_t *drv, ErlDrvTermData erratom);
static void reply_error_msgdup(lua_drv_t *drv, ErlDrvTermData erratom, char *msg);

static char* decode_strdup(char *buf, int *index);

/* ********************************************************************
 *
 *   General Lua API
 *
 * *******************************************************************/

void
erl_lua_call(lua_drv_t *drv, char *buf, int index)
{
  long args, results;
  
  ei_decode_long(buf, &index, &args);
  ei_decode_long(buf, &index, &results);
  
  lua_call(drv->L, args, results);
  
  reply_ok(drv);
}

void
erl_lua_concat(lua_drv_t *drv, char *buf, int index)
{
  long n;
  
  ei_decode_long(buf, &index, &n);
  
  lua_concat(drv->L, n);
  
  reply_ok(drv);
}

void
erl_lua_getfield(lua_drv_t *drv, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_strdup(buf, &index);
  
  lua_getfield(drv->L, i, name);
  
  reply_ok(drv);
  free(name);
}

void
erl_lua_getglobal(lua_drv_t *drv, char *buf, int index)
{
  char *name;

  name = decode_strdup(buf, &index);
  
  lua_getglobal(drv->L, name);
  
  reply_ok(drv);
  free(name);
}

void
erl_lua_gettop(lua_drv_t *drv, char *buf, int index)
{
  int size;
  
  size = lua_gettop(drv->L);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) size,
        ERL_DRV_TUPLE,  2
  };
  reply(drv, spec, sizeof(spec));
}

void
erl_lua_next(lua_drv_t *drv, char *buf, int index)
{
	long n;
  
	ei_decode_long(buf, &index, &n);
  
	lua_next(drv->L, n);

	reply_ok(drv);
}

void
erl_lua_pushboolean(lua_drv_t *drv, char *buf, int index)
{
  int b;
  
  ei_decode_boolean(buf, &index, &b);
  
  lua_pushboolean(drv->L, b);
  
  reply_ok(drv);
}

void
erl_lua_pushinteger(lua_drv_t *drv, char *buf, int index)
{
  long long num;
  
  ei_decode_longlong(buf, &index, &num);
  
  lua_pushinteger(drv->L, num);
  
  reply_ok(drv);
}

void
erl_lua_pushstring(lua_drv_t *drv, char *buf, int index)
{
  char *str;
  
  str = decode_strdup(buf, &index);
  
  lua_pushstring(drv->L, str);
  
  reply_ok(drv);
  free(str);
}

void
erl_lua_pushnil(lua_drv_t *drv, char *buf, int index)
{
  lua_pushnil(drv->L);
  reply_ok(drv);
}

void
erl_lua_pushnumber(lua_drv_t *drv, char *buf, int index)
{
  double dnum;
  long long lnum;
  int type, len;
  
  ei_get_type(buf, &index, &type, &len);
  
  switch (type) {
  case ERL_FLOAT_EXT:
    ei_decode_double(buf, &index, &dnum);
    lua_pushnumber(drv->L, dnum);
    break;
  default:
    ei_decode_longlong(buf, &index, &lnum);
    lua_pushnumber(drv->L, lnum);
    break;
  }
  
  reply_ok(drv);
}

void
erl_lua_remove(lua_drv_t *drv, char *buf, int index)
{
  long i;
  
  ei_decode_long(buf, &index, &i);
  
  lua_remove(drv->L, i);
  
  reply_ok(drv);
}

void
erl_lua_setfield(lua_drv_t *drv, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_strdup(buf, &index);
  
  lua_setfield(drv->L, i, name);
  
  reply_ok(drv);
  free(name);
}

void
erl_lua_setglobal(lua_drv_t *drv, char *buf, int index)
{
  char *name;
  
  name = decode_strdup(buf, &index);
  
  lua_setglobal(drv->L, name);
  
  reply_ok(drv);
  free(name);
}

void
erl_lua_toboolean(lua_drv_t *drv, char *buf, int index)
{
  long i;
  int res;
  ErlDrvTermData spec[2];
  spec[0] = ERL_DRV_ATOM;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_toboolean(drv->L, i);
  if (res)
    spec[1] = driver_mk_atom("true");
  else
    spec[1] = driver_mk_atom("false");
  reply(drv, spec, sizeof(spec));
}

void
erl_lua_tointeger(lua_drv_t *drv, char *buf, int index)
{
  long i;
  long long result;
  
  ei_decode_long(buf, &index, &i);
  
  result = lua_tointeger(drv->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, ATOM_OK,					    /* the atom 'ok' */
        ERL_DRV_INT, (ErlDrvTermData) result,      /* the int result */
        ERL_DRV_TUPLE,  2                      /* and it's a 2-tuple */
  };
  reply(drv, spec, sizeof(spec));
}

void
erl_lua_tolstring(lua_drv_t *drv, char *buf, int index)
{
  size_t len;
  long i;
  const char *str;
  
  ei_decode_long(buf, &index, &i);
  
  str = lua_tolstring(drv->L, i, &len);

  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, ATOM_OK,					    /* the atom 'ok' */
        ERL_DRV_STRING, (ErlDrvTermData) str, len,     /* the string */
        ERL_DRV_TUPLE,  2                      /* and it's a 2-tuple */
  };
  reply(drv, spec, sizeof(spec));
}

/*---------------------------------------------------------------------
*
*	lua_tolstring: http://www.lua.org/manual/5.1/manual.html#lua_tolstring
*
*	[-0, +0, m] <- no change to stack, may throw error on low memory
*
*	const char *lua_tolstring (lua_State *L, int index, size_t
*	*len);
*
*	Converts the Lua value at the given acceptable index to a C
*	string. If len is not NULL, it also sets *len with the string
*	length. The Lua value must be a string or a number; otherwise,
*	the function returns NULL. If the value is a number, then
*	lua_tolstring also changes the actual value in the stack to a
*	string. (This change confuses lua_next when lua_tolstring is
*	applied to keys during a table traversal.)
*
*	lua_tolstring returns a fully aligned pointer to a string
*	inside the Lua state. This string always has a zero ('\0')
*	after its last character (as in C), but can contain other zeros
*	in its body. Because Lua has garbage collection, there is no
*	guarantee that the pointer returned by lua_tolstring will be
*	valid after the corresponding value is removed from the stack. 
*
*--------------------------------------------------------------------*/


/* TODO: return a binary instead of a list that is then converted to a binary */
void
erl_lua_tonumber(lua_drv_t *drv, char *buf, int index)
{
  long i;
  double res;
  int encode_i = 0;
  int size;
  char *eibuf;
    
  ei_decode_long(buf, &index, &i);
  
  res = lua_tonumber(drv->L, i);
  
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
        ERL_DRV_ATOM, ATOM_OK,					    /* the atom 'ok' */
        ERL_DRV_STRING, (ErlDrvTermData) eibuf, size,  /* the buffer */
        ERL_DRV_TUPLE,  2                      /* and it's a 2-tuple */
  };
  reply(drv, spec, sizeof(spec));
  free(eibuf);
  //driver_free_binary(bin);
}

void
erl_lua_type(lua_drv_t *drv, char *buf, int index)
{
  long i;
  int lua_t;
  
  ei_decode_long(buf, &index, &i);
  
  lua_t = lua_type(drv->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) lua_t,
        ERL_DRV_TUPLE,  2
  };
  reply(drv, spec, sizeof(spec));
}

void
erl_lua_no_command(lua_drv_t *drv)
{  

  /* return value: "No Command Found" */
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_ERROR,              /* the atom 'error' */
        ERL_DRV_STRING, 
        (ErlDrvTermData) "No Command Found", 16,       /* the string */
        ERL_DRV_TUPLE,  2                      /* and it's a 2-tuple */
  };
  reply(drv, spec, sizeof(spec));
}


/* ********************************************************************
 *
 *   Lua Auxiliary Functions
 *
 * *******************************************************************/


void
erl_lual_dostring(lua_drv_t *drv, char *buf, int index)
{
	char *code = decode_strdup(buf, &index);

	int ok = !luaL_dostring(drv->L, code); // sic '!' TODO: errmem?

    free(code);
    if(ok)
		reply_ok(drv);
	else
    	reply_error(drv);
}

void
erl_lual_dofile(lua_drv_t *drv, char *buf, int index)
{
	char *code;

	code = decode_strdup(buf, &index);
	
	int ok = !luaL_dofile(drv->L, code); // sic '!' TODO: errmem?

    free(code);
    if(ok)
		reply_ok(drv);
	else
		reply_error(drv);
}

/*---------------------------------------------------------------------
*
*	lual_loadstring: http://www.lua.org/manual/5.1/manual.html#lual_loadstring
*
*	int luaL_loadstring (lua_State *L, const char *s);
*
*	[-0, +1, m] <- leaves one result on stack, may throw error on low memory
*
*	Loads a string as a Lua chunk. This function uses lua_load to load the chunk
*	in the zero-terminated string s.
*	This function returns the same results as lua_load.
*	Also as lua_load, this function only loads the chunk; it does not run it.
*
*	int lua_load (lua_State *L,
*	              lua_Reader reader,
*	              void *data,
*	              const char *chunkname);
*
*	Loads a Lua chunk. If there are no errors, lua_load pushes the compiled
*	chunk as a Lua function on top of the stack. Otherwise, it pushes an error
*	 message. The return values of lua_load are:
*
*		*	0: no errors;
*		*	LUA_ERRSYNTAX: syntax error during pre-compilation;
*		*   LUA_ERRMEM: memory allocation error.
*
*	This function only loads a chunk; it does not run it.
*	lua_load automatically detects whether the chunk is text or binary, and
*	 loads it accordingly (see program luac).
*	The lua_load function uses a user-supplied reader function to read the
*	chunk (see lua_Reader). The data argument is an opaque value passed to
*	the reader function.
*	The chunkname argument gives a name to the chunk, which is used for
*	error messages and in debug information (see ¤3.8).*
*
*--------------------------------------------------------------------*/

/**
 * parse a string, leave result on stack as anonymous function.
 * 
 * Call from Erlang with lua:loadstring(L, String).
 */
void
erl_lual_loadstring(lua_drv_t *drv, char *buf, int index)
{
	char *code = decode_strdup(buf, &index);

	int ret = luaL_loadstring(drv->L, code);

    free(code);
    if(ret == 0)
		reply_ok(drv);
    else if (ret == LUA_ERRMEM)
   		reply_errmem(drv);
	else
    	reply_error(drv);
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
erl_luac_print(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *str	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, "print"); /* function to call */
	lua_pushstring(L, str);          /* push text to print on stack */
    lua_call(L, 1, 0);     /* call 'print' w/ 1 arguments, 0 result */

	reply_ok(drv);
	free(str);
}

/**
 * Lua-like print(var)
 * 
 * Call from Erlang with lua:print_variable(L, VarName).
 */
void
erl_luac_print_variable(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *str	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, "print"); /* function to call */
	lua_getfield(L, LUA_GLOBALSINDEX, str); /* push variable on stack */
    lua_call(L, 1, 0);     /* call 'print' w/ 1 arguments, 0 result */

	reply_ok(drv);
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
 * ********************************************************************
 *
 *   Broad Brush Simple Function Calls, without or with string params.
 *
 * ********************************************************************
 * ********************************************************************
 *
 * This is a pretty pragmatic approach that covers a number of more
 * standard cases in a faster fashion than single port commands.
 *
 *-------------------------------------------------------------------*/
 
/**
 * Calls a function with no parameters and no return value.
 */
void
erl_luac_func_0_0(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) { 
	    lua_call(L, 0, 0);  /* call function w/o arguments, 0 result */
		reply_ok(drv);
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);
	
	free(name);
}


/**
 * Calls a function with 1 string parameter and no return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_1_0(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
		lua_pushstring(L, par1);              /* push string in par1 */
	    lua_call(L, 1, 0);   /* call function w/1 argument, 0 result */
		reply_ok(drv);
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
}


/**
 * Calls a function with 2 string parameters and no return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_2_0(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);
	char *par2	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
		lua_pushstring(L, par1);              /* push string in par1 */
		lua_pushstring(L, par2);              /* push string in par2 */
	    lua_call(L, 2, 0);   /* call function w/2 argument, 0 result */
		reply_ok(drv);
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
	free(par2);
}


/**
 * Calls a function with 3 string parameters and no return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_3_0(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);
	char *par2	 = decode_strdup(buf, &index);
	char *par3	 = decode_strdup(buf, &index);

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
		lua_pushstring(L, par1);              /* push string in par1 */
		lua_pushstring(L, par2);              /* push string in par2 */
		lua_pushstring(L, par3);              /* push string in par3 */
	    lua_call(L, 3, 0);   /* call function w/3 argument, 0 result */
		reply_ok(drv);
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
	free(par2);
	free(par3);
}
 


/**
 * Calls a function with no parameter and 1 string return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_0_1(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	const char *str;
	size_t len;

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
	
		/* call function w/0 arguments, 1 string result */
		lua_call(L, 0, 1);   

		/* turn result on top of stack (num or str) into a C string */
		str = lua_tolstring(L, -1, &len);
    
	    /* stage the string as return value */
		reply_string(drv, str, len);

		/* It's copied, now delete it from the stack */
    	lua_remove(L, -1);
	
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
}


/**
 * Calls a function with 1 string parameter and 1 string return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_1_1(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);
	const char *str;
	size_t len;

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
	
		 /* push string parameter */
 		lua_pushstring(L, par1);                    

		/* call function w/1 arguments, 1 string result */
		lua_call(L, 1, 1);   

		/* turn result on top of stack (num or str) into a C string */
		str = lua_tolstring(L, -1, &len);
    
	    /* stage the string as return value */
		reply_string(drv, str, len);

		/* It's copied, now delete it from the stack */
   		lua_remove(L, -1);
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
}


/**
 * Calls a function with 2 string parameters and 1 string return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_2_1(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);
	char *par2	 = decode_strdup(buf, &index);
	const char *str;
	size_t len;

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
	
		 /* push string parameters */
 		lua_pushstring(L, par1);                    
 		lua_pushstring(L, par2);                    

		/* call function w/2 arguments, 1 string result */
		lua_call(L, 2, 1);   

		/* turn result on top of stack (num or str) into a C string */
		str = lua_tolstring(L, -1, &len);
    
	    /* stage the string as return value */
		reply_string(drv, str, len);

		/* It's copied, now delete it from the stack */
    	lua_remove(L, -1);
	
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
	free(par2);
}


/**
 * Calls a function with 3 string parameters and 1 string return value.
 * TODO: NOT TESTED.
 */
void
erl_luac_func_3_1(lua_drv_t *drv, char *buf, int index)
{
	lua_State *L = drv->L;
	char *name	 = decode_strdup(buf, &index);
	char *par1	 = decode_strdup(buf, &index);
	char *par2	 = decode_strdup(buf, &index);
	char *par3	 = decode_strdup(buf, &index);
	const char *str;
	size_t len;

	lua_getfield(L, LUA_GLOBALSINDEX, name);     /* function to call */
	if(lua_isfunction(L, -1)) {
	
		 /* push string parameters */
 		lua_pushstring(L, par1);                    
 		lua_pushstring(L, par2);                    
 		lua_pushstring(L, par3);                    

		/* call function w/3 arguments, 1 string result */
		lua_call(L, 3, 1);   

		/* turn result on top of stack (num or str) into a C string */
		str = lua_tolstring(L, -1, &len);
    
	    /* stage the string as return value */
		reply_string(drv, str, len);

		/* It's copied, now delete it from the stack */
    	lua_remove(L, -1);
	
	} else
		reply_error_msgdup(drv, ATOM_NOFUNC, name);

	free(name);
	free(par1);
	free(par2);
	free(par3);
}
 

/* ********************************************************************
 * ********************************************************************
 *
 *   Reply Functions
 *
 * ********************************************************************
 * ********************************************************************
 *
 * Replies are asynchronous, arriving as messages at the Erlang port.
 * The vehicle for passing state back is an ErlDrvTermData array.
 *  
 *  The usual order is a call to a reply function and then freeing up
 *  memory used in a function. Note that e.g. strings passed into the
 *  reply functions can be freed immediately after since stuff given
 *  to the ErlDrvTermData[] is copied.
 *
 *---------------------------------------------------------------------
 * 
 *	... am I free to destroy these buffers once that call returns?
 *	The data is copied. You can do what you want with the buffers after 
 *	driver_send_term or driver_output_term returns.
 *
 *	http://www.erlang.org/pipermail/erlang-questions/2009-May/043849.html
 *
 *-------------------------------------------------------------------*/
                                           

/**
 * Ship a result as ErlDrvTermData to Erlang, which will deep copy it,
 * including strings. So they need not be dups but can be direct from
 * the heap, subject to gc etc.
 */
static void
reply(const lua_drv_t *drv, ErlDrvTermData *spec, size_t size)
{
	driver_output_term(drv->port, spec, size / sizeof(ErlDrvTermData));
}

/**
 * Reply ok to Erlang, as atom 'ok'.
 */
static void
reply_ok(lua_drv_t *drv)
{
	ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_OK};

	/* Ship to Erlang. */
	reply(drv, spec, sizeof(spec));
}

/**
 * Reply error to Erlang, as atom 'error'.
 */
static void
reply_error(lua_drv_t *drv)
{ 
	ErlDrvTermData spec[] = { ERL_DRV_ATOM, ATOM_ERROR };

	/* Ship to Erlang. */
	reply(drv, spec, sizeof(spec));
}

/**
 * Reply memory error to Erlang, as atom 'errmem'.
 */
static void
reply_errmem(lua_drv_t *drv)
{ 
	ErlDrvTermData spec[] = { ERL_DRV_ATOM, ATOM_ERRMEM };

	/* Ship to Erlang. */
	reply(drv, spec, sizeof(spec));
}

/** new, not yet used:
 * Reply error to Erlang, as tuple { error, <erratom> }
 * Atoms are defined in commands.h. You could define them on the fly 
 * from a string. Don't encourage that, as this may deplete atom
 * storage capacity in the server if by mistake ever different strings
 * are fed into the on-the-fly atom creation. Stick to defining them
 * in commands.h and using a define here.
 * /
static void
reply_error_atom(lua_drv_t *drv, ErlDrvTermData erratom)
{ 
	ErlDrvTermData spec[] = { 			   / * note: reverse grouping * /
	  		ERL_DRV_ATOM, ATOM_ERROR,
		  	ERL_DRV_ATOM, erratom,    / * atoms defined in commands.h * /
        ERL_DRV_TUPLE,  2                      / * and it's a 2-tuple * /
  		};

	/ * Ship to Erlang. * /
	reply(drv, spec, sizeof(spec));
}
*/

/** 
 * Reply error to Erlang, as tuple { error, <erratom>, <"message"> }.
 * For atoms, see reply_error_atom().
 */
static void
reply_error_msgdup(lua_drv_t *drv, ErlDrvTermData erratom, char *msg)
{ 
	ErlDrvTermData spec[] = {              /* note: reverse grouping */
		  	ERL_DRV_ATOM, ATOM_ERROR,
	  		ERL_DRV_ATOM, erratom,    /* atoms defined in commands.h */
    	    ERL_DRV_STRING, (ErlDrvTermData) msg, strlen(msg), 		
        ERL_DRV_TUPLE,  3                      /* and it's a 3-tuple */
  		};
  		
	/* Ship to Erlang. Which will copy the spec and the string. */
	reply(drv, spec, sizeof(spec));
}

/*
 *
 */
static void
reply_string(lua_drv_t *drv, const char *str, size_t len)
{
	/* Construct the spec wrap for Erlang. */
 	ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, ATOM_OK,					    /* the atom 'ok' */
        ERL_DRV_STRING, (ErlDrvTermData) str, len,     /* the string */
        ERL_DRV_TUPLE,  2                      /* and it's a 2-tuple */
	};
  
	/* Ship to Erlang. Which will copy the spec and the string. */
	reply(drv, spec, sizeof(spec));
}


/* ********************************************************************
 *
 *   Utility
 *
 * *******************************************************************/

/**
 * Decode and duplicate the string in the buffer.
 * Moves the &index up to behind the decoded part.
 */
static char*
decode_strdup(char *buf, int *index)
{
  int type, length;
  char *str;
  
  ei_get_type(buf, index, &type, &length); // TODO utf8? preflight?
  str = malloc(sizeof(char) * (length + 1));
  ei_decode_string(buf, index, str);
  /* The index is updated to point right after the term encoded/decoded.
   * http://www.erlang.org/doc/man/ei.html */
   
  return str;
}
