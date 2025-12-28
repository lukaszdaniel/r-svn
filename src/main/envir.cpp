/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2025  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *
 *
 *  Environments:
 *
 *  All the action of associating values with symbols happens
 *  in this code.  An environment is (essentially) a list of
 *  environment "frames" of the form
 *
 *	FRAME(envir) = environment frame
 *	ENCLOS(envir) = parent environment
 *	HASHTAB(envir) = (optional) hash table
 *
 *  Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 *  When the value of a symbol is required, the environment is
 *  traversed frame-by-frame until a value is found.
 *
 *  If a value is not found during the traversal, the symbol's
 *  "value" slot is inspected for a value.  This "top-level"
 *  environment is where system functions and variables reside.
 *
 *  Environments with the NO_SPECIAL_SYMBOLS flag set are known to not
 *  contain any special symbols, as indicated by the IS_SPECIAL_SYMBOL
 *  macro.  Lookup for such a symbol can then bypass this environment
 *  without searching it.
 */

/* R 1.8.0: namespaces are no longer experimental, so the following
 *  are no longer 'experimental options', but rather three sections
 *  describing the API:
 *
 * NAMESPACES:
 *     R_BaseNamespace holds an environment that has R_GlobalEnv as
 *     its parent.  This environment does not actually contain any
 *     bindings of its own.  Instead, it redirects all fetches and
 *     assignments to the SYMVALUE fields of the base (R_BaseEnv)
 *     environment.  If evaluation occurs in R_BaseNamespace, then
 *     base is searched before R_GlobalEnv.
 *
 * ENVIRONMENT_LOCKING: Locking an environment prevents new bindings
 *     from being created and existing bindings from being removed.
 *
 * FANCY_BINDINGS: We have binding locking and "active bindings".
 *     When a binding is locked, its value cannot be changed.  It may
 *     still be removed from the environment if the environment is not
 *     locked.
 *
 *     Active bindings contain a function in their value cell.
 *     Getting the value of an active binding calls this function with
 *     no arguments and returns the result.  Assigning to an active
 *     binding calls this function with one argument, the new value.
 *     Active bindings may be useful for mapping external variables,
 *     such as C variables or data base entries, to R variables.  They
 *     may also be useful for making some globals thread-safe.
 *
 *     Bindings are marked as locked or active using bits 14 and 15 in
 *     their gp fields.  Since the save/load code writes out this
 *     field it means the value will be preserved across save/load.
 *     But older versions of R will interpret the entire gp field as
 *     the MISSING field, which may cause confusion.  If we keep this
 *     code, then we will need to make sure that there are no
 *     locked/active bindings in workspaces written for older versions
 *     of R to read.
 *
 * LT */

/** @file envir.cpp
 *
 * Environments: all the action of associating values with symbols
 * happens in this code.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Localization.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/LogicalVector.hpp>
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h> // for R_Suicide()
#include <R_ext/ObjectTable.h>

using namespace R;
using namespace CXXR;

#define FAST_BASE_CACHE_LOOKUP  /* Define to enable fast lookups of symbols */
				/*    in global cache from base environment */

#define IS_USER_DATABASE(rho)  (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"))

/* various definitions of macros/functions in Defn.h */

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
/*#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))*/

/* use the same bits (15 and 14) in symbols and bindings */
static SEXP getActiveValue(SEXP);
static R_INLINE SEXP BINDING_VALUE(SEXP b)
{
    if (BNDCELL_TAG(b)) {
	R_expand_binding_value(b);
	return CAR0(b);
    }
    if (IS_ACTIVE_BINDING(b)) return getActiveValue(CAR(b));
    else return CAR(b);
}
#ifdef TESTING_WRITE_BARRIER
#define SYMBOL_BINDING_VALUE(s) ((IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s)))
#else
#define SYMBOL_BINDING_VALUE(s) ((IS_ACTIVE_BINDING(s) ? getActiveValue(SYMVALUE(s)) : SYMVALUE(s).get()))
#endif
#define SYMBOL_HAS_BINDING(s) (IS_ACTIVE_BINDING(s) || (SYMVALUE(s) != R_UnboundValue))

#define SET_BINDING_VALUE(b,val) do { \
  SEXP __b__ = (b); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__b__)) \
    error(_("cannot change value of locked binding for '%s'"), \
	  CHAR(PRINTNAME(TAG(__b__)))); \
  if (IS_ACTIVE_BINDING(__b__)) { \
    PROTECT(__val__); \
    setActiveValue(CAR(__b__), __val__); \
    UNPROTECT(1); \
   } else \
    SET_BNDCELL(__b__, __val__); \
} while (0)

#define SET_SYMBOL_BINDING_VALUE(sym, val) do { \
  SEXP __sym__ = (sym); \
  SEXP __val__ = (val); \
  if (BINDING_IS_LOCKED(__sym__)) \
    error(_("cannot change value of locked binding for '%s'"), \
	  CHAR(PRINTNAME(__sym__))); \
  if (IS_ACTIVE_BINDING(__sym__)) { \
    PROTECT(__val__); \
    setActiveValue(SYMVALUE(__sym__), __val__); \
    UNPROTECT(1); \
  } else \
    SET_SYMVALUE(__sym__, __val__); \
} while (0)

static void setActiveValue(SEXP fun, SEXP val)
{
    SEXP qfun = lang3(R_DoubleColonSymbol, R_BaseSymbol, R_QuoteSymbol);
    SEXP arg = lang2(qfun, val);
    SEXP expr = lang2(fun, arg);
    PROTECT(expr);
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}

static SEXP getActiveValue(SEXP fun)
{
    SEXP expr = LCONS(fun, R_NilValue);
    PROTECT(expr);
    expr = eval(expr, R_GlobalEnv);
    UNPROTECT(1);
    /* mark unmutable to prevent mutations in complex assignments */
    MARK_NOT_MUTABLE(expr);
    return expr;
}

/* Macro version of isNull for only the test against R_NilValue */
#define ISNULL(x) ((x) == R_NilValue)

/* Function to determine whethr an environment contains special symbols */
attribute_hidden bool R::R_envHasNoSpecialSymbols(SEXP env)
{
    if (HASHTAB(env) != R_NilValue)
	return false;

    for (SEXP frame = FRAME(env); frame != R_NilValue; frame = CDR(frame))
	if (IS_SPECIAL_SYMBOL(TAG(frame)))
	    return false;

    return true;
}

/*----------------------------------------------------------------------

  Hash Tables

  We use a basic separate chaining algorithm.	A hash table consists
  of SEXP (vector) which contains a number of SEXPs (lists).

  The only non-static function is R_NewHashedEnv, which allows code to
  request a hashed environment.  All others are static to allow
  internal changes of implementation without affecting client code.
*/

#define HASHSIZE(x)	     ((int) STDVEC_LENGTH(x))
#define HASHPRI(x)	     ((int) STDVEC_TRUELENGTH(x))
#define HASHTABLEGROWTHRATE  1.2
#define HASHMINSIZE	     29
#define SET_HASHPRI(x,v)     SET_TRUELENGTH(x,v)
#define HASHCHAIN(table, i)  ((SEXP *) STDVEC_DATAPTR(table))[i]

#define IS_HASHED(x)	     (HASHTAB(x) != R_NilValue)

/*----------------------------------------------------------------------

  String Hashing

  This is taken from the second edition of the "Dragon Book" by
  Aho, Ullman and Sethi.

*/

/* was extern: used in this file and names.c (for the symbol table).

   This hash function seems to work well enough for symbol tables,
   and hash tables get saved as part of environments so changing it
   is a major decision.
 */
attribute_hidden int R::R_Newhashpjw(const char *s)
{
    unsigned h = 0, g;
    for (const char *p = s; *p; p++) {
	h = (h << 4) + (*p);
	if ((g = h & 0xf0000000) != 0) {
	    h = h ^ (g >> 24);
	    h = h ^ g;
	}
    }
    return h;
}

/*----------------------------------------------------------------------

  R_HashSet

  Hashtable set function.  Sets 'symbol' in 'table' to be 'value'.
  'hashcode' must be provided by user.	Allocates some memory for list
  entries.

*/

static void R_HashSet(int hashcode, SEXP symbol, SEXP table, SEXP value,
		      bool frame_locked)
{
    /* Grab the chain from the hashtable */
    SEXP chain = VECTOR_ELT(table, hashcode);

    /* Search for the value in the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) {
	    SET_BINDING_VALUE(chain, value);
	    SET_MISSING(chain, 0);	/* Over-ride for new value */
	    return;
	}
    if (frame_locked)
	error("%s", _("cannot add bindings to a locked environment"));
    if (ISNULL(chain))
	SET_HASHPRI(table, HASHPRI(table) + 1);
    /* Add the value into the chain */
    SET_VECTOR_ELT(table, hashcode, CONS(value, VECTOR_ELT(table, hashcode)));
    SET_TAG(VECTOR_ELT(table, hashcode), symbol);
    return;
}



/*----------------------------------------------------------------------

  R_HashGet

  Hashtable get function.  Returns 'value' from 'table' indexed by
  'symbol'.  'hashcode' must be provided by user.  Returns
  'R_UnboundValue' if value is not present.

*/

static SEXP R_HashGet(int hashcode, SEXP symbol, SEXP table)
{
    SEXP chain;

    /* Grab the chain from the hashtable */
    chain = HASHCHAIN(table, hashcode);
    /* Retrieve the value from the chain */
    for (; chain != R_NilValue ; chain = CDR(chain))
	if (TAG(chain) == symbol) return BINDING_VALUE(chain);
    /* If not found */
    return R_UnboundValue;
}

static bool R_HashExists(int hashcode, SEXP symbol, SEXP table)
{
    /* Grab the chain from the hashtable */
    SEXP chain = VECTOR_ELT(table, hashcode);
    /* Find the binding in the chain */
    for (; chain != R_NilValue ; chain = CDR(chain))
	if (TAG(chain) == symbol) return true;
    /* If not found */
    return false;
}



/*----------------------------------------------------------------------

  R_HashGetLoc

  Hashtable get location function. Just like R_HashGet, but returns
  location of variable, rather than its value. Returns R_NilValue
  if not found.

*/

static SEXP R_HashGetLoc(int hashcode, SEXP symbol, SEXP table)
{
    /* Grab the chain from the hashtable */
    SEXP chain = VECTOR_ELT(table, hashcode);
    /* Retrieve the value from the chain */
    for (; !ISNULL(chain); chain = CDR(chain))
	if (TAG(chain) == symbol) return chain;
    /* If not found */
    return R_NilValue;
}



/*----------------------------------------------------------------------

  R_NewHashTable

  Hash table initialisation function.  Creates a table of size 'size'
  that increases in size by 'growth_rate' after a threshold is met.

*/

static SEXP R_NewHashTable(int size)
{
    SEXP table;

    if (size <= 0) size = HASHMINSIZE;

    /* Allocate hash table in the form of a vector */
    PROTECT(table = allocVector(VECSXP, size));
    SET_HASHPRI(table, 0);
    UNPROTECT(1);
    return(table);
}

/*----------------------------------------------------------------------

  R_NewHashedEnv

  Returns a new environment with a hash table initialized with default
  size.  The only non-static hash table function.
*/

SEXP R::R_NewHashedEnv(SEXP enclos, int size)
{
    SEXP s;

    PROTECT(enclos);
    PROTECT(s = NewEnvironment(R_NilValue, R_NilValue, enclos));
    SET_HASHTAB(s, R_NewHashTable(size));
    UNPROTECT(2);
    return s;
}


/*----------------------------------------------------------------------

  R_HashDelete

  Hash table delete function. Symbols are completely removed from the table;
  there is no way to mark a symbol as not present without actually removing
  it.
*/

static std::pair<SEXP, bool> RemoveFromList(SEXP thing, SEXP list);

static bool R_HashDelete(int hashcode, SEXP symbol, SEXP env)
{
    SEXP hashtab = HASHTAB(env);
    int idx = hashcode % HASHSIZE(hashtab);
    auto list = RemoveFromList(symbol, VECTOR_ELT(hashtab, idx));
    if (list.second) {
	if (env == R_GlobalEnv)
	    R_DirtyImage = 1;
	if (list.first == R_NilValue)
	    SET_HASHPRI(hashtab, HASHPRI(hashtab) - 1);
	SET_VECTOR_ELT(hashtab, idx, list.first);
    }
    return list.second;
}




/*----------------------------------------------------------------------

  R_HashResize

  Hash table resizing function Increase the size of the hash table by
  the growth_rate of the table.	 The vector is reallocated, however
  the lists with in the hash table have their pointers shuffled around
  so that they are not reallocated.

*/

static SEXP R_HashResize(SEXP table)
{
    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error(_("first argument ('table') not of type '%s', from %s"), "VECSXP", "R_HashResize");

    /* This may have to change.	 The growth rate should
       be independent of the size (not implemented yet) */
    /* hash_grow = HASHSIZE(table); */

    /* Allocate the new hash table */
    SEXP new_table = R_NewHashTable(1 + (int)(HASHSIZE(table) * HASHTABLEGROWTHRATE));
    for (int counter = 0; counter < length(table); counter++) {
	SEXP chain = VECTOR_ELT(table, counter);
	while (!ISNULL(chain)) {
	    int new_hashcode = R_Newhashpjw(CHAR(PRINTNAME(TAG(chain)))) %
		HASHSIZE(new_table);
	    SEXP new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a primary slot then increase HASHPRI */
	    if (ISNULL(new_chain))
		SET_HASHPRI(new_table, HASHPRI(new_table) + 1);
	    SEXP tmp_chain = chain;
	    chain = CDR(chain);
	    SETCDR(tmp_chain, new_chain);
	    SET_VECTOR_ELT(new_table, new_hashcode,  tmp_chain);
#ifdef MIKE_DEBUG
	    fprintf(stdout, "HASHSIZE = %d\nHASHPRI = %d\ncounter = %d\nHASHCODE = %d\n",
		    HASHSIZE(table), HASHPRI(table), counter, new_hashcode);
#endif
	}
    }
    /* Some debugging statements */
#ifdef MIKE_DEBUG
    fprintf(stdout, "Resized O.K.\n");
    fprintf(stdout, "Old size: %d, New size: %d\n",
	    HASHSIZE(table), HASHSIZE(new_table));
    fprintf(stdout, "Old pri: %d, New pri: %d\n",
	    HASHPRI(table), HASHPRI(new_table));
#endif
    return new_table;
} /* end R_HashResize */



/*----------------------------------------------------------------------

  R_HashSizeCheck

  Hash table size rechecking function.	Compares the load factor
  (size/# of primary slots used)  to a particular threshold value.
  Returns true if the table needs to be resized.

*/

static int R_HashSizeCheck(SEXP table)
{
    int resize;
    double thresh_val;

    /* Do some checking */
    if (TYPEOF(table) != VECSXP)
	error(_("first argument ('table') not of type '%s', from %s"), "VECSXP", "R_HashSizeCheck");
    resize = 0; thresh_val = 0.85;
    if ((double)HASHPRI(table) > (double)HASHSIZE(table) * thresh_val)
	resize = 1;
    return resize;
}



/*----------------------------------------------------------------------

  R_HashFrame

  Hashing for environment frames.  This function ensures that the
  first frame in the given environment has been hashed.	 Ultimately
  all enironments should be created in hashed form.  At that point
  this function will be redundant.

*/

static SEXP R_HashFrame(SEXP rho)
{
    /* Do some checking */
    if (TYPEOF(rho) != ENVSXP)
	error(_("first argument ('table') not of type '%s', from %s"), "ENVSXP", "R_HashFrame");
    SEXP table = HASHTAB(rho);
    SEXP frame = FRAME(rho);
    while (!ISNULL(frame)) {
	if (!HASHASH(PRINTNAME(TAG(frame))) ) {
	    SET_HASHVALUE(PRINTNAME(TAG(frame)),
			  R_Newhashpjw(CHAR(PRINTNAME(TAG(frame)))));
	    SET_HASHASH(PRINTNAME(TAG(frame)), 1);
	}
	int hashcode = HASHVALUE(PRINTNAME(TAG(frame))) % HASHSIZE(table);
	SEXP chain = VECTOR_ELT(table, hashcode);
	/* If using a primary slot then increase HASHPRI */
	if (ISNULL(chain)) SET_HASHPRI(table, HASHPRI(table) + 1);
	SEXP tmp_chain = frame;
	frame = CDR(frame);
	SETCDR(tmp_chain, chain);
	SET_VECTOR_ELT(table, hashcode, tmp_chain);
    }
    SET_FRAME(rho, R_NilValue);
    return rho;
}


/* ---------------------------------------------------------------------

   R_HashProfile

   Profiling tool for analyzing hash table performance.  Returns a
   three element list with components:

   size: the total size of the hash table

   nchains: the number of non-null chains in the table (as reported by
	    HASHPRI())

   counts: an integer vector the same length as size giving the length of
	   each chain (or zero if no chain is present).  This allows
	   for assessing collisions in the hash table.
 */

static SEXP R_HashProfile(SEXP table)
{
    SEXP chain, ans, chain_counts, nms;
    int count;

    PROTECT(ans = allocVector(VECSXP, 3));
    PROTECT(nms = allocVector(STRSXP, 3));
    SET_STRING_ELT(nms, 0, mkChar("size"));    /* size of hashtable */
    SET_STRING_ELT(nms, 1, mkChar("nchains")); /* number of non-null chains */
    SET_STRING_ELT(nms, 2, mkChar("counts"));  /* length of each chain */
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(1);

    SET_VECTOR_ELT(ans, 0, ScalarInteger(length(table)));
    SET_VECTOR_ELT(ans, 1, ScalarInteger(HASHPRI(table)));

    PROTECT(chain_counts = allocVector(INTSXP, length(table)));
    for (int i = 0; i < length(table); i++) {
	chain = VECTOR_ELT(table, i);
	count = 0;
	for (; chain != R_NilValue ; chain = CDR(chain)) {
	    count++;
	}
	INTEGER(chain_counts)[i] = count;
    }

    SET_VECTOR_ELT(ans, 2, chain_counts);

    UNPROTECT(2);
    return ans;
}



/*----------------------------------------------------------------------

  Environments

  The following code implements variable searching for environments.

*/


/*----------------------------------------------------------------------

  InitGlobalEnv

  Create the initial global environment.  The global environment is
  no longer a linked list of environment frames.  Instead it is a
  vector of environments which is searched from beginning to end.

  Note that only the first frame of each of these environments is
  searched.  This is intended to make it possible to implement
  namespaces at some (indeterminate) point in the future.

  We hash the initial environment.  100 is a magic number discovered
  by Ross.  Change it if you feel inclined.

*/

#define USE_GLOBAL_CACHE
#ifdef USE_GLOBAL_CACHE  /* NB leave in place: see below */
/* Global variable caching.  A cache is maintained in a hash table,
   R_GlobalCache.  The entry values are either R_UnboundValue (a
   flushed cache entry), the binding LISTSXP cell from the environment
   containing the binding found in a search from R_GlobalEnv, or a
   symbol if the globally visible binding lives in the base package.
   The cache for a variable is flushed if a new binding for it is
   created in a global frame or if the variable is removed from any
   global frame.

   Symbols in the global cache with values from the base environment
   are flagged with BASE_SYM_CACHED, so that their value can be
   returned immediately without needing to look in the hash table.
   They must still have entries in the hash table, however, so that
   they can be flushed as needed.

   To make sure the cache is valid, all binding creations and removals
   from global frames must go through the interface functions in this
   file.

   Initially only the R_GlobalEnv frame is a global frame.  Additional
   global frames can only be created by attach.  All other frames are
   considered local.  Whether a frame is local or not is recorded in
   the highest order bit of the ENVFLAGS field (the gp field of
   sxpinfo).

   It is possible that the benefit of caching may be significantly
   reduced if we introduce namespace management.  Since maintaining
   cache integrity is a bit tricky and since it might complicate
   threading a bit (I'm not sure it will but it needs to be thought
   through if nothing else) it might make sense to remove caching at
   that time.  To make that easier, the ifdef's should probably be
   left in place.

   L. T. */

#define GLOBAL_FRAME_MASK (1<<15)
#define IS_GLOBAL_FRAME(e) (ENVFLAGS(e) & GLOBAL_FRAME_MASK)
#define MARK_AS_GLOBAL_FRAME(e) \
  SET_ENVFLAGS(e, ENVFLAGS(e) | GLOBAL_FRAME_MASK)
#define MARK_AS_LOCAL_FRAME(e) \
  SET_ENVFLAGS(e, ENVFLAGS(e) & (~ GLOBAL_FRAME_MASK))

#define INITIAL_CACHE_SIZE 1000

static SEXP R_GlobalCache, R_GlobalCachePreserve;
#endif
static SEXP R_BaseNamespaceName;
static SEXP R_NamespaceSymbol;

attribute_hidden void R::InitBaseEnv(void)
{
    Environment::initialize();
    // R_EmptyEnv = NewEnvironment(R_NilValue, R_NilValue, R_NilValue);
    // R_BaseEnv = NewEnvironment(R_NilValue, R_NilValue, R_EmptyEnv);
}

attribute_hidden void R::InitGlobalEnv(void)
{
    R_NamespaceSymbol = install(".__NAMESPACE__.");

    R_GlobalEnv = R_NewHashedEnv(R_BaseEnv, 0);
    R_MethodsNamespace = R_GlobalEnv; // so it is initialized.
#ifdef NEW_CODE /* Not used */
    HASHTAB(R_GlobalEnv) = R_NewHashTable(100);
#endif
#ifdef USE_GLOBAL_CACHE
    MARK_AS_GLOBAL_FRAME(R_GlobalEnv);
    R_GlobalCache = R_NewHashTable(INITIAL_CACHE_SIZE);
    R_GlobalCachePreserve = CONS(R_GlobalCache, R_NilValue);
    R_PreserveObject(R_GlobalCachePreserve);
#endif
    R_BaseNamespace = NewEnvironment(R_NilValue, R_NilValue, R_GlobalEnv);
    R_PreserveObject(R_BaseNamespace);
    SET_SYMVALUE(install(".BaseNamespaceEnv"), R_BaseNamespace);
    R_BaseNamespaceName = ScalarString(mkChar("base"));
    R_PreserveObject(R_BaseNamespaceName);
    R_NamespaceRegistry = R_NewHashedEnv(R_NilValue, 0);
    R_PreserveObject(R_NamespaceRegistry);
    defineVar(R_BaseSymbol, R_BaseNamespace, R_NamespaceRegistry);
    /**** needed to properly initialize the base namespace */
}

#ifdef USE_GLOBAL_CACHE
static int hashIndex(SEXP symbol, SEXP table)
{
    SEXP c = PRINTNAME(symbol);
    if (!HASHASH(c)) {
	SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	SET_HASHASH(c, 1);
    }
    return HASHVALUE(c) % HASHSIZE(table);
}

static void R_FlushGlobalCache(SEXP sym)
{
    SEXP entry = R_HashGetLoc(hashIndex(sym, R_GlobalCache), sym,
			      R_GlobalCache);
    if (entry != R_NilValue) {
	SETCAR(entry, R_UnboundValue);
#ifdef FAST_BASE_CACHE_LOOKUP
	UNSET_BASE_SYM_CACHED(sym);
#endif
    }
}

static void R_FlushGlobalCacheFromTable(SEXP table)
{
    int size = HASHSIZE(table);
    for (int i = 0; i < size; i++) {
	for (SEXP chain = VECTOR_ELT(table, i); chain != R_NilValue; chain = CDR(chain))
	    R_FlushGlobalCache(TAG(chain));
    }
}

/**
 Flush the cache based on the names provided by the user defined
 table, specifically returned from calling objects() for that
 table.
 */
static void R_FlushGlobalCacheFromUserTable(SEXP udb)
{
    R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(udb);
    SEXP names = tb->objects(tb);
    int n = length(names);
    for (int i = 0; i < n ; i++)
	R_FlushGlobalCache(Rf_installTrChar(STRING_ELT(names,i)));
}

static void R_AddGlobalCache(SEXP symbol, SEXP place)
{
    int oldpri = HASHPRI(R_GlobalCache);
    R_HashSet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache, place,
	      FALSE);
#ifdef FAST_BASE_CACHE_LOOKUP
    if (symbol == place)
	SET_BASE_SYM_CACHED(symbol);
    else
	UNSET_BASE_SYM_CACHED(symbol);
#endif
    if (oldpri != HASHPRI(R_GlobalCache) &&
	HASHPRI(R_GlobalCache) > 0.85 * HASHSIZE(R_GlobalCache)) {
	R_GlobalCache = R_HashResize(R_GlobalCache);
	SETCAR(R_GlobalCachePreserve, R_GlobalCache);
    }
}

static SEXP R_GetGlobalCacheLoc(SEXP symbol)
{
#ifdef FAST_BASE_CACHE_LOOKUP
    if (BASE_SYM_CACHED(symbol))
	return symbol;
#endif

    return R_HashGet(hashIndex(symbol, R_GlobalCache), symbol, R_GlobalCache);
}
#endif /* USE_GLOBAL_CACHE */

/*----------------------------------------------------------------------

  unbindVar

  Remove a value from an environment. This happens only in the frame
  of the specified environment.

  FIXME ? should this also unbind the symbol value slot when rho is
  R_BaseEnv.
  This is only called from eval.c in applydefine and bcEval
  (and applydefine only works for unhashed environments, so not base).
*/

static std::pair<SEXP, bool> RemoveFromList(SEXP thing, SEXP list)
{
    if (list == R_NilValue) {
	return std::pair<SEXP, bool>(R_NilValue, false);
    }
    else if (TAG(list) == thing) {
	SET_BNDCELL(list, R_UnboundValue); /* in case binding is cached */
	LOCK_BINDING(list);                /* in case binding is cached */
	SEXP rest = CDR(list);
	SETCDR(list, R_NilValue);          /* to fix refcnt on 'rest' */
	return std::pair<SEXP, bool>(rest, true);
    }
    else {
	SEXP last = list;
	SEXP next = CDR(list);
	while (next != R_NilValue) {
	    if (TAG(next) == thing) {
		SETCAR(next, R_UnboundValue); /* in case binding is cached */
		LOCK_BINDING(next);           /* in case binding is cached */
		SETCDR(last, CDR(next));
		SETCDR(next, R_NilValue);     /* to fix refcnt on 'list' */
		return std::pair<SEXP, bool>(list, true);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return std::pair<SEXP, bool>(list, false);
    }
}

attribute_hidden void R::unbindVar(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseNamespace)
	error("%s", _("cannot unbind in the base namespace"));
    if (rho == R_BaseEnv)
	error("%s", _("unbind in the base environment is unimplemented"));
    if (FRAME_IS_LOCKED(rho))
	error("%s", _("cannot remove bindings from a locked environment"));
    if (HASHTAB(rho) == R_NilValue) {
	auto list = RemoveFromList(symbol, FRAME(rho));
	if (list.second) {
	    if (rho == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(rho, list.first);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(rho))
		R_FlushGlobalCache(symbol);
#endif
	}
    }
    else {
	/* This branch is used e.g. via sys.source, utils::data */
	SEXP c = PRINTNAME(symbol);
	if (!HASHASH(c)) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	bool found = R_HashDelete(hashcode, symbol, rho);
#ifdef USE_GLOBAL_CACHE
	if (found && IS_GLOBAL_FRAME(rho))
	     R_FlushGlobalCache(symbol);
#endif
    }
}



/*----------------------------------------------------------------------

  findVarLocInFrame

  Look up the location of the value of a symbol in a
  single environment frame.  Almost like R_findVarInFrame, but
  does not return the value. R_NilValue if not found.

  Callers set *canCache = TRUE or NULL
*/

static SEXP findVarLocInFrame(SEXP rho, SEXP symbol, bool *canCache)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return (SYMVALUE(symbol) == R_UnboundValue) ? R_NilValue : symbol;

    if (!rho || rho == R_EmptyEnv)
	return R_NilValue;

    if (IS_USER_DATABASE(rho)) {
	SEXP tmp = R_NilValue;
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	/* Better to use exists() here if we don't actually need the value! */
	SEXP val = table->get(CHAR(PRINTNAME(symbol)), (Rboolean *) canCache, table);
	if (val != R_UnboundValue) {
	    /* The result should probably be identified as being from
	       a user database, or maybe use an active binding
	       mechanism to allow setting a new value to get back to
	       the data base. */
	    tmp = allocSExp(LISTSXP);
	    SETCAR(tmp, val);
	    SET_TAG(tmp, symbol);
	    /* If the database has a canCache method, then call that.
	       Otherwise, we believe the setting for canCache. */
	    if (canCache && table->canCache) {
		PROTECT(tmp);
		*canCache = table->canCache(CHAR(PRINTNAME(symbol)), table);
		UNPROTECT(1);
	    }
	    MARK_NOT_MUTABLE(val); /* to keep complex assignment code sane */
	}
	return(tmp);
    }

    if (HASHTAB(rho) == R_NilValue) {
	SEXP frame = FRAME(rho);
	while (frame != R_NilValue && TAG(frame) != symbol)
	    frame = CDR(frame);
	return frame;
    }
    else {
	SEXP c = PRINTNAME(symbol);
	if (!HASHASH(c)) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c,  1);
	}
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_NilValue' if not found */
	return R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
    }
}

/** @brief External version and accessor functions.
 * 
 * @return Returned value is cast as an opaque pointer to ensure it is only used by routines in this
 * group. This allows the implementation to be changed without needing to change other files.
 */
R_varloc_t R::R_findVarLocInFrame(SEXP rho, SEXP symbol)
{
    SEXP binding = findVarLocInFrame(rho, symbol, NULL);
    R_varloc_t val;
    val.cell = binding == R_NilValue ? NULL : binding;
    return val;
}

attribute_hidden
SEXP R::R_GetVarLocValue(R_varloc_t vl)
{
    SEXP cell = vl.cell;
    if (cell == NULL || cell == R_UnboundValue)
	return R_UnboundValue;
    else if (TYPEOF(cell) == SYMSXP)
	return SYMBOL_BINDING_VALUE(cell);
    else return BINDING_VALUE(cell);
}

attribute_hidden
SEXP R::R_GetVarLocSymbol(R_varloc_t vl)
{
    return TAG(vl.cell);
}

/* used in methods */
bool R::R_GetVarLocMISSING(R_varloc_t vl)
{
    return MISSING(vl.cell);
}

attribute_hidden
void R::R_SetVarLocValue(R_varloc_t vl, SEXP value)
{
    SET_BINDING_VALUE(vl.cell, value);
}


/*----------------------------------------------------------------------

  R_findVarInFrame

  Look up the value of a symbol in a single environment frame.	This
  is the basic building block of all variable lookups.

  It is important that this be as efficient as possible.

  The final argument is usually TRUE and indicates whether the
  lookup is being done in order to get the value (TRUE) or
  simply to check whether there is a value bound to the specified
  symbol in this frame (FALSE).  This is used for get() and exists().
*/

// In Rinternals.h
SEXP Rf_findVarInFrame3(SEXP rho, SEXP symbol, Rboolean doGet)
{
    if (TYPEOF(rho) == NILSXP)
	error("%s", _("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return SYMBOL_BINDING_VALUE(symbol);

    if (rho == R_EmptyEnv)
	return R_UnboundValue;

    if (IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	SEXP val = R_UnboundValue;
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if (table->active) {
	    if (doGet)
		val = table->get(CHAR(PRINTNAME(symbol)), NULL, table);
	    else {
		if (table->exists(CHAR(PRINTNAME(symbol)), NULL, table))
		    val = table->get(CHAR(PRINTNAME(symbol)), NULL, table);
		else
		    val = R_UnboundValue;
	    }
	    MARK_NOT_MUTABLE(val); /* to keep complex assignment code sane */
	}
	return(val);
    } else if (HASHTAB(rho) == R_NilValue) {
	SEXP frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol)
		return BINDING_VALUE(frame);
	    frame = CDR(frame);
	}
    }
    else {
	SEXP c = PRINTNAME(symbol);
	if (!HASHASH(c)) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	/* Will return 'R_UnboundValue' if not found */
	return(R_HashGet(hashcode, symbol, HASHTAB(rho)));
    }
    return R_UnboundValue;
}

/* This variant of findVarinFrame3 is needed to avoid running active
   binding functions in calls to exists() with mode = "any" */
Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol)
{
    if (TYPEOF(rho) == NILSXP)
	error("%s", _("use of NULL environment is defunct"));

    if (rho == R_BaseNamespace || rho == R_BaseEnv)
	return (Rboolean) SYMBOL_HAS_BINDING(symbol);

    if (rho == R_EmptyEnv)
	return FALSE;

    if (IS_USER_DATABASE(rho)) {
	/* Use the objects function pointer for this symbol. */
	Rboolean val = FALSE;
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if (table->active) {
	    val = (table->exists(CHAR(PRINTNAME(symbol)), NULL, table));
	}
	return val;
    } else if (HASHTAB(rho) == R_NilValue) {
	SEXP frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol)
		return TRUE;
	    frame = CDR(frame);
	}
    }
    else {
	SEXP c = PRINTNAME(symbol);
	if (!HASHASH(c)) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	return (Rboolean) R_HashExists(hashcode, symbol, HASHTAB(rho));
    }
    return FALSE;
}

attribute_hidden SEXP R::R_findVarInFrame(SEXP rho, SEXP symbol)
{
    return findVarInFrame3(rho, symbol, TRUE);
}

SEXP Rf_findVarInFrame(SEXP rho, SEXP symbol)
{
    return R_findVarInFrame(rho, symbol);
}

/** @brief Read the S3 meta-variables from a given (single) frame.
 * R_UnboundValue marks that respective variable is not present.
 * This function is optimized to be fast in the common case when the
 * S3 meta-variables are in the expected order and that the frame is
 * represented by a pairlist.
 */
attribute_hidden
void R::readS3VarsFromFrame(SEXP rho,
    SEXP *dotGeneric, SEXP *dotGroup, SEXP *dotClass, SEXP *dotMethod,
    SEXP *dotGenericCallEnv, SEXP *dotGenericDefEnv) {

    SEXP frame = FRAME(rho);

    if (TYPEOF(rho) == NILSXP ||
	rho == R_BaseNamespace || rho == R_BaseEnv || rho == R_EmptyEnv ||
	IS_USER_DATABASE(rho) || HASHTAB(rho) != R_NilValue) goto slowpath;


    /*
    This code speculates there is a specific order of S3 meta-variables.  It
    holds in most (perhaps all non-fabricated) cases.  If at any time this
    ceased to hold, this code will fall back to the slowpath, which may be
    slow but still correct.
    */

    for (;TAG(frame) != R_dot_Generic; frame = CDR(frame))
	if (frame == R_NilValue) goto slowpath;
    *dotGeneric = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Class) goto slowpath;
    *dotClass = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Method) goto slowpath;
    *dotMethod = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_Group) goto slowpath;
    *dotGroup = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_GenericCallEnv) goto slowpath;
    *dotGenericCallEnv = BINDING_VALUE(frame);
    frame = CDR(frame);

    if (TAG(frame) != R_dot_GenericDefEnv) goto slowpath;
    *dotGenericDefEnv = BINDING_VALUE(frame);

    return;

slowpath:
    /* fall back to the slow but general implementation */

    *dotGeneric = R_findVarInFrame(rho, R_dot_Generic);
    *dotClass = R_findVarInFrame(rho, R_dot_Class);
    *dotMethod = R_findVarInFrame(rho, R_dot_Method);
    *dotGroup = R_findVarInFrame(rho, R_dot_Group);
    *dotGenericCallEnv = R_findVarInFrame(rho, R_dot_GenericCallEnv);
    *dotGenericDefEnv = R_findVarInFrame(rho, R_dot_GenericDefEnv);
}


/*----------------------------------------------------------------------

  findVar

  Look up a symbol in an environment.

*/

#ifdef USE_GLOBAL_CACHE
/* findGlobalVar searches for a symbol value starting at R_GlobalEnv,
   so the cache can be used. */
static SEXP findGlobalVarLoc(SEXP symbol)
{
    bool canCache = true;
    SEXP vl = R_GetGlobalCacheLoc(symbol);
    if (vl != R_UnboundValue)
	return vl;
    for (SEXP rho = R_GlobalEnv; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
	if (rho != R_BaseEnv) { /* we won't have R_BaseNamespace */
	    vl = findVarLocInFrame(rho, symbol, &canCache);
	    if (vl != R_NilValue) {
		if (canCache)
		    R_AddGlobalCache(symbol, vl);
		return vl;
	    }
	}
	else {
	    if (SYMVALUE(symbol) != R_UnboundValue)
		R_AddGlobalCache(symbol, symbol);
	    return symbol;
	}
    }
    return R_NilValue;
}

static R_INLINE SEXP findGlobalVar(SEXP symbol)
{
    SEXP loc = findGlobalVarLoc(symbol);
    switch (TYPEOF(loc)) {
    case NILSXP: return R_UnboundValue;
    case SYMSXP: return SYMBOL_BINDING_VALUE(symbol);
    default: return BINDING_VALUE(loc);
                    /* loc is protected by callee when needed */
    }
}
#endif

attribute_hidden SEXP R::R_findVar(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if (TYPEOF(rho) == NILSXP)
	error("%s", _("use of NULL environment is defunct"));

    if (!isEnvironment(rho))
	error(_("argument to '%s' is not an environment"), "findVar");

#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_EmptyEnv) {
	vl = R_findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVar(symbol);
    else
	return R_UnboundValue;
#else
    while (rho != R_EmptyEnv) {
	vl = R_findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) return (vl);
	rho = ENCLOS(rho);
    }
    return R_UnboundValue;
#endif
}

/** @brief Look up a symbol in an environment.
 */
SEXP Rf_findVar(SEXP symbol, SEXP rho)
{
    return R_findVar(symbol, rho);
}

static SEXP findVarLoc(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if (TYPEOF(rho) == NILSXP)
	error("%s", _("use of NULL environment is defunct"));

    if (!isEnvironment(rho))
	error(_("argument to '%s' is not an environment"), "findVarLoc");

#ifdef USE_GLOBAL_CACHE
    /* This first loop handles local frames, if there are any.  It
       will also handle all frames if rho is a global frame other than
       R_GlobalEnv */
    while (rho != R_GlobalEnv && rho != R_EmptyEnv) {
	vl = findVarLocInFrame(rho, symbol, NULL);
	if (vl != R_NilValue) return vl;
	rho = ENCLOS(rho);
    }
    if (rho == R_GlobalEnv)
	return findGlobalVarLoc(symbol);
    else
	return R_NilValue;
#else
    while (rho != R_EmptyEnv) {
	vl = R_findVarInLocFrame(rho, symbol, NULL);
	if (vl != R_NilValue) return vl;
	rho = ENCLOS(rho);
    }
    return R_NilValue;
#endif
}

R_varloc_t R::R_findVarLoc(SEXP symbol, SEXP rho)
{
    SEXP binding = findVarLoc(symbol, rho);
    R_varloc_t val;
    val.cell = binding == R_NilValue ? NULL : binding;
    return val;
}


/** @brief Look up a symbol in an environment.
 * 
 * Ignore any values which are not of the specified type.
 */
attribute_hidden SEXP R::findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, bool inherits)
{
    SEXP vl;
    while (rho != R_EmptyEnv) {
	vl = R_findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if (TYPEOF(vl) == mode) return vl;
	    if (mode == FUNSXP && Rf_isFunction(vl))
		return (vl);
	}
	if (inherits)
	    rho = ENCLOS(rho);
	else
	    return (R_UnboundValue);
    }
    return (R_UnboundValue);
}

/** @brief Look up a symbol in an environment.
 * 
 * Ignore any values which are not of the specified mode.
 */
static SEXP findVar1mode(SEXP symbol, SEXP rho, SEXPTYPE mode, bool wants_S4,
	     bool inherits, bool doGet)
{
    SEXP vl;
    SEXPTYPE tl;
    if (mode == INTSXP) mode = REALSXP;
    if (mode == FUNSXP || mode ==  BUILTINSXP || mode == SPECIALSXP)
	mode = CLOSXP;
    while (rho != R_EmptyEnv) {
	if (!doGet && mode == ANYSXP)
	    vl = R_existsVarInFrame(rho, symbol) ? R_NilValue : R_UnboundValue;
	else
	    vl = findVarInFrame3(rho, symbol, (Rboolean) doGet);

	if (vl != R_UnboundValue) {
	    if (mode == ANYSXP) return vl;
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    tl = TYPEOF(vl);
	    if (tl == INTSXP) tl = REALSXP;
	    if (tl == FUNSXP || tl ==  BUILTINSXP || tl == SPECIALSXP)
		tl = CLOSXP;
	    if (tl == mode) {
		if (tl == OBJSXP) {
		    if ((wants_S4 && IS_S4_OBJECT(vl)) ||
			(!wants_S4 && !IS_S4_OBJECT(vl)))
			return vl;
		}
		else return vl;
	    }
	}
	if (inherits)
	    rho = ENCLOS(rho);
	else
	    return (R_UnboundValue);
    }
    return (R_UnboundValue);
}


/*
   ddVal ("dot-dot-value"):
   a function to take a name and determine if it is of the form
   ..x where x is an integer; if so x is returned otherwise 0 is returned
*/
static int ddVal(SEXP symbol)
{
    char *endp;
    int rval;

    const char *buf = CHAR(PRINTNAME(symbol));
    if (streqln(buf,"..",2) && strlen(buf) > 2) {
	buf += 2;
	rval = (int) strtol(buf, &endp, 10);
	if (*endp != '\0')
	    return 0;
	else
	    return rval;
    }
    return 0;
}

#define length_DOTS(_v_) (TYPEOF(_v_) == DOTSXP ? length(_v_) : 0)

SEXP ddfind(int i, SEXP rho)
{
    if (i <= 0)
	error(_("indexing '...' with non-positive index %d"), i);
    /* first look for ... symbol  */
    SEXP vl = R_findVar(R_DotsSymbol, rho);
    if (vl != R_UnboundValue) {
	if (length_DOTS(vl) >= i) {
	    vl = nthcdr(vl, i - 1);
	    return(CAR(vl));
	}
	else // length(...) < i
	    error(n_("the ... list contains fewer than %d element",
			   "the ... list contains fewer than %d elements", i),
                  i);
    }
    else error(_("..%d used in an incorrect context, no ... to look in"), i);

    return R_NilValue;
}

/** @brief This function fetches the variables ..1, ..2, etc from the first
 * frame of the environment passed as the second argument to ddfindVar.
 * 
 * These variables are implicitly defined whenever a ... object is
 * created.
 * 
 * To determine values for the variables we first search for an
 * explicit definition of the symbol, them we look for a ... object in
 * the frame and then walk through it to find the appropriate values.
 * If no value is obtained we return R_UnboundValue.
 * 
 * It is an error to specify a .. index longer than the length of the
 * ... object the value is sought in.
 */
attribute_hidden
SEXP R::ddfindVar(SEXP symbol, SEXP rho)
{
    int i = ddVal(symbol);
    return ddfind(i, rho);
}

attribute_hidden SEXP do_dotsElt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "n");

    SEXP si = CAR(args);
    if (!isNumeric(si) || XLENGTH(si) != 1)
	errorcall(call, "%s", _("indexing '...' with an invalid index"));
    int i = asInteger(si);
    return eval(ddfind(i, env), env);
}

attribute_hidden SEXP do_dotsLength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP vl = R_findVar(R_DotsSymbol, env);
    if (vl == R_UnboundValue)
	error("%s", _("incorrect context: the current call has no '...' to look in"));
    // else
    return ScalarInteger(length_DOTS(vl));
}

attribute_hidden SEXP do_dotsNames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    GCStackRoot<> vl;
    vl = R_findVar(R_DotsSymbol, env);

    if (vl == R_UnboundValue)
	error("%s", _("incorrect context: the current call has no '...' to look in"));
    // else
    GCStackRoot<> out(R_NilValue);
    int n = length_DOTS(vl);
    bool named = false;
    for (int i = 0; i < n; i++) {
	if (TAG(vl.get()) != R_NilValue) {
	    if (!named) { named = true;
		out = allocVector(STRSXP, n); // and is filled with "" already
	    }
	    SET_STRING_ELT(out, i, PRINTNAME(TAG(vl.get())));
	}
        vl = CDR(vl.get());
    }
    if (!named) {
	out = R_NilValue;
    }

    return out;
}

#undef length_DOTS

#ifdef UNUSED
/** @brief This function does a variable lookup, but uses dynamic scoping rules
 * rather than the lexical scoping rules used in findVar.
 * 
 * @return return R_UnboundValue if the symbol isn't located and the calling
 * function needs to handle the errors.
 */
SEXP R::dynamicfindVar(SEXP symbol, RCNTXT *cptr)
{
    SEXP vl;
    while (cptr && !isTopLevelContext(cptr)) {
	if (cptr->callflag & CTXT_FUNCTION) {
	    vl = R_findVarInFrame(cptr->cloenv, symbol);
	    if (vl != R_UnboundValue) return vl;
	}
	cptr = cptr->nextcontext;
    }
    return R_UnboundValue;
}
#endif



/*----------------------------------------------------------------------

  findFun

  Search for a function in an environment This is a specially modified
  version of findVar which ignores values its finds if they are not
  functions.

 [ NEEDED: This needs to be modified so that a search for an arbitrary mode can
  be made.  Then findVar and findFun could become same function.]

  This could call findVar1.  NB: they behave differently on failure.
*/

attribute_hidden
SEXP R::findFun3(SEXP symbol, SEXP rho, SEXP call)
{
    SEXP vl;

    /* If the symbol is marked as special, skip to the first
       environment that might contain such a symbol. */
    if (IS_SPECIAL_SYMBOL(symbol)) {
	while (rho != R_EmptyEnv && NO_SPECIAL_SYMBOLS(rho))
	    rho = ENCLOS(rho);
    }

    while (rho != R_EmptyEnv) {
	/* This is not really right.  Any variable can mask a function */
#ifdef USE_GLOBAL_CACHE
	if (rho == R_GlobalEnv)
#ifdef FAST_BASE_CACHE_LOOKUP
	    if (BASE_SYM_CACHED(symbol))
		vl = SYMBOL_BINDING_VALUE(symbol);
	    else
		vl = findGlobalVar(symbol);
#else
	    vl = findGlobalVar(symbol);
#endif
	else
	    vl = R_findVarInFrame(rho, symbol);
#else
	vl = R_findVarInFrame(rho, symbol);
#endif
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		if (PROMISE_IS_EVALUATED(vl))
		    vl = PRVALUE(vl);
		else {
		    PROTECT(vl);
		    vl = eval(vl, rho);
		    UNPROTECT(1);
		}
	    }
	    if (Rf_isFunction(vl))
		return (vl);
	    if (vl == R_MissingArg)
	        R_MissingArgError(symbol, call, "getMissingError");

	}
	rho = ENCLOS(rho);
    }
    errorcall_cpy(call,
                  _("could not find function \"%s\""),
                  EncodeChar(PRINTNAME(symbol)));
    /* NOT REACHED */
    return R_UnboundValue;
}

SEXP Rf_findFun(SEXP symbol, SEXP rho)
{
    return findFun3(symbol, rho, R_CurrentExpression);
}

/** @brief Assign a value in a specific environment frame.
 */
void Rf_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    if (value == R_UnboundValue)
	error("%s", _("attempt to bind a variable to R_UnboundValue"));
    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv) R_DirtyImage = 1;

    if (rho == R_EmptyEnv)
	error("%s", _("cannot assign values in the empty environment"));

    if (IS_USER_DATABASE(rho)) {
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if (table->assign == NULL)
	    error("%s", _("cannot assign variables to this database"));
	PROTECT(value);
	table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif
	return;
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	gsetVar(symbol, value, rho);
    } else {
#ifdef USE_GLOBAL_CACHE
	if (IS_GLOBAL_FRAME(rho)) R_FlushGlobalCache(symbol);
#endif

	if (IS_SPECIAL_SYMBOL(symbol))
	    UNSET_NO_SPECIAL_SYMBOLS(rho);

	if (HASHTAB(rho) == R_NilValue) {
	    /* First check for an existing binding */
	    SEXP frame = FRAME(rho);
	    while (frame != R_NilValue) {
		if (TAG(frame) == symbol) {
		    SET_BINDING_VALUE(frame, value);
		    SET_MISSING(frame, 0);	/* Over-ride */
		    return;
		}
		frame = CDR(frame);
	    }
	    if (FRAME_IS_LOCKED(rho))
		error("%s", _("cannot add bindings to a locked environment"));
	    SET_FRAME(rho, CONS(value, FRAME(rho)));
	    SET_TAG(FRAME(rho), symbol);
	}
	else {
	    SEXP c = PRINTNAME(symbol);
	    if (!HASHASH(c)) {
		SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
		SET_HASHASH(c, 1);
	    }
	    int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	    R_HashSet(hashcode, symbol, HASHTAB(rho), value,
		      FRAME_IS_LOCKED(rho));
	    if (R_HashSizeCheck(HASHTAB(rho)))
		SET_HASHTAB(rho, R_HashResize(HASHTAB(rho)));
	}
    }
}

/** @brief Add given variables (addVars - list) to given environment (env) unless
 * they are already there.
 * 
 * Env is a "new" environment, created by NewEnvironment, as in applyClosure (so it list-based).
 * Slots for vars are re-used.  The addVars list itself can have duplicit variables.
 * 
 * The implementation is performance optimized towards the common case that
 * the variables from addVars are not present in env and that addVars does
 * not have duplicit variables.
 */
attribute_hidden
void R::addMissingVarsToNewEnv(SEXP env, SEXP addVars)
{
    if (addVars == R_NilValue) return;

    /* temporary sanity check */
    if (Environment::isA(addVars))
	error("%s", _("additional variables should now be passed as a list, not in an environment"));

    /* append variables from env after addVars */
    SEXP aprev = addVars;
    SEXP a = CDR(addVars);
    while (a != R_NilValue) {
	aprev = a;
	a = CDR(a);
    }
    SETCDR(aprev, FRAME(env));
    SET_FRAME(env, addVars);

    /* remove duplicates - a variable listed later has precedence over a
       variable listed sooner */
    for (SEXP end = CDR(addVars); end != R_NilValue; end = CDR(end)) {
	SEXP endTag = TAG(end);
	SEXP sprev = R_NilValue;
	for (SEXP s = addVars; s != end; s = CDR(s)) {
	    if (TAG(s) == endTag) {
		/* remove variable s from the list, because it is overridden by "end" */
		if (sprev == R_NilValue) {
		    addVars = CDR(s);
		    SET_FRAME(env, addVars);
		} else
		    SETCDR(sprev, CDR(s));
	    } else
		sprev = s;
	}
    }
}

/*----------------------------------------------------------------------

  setVarInFrame

  Assign a new value to an existing symbol in a frame.
  Return the symbol if successful and R_NilValue if not.

  [ Taken static in 2.4.0: not called for emptyenv or baseenv. ]
*/

static SEXP setVarInFrame(SEXP rho, SEXP symbol, SEXP value)
{
    /* R_DirtyImage should only be set if assigning to R_GlobalEnv. */
    if (rho == R_GlobalEnv) R_DirtyImage = 1;
    if (rho == R_EmptyEnv) return R_NilValue;

    if (IS_USER_DATABASE(rho)) {
	/* FIXME: This does not behave as described */
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(rho));
	if (table->assign == NULL)
	    error("%s", _("cannot assign variables to this database"));
	PROTECT(value);
	SEXP result = table->assign(CHAR(PRINTNAME(symbol)), value, table);
	UNPROTECT(1);
	return(result);
    }

    if (rho == R_BaseNamespace || rho == R_BaseEnv) {
	if (SYMVALUE(symbol) == R_UnboundValue) return R_NilValue;
	SET_SYMBOL_BINDING_VALUE(symbol, value);
	return symbol;
    }

    if (HASHTAB(rho) == R_NilValue) {
	SEXP frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol) {
		SET_BINDING_VALUE(frame, value);
		SET_MISSING(frame, 0);	/* same as defineVar */
		return symbol;
	    }
	    frame = CDR(frame);
	}
    } else {
	/* Do the hash table thing */
	SEXP c = PRINTNAME(symbol);
	if (!HASHASH(c)) {
	    SET_HASHVALUE(c, R_Newhashpjw(CHAR(c)));
	    SET_HASHASH(c, 1);
	}
	int hashcode = HASHVALUE(c) % HASHSIZE(HASHTAB(rho));
	SEXP frame = R_HashGetLoc(hashcode, symbol, HASHTAB(rho));
	if (frame != R_NilValue) {
	    SET_BINDING_VALUE(frame, value);
	    SET_MISSING(frame, 0);	/* same as defineVar */
	    return symbol;
	}
    }
    return R_NilValue; /* -Wall */
}


/** @brief Assign a new value to bound symbol.
 * 
 * Note this does the "inherits" case.
 * I.e. it searches frame-by-frame for a symbol and binds the
 * given value to the first symbol encountered.  If no symbol is
 * found then a binding is created in the global environment.
 * 
 * Changed in R 2.4.0 to look in the base environment (previously the
 * search stopped befor the base environment, but would (and still
 * does) assign into the base namespace if that is on the search and
 * the symbol existed there).
 */
void Rf_setVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP vl;
    while (rho != R_EmptyEnv) {
	vl = setVarInFrame(rho, symbol, value);
	if (vl != R_NilValue) return;
	rho = ENCLOS(rho);
    }
    defineVar(symbol, value, R_GlobalEnv);
}

/** @brief Assignment in the base environment.
 * 
 * Here we assign directly into the base environment.
 */
void R::gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    if (FRAME_IS_LOCKED(rho)) {
	if (SYMVALUE(symbol) == R_UnboundValue)
	    error(_("cannot add binding of '%s' to the base environment"),
		  CHAR(PRINTNAME(symbol)));
    }
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(symbol);
#endif
    SET_SYMBOL_BINDING_VALUE(symbol, value);
}

/*----------------------------------------------------------------------

  do_assign : .Internal(assign(x, value, envir, inherits))

*/
attribute_hidden SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name=R_NilValue, val, aenv;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error("%s", _("invalid first argument"));
    else {
	if (length(CAR(args)) > 1)
	    warning("%s", _("only the first element is used as variable name"));
	name = installTrChar(STRING_ELT(CAR(args), 0));
    }
    PROTECT(val = CADR(args));
    aenv = CADDR(args);
    aenv = simple_as_environment(aenv);
    if (aenv == R_NilValue)
	error(_("invalid '%s' argument"), "envir");
    if (asLogicalNoNA(CADDDR(args), "inherits"))
	setVar(name, val, aenv);
    else
	defineVar(name, val, aenv);
    UNPROTECT(1);
    return val;
}


/**
 * @example .Internal(list2env(x, envir))
 */
attribute_hidden SEXP do_list2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xnms, envir;
    int n;
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != VECSXP)
	error("%s", _("first argument must be a named list"));
    x = CAR(args);
    n = LENGTH(x);
    xnms = getAttrib(x, R_NamesSymbol);
    PROTECT(xnms);
    if (n && (TYPEOF(xnms) != STRSXP || LENGTH(xnms) != n))
	error("%s", _("names(x) must be a character vector of the same length as x"));
    envir = CADR(args);
    if (TYPEOF(envir) != ENVSXP)
	error(_("'%s' must be an environment"), "envir");

    for (int i = 0; i < n; i++) {
	SEXP name = installTrChar(STRING_ELT(xnms, i));
	defineVar(name, lazy_duplicate(VECTOR_ELT(x, i)), envir);
    }
    UNPROTECT(1); /* xnms */

    return envir;
}


/*----------------------------------------------------------------------

  do_remove

  @brief Remove variable

  @details There are three arguments to do_remove; a list of names to remove,
  an optional environment (if missing set it to R_GlobalEnv) and
  inherits, a logical indicating whether to look in the parent env if
  a symbol is not found in the supplied env.  This is ignored if
  environment is not specified.

*/

static bool RemoveVariable(SEXP name, int hashcode, SEXP env)
{
    if (env == R_BaseNamespace)
	error("%s", _("cannot remove variables from base namespace"));
    if (env == R_BaseEnv)
	error("%s", _("cannot remove variables from the base environment"));
    if (env == R_EmptyEnv)
	error("%s", _("cannot remove variables from the empty environment"));
    if (FRAME_IS_LOCKED(env))
	error("%s", _("cannot remove bindings from a locked environment"));

    if (IS_USER_DATABASE(env)) {
	R_ObjectTable *table = (R_ObjectTable *) R_ExternalPtrAddr(HASHTAB(env));
	if (table->remove == NULL)
	    error("%s", _("cannot remove variables from this database"));
	return(table->remove(CHAR(PRINTNAME(name)), table));
    }

    if (IS_HASHED(env)) {
	bool found = R_HashDelete(hashcode, name, env);
#ifdef USE_GLOBAL_CACHE
	if (found && IS_GLOBAL_FRAME(env))
	    R_FlushGlobalCache(name);
#endif
	return found;
    } else {
	auto list = RemoveFromList(name, FRAME(env));
	if (list.second) {
	    if (env == R_GlobalEnv) R_DirtyImage = 1;
	    SET_FRAME(env, list.first);
#ifdef USE_GLOBAL_CACHE
	    if (IS_GLOBAL_FRAME(env))
		R_FlushGlobalCache(name);
#endif
	}
	return list.second;
    }
}

/** @brief Remove elements for an environment
 * 
 * There are three arguments to do_remove; a list of names to remove,
 * an optional environment (if missing set it to R_GlobalEnv) and
 * inherits, a logical indicating whether to look in the parent env if
 * a symbol is not found in the supplied env.  This is ignored if
 * environment is not specified.
 * 
 * @example .Internal(remove(list, envir, inherits))
 */
attribute_hidden SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* .Internal(remove(list, envir, inherits)) */

    checkArity(op, args);

    SEXP name = CAR(args);
    if (TYPEOF(name) == NILSXP) return R_NilValue;
    if (!isString(name))
	error("%s", _("invalid first argument"));
    args = CDR(args);

    SEXP envarg = simple_as_environment(CAR(args));
    if (envarg == R_NilValue)
	error(_("invalid '%s' argument"), "envir");
    args = CDR(args);

    bool ginherits = asLogicalNoNA(CAR(args), "inherits");

    bool done = false;
    for (int i = 0; i < LENGTH(name); i++) {
	SEXP tsym = installTrChar(STRING_ELT(name, i));
	int hashcode;
	if (!HASHASH(PRINTNAME(tsym)))
	    hashcode = R_Newhashpjw(CHAR(PRINTNAME(tsym)));
	else
	    hashcode = HASHVALUE(PRINTNAME(tsym));
	SEXP tenv = envarg;
	while (tenv != R_EmptyEnv) {
	    done = RemoveVariable(tsym, hashcode, tenv);
	    if (done || !ginherits)
		break;
	    tenv = ENCLOS(tenv);
	}
	if (!done)
	    warning(_("object '%s' not found"), EncodeChar(PRINTNAME(tsym)));
    }
    return R_NilValue;
}

void R_removeVarFromFrame(SEXP name, SEXP env)
{
    int hashcode = -1;

    if (TYPEOF(env) == NILSXP)
	error("%s", _("use of NULL environment is defunct"));

    if (!isEnvironment(env))
	error(_("argument to '%s' is not an environment"), "R_removeVarFromFrame");

    if (TYPEOF(name) != SYMSXP)
	error("%s", _("not a symbol"));

    if (IS_HASHED(env)) {
	if (!HASHASH(PRINTNAME(name)))
	    hashcode = R_Newhashpjw(CHAR(PRINTNAME(name)));
	else
	    hashcode = HASHVALUE(PRINTNAME(name));
    }
    RemoveVariable(name, hashcode, env);
}


static SEXPTYPE str2mode(const char *modestr, bool *pS4)
{
    if (streql(modestr, "function"))
	return FUNSXP;
    else if (streql(modestr, "S4")) {
	if (pS4 != NULL)
	    *pS4 = TRUE;
	return OBJSXP;
    }
    else {
	SEXPTYPE gmode = str2type(modestr);
	if (gmode == (SEXPTYPE) (-1))
	    error(_("invalid '%s' argument '%s'"), "mode", modestr);
	return gmode;
    }
}

/** @brief This function returns the SEXP associated with the character
 * argument.
 * 
 * It needs the environment of the calling function as a default.
 * 
 * @example exists (x, envir, mode, inherits)
 * @example get    (x, envir, mode, inherits)
 * @example get0   (x, envir, mode, inherits, value_if_not_exists)
 */
attribute_hidden SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, genv, t1 = R_NilValue;
    int where;
    checkArity(op, args);

    /* The first arg is the object name */
    /* It must be present and a non-empty string */

    if (TYPEOF(CAR(args)) == SYMSXP)
	t1 = CAR(args);
    else if (isValidStringF(CAR(args))) {
	if (XLENGTH(CAR(args)) > 1)
	    error("%s", _("first argument has length > 1"));
	t1 = installTrChar(STRING_ELT(CAR(args), 0));
    }
    else
	error("%s", _("invalid first argument"));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(CADR(args)) == ENVSXP)
	genv = CADR(args);
    else if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
	where = asInteger(CADR(args));
	genv = R_sysframe(where, R_GlobalContext);
    }
    else if (TYPEOF(CADR(args)) == NILSXP) {
	error("%s", _("use of NULL environment is defunct"));
	genv = R_NilValue;  /* -Wall */
    }
    else if (TYPEOF((genv = simple_as_environment(CADR(args), true))) != ENVSXP) {
	error(_("invalid '%s' argument"), "envir");
	genv = R_NilValue;  /* -Wall */
    }

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    SEXPTYPE gmode;
    bool wants_S4 = FALSE;
    if (isString(CADDR(args)))
	gmode = str2mode(CHAR(STRING_ELT(CADDR(args), 0)), &wants_S4);
    else {
	error(_("invalid '%s' argument"), "mode");
	gmode = FUNSXP;/* -Wall */
    }

    bool ginherits = asLogicalNoNA(CADDDR(args), "inherits");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, wants_S4, ginherits, PRIMVAL(op));
    if (rval == R_MissingArg) { // signal a *classed* error:
	R_MissingArgError(t1, call, "getMissingError");
    }

    switch (PRIMVAL(op) ) {
    case 0: // exists(.) :
	return ScalarLogical(rval != R_UnboundValue);
	break;

    case 1: // have get(.)
	if (rval == R_UnboundValue) {
	    if (gmode == ANYSXP)
		error(_("object '%s' not found"), EncodeChar(PRINTNAME(t1)));
	    else
		error(_("object '%s' of mode '%s' was not found"),
		      CHAR(PRINTNAME(t1)),
		      CHAR(STRING_ELT(CADDR(args), 0))); /* ASCII */
	}

#     define GET_VALUE(rval) do {				\
	    /* We need to evaluate if it is a promise */	\
	    if (TYPEOF(rval) == PROMSXP) {			\
		PROTECT(rval);					\
		rval = eval(rval, genv);			\
		UNPROTECT(1);					\
	    }							\
	    ENSURE_NAMED(rval);					\
	} while (0)

	GET_VALUE(rval);
	break;

    case 2: // get0(.)
	if (rval == R_UnboundValue)
	    return CAD4R(args);// i.e.  value_if_not_exists
	GET_VALUE(rval);
	break;
    }
    return rval;
}
#undef GET_VALUE

static SEXP gfind(const char *name, SEXP env,
		  SEXPTYPE mode, bool wants_S4,
		  SEXP ifnotfound, bool inherits, SEXP enclos)
{
    SEXP rval, t1, R_fcall, var;

    t1 = install(name);

    /* Search for the object - last arg is 1 to 'get' */
    rval = findVar1mode(t1, env, mode, wants_S4, inherits, TRUE);

    if (rval == R_UnboundValue) {
	if (FunctionBase::isA(ifnotfound)) {
	    PROTECT(var = mkString(name));
	    PROTECT(R_fcall = LCONS(ifnotfound, CONS(var, R_NilValue)));
	    rval = eval(R_fcall, enclos);
	    UNPROTECT(2);
	} else
	    rval = ifnotfound;
    }

    /* We need to evaluate if it is a promise */
    if (TYPEOF(rval) == PROMSXP) {
	PROTECT(rval);
	rval = eval(rval, env);
	UNPROTECT(1);
    }
    ENSURE_NAMED(rval);
    return rval;
}


/** @brief Get multiple values from an environment
 * 
 * @example .Internal(mget(x, envir, mode, ifnotfound, inherits))
 * 
 * @return  a list of the same length as x, a character vector (of names).
 */
attribute_hidden SEXP do_mget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, x, mode, ifnotfound;
    int nvals, nmode, nifnfnd;

    checkArity(op, args);

    x = CAR(args);

    nvals = length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!isString(x) )
	error("%s", _("invalid first argument"));
    for (int i = 0; i < nvals; i++)
	if (isNull(STRING_ELT(x, i)) || !CHAR(STRING_ELT(x, 0))[0])
	    error(_("invalid name in position %d"), i+1);

    env = CADR(args);
    if (ISNULL(env)) {
	error("%s", _("use of NULL environment is defunct"));
    } else if (!isEnvironment(env))
	error("%s", _("second argument must be an environment"));

    mode = CADDR(args);
    nmode = length(mode);
    if (!isString(mode))
	error(_("invalid '%s' argument"), "mode");

    if (nmode != nvals && nmode != 1)
	error(_("wrong length for '%s' argument"), "mode");

    PROTECT(ifnotfound = coerceVector(CADDDR(args), VECSXP));
    nifnfnd = length(ifnotfound);
    if (!isVector(ifnotfound))
	error(_("invalid '%s' argument"), "ifnotfound");

    if (nifnfnd != nvals && nifnfnd != 1)
	error(_("wrong length for '%s' argument"), "ifnotfound");

    bool ginherits = asLogicalNoNA(CAD4R(args), "inherits");

    PROTECT(ans = allocVector(VECSXP, nvals));

    for (int i = 0; i < nvals; i++) {
	bool wants_S4 = FALSE;
	const char *modestr = CHAR(STRING_ELT(CADDR(args), i % nmode));
	SEXPTYPE gmode = str2mode(modestr, &wants_S4);
	SEXP nf = VECTOR_ELT(ifnotfound, i % nifnfnd);
	SEXP ans_i = gfind(translateChar(STRING_ELT(x, i % nvals)), env,
			   gmode, wants_S4, nf, ginherits, rho);
	SET_VECTOR_ELT(ans, i, lazy_duplicate(ans_i));
    }

    setAttrib(ans, R_NamesSymbol, lazy_duplicate(x));
    UNPROTECT(2);
    return(ans);
}

// In Rinternals.h
SEXP R_getVarEx(SEXP sym, SEXP rho, Rboolean inherits, SEXP ifnotfound)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("first argument to '%s' must be a symbol"), __func__);
    if (TYPEOF(rho) != ENVSXP)
	error(_("second argument to '%s' must be an environment"), __func__);

    SEXP val = inherits ? R_findVar(sym, rho) : R_findVarInFrame(rho, sym);
    if (val == R_MissingArg)
	R_MissingArgError_c(EncodeChar(PRINTNAME(sym)), getLexicalCall(rho), "getVarExError");
    else if (val == R_UnboundValue)
	return ifnotfound;
    else if (TYPEOF(val) == PROMSXP) {
	PROTECT(val);
	val = eval(val, rho);
	UNPROTECT(1);
    }
    return val;
}

// In Rinternals.h
SEXP R_getVar(SEXP sym, SEXP rho, Rboolean inherits)
{
    SEXP val = R_getVarEx(sym, rho, inherits, R_UnboundValue);
    if (val == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    return val;
}


/*----------------------------------------------------------------------

  do_missing

  This function tests whether the symbol passed as its first argument
  is a missing argument to the current closure.  rho is the
  environment that missing was called from.

  R_isMissing is called on the not-yet-evaluated value of an argument,
  if this is a symbol, as it could be a missing argument that has been
  passed down.  So 'symbol' is the promise value, and 'rho' its
  evaluation argument.

  It is also called in arithmetic.c. for e.g. do_log
*/

static SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
	while(TYPEOF(PREXPR(p)) == PROMSXP) {
	    p = PREXPR(p);
	}
    }
    return p;
}

// missing() for the case of promise aka *un*evaluated symbol:
attribute_hidden bool R::R_isMissing(SEXP symbol, SEXP rho)
{
    int ddv=0;
    SEXP vl, s;

    if (symbol == R_MissingArg) /* Yes, this can happen */
	return true;

    /* check for infinite recursion */
    R_CheckStack();

    if (DDVAL(symbol)) {
	s = R_DotsSymbol;
	ddv = ddVal(symbol);
    }
    else
	s = symbol;

    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return false;  /* is this really the right thing to do? LT */

    vl = findVarLocInFrame(rho, s, NULL);
    if (vl != R_NilValue) {
	if (DDVAL(symbol)) {
	    if (length(CAR(vl)) < ddv || CAR(vl) == R_MissingArg)
		return TRUE;
	    /* defineVar(symbol, value, R_GlobalEnv); */
	    else
		vl = nthcdr(CAR(vl), ddv-1);
	}
	if (MISSING(vl) == 1 ||
	    (BNDCELL_TAG(vl) == NILSXP && CAR(vl) == R_MissingArg))
	    return true;
	if (IS_ACTIVE_BINDING(vl))
	    return false;
	if (BNDCELL_TAG(vl))
	    return false;
	SETCAR(vl, findRootPromise(CAR(vl)));
	if (TYPEOF(CAR(vl)) == PROMSXP &&
	    !PROMISE_IS_EVALUATED(CAR(vl)) &&
	    TYPEOF(PREXPR(CAR(vl))) == SYMSXP) {
	    /* This code uses the PRSEEN value to detect cycles.  If a
	       cycle occurs then a missing argument was encountered,
	       so the return value is TRUE.  It would be a little
	       safer to use the promise stack to ensure unsetting of
	       the bits in the event of a longjump, but doing so would
	       require distinguishing between evaluating promises and
	       checking for missingness.  Because of the test above
	       for an active binding a longjmp should only happen if
	       the stack check fails.  LT */
	    if (PRSEEN(CAR(vl)) == UNDER_EVALUATION)
		return true;
	    else {
		int oldseen = PRSEEN(CAR(vl));
		SET_PRSEEN(CAR(vl), UNDER_EVALUATION);
		PROTECT(vl);
		bool val = R_isMissing(PREXPR(CAR(vl)), PRENV(CAR(vl)));
		UNPROTECT(1); /* vl */
		/* The oldseen value will usually be 0, but might be 2
		   from an interrupted evaluation. LT */
		SET_PRSEEN(CAR(vl), oldseen);
		return val;
	    }
	}
	else
	    return false;
    }
    return false;
}

// workhorse of do_missing()  == R's missing(); more generally useful -> ./bind.c
attribute_hidden
bool R::R_missing(SEXP var, SEXP rho)
{
    int ddv = 0;
    SEXP s = var;
    if (DDVAL(var)) {
	ddv = ddVal(var);
	var = R_DotsSymbol;
    }

    SEXP t = findVarLocInFrame(rho, var, NULL);
    if (t != R_NilValue) {
	if (DDVAL(s)) {
	    if (length(CAR(t)) < ddv  || CAR(t) == R_MissingArg) {
		return true;
	    }
	    else
		t = nthcdr(CAR(t), ddv-1);
	}
	if (BNDCELL_TAG(t)) return false;
	if (MISSING(t) || CAR(t) == R_MissingArg) {
	    return true;
	}
    }
    else  /* it wasn't an argument to the function */
	error(_("'missing(%s)' did not find an argument"), CHAR(PRINTNAME(var)));

    t = CAR(t);
    if (TYPEOF(t) != PROMSXP) {
	return false;
    }
    // deal with promise :
    t = findRootPromise(t);
    if (!isSymbol(PREXPR(t)))
	return false;
    else {
	return R_isMissing(PREXPR(t), PRENV(t));
    }
}

/** @brief This function tests whether the symbol passed as its first argument
 * is a missing argument to the current closure.  rho is the
 * environment that missing was called from.
 * 
 * This is primitive and a SPECIALSXP
 * 
 * R_isMissing is called on the not-yet-evaluated value of an argument,
 * if this is a symbol, as it could be a missing argument that has been
 * passed down.  So 'symbol' is the promise value, and 'rho' its
 * evaluation argument.
 * 
 * @note It is also called in arithmetic.cpp. for e.g. do_log
 */
attribute_hidden SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    SEXP sym = CAR(args);
    if (isString(sym) && length(sym) == 1)
	sym = installTrChar(STRING_ELT(CAR(args), 0));
    if (!isSymbol(sym))
	errorcall(call, "%s", _("invalid use of 'missing'"));

    GCStackRoot<LogicalVector> rval;
    rval = LogicalVector::createScalar(R_missing(sym, rho));
    return rval;
}

/**
 * @return the current global environment.
 */
attribute_hidden SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}

/**
 * @return the current base environment.
 */
attribute_hidden SEXP do_baseenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_BaseEnv;
}

/**
 * @return the current empty environment.
 */
attribute_hidden SEXP do_emptyenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_EmptyEnv;
}

static void set_attach_frame_value(SEXP p, SEXP s)
{
    if (IS_ACTIVE_BINDING(p))
	R_MakeActiveBinding(TAG(p), CAR(p), s);
    else
	defineVar(TAG(p), lazy_duplicate(CAR(p)), s);
}

/** ----------------------------------------------------------------------

  @name do_attach

  @details To attach a list we make up an environment and insert components
  of the list in as the values of this env and install the tags from
  the list as the names.

*/
attribute_hidden SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, s, t, x;
    int hsize;

    checkArity(op, args);

    int pos = asInteger(CADR(args));
    if (pos == NA_INTEGER)
	error("%s", _("'pos' must be an integer"));

    name = CADDR(args);
    if (!isValidStringF(name))
	error(_("invalid '%s' argument"), "name");

    bool isSpecial = IS_USER_DATABASE(CAR(args));

    if (!isSpecial) {
	if (isNewList(CAR(args))) {
	    SETCAR(args, VectorToPairList(CAR(args)));

	    for (x = CAR(args); x != R_NilValue; x = CDR(x))
		if (TAG(x) == R_NilValue)
		    error("%s", _("all elements of a list must be named"));
	    PROTECT(s = allocSExp(ENVSXP));
	    SET_FRAME(s, shallow_duplicate(CAR(args)));
	} else if (isEnvironment(CAR(args))) {
	    SEXP p, loadenv = CAR(args);

	    PROTECT(s = allocSExp(ENVSXP));
	    if (HASHTAB(loadenv) != R_NilValue) {
		int n = length(HASHTAB(loadenv));
		for (int i = 0; i < n; i++) {
		    p = VECTOR_ELT(HASHTAB(loadenv), i);
		    while (p != R_NilValue) {
			set_attach_frame_value(p, s);
			p = CDR(p);
		    }
		}
		/* FIXME: duplicate the hash table and assign here */
	    } else {
		for (p = FRAME(loadenv); p != R_NilValue; p = CDR(p))
		    set_attach_frame_value(p, s);
	    }
	} else {
	    error("%s", _("'attach' only works for lists, data frames and environments"));
	    s = R_NilValue; /* -Wall */
	}

	/* Connect FRAME(s) into HASHTAB(s) */
	if (length(s) < HASHMINSIZE)
	    hsize = HASHMINSIZE;
	else
	    hsize = length(s);

	SET_HASHTAB(s, R_NewHashTable(hsize));
	s = R_HashFrame(s);

	/* FIXME: A little inefficient */
	while (R_HashSizeCheck(HASHTAB(s)))
	    SET_HASHTAB(s, R_HashResize(HASHTAB(s)));

    } else { /* is a user object */
	/* Having this here (rather than below) means that the onAttach routine
	   is called before the table is attached. This may not be necessary or
	   desirable. */
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(CAR(args));
	if (tb->onAttach)
	    tb->onAttach(tb);
	PROTECT(s = allocSExp(ENVSXP));
	SET_HASHTAB(s, CAR(args));
	setAttrib(s, R_ClassSymbol, getAttrib(HASHTAB(s), R_ClassSymbol));
    }

    setAttrib(s, R_NameSymbol, name);
    for (t = R_GlobalEnv; ENCLOS(t) != R_BaseEnv && pos > 2; t = ENCLOS(t))
	pos--;

    if (ENCLOS(t) == R_BaseEnv) {
	SET_ENCLOS(t, s);
	SET_ENCLOS(s, R_BaseEnv);
    }
    else {
	x = ENCLOS(t);
	SET_ENCLOS(t, s);
	SET_ENCLOS(s, x);
    }

    if (!isSpecial) { /* Temporary: need to remove the elements identified by objects(CAR(args)) */
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
#endif
    } else {
#ifdef USE_GLOBAL_CACHE
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_GLOBAL_FRAME(s);
#endif
    }

    UNPROTECT(1); /* s */
    return s;
}

/** @brief Detach the specified environment.
 * 
 * Detachment only takes place by position.
 */
attribute_hidden SEXP do_detach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, x;
    int pos, n;
    bool isSpecial = false;

    checkArity(op, args);
    pos = asInteger(CAR(args));

    for (n = 2, t = ENCLOS(R_GlobalEnv); t != R_BaseEnv; t = ENCLOS(t))
	n++;

    if (pos == n) /* n is the length of the search list */
	error("%s", _("detaching \"package:base\" is not allowed"));

    for (t = R_GlobalEnv ; ENCLOS(t) != R_BaseEnv && pos > 2 ; t = ENCLOS(t))
	pos--;
    if (pos != 2) {
	error(_("invalid '%s' argument"), "pos");
	s = t;	/* for -Wall */
    }
    else {
	PROTECT(s = ENCLOS(t));
	x = ENCLOS(s);
	SET_ENCLOS(t, x);
	isSpecial = IS_USER_DATABASE(s);
	if (isSpecial) {
	    R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(s));
	    if (tb->onDetach) tb->onDetach(tb);
	}

	SET_ENCLOS(s, R_BaseEnv);
    }
#ifdef USE_GLOBAL_CACHE
    if (!isSpecial) {
	R_FlushGlobalCacheFromTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s);
    } else {
	R_FlushGlobalCacheFromUserTable(HASHTAB(s));
	MARK_AS_LOCAL_FRAME(s); /* was _GLOBAL_ prior to 2.4.0 */
    }
#endif
    UNPROTECT(1);
    return s;
}



/** @brief Print out the current search path.
 */
attribute_hidden SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name;
    int i, n;

    checkArity(op, args);
    n = 2;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, mkChar("package:base"));
    i = 1;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if (!isString(name) || length(name) < 1)
	    SET_STRING_ELT(ans, i, mkChar("(unknown)"));
	else
	    SET_STRING_ELT(ans, i, STRING_ELT(name, 0));
	i++;
    }
    UNPROTECT(1);
    return ans;
}


#define NONEMPTY_(_FRAME_) \
    CHAR(PRINTNAME(TAG(_FRAME_)))[0] != '.'

/** @brief This code implements the functionality of the "ls" and "objects"
 * functions.
 * 
 * @example ls(envir, all.names, sorted)
 */
static int FrameSize(SEXP frame, bool all)
{
    int count = 0;
    if (all) {
	while (frame != R_NilValue) {
	    count += 1;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame))
		count += 1;
	    frame = CDR(frame);
	}
    }
    return count;
}

static void FrameNames(SEXP frame, bool all, SEXP names, int *indx)
{
    if (all) {
	while (frame != R_NilValue) {
	    SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
	    (*indx)++;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame)) {
		SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
		(*indx)++;
	    }
	    frame = CDR(frame);
	}
    }
}

static void FrameValues(SEXP frame, bool all, SEXP values, int *indx)
{
    if (all) {
	while (frame != R_NilValue) {
#         define DO_FrameValues						\
	    SEXP value = BINDING_VALUE(frame);				\
	    if (TYPEOF(value) == PROMSXP) {				\
		PROTECT(value);						\
		value = eval(value, R_GlobalEnv);			\
		UNPROTECT(1);						\
	    }								\
	    SET_VECTOR_ELT(values, *indx, lazy_duplicate(value));	\
	    (*indx)++

	    DO_FrameValues;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame)) {
		DO_FrameValues;
	    }
	    frame = CDR(frame);
	}
    }
}
#undef DO_FrameValues
#undef NONEMPTY_

#define CHECK_HASH_TABLE(table) do {		\
	if (TYPEOF(table) != VECSXP)		\
	    error("%s", _("bad hash table contents"));	\
    } while (0)

static int HashTableSize(SEXP table, bool all)
{
    CHECK_HASH_TABLE(table);
    int count = 0;
    int n = length(table);
    for (int i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void HashTableNames(SEXP table, int all, SEXP names, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    for (int i = 0; i < n; i++)
	FrameNames(VECTOR_ELT(table, i), all, names, indx);
}

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    for (int i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

namespace
{
    bool BuiltinTest(const Symbol *sym, bool all, bool internal_only)
    {
        if (sym == R_NilValue)
            return false;

        if (internal_only)
            return (sym->internal() != R_NilValue);

        if ((all || !isDotSymbol(sym)) && (sym->value() != R_UnboundValue))
            return true;
        return false;
    }
} // anonymous namespace

static int BuiltinSize(bool all, bool internal_only)
{
    int count = 0;
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        const Symbol *sym = it->second;
        if (BuiltinTest(sym, all, internal_only))
            ++count;
    }
    return count;
}

static void BuiltinNames(bool all, bool internal_only, SEXP names, int *indx)
{
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        const Symbol *sym = it->second;
        if (BuiltinTest(sym, all, internal_only))
		    SET_STRING_ELT(names, (*indx)++, const_cast<String *>(sym->name()));
    }
}

static void BuiltinValues(bool all, bool internal_only, SEXP values, int *indx)
{
    GCStackRoot<> vl;
    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
    {
        const Symbol *sym = it->second;
        if (BuiltinTest(sym, all, internal_only)) {
		    vl = sym->value();
		    if (Promise::isA(vl)) {
			vl = Evaluator::evaluate(vl, Environment::base());
		    }
		    SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
        }
    }
}

/**
 * @example .Internal(ls(envir, all.names, sorted))
 */
attribute_hidden SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(CAR(args)));
	return(tb->objects(tb));
    }

    SEXP env = CAR(args);

    /* if (env == R_BaseNamespace) env = R_BaseEnv; */

    bool all = asLogicalNAFalse(CADR(args));

    bool sort_nms = asLogicalNAFalse(CADDR(args)); /* sorted = TRUE/FALSE */

    return R_lsInternal3(env, (Rboolean) all, (Rboolean) sort_nms);
}

/** @brief Takes an environment, a boolean indicating whether to get all
 * names and a boolean if sorted is desired
 */
// In Rinternals.h
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    if (IS_USER_DATABASE(env)) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(env));
	return(tb->objects(tb));
    }

    /* Step 1 : Compute the Vector Size */
    int k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k += BuiltinSize(all, 0);
    else if (isEnvironment(env) ||
	isEnvironment(env = simple_as_environment(env, true))) {
	if (HASHTAB(env) != R_NilValue)
	    k += HashTableSize(HASHTAB(env), all);
	else
	    k += FrameSize(FRAME(env), all);
    }
    else
	error(_("invalid '%s' argument"), "envir");

    /* Step 2 : Allocate and Fill the Result */
    SEXP ans = PROTECT(allocVector(STRSXP, k));
    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, ans, &k);
    else if (isEnvironment(env)) {
	if (HASHTAB(env) != R_NilValue)
	    HashTableNames(HASHTAB(env), all, ans, &k);
	else
	    FrameNames(FRAME(env), all, ans, &k);
    }

    if (sorted) sortVector(ans, FALSE);
    UNPROTECT(1);
    return ans;
}

/* non-API version used in several packages */
// in Rinternals.h
SEXP R_lsInternal(SEXP env, Rboolean all)
{
    return R_lsInternal3(env, all, TRUE);
}

/** @brief transform an environment into a named list
 * 
 * @example as.list.environment(.)
 */
attribute_hidden SEXP do_env2list(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names;
    int k;

    checkArity(op, args);

    env = CAR(args);
    if (ISNULL(env))
	error("%s", _("use of NULL environment is defunct"));
    if (!isEnvironment(env)) {
	SEXP xdata;
	if (IS_S4_OBJECT(env) && TYPEOF(env) == OBJSXP &&
	    (xdata = R_getS4DataSlot(env, ENVSXP)) != R_NilValue)
	    env = xdata;
	else
	    error(_("'%s' must be an environment"), "x");
    }

    bool all = asLogicalNAFalse(CADR(args)); /* all.names = TRUE/FALSE */

    bool sort_nms = asLogicalNAFalse(CADDR(args)); /* sorted = TRUE/FALSE */

    // k := length(env) = envxlength(env) :
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(names = allocVector(STRSXP, k));
    PROTECT(ans = allocVector(VECSXP, k));

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, ans, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, ans, &k);
    else
	FrameValues(FRAME(env), all, ans, &k);

    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinNames(all, 0, names, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableNames(HASHTAB(env), all, names, &k);
    else
	FrameNames(FRAME(env), all, names, &k);

    if (k == 0) { // no sorting, keep NULL names
	UNPROTECT(2);
	return(ans);
    }
    if (sort_nms) {
	// return list with *sorted* names
	SEXP sind = PROTECT(allocVector(INTSXP, k));
	int *indx = INTEGER(sind);
	for (int i = 0; i < k; i++) indx[i] = i;
	orderVector1(indx, k, names, /* nalast */ true, /* decreasing */ false,
		     R_NilValue);
	SEXP ans2   = PROTECT(allocVector(VECSXP, k));
	SEXP names2 = PROTECT(allocVector(STRSXP, k));
	for (int i = 0; i < k; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]));
	    SET_VECTOR_ELT(ans2,   i, VECTOR_ELT(ans,   indx[i]));
	}
	setAttrib(ans2, R_NamesSymbol, names2);
	UNPROTECT(5);
	return(ans2);
    }
    else {
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(2);
	return(ans);
    }
}

/** @brief apply a function to all objects in an environment
 * 
 * @return return the results in a list.
 * 
 * @example lapply(as.list(env, all.names=all.names), FUN, ...)
 * 
 * @note This is a special .Internal
 */
attribute_hidden SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, R_fcall, FUN, tmp, tmp2, ind;
    int k, k2;

    checkArity(op, args);

    PROTECT(env = eval(CAR(args), rho));
    if (ISNULL(env))
	error("%s", _("use of NULL environment is defunct"));
    if (!isEnvironment(env))
	error(_("'%s' must be an environment"), "env");

    FUN = CADR(args);
    if (!isSymbol(FUN))
	error("%s", _("arguments must be symbolic"));

    /* 'all.names' : */
    bool all = asLogicalNAFalse(PROTECT(eval(CADDR(args), rho)));
    UNPROTECT(1);

    /* 'USE.NAMES' : */
    bool useNms = asLogicalNAFalse(PROTECT(eval(CADDDR(args), rho)));
    UNPROTECT(1);

    if (env == R_BaseEnv || env == R_BaseNamespace)
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(ans  = allocVector(VECSXP, k));
    PROTECT(tmp2 = allocVector(VECSXP, k));

    k2 = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	BuiltinValues(all, 0, tmp2, &k2);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, tmp2, &k2);
    else
	FrameValues(FRAME(env), all, tmp2, &k2);

    SEXP Xsym = install("X");
    SEXP isym = install("i");
    PROTECT(ind = allocVector(INTSXP, 1));
    /* tmp :=  `[`(<elist>, i) */
    PROTECT(tmp = LCONS(R_Bracket2Symbol,
			CONS(Xsym, CONS(isym, R_NilValue))));
    /* fcall :=  <FUN>( tmp, ... ) */
    PROTECT(R_fcall = LCONS(FUN, CONS(tmp, CONS(R_DotsSymbol, R_NilValue))));

    defineVar(Xsym, tmp2, rho);
    INCREMENT_NAMED(tmp2);
    defineVar(isym, ind, rho);
    INCREMENT_NAMED(ind);

    for (int i = 0; i < k2; i++) {
	INTEGER(ind)[0] = i+1;
	SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(tmp))
	    tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    if (useNms) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, k));
	k = 0;
	if (env == R_BaseEnv || env == R_BaseNamespace)
	    BuiltinNames(all, 0, names, &k);
	else if (HASHTAB(env) != R_NilValue)
	    HashTableNames(HASHTAB(env), all, names, &k);
	else
	    FrameNames(FRAME(env), all, names, &k);

	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(6);
    return(ans);
}

/* Both leak out via inlining in ../library/tools/src/ */
int R::Rf_envlength(SEXP rho)
{
    if (IS_USER_DATABASE(rho)) {
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(rho));
	return length(tb->objects(tb));
    } else if (HASHTAB(rho) != R_NilValue)
	return HashTableSize(HASHTAB(rho), 1);
    else if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return BuiltinSize(1, 0);
    else
	return FrameSize(FRAME(rho), 1);
}

R_xlen_t R::Rf_envxlength(SEXP rho)
{
    if (IS_USER_DATABASE(rho)) {
	R_ObjectTable *tb = (R_ObjectTable*) R_ExternalPtrAddr(HASHTAB(rho));
	return xlength(tb->objects(tb));
    } else if (HASHTAB(rho) != R_NilValue)
	return HashTableSize(HASHTAB(rho), 1);
    else if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return BuiltinSize(1, 0);
    else
	return FrameSize(FRAME(rho), 1);
}

/**
 * @return Return the names of all the built in functions.  These are fetched
 * directly from the symbol table.
 */
attribute_hidden SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> ans;
    checkArity(op, args);
    bool intern = asLogicalNAFalse(CAR(args));
    int nelts = BuiltinSize(true, intern);
    ans = allocVector(STRSXP, nelts);
    nelts = 0;
    BuiltinNames(1, intern, ans, &nelts);
    sortVector(ans, true);
    return ans;
}

/**
 * This function returns the environment at a specified position in the
 * search path or the environment of the caller of
 * pos.to.env (? but pos.to.env is usually used in arg lists and hence
 * is evaluated in the calling environment so this is one higher).
 *
 * When pos = -1 the environment of the closure that pos2env is
 * evaluated in is obtained. Note: this relies on pos.to.env being
 * a primitive.
 */
static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;

    if (pos == NA_INTEGER || pos < -1 || pos == 0) {
	errorcall(call, _("invalid '%s' argument"), "pos");
	env = call;/* just for -Wall */
    }
    else if (pos == -1) {
	/* make sure the context is a funcall */
	RCNTXT *cptr = R_GlobalContext;
	while(cptr && !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext
	       != NULL )
	    cptr = cptr->nextcontext;
	if (!(cptr->callflag & CTXT_FUNCTION))
	    errorcall(call, "%s", _("no enclosing environment"));

	env = cptr->sysparent;
	if (R_GlobalEnv != R_NilValue && env == R_NilValue)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    return env;
}

/**
 * @note this is primitive
 */
attribute_hidden SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int npos;
    checkArity(op, args);
    check1arg(args, call, "x");

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
	errorcall(call, _("invalid '%s' argument"), "pos");
    if (npos == 1)
	env = pos2env(INTEGER(pos)[0], call);
    else {
	PROTECT(env = allocVector(VECSXP, npos));
	for (int i = 0; i < npos; i++) {
	    SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
	}
	UNPROTECT(1); /* env */
    }
    UNPROTECT(1); /* pos */
    return env;
}

static SEXP matchEnvir(SEXP call, const char *what)
{
    SEXP name;
    CXXR::RAllocStack::Scope rscope;
    if (streql(".GlobalEnv", what))
	return R_GlobalEnv;
    if (streql("package:base", what))
	return R_BaseEnv;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if (isString(name) && length(name) > 0 &&
	   streql(translateChar(STRING_ELT(name, 0)), what)) {
	    return t;
	}
    }
    errorcall(call, _("no item called \"%s\" on the search list"), what);
    /* not reached */
    return R_NilValue;
}

/**
 * @note this is primitive
 */
attribute_hidden SEXP do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = CAR(args), ans;
    checkArity(op, args);
    check1arg(args, call, "x");
    if (isEnvironment(arg))
	return arg;
    /* DispatchOrEval internal generic: as.environment */
    if (isObject(arg) &&
       DispatchOrEval(call, op, "as.environment", args, rho, &ans, 0, 1))
	return ans;
    switch (TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, translateChar(asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, args, rho);
    case NILSXP:
	errorcall(call, "%s", _("using 'as.environment(NULL)' is defunct"));
	return R_BaseEnv;	/* -Wall */
    case OBJSXP: {
	/* dispatch was tried above already */
	SEXP dot_xData = R_getS4DataSlot(arg, ENVSXP);
	if (!isEnvironment(dot_xData))
	    errorcall(call, "%s", _("S4 object does not extend class \"environment\""));
	else
	    return(dot_xData);
    }
    case VECSXP: {
	/* implement as.environment.list() {isObject(.) is false for a list} */
	SEXP call, val;
	PROTECT(call = lang4(install("list2env"), arg,
			     /* envir = */R_NilValue,
			     /* parent = */R_EmptyEnv));
	val = eval(call, rho);
	UNPROTECT(1);
	return val;
    }
    default:
	errorcall(call, "%s", _("invalid object for 'as.environment'"));
	return R_NilValue;	/* -Wall */
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    if (IS_S4_OBJECT(env) && (TYPEOF(env) == OBJSXP))
	env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	if (bindings) {
	    for (Symbol::const_iterator it = Symbol::begin(); it != Symbol::end(); ++it)
	    {
		    const SEXP sym = it->second;
		    if (SYMVALUE(sym) != R_UnboundValue)
			LOCK_BINDING(sym);
	    }
	}
	LOCK_FRAME(env);
	return;
    }

    if (TYPEOF(env) != ENVSXP)
	error("%s", _("not an environment"));
    if (bindings) {
	if (IS_HASHED(env)) {
	    SEXP table = HASHTAB(env);
	    int size = HASHSIZE(table);
	    for (int i = 0; i < size; i++)
		for (SEXP chain = VECTOR_ELT(table, i);
		     chain != R_NilValue;
		     chain = CDR(chain))
		    LOCK_BINDING(chain);
	}
	else {
	    for (SEXP frame = FRAME(env); frame != R_NilValue; frame = CDR(frame))
		LOCK_BINDING(frame);
	}
    }
    LOCK_FRAME(env);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    return (Rboolean) (FRAME_IS_LOCKED(env) != 0);
}

attribute_hidden SEXP do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP frame = CAR(args);
    Rboolean bindings = (Rboolean) asRbool(CADR(args), call);
    R_LockEnvironment(frame, bindings);
    return R_NilValue;
}

attribute_hidden SEXP do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogical(R_EnvironmentIsLocked(CAR(args)));
}

void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	LOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	LOCK_BINDING(binding);
    }
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	UNLOCK_BINDING(sym);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	UNLOCK_BINDING(binding);
    }
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    if (!FunctionBase::isA(fun))
	error("%s", _("not a function"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace) {
	if (SYMVALUE(sym) != R_UnboundValue && !IS_ACTIVE_BINDING(sym))
	    error("%s", _("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(sym))
	    error("%s", _("cannot change active binding if binding is locked"));
	SET_SYMVALUE(sym, fun);
	SET_ACTIVE_BINDING_BIT(sym);
	/* we don't need to worry about the global cache here as
	   a regular binding cannot be changed */
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue) {
	    defineVar(sym, fun, env); /* fails if env is locked */
	    binding = findVarLocInFrame(env, sym, NULL);
	    SET_ACTIVE_BINDING_BIT(binding);
	}
	else if (!IS_ACTIVE_BINDING(binding))
	    error("%s", _("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(binding))
	    error("%s", _("cannot change active binding if binding is locked"));
	else
	    SETCAR(binding, fun);
    }
}

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return (Rboolean) (BINDING_IS_LOCKED(sym) != 0);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	return (Rboolean) (BINDING_IS_LOCKED(binding) != 0);
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));
    if (env == R_BaseEnv || env == R_BaseNamespace)
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return (Rboolean) (IS_ACTIVE_BINDING(sym) != 0);
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	return (Rboolean) (IS_ACTIVE_BINDING(binding) != 0);
    }
}

attribute_hidden bool R::R_HasFancyBindings(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table = HASHTAB(rho);
	int size = HASHSIZE(table);
	for (int i = 0; i < size; i++)
	    for (SEXP chain = VECTOR_ELT(table, i);
		 chain != R_NilValue;
		 chain = CDR(chain))
		if (IS_ACTIVE_BINDING(chain) || BINDING_IS_LOCKED(chain))
		    return TRUE;
	return FALSE;
    }
    else {
	for (SEXP frame = FRAME(rho); frame != R_NilValue; frame = CDR(frame))
	    if (IS_ACTIVE_BINDING(frame) || BINDING_IS_LOCKED(frame))
		return TRUE;
	return FALSE;
    }
}

SEXP R_ActiveBindingFunction(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error("%s", _("not a symbol"));
    env = simple_as_environment(env);
    if (env == R_NilValue)
	error("%s", _("not an environment"));

    if (env == R_BaseEnv || env == R_BaseNamespace) {
	SEXP val = SYMVALUE(sym);
	if (val == R_UnboundValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	if (!IS_ACTIVE_BINDING(sym))
	    error(_("no active binding for \"%s\""),
		  EncodeChar(PRINTNAME(sym)));
	return val;
    }
    else {
	SEXP binding = findVarLocInFrame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), EncodeChar(PRINTNAME(sym)));
	if (!IS_ACTIVE_BINDING(binding))
	    error(_("no active binding for \"%s\""),
		  EncodeChar(PRINTNAME(sym)));
	return CAR(binding);
    }
}

attribute_hidden SEXP do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    switch (PRIMVAL(op)) {
    case 0:
	R_LockBinding(sym, env);
	break;
    case 1:
	R_unLockBinding(sym, env);
	break;
    default:
	error("%s", _("unknown op"));
    }
    return R_NilValue;
}

attribute_hidden SEXP do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP sym = CAR(args);
    SEXP env = CADR(args);
    return ScalarLogical(R_BindingIsLocked(sym, env));
}

attribute_hidden SEXP do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP sym = CAR(args);
    SEXP fun = CADR(args);
    SEXP env = CADDR(args);
    R_MakeActiveBinding(sym, fun, env);
    return R_NilValue;
}

attribute_hidden SEXP do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP sym = CAR(args);
    SEXP env = CADR(args);
    return ScalarLogical(R_BindingIsActive(sym, env));
}

attribute_hidden SEXP do_activeBndFun(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP sym = CAR(args);
    SEXP env = CADR(args);
    return R_ActiveBindingFunction(sym, env);
}

/* This is a .Internal with no wrapper */
attribute_hidden SEXP do_mkUnbound(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym;
    checkArity(op, args);
    sym = CAR(args);

    if (TYPEOF(sym) != SYMSXP) error("%s", _("not a symbol"));
    /* This is not quite the same as SET_SYMBOL_BINDING_VALUE as it
       does not allow active bindings to be unbound */
    if (FRAME_IS_LOCKED(R_BaseEnv))
	error("%s", _("cannot remove bindings from a locked environment"));
    if (R_BindingIsLocked(sym, R_BaseEnv))
	error("%s", _("cannot unbind a locked binding"));
    if (R_BindingIsActive(sym, R_BaseEnv))
	error("%s", _("cannot unbind an active binding"));
    SET_SYMVALUE(sym, R_UnboundValue);
#ifdef USE_GLOBAL_CACHE
    R_FlushGlobalCache(sym);
#endif
    return R_NilValue;
}

/* C version of new.env */
SEXP R_NewEnv(SEXP enclos, int hash, int size)
{
    if (hash)
	return R_NewHashedEnv(enclos, size);
    else
	return NewEnvironment(R_NilValue, R_NilValue, enclos);
}

attribute_hidden void R::R_RestoreHashCount(SEXP rho)
{
    if (IS_HASHED(rho)) {
	int count = 0;

	SEXP table = HASHTAB(rho);
	int size = HASHSIZE(table);
	for (int i = 0; i < size; i++)
	    if (VECTOR_ELT(table, i) != R_NilValue)
		count++;
	SET_HASHPRI(table, count);
    }
}

Rboolean R_IsPackageEnv(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if (isString(name) && length(name) > 0 &&
	   streqln(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return TRUE;
	else
	    return FALSE;
    }
    else
	return FALSE;
}

SEXP R_PackageEnvName(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	const char *packprefix = "package:";
	size_t pplen = strlen(packprefix);
	if (isString(name) && length(name) > 0 &&
	   streqln(packprefix, CHAR(STRING_ELT(name, 0)), pplen)) /* ASCII */
	    return name;
	else
	    return R_NilValue;
    }
    else
	return R_NilValue;
}

attribute_hidden SEXP R::R_FindPackageEnv(SEXP info)
{
    SEXP expr, val;
    PROTECT(info);
    SEXP s_findPackageEnv = install("findPackageEnv");
    PROTECT(expr = LCONS(s_findPackageEnv, CONS(info, R_NilValue)));
    val = eval(expr, R_BaseEnv);
    UNPROTECT(2);
    return val;
}

Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return TRUE;
    else if (TYPEOF(rho) == ENVSXP) {
	GCStackRoot<> info;
	info = R_findVarInFrame(rho, R_NamespaceSymbol);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    SEXP spec = R_findVarInFrame(info, install("spec"));
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return TRUE;
	    else
		return FALSE;
	}
	else return FALSE;
    }
    else return FALSE;
}

attribute_hidden SEXP do_isNSEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_IsNamespaceEnv(CAR(args)) ? mkTrue() : mkFalse();
}

SEXP R_NamespaceEnvSpec(SEXP rho)
{
    /* The namespace spec is a character vector that specifies the
       namespace.  The first element is the namespace name.  The
       second element, if present, is the namespace version.  Further
       elements may be added later. */
    if (rho == R_BaseNamespace)
	return R_BaseNamespaceName;
    else if (TYPEOF(rho) == ENVSXP) {
	SEXP info = R_findVarInFrame(rho, R_NamespaceSymbol);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
	    PROTECT(info);
	    SEXP spec = R_findVarInFrame(info, install("spec"));
	    UNPROTECT(1);
	    if (spec != R_UnboundValue &&
		TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
		return spec;
	    else
		return R_NilValue;
	}
	else return R_NilValue;
    }
    else return R_NilValue;
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP expr, val;
    PROTECT(info);
    SEXP s_getNamespace = install("getNamespace");
    PROTECT(expr = LCONS(s_getNamespace, CONS(info, R_NilValue)));
    val = eval(expr, R_BaseEnv);
    UNPROTECT(2);
    return val;
}

static SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
	break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = installTrChar(STRING_ELT(name, 0));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, "%s", _("bad namespace name"));
    }
    return name;
}

// .Internal(registerNamespace(name, env))
attribute_hidden SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP name = checkNSname(call, CAR(args));
    SEXP val = CADR(args);
    if (R_findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
	errorcall(call, "%s", _("namespace already registered"));
    defineVar(name, val, R_NamespaceRegistry);
    return R_NilValue;
}

// .Internal(unregisterNamespace(nsname))
attribute_hidden SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name;
    int hashcode;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    if (R_findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
	errorcall(call, "%s", _("namespace not registered"));
    if (!HASHASH(PRINTNAME(name)))
	hashcode = R_Newhashpjw(CHAR(PRINTNAME(name)));
    else
	hashcode = HASHVALUE(PRINTNAME(name));
    RemoveVariable(name, hashcode, R_NamespaceRegistry);
    return R_NilValue;
}

// .Internal(getRegisteredNamespace(name))  ==  .getNamespace(name)
// .Internal(isRegisteredNamespace (name))  ==  isNamespaceLoaded(name)
attribute_hidden SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, PROTECT(coerceVector(CAR(args), SYMSXP)));
    UNPROTECT(1);
    val = R_findVarInFrame(R_NamespaceRegistry, name);

    switch (PRIMVAL(op)) {
    case 0: // get..()
	if (val == R_UnboundValue)
	    return R_NilValue;
	else
	    return val;
    case 1: // is..()
	return ScalarLogical(val == R_UnboundValue ? FALSE : TRUE);

    default: error("%s", _("unknown op"));
    }
    return R_NilValue; // -Wall
}

// .Internal(getNamespaceRegistry())
attribute_hidden SEXP do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_NamespaceRegistry;
}

static SEXP getVarValInFrame(SEXP rho, SEXP sym, int unbound_ok)
{
    SEXP val = R_findVarInFrame(rho, sym);
    if (!unbound_ok && val == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(val) == PROMSXP) {
	PROTECT(val);
	val = eval(val, R_EmptyEnv);
	UNPROTECT(1);
    }
    return val;
}

static SEXP checkVarName(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP: break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = installTrChar(STRING_ELT(name, 0));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, "%s", _("bad variable name"));
    }
    return name;
}

static SEXP callR1(SEXP fun, SEXP arg)
{
    static SEXP R_xSymbol = NULL;
    if (R_xSymbol == NULL)
	R_xSymbol = install("x");

    SEXP rho = PROTECT(NewEnvironment(R_NilValue, R_NilValue, R_BaseNamespace));
    defineVar(R_xSymbol, arg, rho);
    SEXP expr = PROTECT(lang2(fun, R_xSymbol));
    SEXP val = eval(expr, rho);
    /**** ideally this should clear out rho if it isn't captured - LT */
    UNPROTECT(2); /* rho, expr */
    return val;
}

attribute_hidden SEXP R_getNSValue(SEXP call, SEXP ns, SEXP name, bool exported)
{
    static SEXP R_loadNamespaceSymbol = NULL;
    static SEXP R_exportsSymbol = NULL;
    static SEXP R_lazydataSymbol = NULL;
    static SEXP R_getNamespaceNameSymbol = NULL;
    if (R_loadNamespaceSymbol == NULL) {
	R_loadNamespaceSymbol = install("loadNamespace");
	R_exportsSymbol = install("exports");
	R_lazydataSymbol = install("lazydata");
	R_getNamespaceNameSymbol = install("getNamespaceName");
    }

    if (R_IsNamespaceEnv(ns))
	PROTECT(ns);
    else {
	SEXP pkg = checkNSname(call, ns);
	ns = R_findVarInFrame(R_NamespaceRegistry, pkg);
	if (ns == R_UnboundValue)
	    ns = callR1(R_loadNamespaceSymbol, pkg);
	PROTECT(ns);
	if (!R_IsNamespaceEnv(ns))
	    errorcall(call, "%s", _("bad namespace"));
    }

    name = checkVarName(call, name);

    SEXP val;

    /* base or non-exported variables */
    if (ns == R_BaseNamespace || !exported) {
	val = getVarValInFrame(ns, name, FALSE);
	UNPROTECT(1); /* ns */
	return val;
    }

    /* exported variables */
    SEXP info = PROTECT(getVarValInFrame(ns, R_NamespaceSymbol, FALSE));
    SEXP exports = PROTECT(getVarValInFrame(info, R_exportsSymbol, FALSE));
    SEXP exportName = PROTECT(getVarValInFrame(exports, name, TRUE));
    if (exportName != R_UnboundValue) {
	val = eval(checkVarName(call, exportName), ns);
	UNPROTECT(4);  /* ns, info, exports, exportName */
	return val;
    }

    /* lazydata */
    SEXP ld = PROTECT(getVarValInFrame(info, R_lazydataSymbol, FALSE));
    val = getVarValInFrame(ld, name, TRUE);
    if (val != R_UnboundValue) {
	UNPROTECT(5); /* ns, info, exports, exportName, ld */
	return val;
    }

    SEXP nsname = PROTECT(callR1(R_getNamespaceNameSymbol, ns));
    if (TYPEOF(nsname) != STRSXP || LENGTH(nsname) != 1)
	errorcall(call, "%s", _("bad value returned by 'getNamespaceName'"));
    errorcall_cpy(call,
		  _("'%s' is not an exported object from 'namespace:%s'"),
		  EncodeChar(PRINTNAME(name)),
		  CHAR(STRING_ELT(nsname, 0)));
    return NULL; /* not reached */
}

attribute_hidden SEXP do_getNSValue(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ns = CAR(args);
    SEXP name = CADR(args);
    bool exported = asLogical(CADDR(args));

    return R_getNSValue(R_NilValue, ns, name, exported);
}

attribute_hidden
SEXP do_colon2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    /* use R_NilValue for the call to avoid changing the error message */
    return R_getNSValue(R_NilValue, CAR(args), CADR(args), TRUE);
}

attribute_hidden
SEXP do_colon3(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_getNSValue(call, CAR(args), CADR(args), FALSE);
}

attribute_hidden SEXP do_importIntoEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* This function copies values of variables from one environment
       to another environment, possibly with different names.
       Promises are not forced and active bindings are preserved. */
    SEXP impenv, impnames, expenv, expnames;
    SEXP impsym, expsym, val;

    checkArity(op, args);

    impenv = CAR(args); args = CDR(args);
    impnames = CAR(args); args = CDR(args);
    expenv = CAR(args); args = CDR(args);
    expnames = CAR(args); args = CDR(args);

    impenv = simple_as_environment(impenv);
    if (impenv == R_NilValue)
	error("%s", _("bad import environment argument"));
    expenv = simple_as_environment(expenv);
    if (expenv == R_NilValue)
	error("%s", _("bad export environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
	error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
	error("%s", _("length of import and export names must match"));

    int n = LENGTH(impnames);
    for (int i = 0; i < n; i++) {
	impsym = installTrChar(STRING_ELT(impnames, i));
	expsym = installTrChar(STRING_ELT(expnames, i));

	/* find the binding--may be a CONS cell or a symbol */
	SEXP binding = R_NilValue;
	for (SEXP env = expenv;
	     env != R_EmptyEnv && binding == R_NilValue;
	     env = ENCLOS(env))
	    if (env == R_BaseNamespace) {
		if (SYMVALUE(expsym) != R_UnboundValue)
		    binding = expsym;
	    } else
		binding = findVarLocInFrame(env, expsym, NULL);
	if (binding == R_NilValue)
	    binding = expsym;

	/* get value of the binding; do not force promises */
	if (TYPEOF(binding) == SYMSXP) {
	    if (SYMVALUE(expsym) == R_UnboundValue)
		error(_("exported symbol '%s' has no value"),
		      CHAR(PRINTNAME(expsym)));
	    val = SYMVALUE(expsym);
	}
	else val = CAR(binding);

	/* import the binding */
	if (IS_ACTIVE_BINDING(binding))
	    R_MakeActiveBinding(impsym, val, impenv);
	/* This is just a tiny optimization */
	else if (impenv == R_BaseNamespace || impenv == R_BaseEnv)
	    gsetVar(impsym, val, impenv);
	else
	    defineVar(impsym, val, impenv);
    }
    return R_NilValue;
}


attribute_hidden SEXP do_envprofile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Return a list containing profiling information given a hashed
       environment.  For non-hashed environments, this function
       returns R_NilValue.  This seems appropriate since there is no
       way to test whether an environment is hashed at the R level.
    */
    checkArity(op, args);
    SEXP env, ans = R_NilValue /* -Wall */;
    env = CAR(args);
    if (isEnvironment(env)) {
	if (IS_HASHED(env))
	    ans = R_HashProfile(HASHTAB(env));
    } else
	error("%s", _("argument must be a hashed environment"));
    return ans;
}

SEXP Rf_mkCharCE(const char *name, cetype_t enc)
{
    return String::obtain(name, enc);
}

/* no longer used in R but documented in 2.7.x */
SEXP Rf_mkCharLen(const char *name, int len)
{
    return String::obtain(name, len, CE_NATIVE);
}

/** @brief Make a character (CHARSXP) variable.
 * 
 * @param name character string to be used when creating character variable
 * 
 * @return CHARXSP object
 * 
 * @note See Rinlinedfuns.h
 */
SEXP Rf_mkChar(const char *name)
{
    return String::obtain(name);
}

attribute_hidden SEXP R::mkCharWUTF8(const wchar_t *wname)
{
    return String::obtain(wname);
}

attribute_hidden void R::InitStringHash(void)
{
}

namespace
{
    void reportInvalidString(SEXP cval, int actionWhenInvalid)
    {
        int oldout = R_OutputCon;
        int olderr = R_ErrorCon;
        R_OutputCon = 2;
        R_ErrorCon = 2;
        REprintf(" ----------- FAILURE REPORT -------------- \n");
        REprintf(" --- failure: %s ---\n", "invalid string was created");
        REprintf(" --- srcref --- \n");
        SrcrefPrompt("", R_getCurrentSrcref());
        REprintf("\n");
        REprintf(" --- call from context --- \n");
        PrintValue(R_GlobalContext->call);
        REprintf(" --- R stacktrace ---\n");
        printwhere();
        REprintf(" --- current native encoding: %s ---\n", R_nativeEncoding());
        const char *enc = "native/unknown";
        if (IS_LATIN1(cval))
            enc = "latin1";
        else if (IS_UTF8(cval))
            enc = "UTF-8";
        else if (IS_BYTES(cval))
            enc = "bytes";  // called in error
        REprintf(" --- declared string encoding: %s ---\n", enc);
        REprintf(" --- string (printed):\n");
        PrintValue(cval);
        REprintf(" --- string (bytes with ASCII chars):\n");
        for (int i = 0; i < LENGTH(cval); i++) {
            if (i > 0)
                REprintf(" ");
            unsigned char b = (unsigned char)CHAR(cval)[i];
            REprintf("%2x", b);
            if (b > 0 && b <= 127) REprintf("(%c) ", b);
        }
        REprintf("\n");
        REprintf(" --- function from context --- \n");
        if (R_GlobalContext->callfun != NULL && Closure::isA(R_GlobalContext->callfun))
            PrintValue(R_GlobalContext->callfun);
        REprintf(" --- function search by body ---\n");
        if (R_GlobalContext->callfun != NULL && Closure::isA(R_GlobalContext->callfun))
            findFunctionForBody(R_ClosureExpr(R_GlobalContext->callfun));
        REprintf(" ----------- END OF FAILURE REPORT -------------- \n");
        R_OutputCon = oldout;
        R_ErrorCon = olderr;

        if (actionWhenInvalid == 3)
        {
            R_Suicide(_("invalid string was created"));
        }
        else if (actionWhenInvalid > 0) {
            CXXR::RAllocStack::Scope rscope;
            const char *native_str;
            const char *from = "";
            if (IS_UTF8(cval))
                from = "UTF-8";
            else if (IS_LATIN1(cval))
                from = "CP1252";

            native_str = reEnc3(CHAR(cval), from, "", 1);
            if (actionWhenInvalid == 1)
                warning(_("invalid string %s"), native_str);
            else if (actionWhenInvalid == 2)
                error(_("invalid string %s"), native_str);
        }
    }

    void handleEmbeddedNull(const std::string &name, cetype_t enc, bool is_ascii)
    {
    /* This is tricky: we want to make a reasonable job of
       representing this string, and EncodeString() is the most
       comprehensive */
        SEXP c = CXXR_allocCharsxp(name, enc, is_ascii);
        error(_("embedded nul in string: '%s'"), EncodeString(c, 0, 0, Rprt_adj_none));
    }

    void validateString(SEXP cval)
    {
        static int checkValid = -1;
        static int actionWhenInvalid = 0;
        if (checkValid && !IS_ASCII(cval)) {
            if (checkValid == -1) {
                checkValid = 0;
                /* _R_CHECK_STRING_VALIDITY_ = XY (decimal)

                   Y = 0 ... no checks
                   Y = 1 ... check marked strings
                   Y = 2 ... check also native strings

                   X = 0 ... just print
                   X = 1 ... print + issue a warning
                   X = 2 ... print + throw R error
                   X = 3 ... print + abort R

                   This is experimental and will be likely changed or
                   removed.
                */
                const char *p = getenv("_R_CHECK_STRING_VALIDITY_");
                if (p) {
                    checkValid = atoi(p);
                    actionWhenInvalid = checkValid / 10;
                    checkValid -= actionWhenInvalid * 10;

                    if (checkValid < 0 || checkValid > 2) {
                        checkValid = 0;
                        actionWhenInvalid = 0;
                    }
                    if (actionWhenInvalid < 0 || actionWhenInvalid > 3)
                        actionWhenInvalid = 0;
                }
            }
            if (checkValid >= 1) {
            /* check strings flagged UTF-8 and latin1 */
                if (IS_UTF8(cval)) {
                    if (!utf8Valid(CHAR(cval)))
                        reportInvalidString(cval, actionWhenInvalid);
                    return;
                }
                else if (IS_LATIN1(cval)) {
                    CXXR::RAllocStack::Scope rscope;
                    const wchar_t *dummy = wtransChar2(cval);
                    if (!dummy)
                        reportInvalidString(cval, actionWhenInvalid);
                    return;
                }
            }
            if (checkValid >= 2 && !IS_BYTES(cval)) {
            /* check strings flagged native/unknown */
                if (known_to_be_utf8) {
                    if (!utf8Valid(CHAR(cval)))
                        reportInvalidString(cval, actionWhenInvalid);
                    return;
                }
                else if (!mbcsValid(CHAR(cval))) {
                    reportInvalidString(cval, actionWhenInvalid);
                    return;
                }
            }
        }
    }
} // anonymous namespace

String *String::obtain(const std::string &name, cetype_t enc)
{
    // These encodings are acceptable for lookup.
    // For insertion only the first 4 are considered valid (checked again later):
    cetype_t lookable_enc;
    switch (enc)
    {
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
        lookable_enc = enc;
        break;
    case CE_SYMBOL:
    case CE_ANY:
        lookable_enc = CE_NATIVE;
        break;
    default:
        error(_("unknown encoding: %d"), enc);
    }

    bool is_ascii = TRUE;

    for (char c : name)
    {
        if ((unsigned int)c > 127)
            is_ascii = FALSE;
        if (c == '\0')
        {
            handleEmbeddedNull(name, enc, is_ascii);
        }
    }

    if (is_ascii)
        enc = CE_NATIVE;

    auto found = s_hash_table.find(key(name, lookable_enc));
    if (found != s_hash_table.end())
    {
        return found->second;
    }

    /* no cached value; need to allocate one and add to the cache */
    String *value = String::create(name, enc, is_ascii);
    validateString(value);
    auto [it, inserted] = s_hash_table.emplace(map::value_type(key(name, enc), value));

    return it->second;
}

/* mkCharLenCE - make a character (CHARSXP) variable and set its
   encoding bit.  If a CHARSXP with the same string already exists in
   the global CHARSXP cache, String::s_hash_table, it is returned.  Otherwise,
   a new CHARSXP is created, added to the cache and then returned. */

SEXP Rf_mkCharLenCE(const char *name, int len, cetype_t enc)
{
    return String::obtain(name, len, enc);
}


#ifdef DEBUG_SHOW_CHARSXP_CACHE
/* Call this from gdb with

       call do_show_cache(10)

   for the first 10 cache chains in use. */
void do_show_cache(int n)
{
    Rprintf("Cache size: %zu\n", String::s_hash_table.size());
    Rprintf("Cache buckets:  %zu\n", String::s_hash_table.bucket_count());
    for (int i = 0; i < n && i < String::s_hash_table.bucket_count(); i++) {
	    Rprintf("# %d: ", i);
        for (auto it = String::s_hash_table.begin(i); it != String::s_hash_table.end(i); ++it)
            {
		if (IS_UTF8(it->second))
		    Rprintf("U");
		else if (IS_LATIN1(it->second))
		    Rprintf("L");
		else if (IS_BYTES(it->second))
		    Rprintf("B");
		Rprintf("|%s| ", CHAR(it->second));
	    }
	    Rprintf("\n");
    }
}

void do_write_cache()
{
    FILE *f = fopen("/tmp/CACHE", "w");
    if (f != NULL) {
	fprintf(f, "Cache size: %zu\n", String::s_hash_table.size());
	fprintf(f, "Cache buckets:  %zu\n", String::s_hash_table.bucket_count());
	for (int i = 0; i < String::s_hash_table.bucket_count(); i++) {
		fprintf(f, "# %d: ", i);
		for (auto it = String::s_hash_table.begin(i); it != String::s_hash_table.end(i); ++it)
		{
		    if (IS_UTF8(it->second))
			fprintf(f, "U");
		    else if (IS_LATIN1(it->second))
			fprintf(f, "L");
		    else if (IS_BYTES(it->second))
			fprintf(f, "B");
		    fprintf(f, "|%s| ", CHAR(it->second));
		}
		fprintf(f, "\n");
	}
	fclose(f);
    }
}
#endif /* DEBUG_SHOW_CHARSXP_CACHE */

// topenv

SEXP Rf_topenv(SEXP target, SEXP envir) {
    SEXP env = envir;
    while (env != R_EmptyEnv) {
	if (env == target || env == R_GlobalEnv ||
	    env == R_BaseEnv || env == R_BaseNamespace ||
	    R_IsPackageEnv(env) || R_IsNamespaceEnv(env) ||
	    R_existsVarInFrame(env, R_dot_packageName)) {
	    return env;
	} else {
	    env = ENCLOS(env);
	}
    }
    return R_GlobalEnv;
}

/** topenv():
 *
 * .Internal(topenv(envir, matchThisEnv))
 *
 * @return
 */
attribute_hidden SEXP do_topenv(SEXP call, SEXP op, SEXP args, SEXP rho) {
    checkArity(op, args);
    SEXP envir = CAR(args);
    SEXP target = CADR(args); // = matchThisEnv, typically NULL (R_NilValue)
    if (TYPEOF(envir) != ENVSXP) envir = rho; // envir = parent.frame()
    if (target != R_NilValue && TYPEOF(target) != ENVSXP)  target = R_NilValue;
    return topenv(target, envir);
}

attribute_hidden bool R::isUnmodifiedSpecSym(SEXP sym, SEXP env) {
    if (sym && !IS_SPECIAL_SYMBOL(sym))
	return false;
    while (env != R_EmptyEnv)
    {
	if (env && !NO_SPECIAL_SYMBOLS(env) && env != R_BaseEnv
		&& env != R_BaseNamespace && R_existsVarInFrame(env, sym))
	    return false;
        env = ENCLOS(env);
    }
    return true;
}

attribute_hidden
void findFunctionForBodyInNamespace(SEXP body, SEXP nsenv, SEXP nsname) {
    if (R_IsNamespaceEnv(nsenv) != TRUE)
	error("%s", _("argument 'nsenv' is not a namespace"));
    SEXP args = PROTECT(list3(nsenv /* x */,
	R_TrueValue /* all.names */,
	R_FalseValue /* sorted */));
    SEXP env2listOp = INTERNAL(install("env2list"));

    SEXP elist = do_env2list(R_NilValue, env2listOp, args, R_NilValue);
    PROTECT(elist);
    R_xlen_t n = xlength(elist);
    SEXP names = PROTECT(getAttrib(elist, R_NamesSymbol));
    for (R_xlen_t i = 0; i < n; i++) {
	SEXP value = VECTOR_ELT(elist, i);
	const char *vname = CHAR(STRING_ELT(names, i));
	/* the constants checking requires shallow comparison */
	if (TYPEOF(value) == CLOSXP && R_ClosureExpr(value) == body)
	    REprintf(_("Function %s in namespace %s has this body.\n"),
		vname,
		CHAR(PRINTNAME(nsname)));
	/* search S4 registry */
	const char *s4prefix = ".__T__";
	if (TYPEOF(value) == ENVSXP &&
		streqln(vname, s4prefix, strlen(s4prefix))) {
	    SETCAR(args, value); /* re-use args */
	    SEXP rlist = do_env2list(R_NilValue, env2listOp, args, R_NilValue);
	    PROTECT(rlist);
	    R_xlen_t rn = xlength(rlist);
	    SEXP rnames = PROTECT(getAttrib(rlist, R_NamesSymbol));
	    for (R_xlen_t ri = 0; ri < rn; ri++) {
		SEXP rvalue = VECTOR_ELT(rlist, ri);
		/* the constants checking requires shallow comparison */
		if (TYPEOF(rvalue) == CLOSXP &&
			R_ClosureExpr(rvalue) == body)
		    REprintf(_("S4 Method %s defined in namespace %s with signature %s has this body.\n"),
			vname + strlen(s4prefix),
			CHAR(PRINTNAME(nsname)),
			CHAR(STRING_ELT(rnames, ri)));
	    }
	    UNPROTECT(2); /* rlist, rnames */
	}
    }
    UNPROTECT(3); /* names, elist, args */
}

/** @brief For a given function body, try to find a closure and
 * the name of its binding (and the name of the package).
 * 
 * @note For debugging.
 */
attribute_hidden void R::findFunctionForBody(SEXP body) {
    SEXP nstable = HASHTAB(R_NamespaceRegistry);
    CHECK_HASH_TABLE(nstable);
    int n = length(nstable);
    for (int i = 0; i < n; i++) {
	SEXP frame = VECTOR_ELT(nstable, i);
	while (frame != R_NilValue) {
	    findFunctionForBodyInNamespace(body, CAR(frame), TAG(frame));
	    frame = CDR(frame);
	}
    }
}

