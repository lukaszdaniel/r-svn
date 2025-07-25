/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2025   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* <UTF8>
   Does byte-level handling in primitive_case, but only of ASCII chars
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Defn.h>
#include "localization.h"

#include "RSMethods.h"
#include "methods.h"
#include <Rinternals.h>

using namespace R;

#define STRING_VALUE(x)		CHAR(asChar(x))

#if !defined(snprintf) && defined(HAVE_DECL_SNPRINTF) && !HAVE_DECL_SNPRINTF
extern int snprintf(char *s, size_t n, const char *format, ...);
#endif

/* the following utilities are included here for now, as statics.  But
   they will eventually be C implementations of slot, data.class,
   etc. */

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, bool firstTry,
			bool evalArgs);
static SEXP R_loadMethod(SEXP f, SEXP fname, SEXP ev);
static SEXP R_selectByPackage(SEXP f, SEXP classes, int nargs);

/* objects, mostly symbols, that are initialized once to save a little time */
static bool initialized = 0;
static SEXP s_dot_Methods, s_skeleton, s_expression, s_function,
    s_getAllMethods, s_objectsEnv, s_MethodsListSelect,
    s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_generic,
    s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets,
    s_argument, s_allMethods, s_base;
static SEXP R_FALSE, R_TRUE;
static bool table_dispatch_on = true;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


SEXP R_quick_method_check(SEXP object, SEXP fsym, SEXP fdef);

static SEXP R_target, R_defined, R_nextMethod, R_dot_nextMethod,
    R_loadMethod_name, R_methods_name, R_tripleColon_name;

static SEXP Methods_Namespace = NULL;

static const char *check_single_string(SEXP, bool, const char *);
static const char *check_symbol_or_string(SEXP obj, bool nonEmpty,
                                          const char *what);
static const char *class_string(SEXP obj);

typedef struct {
    SEXP e;
    SEXP env;
} R_evalWrapper_t;

static SEXP evalWrapper(void *data_)
{
    R_evalWrapper_t *data = (R_evalWrapper_t *) data_;
    return eval(data->e, data->env);
}

/* Like R_withCallingErrorHandler but evaluates an R expression
   instead of a C callback */
static SEXP R_evalHandleError(SEXP e, SEXP env,
			      SEXP (*handler)(SEXP, void *), void *hdata)
{
    R_evalWrapper_t data = { .e = e, .env = env };
    return R_withCallingErrorHandler(&evalWrapper, &data, handler, hdata);
}

typedef struct {
    SEXP e;
    SEXP env;
    void (*finally)(void *);
    void *fdata;
} R_evalWrapperCleanup_t;

static SEXP evalWrapperCleanup(void *data_)
{
    R_evalWrapperCleanup_t *data = (R_evalWrapperCleanup_t *) data_;
    return R_ExecWithCleanup(&evalWrapper, data, data->finally, data->fdata);
}

static SEXP R_evalHandleErrorProtect(SEXP e, SEXP env,
				     SEXP (*handler)(SEXP, void *), void *hdata,
				     void (*finally)(void *), void *fdata)
{
    R_evalWrapperCleanup_t data = { .e = e, .env = env,
				    .finally = finally, .fdata = fdata};

    return R_withCallingErrorHandler(&evalWrapperCleanup, &data,
				     handler, hdata);
}

/* The result might need to be protected as the message might be
   generated in a method */
static SEXP R_conditionMessage(SEXP cond)
{
    SEXP call = PROTECT(lang2(install("conditionMessage"), cond));
    SEXP out = eval(call, R_BaseEnv);

    /* Type check return value so callers can safely extract a C string */
    if (TYPEOF(out) != STRSXP)
	error(_("unexpected type '%s' for condition message"),
	      R_typeToChar(out));
    if (length(out) != 1)
	error("%s", _("condition message must be length 1"));

    UNPROTECT(1);
    return out;
}

static void init_loadMethod(void)
{
    R_target = install("target");
    R_defined = install("defined");
    R_nextMethod = install("nextMethod");
    R_loadMethod_name = install("loadMethod");
    R_dot_nextMethod = install(".nextMethod");
    R_methods_name = install("methods");
    R_tripleColon_name = install(":::");
}


SEXP R_initMethodDispatch(SEXP envir)
{
    if(envir && !isNull(envir))
	Methods_Namespace = envir;
    if(!Methods_Namespace)
	Methods_Namespace = R_GlobalEnv;
    if(initialized)
	return(envir);

    s_dot_Methods = install(".Methods");
    s_skeleton = install("skeleton");
    s_expression = install("expression");
    s_function = install("function");
    s_getAllMethods = install("getAllMethods");
    s_objectsEnv = install("objectsEnv");
    s_MethodsListSelect = install("MethodsListSelect");
    s_sys_dot_frame = install("sys.frame");
    s_sys_dot_call = install("sys.call");
    s_sys_dot_function = install("sys.function");
    s_generic = install("generic");
    s_generic_dot_skeleton = install("generic.skeleton");
    s_subset_gets = install("[<-");
    s_element_gets = install("[[<-");
    s_argument = install("argument");
    s_allMethods = install("allMethods");

    R_FALSE = ScalarLogical(FALSE);
    R_PreserveObject(R_FALSE);
    R_TRUE = ScalarLogical(TRUE);
    R_PreserveObject(R_TRUE);

    /* some strings (NOT symbols) */
    s_missing = mkString("missing");
    setAttrib(s_missing, R_PackageSymbol, mkString("methods"));
    R_PreserveObject(s_missing);
    s_base = mkString("base");
    R_PreserveObject(s_base);
    /*  Initialize method dispatch, using the static */
    R_set_standardGeneric_ptr(
	(table_dispatch_on ? R_dispatchGeneric : R_standardGeneric)
	, Methods_Namespace);
    R_set_quick_method_check(
	(table_dispatch_on ? R_quick_dispatch : R_quick_method_check));

    /* Some special lists of primitive skeleton calls.
       These will be promises under lazy-loading.
    */
    PROTECT(R_short_skeletons =
	    findVar(install(".ShortPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_short_skeletons) == PROMSXP)
	R_short_skeletons = eval(R_short_skeletons, Methods_Namespace);
    R_PreserveObject(R_short_skeletons);
    UNPROTECT(1);
    PROTECT(R_empty_skeletons =
	    findVar(install(".EmptyPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_empty_skeletons) == PROMSXP)
	R_empty_skeletons = eval(R_empty_skeletons, Methods_Namespace);
    R_PreserveObject(R_empty_skeletons);
    UNPROTECT(1);
    if(R_short_skeletons == R_UnboundValue ||
       R_empty_skeletons == R_UnboundValue)
	error("%s", _("could not find the skeleton calls for 'methods' (package detached?): expect very bad things to happen"));
    f_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 0);
    fgets_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 1);
    f_x_skeleton = VECTOR_ELT(R_empty_skeletons, 0);
    fgets_x_skeleton = VECTOR_ELT(R_empty_skeletons, 1);
    init_loadMethod();
    initialized = 1;
    return(envir);
}


/* simplified version of do_subset2_dflt, with no partial matching */
static SEXP R_element_named(SEXP obj, const char * what)
{
    int offset = -1, n;
    SEXP names = getAttrib(obj, R_NamesSymbol);
    n = length(names);
    if(n > 0) {
	for (int i = 0; i < n; i++) {
	    if(streql(what, CHAR(STRING_ELT(names, i)))) {
		offset = i; break;
	    }
	}
    }
    if(offset < 0)
	return R_NilValue;
    else
	return VECTOR_ELT(obj, offset);
}

static SEXP R_insert_element(SEXP mlist, const char * what, SEXP object)
{
    SEXP sym = install(what);
    return R_subassign3_dflt(R_NilValue, mlist, sym, object);
}

SEXP R_el_named(SEXP object, SEXP what)
{
    const char * str;
    str = CHAR(asChar(what));
    return R_element_named(object, str);
}

SEXP R_set_el_named(SEXP object, SEXP what, SEXP value)
{
    const char *str = CHAR(asChar(what));
    return R_insert_element(object, str, value);
}

SEXP R_clear_method_selection(void)
{
    return R_NilValue;
}

static SEXP R_find_method(SEXP mlist, const char *class_, SEXP fname)
{
    /* find the element of the methods list that matches this class,
       but not including inheritance. */
    SEXP value, methods;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue) {
	error(_("no \"allMethods\" slot found in object of class \"%s\" used as methods list for function '%s'"),
	      class_string(mlist), CHAR(asChar(fname)));
	return(R_NilValue); /* -Wall */
    }
    value = R_element_named(methods, class_);
    return value;
}

SEXP R_quick_method_check(SEXP args, SEXP mlist, SEXP fdef)
{
    /* Match the list of args to the methods list. */
    SEXP object, methods, value, retValue = R_NilValue;
    const char *class_;

    if(!mlist)
	return R_NilValue;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue)
	return R_NilValue;
    while(!isNull(args) && !isNull(methods)) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP)
	    /* not observed during tests, but promises in principle could come
	       from DispatchOrEval/R_possible_dispatch */
	    object = eval(object, Methods_Namespace);
	PROTECT(object);
	class_ = CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	UNPROTECT(1); /* object */
	value = R_element_named(methods, class_);
	if(isNull(value) || isFunction(value)){
	    retValue = value;
	    break;
	}
	/* continue matching args down the tree */
	methods = R_do_slot(value, s_allMethods);
    }
    return(retValue);
}

SEXP R_quick_dispatch(SEXP args, SEXP genericEnv, SEXP fdef)
{
    /* Match the list of (possibly promised) args to the methods table. */
    static SEXP  R_allmtable = NULL, R_siglength;
    SEXP object, value, mtable;
    const char *class_; int nsig, nargs;
#define NBUF 200
    char buf[NBUF]; char *ptr;
    if(!R_allmtable) {
	R_allmtable = install(".AllMTable");
	R_siglength = install(".SigLength");
    }
    if(!genericEnv || TYPEOF(genericEnv) != ENVSXP)
	return R_NilValue; /* a bug or not initialized yet */
    mtable = findVarInFrame(genericEnv, R_allmtable);
    if(mtable == R_UnboundValue || TYPEOF(mtable) != ENVSXP)
	return R_NilValue;
    PROTECT(mtable);
    object = findVarInFrame(genericEnv, R_siglength);
    if(object == R_UnboundValue) {
	UNPROTECT(1); /* mtable */
	return R_NilValue;
    }
    switch(TYPEOF(object)) {
    case REALSXP:
	if(LENGTH(object) > 0)
	    nsig = (int) REAL(object)[0];
	else {
	    UNPROTECT(1); /* mtable */
	    return R_NilValue;
	}
	break;
    case INTSXP:
	if(LENGTH(object) > 0)
	    nsig = (int) INTEGER(object)[0];
	else {
	    UNPROTECT(1); /* mtable */
	    return R_NilValue;
	}
	break;
    default:
	UNPROTECT(1); /* mtable */
	return R_NilValue;
    }
    buf[0] = '\0'; ptr = buf;
    nargs = 0;
    while(!isNull(args) && nargs < nsig) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP)
	    object = eval(object, Methods_Namespace);
	if(object == R_MissingArg)
	    class_ = "missing";
	else {
	    PROTECT(object);
	    class_ = CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	    UNPROTECT(1); /* object */
	}
	if(ptr - buf + strlen(class_) + 2 > NBUF) {
	    UNPROTECT(1); /* mtable */
	    return R_NilValue;
	}
	/* NB:  this code replicates .SigLabel().
	   If that changes, e.g. to include
	   the package, the code here must change too.
	   Or, better, the two should use the same C code. */
	if(ptr > buf) { ptr = strcpy(ptr, "#");  ptr += 1;}
	ptr = strcpy(ptr, class_); ptr += strlen(class_);
	nargs++;
    }
    for(; nargs < nsig; nargs++) {
	if(ptr - buf + strlen("missing") + 2 > NBUF) {
	    UNPROTECT(1); /* mtable */
	    return R_NilValue;
	}
	ptr = strcpy(ptr, "#"); ptr +=1;
	ptr = strcpy(ptr, "missing"); ptr += strlen("missing");
    }
    value = findVarInFrame(mtable, install(buf));
    if(value == R_UnboundValue)
	value = R_NilValue;
    UNPROTECT(1); /* mtable */
    return(value);
}

/* call some S language functions */

static SEXP R_S_MethodsListSelectCleanup(SEXP err, void *data)
{
    SEXP fname = (SEXP) data;
    error(_("S language method selection did not return normally when called from internal dispatch for function '%s'"),
	  check_symbol_or_string(fname, true,
				 _("Function name for method selection called internally")));
    return R_NilValue;
}

static SEXP R_S_MethodsListSelect(SEXP fname, SEXP ev, SEXP mlist, SEXP f_env)
{
    SEXP e, val; int n;
    n = isNull(f_env) ? 4 : 5;
    PROTECT(e = allocVector(LANGSXP, n));
    SETCAR(e, s_MethodsListSelect);
    val = CDR(e);
    SETCAR(val, fname);
    val = CDR(val);
    SETCAR(val, ev);
    val = CDR(val);
    SETCAR(val, mlist);
    if(n == 5) {
	    val = CDR(val);
	    SETCAR(val, f_env);
    }
    val = R_evalHandleError(e, Methods_Namespace, &R_S_MethodsListSelectCleanup,
			    fname);
    UNPROTECT(1);
    return val;
}


/* quick tests for generic and non-generic functions.  May mistakenly
   identify non-generics as generics:  a class with data part of type
   CLOSXP and with a slot/attribute named "generic" will qualify.
*/
#define IS_NON_GENERIC(vl) (Rf_isPrimitive(vl) || \
            (TYPEOF(vl) == CLOSXP && getAttrib(vl, s_generic) == R_NilValue))
#define IS_GENERIC(vl) (TYPEOF(vl) == CLOSXP && getAttrib(vl, s_generic) != R_NilValue)
#define PACKAGE_SLOT(vl) getAttrib(vl, R_PackageSymbol)

static SEXP get_generic(SEXP symbol, SEXP rho, SEXP package)
{
    SEXP vl, generic = R_UnboundValue, gpackage;
    const char *pkg;
    bool ok;
    if(!isSymbol(symbol))
	symbol = installTrChar(asChar(symbol));
    pkg = CHAR(STRING_ELT(package, 0)); /* package is guaranteed single string */

    while (rho != R_NilValue) {
	vl = findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    ok = false;
	    if(IS_GENERIC(vl)) {
	      if(strlen(pkg)) {
		  gpackage = PACKAGE_SLOT(vl);
		  check_single_string(gpackage, FALSE, _("The \"package\" slot in generic function object"));
		  ok = streql(pkg, CHAR(STRING_ELT(gpackage, 0)));
		}
		else
		  ok = true;
	    }
	    if(ok) {
		generic = vl;
		break;
	    } else
		vl = R_UnboundValue;
	}
	rho = ENCLOS(rho);
    }
    /* look in base if either generic is missing */
    if(generic == R_UnboundValue) {
	vl = SYMVALUE(symbol);
	if(IS_GENERIC(vl)) {
	    generic = vl;
	    if(strlen(pkg)) {
		gpackage = PACKAGE_SLOT(vl);
		check_single_string(gpackage, FALSE, _("The \"package\" slot in generic function object"));
		if (!streql(pkg, CHAR(STRING_ELT(gpackage, 0)))) generic = R_UnboundValue;
	    }
	}
    }
    return generic;
}

SEXP R_getGeneric(SEXP name, SEXP mustFind, SEXP env, SEXP package)
{
    SEXP value;
    if(isSymbol(name)) {}
    else check_single_string(name, true, _("The argument \"f\" to getGeneric"));
    check_single_string(package, FALSE, _("The argument \"package\" to getGeneric"));
    value = get_generic(name, env, package);
    if(value == R_UnboundValue) {
	if(asLogical(mustFind)) {
	    error((env == R_GlobalEnv)
		  ? _("no generic function definition found for '%s'")
		  : _("no generic function definition found for '%s' in the supplied environment"),
		  CHAR(asChar(name)));
	}
	value = R_NilValue;
    }
    return value;
}


/* C version of the standardGeneric R function. */
SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    SEXP f_env=R_BaseEnv, mlist=R_NilValue, f, val=R_NilValue, fsym; /* -Wall */
    int nprotect = 0;

    if(!initialized)
	R_initMethodDispatch(NULL);
    fsym = fname;
    /* TODO:  the code for do_standardGeneric does a test of fsym,
     * with a less informative error message.  Should combine them.*/
    if(!isSymbol(fsym)) {
	const char *fname = check_single_string(fsym, true, _("The function name in the call to standardGeneric"));
	fsym = install(fname);
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	PROTECT(mlist = findVar(s_dot_Methods, f_env)); nprotect++;
	if(mlist == R_UnboundValue)
            mlist = R_NilValue;
	break;
    case SPECIALSXP: case BUILTINSXP:
        f_env = R_BaseEnv;
	PROTECT(mlist = R_primitive_methods(fdef)); nprotect++;
	break;
    default: error(_("invalid generic function object for method selection for function '%s': expected a function or a primitive, got an object of class \"%s\""),
		   CHAR(asChar(fsym)), class_string(fdef));
    }
    switch(TYPEOF(mlist)) {
    case NILSXP:
    case CLOSXP:
    case SPECIALSXP: case BUILTINSXP:
	f = mlist; break;
    default:
	f = do_dispatch(fname, ev, mlist, TRUE, TRUE);
    }
    if(isNull(f)) {
	SEXP value;
	PROTECT(value = R_S_MethodsListSelect(fname, ev, mlist, f_env)); nprotect++;
	if(isNull(value))
	    error(_("no direct or inherited method for function '%s' for this call"),
		  CHAR(asChar(fname)));
	mlist = value;
	/* now look again.  This time the necessary method should
	   have been inserted in the MethodsList object */
	f = do_dispatch(fname, ev, mlist, FALSE, TRUE);
    }
    /* loadMethod methods */
    if(isObject(f))
	f = R_loadMethod(f, fname, ev);
    switch(TYPEOF(f)) {
    case CLOSXP:
	{
	    if (inherits(f, "internalDispatchMethod")) {
                val = R_deferred_default_method();
            } else {
                PROTECT(f); nprotect++; /* is this needed?? */
                val = R_execMethod(f, ev);
            }
	}
	break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
      val = R_deferred_default_method();
      break;
    default:
	error("%s", _("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

/* Is the argument missing?  This _approximates_ the classic S sense of
   the question (is the argument missing in the call), rather than the
   R semantics (is the value of the argument R_MissingArg), but not if
   computations in the body of the function may have assigned to the
   argument name.
*/
static bool is_missing_arg(SEXP symbol, SEXP ev)
{
    /* Sanity check, so don't translate */
    if (!isSymbol(symbol)) error("%s", _("'symbol' must be a SYMSXP"));
    R_varloc_t loc = R_findVarLocInFrame(ev, symbol);
    if (R_VARLOC_IS_NULL(loc))
	error(_("could not find symbol '%s' in frame of call"),
	      CHAR(PRINTNAME(symbol)));
    return R_GetVarLocMISSING(loc);
}

SEXP R_missingArg(SEXP symbol, SEXP ev)
{
    if(!isSymbol(symbol))
	error(_("invalid symbol in checking for missing argument in method dispatch: expected a name, got an object of class \"%s\""),
	     class_string(symbol));
    if (isNull(ev)) {
	error("%s", _("use of NULL environment is defunct"));
	ev = R_BaseEnv;
    } else
    if(!isEnvironment(ev))
	error(_("invalid environment in checking for missing argument, '%s', in methods dispatch: got an object of class \"%s\""),
	     CHAR(PRINTNAME(symbol)), class_string(ev));
    if(is_missing_arg(symbol, ev))
	return R_TRUE;
    else
	return R_FALSE;
}



SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist, SEXP evalArgs)
{
    return do_dispatch(fname, ev, mlist, TRUE, asLogical(evalArgs));
}

typedef struct {
    SEXP fname;
    SEXP arg_sym;
} argEvalCleanup_t;

static SEXP argEvalCleanup(SEXP err, void *data_)
{
    argEvalCleanup_t *data = (argEvalCleanup_t *) data_;
    error(_("error in evaluating the argument '%s' in selecting a method for function '%s': %s"),
	  CHAR(PRINTNAME(data->arg_sym)),CHAR(asChar(data->fname)),
	  CHAR(STRING_ELT(R_conditionMessage(err), 0)));
    return R_NilValue;
}

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, bool firstTry,
			bool evalArgs)
{
    const char *class_;
    SEXP arg_slot, arg_sym, method, value = R_NilValue;
    int nprotect = 0;
    /* check for dispatch turned off inside MethodsListSelect */
    if(isFunction(mlist))
	return mlist;
    PROTECT(arg_slot = R_do_slot(mlist, s_argument)); nprotect++;
    if(arg_slot == R_NilValue) {
	error(_("object of class \"%s\" used as methods list for function '%s' ( no 'argument' slot)"),
	      class_string(mlist), CHAR(asChar(fname)));
	return(R_NilValue); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = installTrChar(asChar(arg_slot));
    if(arg_sym == R_DotsSymbol || DDVAL(arg_sym) > 0)
	error(_("(in selecting a method for function '%s') '...' and related variables cannot be used for methods dispatch"),
	      CHAR(asChar(fname)));
    if(TYPEOF(ev) != ENVSXP) {
	error(_("(in selecting a method for function '%s') the 'environment' argument for dispatch must be an R environment; got an object of class \"%s\""),
	    CHAR(asChar(fname)), class_string(ev));
	return(R_NilValue); /* -Wall */
    }
    /* find the symbol in the frame, but don't use eval, yet, because
       missing arguments are ok & don't require defaults */
    argEvalCleanup_t cleandata = { .fname = fname, .arg_sym = arg_sym };
    if(evalArgs) {
	if(is_missing_arg(arg_sym, ev))
	    class_ = "missing";
	else {
	    /*  get its class */
	    SEXP arg, class_obj;
	    PROTECT(arg = R_evalHandleError(arg_sym, ev,
					    &argEvalCleanup, &cleandata));
	    nprotect++;
	    PROTECT(class_obj = R_data_class(arg, TRUE)); nprotect++;
	    class_ = CHAR(STRING_ELT(class_obj, 0));
	}
    }
    else {
	/* the arg contains the class as a string */
	SEXP arg;
	PROTECT(arg = R_evalHandleError(arg_sym, ev, &argEvalCleanup,
				        &cleandata));
	nprotect++;
	class_ = CHAR(asChar(arg));
    }
    method = R_find_method(mlist, class_, fname);
    if(isNull(method)) {
      if(!firstTry)
	error(_("no matching method for function '%s' (argument '%s', with class \"%s\")"),
	      EncodeChar(asChar(fname)), EncodeChar(PRINTNAME(arg_sym)), class_);
      UNPROTECT(nprotect);
      return(R_NilValue);
    }
    if(value == R_MissingArg) {/* the check put in before calling
			  function  MethodListSelect in R */
      error(_("recursive use of function '%s' in method selection, with no default method"),
		  CHAR(asChar(fname)));
      return(R_NilValue);
    }
    if(!isFunction(method)) {
	/* assumes method is a methods list itself.  */
	/* call do_dispatch recursively.  Note the NULL for fname; this is
	   passed on to the S language search function for inherited
	   methods, to indicate a recursive call, not one to be stored in
	   the methods metadata */
	method = do_dispatch(R_NilValue, ev, method, firstTry, evalArgs);
    }
    UNPROTECT(nprotect); nprotect = 0;
    return method;
}

SEXP R_M_setPrimitiveMethods(SEXP fname, SEXP op, SEXP code_vec,
			     SEXP fundef, SEXP mlist)
{
    return R_set_prim_method(fname, op, code_vec, fundef, mlist);
    // -> ../../../main/objects.c
}

static SEXP R_nextMethodCallCleanup(SEXP err, void *data)
{
    Rf_error(_("error in evaluating a 'primitive' next method: %s"),
	     CHAR(STRING_ELT(R_conditionMessage(err), 0)));
    return R_NilValue;
}

static void R_nextMethodCallFinally(void *data)
{
    /* reset the methods:  R_NilValue for the mlist argument
       leaves the previous function, methods list unchanged */
    SEXP op = (SEXP) data;
    do_set_prim_method(op, "set", R_NilValue, R_NilValue);
}

SEXP R_nextMethodCall(SEXP matched_call, SEXP ev)
{
    SEXP e, val, args, this_sym, op;
    int i, nargs = length(matched_call)-1;
    bool prim_case;
    /* for primitive .nextMethod's, suppress further dispatch to avoid
     * going into an infinite loop of method calls
    */
    PROTECT(op = findVarInFrame(ev, R_dot_nextMethod));
    if(op == R_UnboundValue)
	error("%s", _("internal error in 'callNextMethod': '.nextMethod' was not assigned in the frame of the method call"));
    PROTECT(e = shallow_duplicate(matched_call));
    prim_case = isPrimitive(op);
    if (!prim_case) {
        if (inherits(op, "internalDispatchMethod")) {
	    SEXP generic = findVarInFrame(ev, R_dot_Generic);
	    if(generic == R_UnboundValue)
	        error("%s", _("internal error in 'callNextMethod': '.Generic' was not assigned in the frame of the method call"));
	    PROTECT(generic);
	    op = INTERNAL(installTrChar(asChar(generic)));
	    UNPROTECT(1); /* generic */
	    prim_case = TRUE;
	}
    }
    if(prim_case) {
	/* retain call to primitive function, suppress method
	   dispatch for it */
        do_set_prim_method(op, "suppress", R_NilValue, R_NilValue);
    }
    else
	SETCAR(e, R_dot_nextMethod); /* call .nextMethod instead */
    args = CDR(e);
    /* e is a copy of a match.call, with expand.dots=FALSE.  Turn each
    <TAG>=value into <TAG> = <TAG>, except  ...= is skipped (if it
    appears) in which case ... was appended. */
    for(i=0; i<nargs; i++) {
	this_sym = TAG(args);
  /* "missing" only possible in primitive */
        if(this_sym != R_NilValue && CAR(args) != R_MissingArg)
	    SETCAR(args, this_sym);
	args = CDR(args);
    }
    if(prim_case)
	val = R_evalHandleErrorProtect(e, ev,
				       &R_nextMethodCallCleanup, NULL,
				       &R_nextMethodCallFinally, op);
    else
	val = eval(e, ev);
    UNPROTECT(2);
    return val;
}


static SEXP R_loadMethod(SEXP def, SEXP fname, SEXP ev)
{
    /* since this is called every time a method is dispatched with a
       definition that has a class, it should be as efficient as
       possible => we build in knowledge of the standard
       MethodDefinition and MethodWithNext slots.  If these (+ the
       class slot) don't account for all the attributes, regular
       dispatch is done. */
    SEXP attrib;
    int found = 1; /* we "know" the class attribute is there */
    PROTECT(def);
    for(SEXP s = attrib = ATTRIB(def); s != R_NilValue; s = CDR(s)) {
	SEXP t = TAG(s);
	if(t == R_target) {
	    defineVar(R_dot_target, CAR(s), ev); found++;
	}
	else if(t == R_defined) {
	    defineVar(R_dot_defined, CAR(s), ev); found++;
	}
	else if(t == R_nextMethod)  {
	    defineVar(R_dot_nextMethod, CAR(s), ev); found++;
	}
	else if(t == R_SrcrefSymbol || t == s_generic)  {
	    /* ignore */ found++;
	}
    }
    defineVar(R_dot_Method, def, ev);

    if(found < length(attrib)) {
        /* this shouldn't be needed but check the generic being
           "loadMethod", which would produce a recursive loop */
        if (streql(CHAR(asChar(fname)), "loadMethod")) {
	    UNPROTECT(1);
            return def;
	}
	SEXP e, val;
	PROTECT(e = allocVector(LANGSXP, 4));
	SETCAR(e,
	       lang3(R_tripleColon_name, R_methods_name, R_loadMethod_name));
	val = CDR(e);
	SETCAR(val, def); val = CDR(val);
	SETCAR(val, fname); val = CDR(val);
	SETCAR(val, ev);
	val = eval(e, ev);
	UNPROTECT(2);
	return val;
    }
    else {
	UNPROTECT(1);
	return def;
    }
}

static SEXP R_selectByPackage(SEXP table, SEXP classes, int nargs) {
    int lwidth; SEXP thisPkg;
    char *buf, *bufptr;
    lwidth = 0;
    for (int i = 0; i<nargs; i++) {
	thisPkg = PACKAGE_SLOT(VECTOR_ELT(classes, i));
	if(thisPkg == R_NilValue)
	    thisPkg = s_base;
	lwidth += (int) strlen(STRING_VALUE(thisPkg)) + 1;
    }
    /* make the label */
    CXXR::RAllocStack::Scope rscope;
    buf = (char *) R_alloc(lwidth + 1, sizeof(char));
    bufptr = buf;
    for (int i = 0; i<nargs; i++) {
	if(i > 0)
	    *bufptr++ = '#';
	thisPkg = PACKAGE_SLOT(VECTOR_ELT(classes, i));
	if(thisPkg == R_NilValue)
	    thisPkg = s_base;
	strcpy(bufptr, STRING_VALUE(thisPkg));
	while(*bufptr)
	    bufptr++;
    }
    /* look up the method by package -- if R_UnboundValue, will go on
     to do inherited calculation */
    SEXP sym = install(buf);

    return findVarInFrame(table, sym);
}

static const char *check_single_string(SEXP obj, bool nonEmpty, const char *what)
{
    const char *string_ = "<unset>"; /* -Wall */
    if(isString(obj)) {
	if(length(obj) != 1)
	    error(_("%s must be a single string (got a character vector of length %d)"),
		  what, length(obj));
	string_ = CHAR(STRING_ELT(obj, 0)); /* FIXME: translateChar? */
	if(nonEmpty && (! string_ || !string_[0]))
	    error(_("%s must be a non-empty string; got an empty string"),
		  what);
    }
    else {
	error(_("%s must be a single string (got an object of class \"%s\")"),
	      what, class_string(obj));
    }
    return string_;
}

static const char *check_symbol_or_string(SEXP obj, bool nonEmpty,
                                          const char *what)
{
    if(isSymbol(obj))
	return CHAR(PRINTNAME(obj));
    else
	return check_single_string(obj, nonEmpty, what);
}

static const char *class_string(SEXP obj)
{
    return CHAR(STRING_ELT(R_data_class(obj, TRUE), 0));
}

/* internal version of paste(".", prefix, name, sep="__"),
   for speed so few checks

   If you decide to change this:
   - don't, you will break all installed S4-using packages!
   - change the hard-coded ".__M__" in namespace.R
*/
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name, SEXP pkg)
{
    const char *prefixString, *nameString, *pkgString;
    CXXR::RAllocStack::Scope rscope;
    SEXP res;

    prefixString = check_single_string(prefix, TRUE,
				       _("The internal prefix (e.g., \"C\") for a meta-data object"));
    nameString = check_single_string(name, false,
				     _("The name of the object (e.g,. a class or generic function) to find in the meta-data"));
    pkgString = check_single_string(pkg, false,
				   _("The name of the package for a meta-data object"));
    size_t len;
    /* fits pkgString version format + '\0' */
    len = strlen(pkgString) + strlen(prefixString) + strlen(nameString) + 7;
    char *str = (char*) R_alloc(len, sizeof(char));

    if(*pkgString)
      snprintf(str, len, ".__%s__%s:%s", prefixString, nameString, pkgString);
    else
      snprintf(str, len, ".__%s__%s", prefixString, nameString);
    res = mkString(str);

    return res;
}

SEXP R_identC(SEXP e1, SEXP e2)
{
    if(TYPEOF(e1) == STRSXP && TYPEOF(e2) == STRSXP &&
       LENGTH(e1) == 1 && LENGTH(e2) == 1 &&
       STRING_ELT(e1, 0) == STRING_ELT(e2, 0))
	return R_TRUE;
    else
	return R_FALSE;
}

SEXP R_getClassFromCache(SEXP class_, SEXP table)
{
    SEXP value;
    if(TYPEOF(class_) == STRSXP) {
	if (LENGTH(class_) == 0) return R_NilValue;
	SEXP package = PACKAGE_SLOT(class_);
	value = findVarInFrame(table, installTrChar(STRING_ELT(class_, 0)));
	if(value == R_UnboundValue)
	    return R_NilValue;
	else if(TYPEOF(package) == STRSXP) {
	    SEXP defPkg = PACKAGE_SLOT(value);
	    /* check equality of package */
	    if(TYPEOF(defPkg) == STRSXP && length(defPkg) ==1 &&
	       STRING_ELT(defPkg,0) != STRING_ELT(package, 0))
		return R_NilValue;
	    else
		return value;
	}
	else /* may return a list if multiple instances of class */
	    return value;
    }
    else if(TYPEOF(class_) != OBJSXP) {
	error("%s", _("class should be either a character-string name or a class definition"));
	return R_NilValue; /* NOT REACHED */
    } else /* assumes a class def, but might check */
	return class_;
}


static SEXP do_inherited_table(SEXP class_objs, SEXP fdef, SEXP mtable, SEXP ev)
{
    static SEXP f = NULL;
    SEXP e, ee;

    if(f == NULL) {
	SEXP dotFind = install(".InheritForDispatch");
	/* NOTE: the call to findFun can lead to recursive (but it seems only one)
	   invocation of do_inherited_table(), and hence this initialization
	   (PR#17923). */
	f = findFun(dotFind, R_MethodsNamespace);
	R_PreserveObject(f);
    }
    PROTECT(e = allocVector(LANGSXP, 4));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, class_objs); ee = CDR(ee);
    SETCAR(ee, fdef); ee = CDR(ee);
    SETCAR(ee, mtable);
    ee = eval(e, ev);
    UNPROTECT(1);
    return ee;
}

static SEXP dots_class(SEXP ev, void *cleandata)
{
    static SEXP call = NULL; SEXP  ee;
    if(call == NULL) {
	SEXP dotFind, f, R_dots;
	dotFind = install(".dotsClass");
	PROTECT(f = findFun(dotFind, R_MethodsNamespace));
	R_dots = install("...");
	call = allocVector(LANGSXP, 2);
	R_PreserveObject(call);
	SETCAR(call,f); ee = CDR(call);
	SETCAR(ee, R_dots);
	UNPROTECT(1);
    }
    return R_evalHandleError(call, ev, &argEvalCleanup, cleandata);
}

static SEXP do_mtable(SEXP fdef, SEXP ev)
{
    static SEXP dotFind = NULL, f; SEXP  e, ee;
    if(dotFind == NULL) {
	dotFind = install(".getMethodsTable");
	f = findFun(dotFind, R_MethodsNamespace);
	R_PreserveObject(f);
    }
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, fdef);
    ee = eval(e, ev);
    UNPROTECT(1);
    return ee;
}

SEXP R_dispatchGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    static SEXP R_mtable = NULL, R_allmtable, R_sigargs, R_siglength, R_dots;
    int nprotect = 0;
    SEXP mtable, classes, thisClass = R_NilValue /* -Wall */, sigargs,
	siglength, f_env = R_NilValue, method, f, val = R_NilValue;
    char *buf, *bufptr;
    int nargs, i, lwidth = 0;

    if(!R_mtable) {
	R_mtable = install(".MTable");
	R_allmtable = install(".AllMTable");
	R_sigargs = install(".SigArgs");
	R_siglength = install(".SigLength");
	R_dots = install("...");
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	break;
    case SPECIALSXP: case BUILTINSXP:
	PROTECT(fdef = R_primitive_generic(fdef)); nprotect++;
	if(TYPEOF(fdef) != CLOSXP) {
	    error(_("failed to get the generic for the primitive \"%s\""),
		  CHAR(asChar(fname)));
	    return R_NilValue;
	}
	f_env = CLOENV(fdef);
	break;
    default:
	error(_("expected a generic function or a primitive for dispatch, got an object of class \"%s\""),
	      class_string(fdef));
    }
    PROTECT(mtable = findVarInFrame(f_env, R_allmtable)); nprotect++;
    if(mtable == R_UnboundValue) {
	do_mtable(fdef, ev); /* Should initialize the generic */
	PROTECT(mtable = findVarInFrame(f_env, R_allmtable)); nprotect++;
    }
    PROTECT(sigargs = findVarInFrame(f_env, R_sigargs)); nprotect++;
    PROTECT(siglength = findVarInFrame(f_env, R_siglength)); nprotect++;
    if(sigargs == R_UnboundValue || siglength == R_UnboundValue ||
       mtable == R_UnboundValue)
	error("%s", _("generic seems not to have been initialized for table dispatch---need to have '.SigArgs' and '.AllMtable' assigned in its environment"));
    nargs = asInteger(siglength);
    PROTECT(classes = allocVector(VECSXP, nargs)); nprotect++;
    if (nargs > LENGTH(sigargs))
	error("%s", _("'.SigArgs' is shorter than '.SigLength' says it should be"));
    for(i = 0; i < nargs; i++) {
	SEXP arg_sym = VECTOR_ELT(sigargs, i);
	if(is_missing_arg(arg_sym, ev))
	    thisClass = s_missing;
	else {
	    /*  get its class */
	    argEvalCleanup_t cleandata = { .fname = fname, .arg_sym = arg_sym };
	    if(arg_sym == R_dots)
		thisClass = dots_class(ev, &cleandata);
	    else {
		SEXP arg = PROTECT(R_evalHandleError(arg_sym, ev,
						     &argEvalCleanup,
						     &cleandata));
		thisClass = R_data_class(arg, TRUE);
		UNPROTECT(1); /* arg */
	    }
	}
	SET_VECTOR_ELT(classes, i, thisClass);
	lwidth += (int) strlen(STRING_VALUE(thisClass)) + 1;
    }
    /* make the label */
    CXXR::RAllocStack::Scope rscope;
    buf = (char *) R_alloc(lwidth + 1, sizeof(char));
    bufptr = buf;
    for(i = 0; i<nargs; i++) {
	if(i > 0)
	    *bufptr++ = '#';
	thisClass = VECTOR_ELT(classes, i);
	strcpy(bufptr, STRING_VALUE(thisClass));
	while(*bufptr)
	    bufptr++;
    }
    method = findVarInFrame(mtable, install(buf));
    if(DUPLICATE_CLASS_CASE(method)) {
	PROTECT(method);
	method = R_selectByPackage(method, classes, nargs);
	UNPROTECT(1);
    }
    if(method == R_UnboundValue) {
	method = do_inherited_table(classes, fdef, mtable, ev);
    }
    /* the rest of this is identical to R_standardGeneric;
       hence the f=method to remind us  */
    f = method;
    switch(TYPEOF(f)) {
    case CLOSXP:
    {
        if (inherits(f, "internalDispatchMethod")) {
            val = R_deferred_default_method();
        } else {
            if(isObject(f))
                f = R_loadMethod(f, fname, ev);
            PROTECT(f); nprotect++; /* is this needed?? */
            val = R_execMethod(f, ev);
        }
    }
    break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
	val = R_deferred_default_method();
	break;
    default:
	error("%s", _("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

SEXP R_set_method_dispatch(SEXP onOff)
{
    int value = asLogical(onOff);
    if (value == NA_LOGICAL) /*  just return previous*/
	return ScalarLogical(table_dispatch_on);
    // else
    bool prev = table_dispatch_on;
    table_dispatch_on = (bool)value;
    if(table_dispatch_on != prev) {
	R_set_standardGeneric_ptr(
	    (table_dispatch_on ? R_dispatchGeneric : R_standardGeneric),
	    Methods_Namespace);
	R_set_quick_method_check(
	    (table_dispatch_on ? R_quick_dispatch : R_quick_method_check));
    }
    return ScalarLogical(prev);
}
