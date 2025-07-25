/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2023  The R Core Team.
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
 */

/** @file logic.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/Logical.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Itermacros.h>

using namespace R;
using namespace CXXR;

/* interval at which to check interrupts, a guess */
/*   if re-enabling, consider a power of two */
// #define NINTERRUPT 10000000


static SEXP lunary(SEXP, SEXP, SEXP);
static SEXP lbinary(SEXP, SEXP, SEXP);
static SEXP binaryLogic(int code, SEXP s1, SEXP s2);
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);


/* & | ! */
attribute_hidden SEXP do_logic(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg1 = CAR(args); //, arg2 = CADR(args)
    bool attr1 = (ATTRIB(arg1) != R_NilValue);
    if (attr1 || ATTRIB(CADR(args)) != R_NilValue) {

        auto dgroup = DispatchGroup("Ops", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }
    /* The above did dispatch to valid S3/S4 methods, including those with
     * "wrong" number of arguments.
     * Now require binary calls to `&` and `|`  or unary calls to `!` : */
    checkArity(op, args);

    if (CDR(args) == R_NilValue) { // one argument  <==>  !(arg1)
	if (!attr1 && IS_SCALAR(arg1, LGLSXP)) {
	    /* directly handle '!' operator for simple logical scalars. */
	    int v = SCALAR_LVAL(arg1);
	    return ScalarLogical(v == NA_LOGICAL ? v : ! v);
	}
	return lunary(call, op, arg1);
    }
    // else : two arguments
    return lbinary(call, op, args);
}

static SEXP lbinary(SEXP call, SEXP op, SEXP args)
{
/* logical binary : "&" or "|" */
    SEXP
	x = CAR(args),
	y = CADR(args);

    if (isRaw(x) && isRaw(y)) {
    }
    else if ( !(isNull(x) || isNumber(x)) ||
	      !(isNull(y) || isNumber(y)) )
	errorcall(call, "%s",
		  _("operations are possible only for numeric, logical or complex types"));

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    bool
	xarray = isArray(x),
	yarray = isArray(y),
	xts = isTs(x),
	yts = isTs(y);
    GCStackRoot<> dims, xnames, ynames;
    if (xarray || yarray) {
	/* if one is a length-atleast-1-array and the
	 * other  is a length-0 *non*array, then do not use array treatment */
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, "%s", _("non-conformable arrays"));
	    dims = getAttrib(x, R_DimSymbol);
	}
	else if (xarray && (ny != 0 || nx == 0)) {
	    dims = getAttrib(x, R_DimSymbol);
	}
	else if (yarray && (nx != 0 || ny == 0)) {
	    dims = getAttrib(y, R_DimSymbol);
	} else
	    dims = R_NilValue;

	xnames = getAttrib(x, R_DimNamesSymbol);
	ynames = getAttrib(y, R_DimNamesSymbol);
    }
    else {
	dims = R_NilValue;
	xnames = getAttrib(x, R_NamesSymbol);
	ynames = getAttrib(y, R_NamesSymbol);
    }

    GCStackRoot<> klass, tsp;
    if (xts || yts) {
	if (xts && yts) {
	    /* could check ts conformance here */
	    tsp = getAttrib(x, R_TspSymbol);
	    klass = getAttrib(x, R_ClassSymbol);
	}
	else if (xts) {
	    if (nx < ny)
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    tsp = getAttrib(x, R_TspSymbol);
	    klass = getAttrib(x, R_ClassSymbol);
	}
	else /*(yts)*/ {
	    if (ny < nx)
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    tsp = getAttrib(y, R_TspSymbol);
	    klass = getAttrib(y, R_ClassSymbol);
	}
    }

    if (nx > 0 && ny > 0) {
	if (((nx > ny) ? nx % ny : ny % nx) != 0) // mismatch
	    warningcall(call, "%s",
			_("longer object length is not a multiple of shorter object length"));
    }

    GCStackRoot<> val;
    if (nx > 0 && ny > 0) {

	if (isRaw(x) && isRaw(y)) {
	    val = binaryLogic2(PRIMVAL(op), x, y);
	}
	else {
	    if(isNull(x))
		val = SETCAR(args, allocVector(LGLSXP, 0));
	    else // isNumeric(x)
		val = SETCAR(args, coerceVector(x, LGLSXP));
	    if(isNull(y))
		y = SETCAR(args, allocVector(LGLSXP, 0));
	    else // isNumeric(y)
		y = SETCADR(args, coerceVector(y, LGLSXP));
	    val = binaryLogic(PRIMVAL(op), val, y);
	}
    } else { // nx == 0 || ny == 0
	val = allocVector((isRaw(x) && isRaw(y)) ? RAWSXP : LGLSXP, 0);
    }

    if (dims != R_NilValue) {
	setAttrib(val, R_DimSymbol, dims);
	if (xnames != R_NilValue)
	    setAttrib(val, R_DimNamesSymbol, xnames);
	else if (ynames != R_NilValue)
	    setAttrib(val, R_DimNamesSymbol, ynames);
    }
    else {
	if (xnames != R_NilValue && XLENGTH(val) == xlength(xnames))
	    setAttrib(val, R_NamesSymbol, xnames);
	else if (ynames != R_NilValue && XLENGTH(val) == xlength(ynames))
	    setAttrib(val, R_NamesSymbol, ynames);
    }

    if (xts || yts) {		/* must set *after* dims! */
	setAttrib(val, R_TspSymbol, tsp);
	setAttrib(val, R_ClassSymbol, klass);
    }

    return val;
}

static SEXP lunary(SEXP call, SEXP op, SEXP arg)
{
    SEXP x, dim, dimnames, names;
    R_xlen_t i, len;

    len = XLENGTH(arg);
    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (!len) return allocVector(LGLSXP, 0);
	errorcall(call, "%s", _("invalid argument type"));
    }
    if (isLogical(arg) || isRaw(arg))
	// copy all attributes in this case
	x = PROTECT(shallow_duplicate(arg));
    else {
	x = PROTECT(allocVector(isRaw(arg) ? RAWSXP : LGLSXP, len));
	PROTECT(names = getAttrib(arg, R_NamesSymbol));
	PROTECT(dim = getAttrib(arg, R_DimSymbol));
	PROTECT(dimnames = getAttrib(arg, R_DimNamesSymbol));
	if(names != R_NilValue) setAttrib(x, R_NamesSymbol, names);
	if(dim != R_NilValue) setAttrib(x, R_DimSymbol, dim);
	if(dimnames != R_NilValue) setAttrib(x, R_DimNamesSymbol, dimnames);
	UNPROTECT(3);
    }
    switch(TYPEOF(arg)) {
    case LGLSXP:
	{
	    int *px = LOGICAL(x);
	    const int *parg = LOGICAL_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		int v = parg[i];
		px[i] = (v == NA_LOGICAL) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case INTSXP:
	{
	    int *px = LOGICAL(x);
	    const int *parg = INTEGER_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		int v = parg[i];
		px[i] = (v == NA_INTEGER) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case REALSXP:
	{
	    int *px = LOGICAL(x);
	    const double *parg = REAL_RO(arg);
	    for (i = 0; i < len; i++){
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		double v = parg[i];
		px[i] = ISNAN(v) ? NA_LOGICAL : v == 0;
	    }
	}
	break;
    case CPLXSXP:
	{
	    int *px = LOGICAL(x);
	    const Rcomplex *parg = COMPLEX_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		Rcomplex v = parg[i];
		px[i] = (ISNAN(v.r) || ISNAN(v.i))
		    ? NA_LOGICAL : (v.r == 0. && v.i == 0.);
	    }
	}
	break;
    case RAWSXP:
	{
	    Rbyte *px = RAW(x);
	    const Rbyte *parg = RAW_RO(arg);
	    for (i = 0; i < len; i++) {
//	        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		px[i] = 0xFF ^ parg[i];
	    }
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("lunary", arg);
    }
    UNPROTECT(1);
    return x;
}

/* && || */
attribute_hidden SEXP do_logic2(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*  &&	and  ||	 */
    SEXP s1, s2;
    int x1, x2;
    int ans = FALSE;

    if (length(args) != 2)
	error(_("'%s' operator requires 2 arguments"),
	      PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = CAR(args);
    s2 = CADR(args);
    PROTECT(s1 = eval(s1, env));
    if (!isNumber(s1))
	errorcall(call, _("invalid %s type in 'x %s y'"),
		  "x", PRIMVAL(op) == 1 ? "&&" : "||");

    x1 = asLogical2(s1, /*checking*/ 1, call);
    UNPROTECT(1); /* s1 */

#define get_2nd							\
	PROTECT(s2 = eval(s2, env));				\
	if (!isNumber(s2))					\
	    errorcall(call, _("invalid %s type in 'x %s y'"),	\
		      "y", PRIMVAL(op) == 1 ? "&&" : "||");	\
	x2 = asLogical2(s2, 1, call);			\
	UNPROTECT(1); /* s2 */

    switch (PRIMVAL(op)) {
    case 1: /* && */
	if (x1 == FALSE)
	    ans = FALSE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		ans = (x2 == NA_LOGICAL || x2) ? NA_LOGICAL : x2;
	    else /* x1 == TRUE */
		ans = x2;
	}
	break;
    case 2: /* || */
	if (x1 == TRUE)
	    ans = TRUE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		ans = (x2 == NA_LOGICAL || !x2) ? NA_LOGICAL : x2;
	    else /* x1 == FALSE */
		ans = x2;
	}
    }
    return ScalarLogical(ans);
}

static SEXP binaryLogic(int code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, i1, i2;
    int x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(LGLSXP, 0);
	return ans;
    }
    ans = allocVector(LGLSXP, n);

    int *px1 = LOGICAL(s1);
    int *px2 = LOGICAL(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case 1:		/* & : AND */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (x1 == 0 || x2 == 0)
		pa[i] = 0;
	    else if (x1 == NA_LOGICAL || x2 == NA_LOGICAL)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = 1;
	});
	break;
    case 2:		/* | : OR */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if ((x1 != NA_LOGICAL && x1) || (x2 != NA_LOGICAL && x2))
		pa[i] = 1;
	    else if (x1 == 0 && x2 == 0)
		pa[i] = 0;
	    else
		pa[i] = NA_LOGICAL;
	});
	break;
    case 3:
	error("%s", _("Unary operator `!' called with two arguments"));
	break;
    }
    return ans;
}

// called only when both  s1 and s2 are  RAWSXP
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, i1, i2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(RAWSXP, 0);
	return ans;
    }
    ans = allocVector(RAWSXP, n);

    switch (code) {
    case 1:		/* & : AND */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    RAW(ans)[i] = x1 & x2;
	});
	break;
    case 2:		/* | : OR */
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    RAW(ans)[i] = x1 | x2;
	});
	break;
    }
    return ans;
}

#define _OP_ALL 1
#define _OP_ANY 2

static Logical checkValues(int op, int na_rm, SEXP x, R_xlen_t n)
{
    R_xlen_t i;
    int has_na = 0;
    int *px = LOGICAL(x);
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	int xi = px[i];
	if (!na_rm && xi == NA_LOGICAL) has_na = 1;
	else {
	    if (xi == TRUE && op == _OP_ANY) return true;
	    if (xi == FALSE && op == _OP_ALL) return false;
	}
    }
    switch (op) {
    case _OP_ANY:
	return has_na ? Logical::NA() : false;
    case _OP_ALL:
	return has_na ? Logical::NA() : true;
    default:
	error("%s", _("bad op value for do_logic3"));
    }
    return Logical::NA(); /* -Wall */
}

/* all, any */
attribute_hidden SEXP do_logic3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    Logical val = (PRIMVAL(op) == _OP_ALL);

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    R_args_enable_refcnt(args);
    SETCDR(call2, args);

    {
        auto dgroup = DispatchGroup("Summary", call2, op, args, env);
        if (dgroup.first) {
            UNPROTECT(2);
            SETCDR(call2, R_NilValue); /* clear refcnt on args */
            R_try_clear_args_refcnt(args);
            return dgroup.second;
        }
    }
    SETCDR(call2, R_NilValue); /* clear refcnt on args */
    R_try_clear_args_refcnt(args);

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical2(ans, /*warn_level*/ 1, call);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(xlength(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    R_typeToChar(t));
	    t = coerceVector(t, LGLSXP);
	}
	val = checkValues(PRIMVAL(op), narm, t, XLENGTH(t));
	if (!val.isNA()) {
	    if ((PRIMVAL(op) == _OP_ANY && val.isTrue())
		|| (PRIMVAL(op) == _OP_ALL && val.isFalse())) {
		has_na = 0;
		break;
	    }
	} else has_na = 1;
    }
    UNPROTECT(2);
    return has_na ? ScalarLogical(NA_LOGICAL) : ScalarLogical(int(val));
}
#undef _OP_ALL
#undef _OP_ANY
