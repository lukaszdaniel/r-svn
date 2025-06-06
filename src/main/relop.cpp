/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
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

/** @file relop.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Minmax.h>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/String.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <cerrno>
#include <R_ext/Itermacros.h>

using namespace R;
using namespace CXXR;

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call);
static SEXP string_relop (RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP raw_relop    (RELOP_TYPE code, SEXP s1, SEXP s2);

#define DO_SCALAR_RELOP(oper, x, y) do {		\
	switch (oper) {					\
	case EQOP: return ScalarLogical((x) == (y));	\
	case NEOP: return ScalarLogical((x) != (y));	\
	case LTOP: return ScalarLogical((x) < (y));	\
	case GTOP: return ScalarLogical((x) > (y));	\
	case LEOP: return ScalarLogical((x) <= (y));	\
	case GEOP: return ScalarLogical((x) >= (y));	\
	}						\
    } while (0)

attribute_hidden SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg1, arg2;
    int argc;

    if (args != R_NilValue &&
	CDR(args) != R_NilValue &&
	CDDR(args) == R_NilValue)
	argc = 2;
    else
	argc = length(args);
    arg1 = CAR(args);
    arg2 = CADR(args);

    if (ATTRIB(arg1) != R_NilValue || ATTRIB(arg2) != R_NilValue) {
        auto dispatched = DispatchGroup("Ops", call, op, args, env);
        if (dispatched.first)
            return dispatched.second;
    }

    if (argc != 2)
	error("%s", _("operator needs two arguments"));

    return do_relop_dflt(call, op, arg1, arg2);
}

#define IS_SCALAR_STRING(x) (TYPEOF(x) == STRSXP && XLENGTH(x) == 1)
#define SYMBOL_STRING_MATCH(x, y) \
    (isSymbol(x) && IS_SCALAR_STRING(y) && Seql(PRINTNAME(x), STRING_ELT(y, 0)))


static R_INLINE bool compute_lang_equal(SEXP x, SEXP y)
{
    if (isSymbol(x))
	return y == x ||
	    (IS_SCALAR_STRING(y) && Seql(PRINTNAME(x), STRING_ELT(y, 0)));
    else if (isSymbol(y))
	return x == y ||
	    (IS_SCALAR_STRING(x) && Seql(STRING_ELT(x, 0), PRINTNAME(y)));

    if (TYPEOF(x) == LANGSXP && ATTRIB(x) != R_NilValue)
	x = LCONS(CAR(x), CDR(x));
    PROTECT(x);
    if (TYPEOF(y) == LANGSXP && ATTRIB(y) != R_NilValue)
	y = LCONS(CAR(y), CDR(y));
    PROTECT(y);

    bool val = R_compute_identical(x, y, 16);
    UNPROTECT(2);
    return val;
}

static SEXP compute_language_relop(SEXP call, SEXP op, SEXP x, SEXP y)
{
    static enum {
	UNINITIALIZED,
	EQONLY,
	IDENTICAL_CALLS,
	IDENTICAL_CALLS_ATTR,
	IDENTICAL,
	ERROR_CALLS,
	ERROR
    } option = UNINITIALIZED;

    if (option == UNINITIALIZED) {
	option = EQONLY;
	const char *val = getenv("_R_COMPARE_LANG_OBJECTS");
	if (val != NULL) {
	    if (streql(val, "eqonly"))
		option = EQONLY;
	    else if (streql(val, "identical_calls"))
		option = IDENTICAL_CALLS;
	    else if (streql(val, "identical_calls_attr"))
		option = IDENTICAL_CALLS_ATTR;
	    else if (streql(val, "identical"))
		option = IDENTICAL;
	    else if (streql(val, "error_calls"))
		option = ERROR_CALLS;
	    else if (streql(val, "error"))
		option = ERROR;
	}
    }

    switch(option) {
    case EQONLY:
	switch(PRIMVAL(op)) {
	case EQOP: return NULL;
	case NEOP: return NULL;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL_CALLS:
	/* this should reproduce the current behavior of == and != for
	   language objects, while signaling errors for <, <=, >, and
	   >=. */
	switch(PRIMVAL(op)) {
	case EQOP:
	    return compute_lang_equal(x, y) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return compute_lang_equal(x, y) ? R_FalseValue : R_TrueValue;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL_CALLS_ATTR:
	if (isSymbol(x) && IS_SCALAR_STRING(y))
	    y = Seql(STRING_ELT(y, 0), PRINTNAME(x)) ? x : R_NilValue;
	else if (isSymbol(y) && IS_SCALAR_STRING(x))
	    x = Seql(STRING_ELT(x, 0), PRINTNAME(y)) ? y : R_NilValue;
	switch(PRIMVAL(op)) {
	case EQOP:
	    return R_compute_identical(x, y, 16) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return R_compute_identical(x, y, 16) ? R_FalseValue : R_TrueValue;
	default:
	    errorcall(call,
		      _("comparison (%s) is not possible for language types"),
		      PRIMNAME(op));
	}
    case IDENTICAL:
	if (SYMBOL_STRING_MATCH(x, y) || SYMBOL_STRING_MATCH(y, x))
	    /* identical(x, y) and the default x == y implementation
	       would disagree, so signal an error instead */
	    errorcall(call, "%s",
		      _("comparing this symbol and string pair is not supported"));
	switch(PRIMVAL(op)) {
	case EQOP:
	    return R_compute_identical(x, y, 16) ? R_TrueValue : R_FalseValue;
	case NEOP:
	    return R_compute_identical(x, y, 16) ? R_FalseValue : R_TrueValue;
	default: errorcall(call,
			   _("comparison (%s) is not possible for language types"),
			   PRIMNAME(op));
	}
    case ERROR_CALLS:
	if (TYPEOF(x) == LANGSXP || TYPEOF(y) == LANGSXP)
	    errorcall(call, "%s", _("comparison of call objects is not supported"));
	return NULL;
    case ERROR:
	errorcall(call, "%s", _("comparison of language objects is not supported"));
    default: return NULL;
    }
}

// also called from cmp_relop() in eval.c :
attribute_hidden SEXP do_relop_dflt(SEXP call, SEXP op, SEXP xarg, SEXP yarg)
{
    RELOP_TYPE oper = (RELOP_TYPE) PRIMVAL(op);
    GCStackRoot<> x(xarg), y(yarg);

    /* handle the REALSXP/INTSXP simple scalar case quickly */
    if (IS_SIMPLE_SCALAR(x, INTSXP)) {
	int ix = SCALAR_IVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ix == NA_INTEGER || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(oper, ix, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ix == NA_INTEGER || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(oper, ix, dy);
	}
    }
    else if (IS_SIMPLE_SCALAR(x, REALSXP)) {
	double dx = SCALAR_DVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ISNAN(dx) || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(oper, dx, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ISNAN(dx) || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(oper, dx, dy);
	}
    }

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    SEXPTYPE
	typex = TYPEOF(x),
	typey = TYPEOF(y);

    /* handle the REALSXP/INTSXP simple vector/scalar case quickly. */
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue &&
	(typex == REALSXP || typex == INTSXP) &&
	(typey == REALSXP || typey == INTSXP) &&
	nx > 0 && ny > 0 && (nx == 1 || ny == 1)) {

	return numeric_relop(oper, x, y);
    }

    /* handle the general case */
    if (isSymbol(x) || TYPEOF(x) == LANGSXP ||
	isSymbol(y) || TYPEOF(y) == LANGSXP) {
	SEXP ans = compute_language_relop(call, op, x, y);
	if (ans != NULL) {
	    return ans;
	}
    }

    bool iS;
    /* That symbols and calls were allowed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    if ((iS = isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	GCStackRoot<> tmp;
	tmp = allocVector(STRSXP, 1);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) :
		       STRING_ELT(deparse1line_ex(x, false,
						DEFAULTDEPARSE | DIGITS17),
				  0));
	x = tmp;
	nx = xlength(x);
    }
    if ((iS = isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	GCStackRoot<> tmp;
	tmp = allocVector(STRSXP, 1);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
		       STRING_ELT(deparse1line_ex(y, false,
						DEFAULTDEPARSE | DIGITS17),
				  0));
	y = tmp;
	ny = xlength(y);
    }

    if (isNull(x)) { x = allocVector(INTSXP,0); nx = xlength(x); }
    if (isNull(y)) { y = allocVector(INTSXP,0); ny = xlength(y); }
    if (!isVector(x) || !isVector(y))
	errorcall(call,
		  _("comparison (%s) is possible only for atomic and list types"),
		  PRIMNAME(op));

#ifdef previous_R_versions
    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP)
	errorcall(call, "%s", _("comparison is not allowed for expressions"));
#endif

    /* ELSE :  x and y are both atomic or list */

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

    if (isString(x) || isString(y)) {
	x = coerceVector(x, STRSXP);
	y = coerceVector(y, STRSXP);
	val = string_relop(oper, x, y);
    }
    else if (isComplex(x) || isComplex(y)) {
	x = coerceVector(x, CPLXSXP);
	y = coerceVector(y, CPLXSXP);
	val = complex_relop(oper, x, y, call);
    }
    else if ((isNumeric(x) || isLogical(x)) && (isNumeric(y) || isLogical(y))) {
        val = numeric_relop(oper, x, y);
    } // rest of cases only apply when 'x' or 'y' is raw
    else if (isReal(x) || isReal(y)) {
	x = coerceVector(x, REALSXP);
	y = coerceVector(y, REALSXP);
	val = numeric_relop(oper, x, y);
    }
    else if (isInteger(x) || isInteger(y)) {
	x = coerceVector(x, INTSXP);
	y = coerceVector(y, INTSXP);
	val = numeric_relop(oper, x, y);
    }
    else if (isLogical(x) || isLogical(y)) {
	x = coerceVector(x, LGLSXP);
	y = coerceVector(y, LGLSXP);
	val = numeric_relop(oper, x, y);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	x = coerceVector(x, RAWSXP);
	y = coerceVector(y, RAWSXP);
	val = raw_relop(oper, x, y);
    } else errorcall(call, "%s", _("comparison of these types is not implemented"));
  } else { // nx == 0 || ny == 0
	val = allocVector(LGLSXP, 0);
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

#define ISNA_INT(x) x == NA_INTEGER

#define NR_HELPER(OP, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
	type1 x1, *px1 = ACCESSOR1(s1);					\
	type2 x2, *px2 = ACCESSOR2(s2);					\
	int *pa = LOGICAL(ans);						\
        MOD_ITERATE2(n, n1, n2, i, i1, i2, {                            \
	    x1 = px1[i1];						\
	    x2 = px2[i2];						\
            if (ISNA1(x1) || ISNA2(x2))                                 \
                pa[i] = NA_LOGICAL;					\
            else                                                        \
                pa[i] = (x1 OP x2);					\
        });                                                             \
    } while (0)

#define NUMERIC_RELOP(type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
    switch (code) {                                                     \
    case EQOP:                                                          \
	NR_HELPER(==, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case NEOP:                                                          \
	NR_HELPER(!=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LTOP:                                                          \
	NR_HELPER(<, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GTOP:                                                          \
	NR_HELPER(>, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LEOP:                                                          \
	NR_HELPER(<=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GEOP:                                                          \
	NR_HELPER(>=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    }                                                                   \
} while(0)

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    if (isInteger(s1) || isLogical(s1)) {
        if (isInteger(s2) || isLogical(s2)) {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, int, INTEGER, ISNA_INT);
        } else {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, double, REAL, ISNAN);
        }
    } else if (isInteger(s2) || isLogical(s2)) {
        NUMERIC_RELOP(double, REAL, ISNAN, int, INTEGER, ISNA_INT);
    } else {
        NUMERIC_RELOP(double, REAL, ISNAN, double, REAL, ISNAN);
    }

    UNPROTECT(2);
    return ans;
}

static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rcomplex x1, x2;
    SEXP ans;

    if (code != EQOP && code != NEOP) {
	errorcall(call, "%s", _("invalid comparison with complex values"));
    }

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rcomplex *px1 = COMPLEX_RO(s1);
    const Rcomplex *px2 = COMPLEX_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r == x2.r && x1.i == x2.i);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r != x2.r || x1.i != x2.i);
	});
	break;
    default:
	/* never happens (-Wall) */
	break;
    }
    UNPROTECT(2);
    return ans;
}


/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, res, i1, i2;
    SEXP ans, c1, c2;
    CXXR::RAllocStack::Scope rscope; // for Scollate

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 1 : 0;
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 0 : 1;
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res < 0) ? 1 : 0;
	    }
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i ,i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res > 0) ? 1 : 0;
	    }
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res <= 0) ? 1 : 0;
	    }
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res >= 0) ? 1 : 0;
	    }
	});
	break;
    }
    UNPROTECT(3);
    return ans;
}

static SEXP raw_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rbyte *px1 = RAW_RO(s1);
    const Rbyte *px2 = RAW_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 == x2);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 != x2);
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 < x2);
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 > x2);
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 <= x2);
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 >= x2);
	});
	break;
    }
    UNPROTECT(2);
    return ans;
}


static SEXP bitwiseNot(SEXP a)
{
    SEXP ans;
    int np = 0;
    if (isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t m = XLENGTH(a);
	    ans = allocVector(INTSXP, m);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a);
	    for (R_xlen_t i = 0; i < m; i++) {
		int aa = pa[i];
		pans[i] = (aa == NA_INTEGER) ? aa : ~aa;
	    }
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitwNot", a);
    }
    if (np) UNPROTECT(np);
    return ans;
}

#define BIT(op, name)							\
    SEXP ans;								\
    int np = 0;								\
    if (isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}	\
    if (isReal(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}	\
    if (TYPEOF(a) != TYPEOF(b))						\
	error("%s", _("'a' and 'b' must have the same type"));		\
    switch(TYPEOF(a)) {							\
    case INTSXP:							\
	{								\
	    R_xlen_t i, ia, ib;						\
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),			\
		mn = (m && n) ? std::max(m, n) : 0;			\
	    ans = allocVector(INTSXP, mn);				\
	    int *pans = INTEGER(ans);					\
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);		\
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {				\
		    int aa = pa[ia]; int bb = pb[ib];			\
		    pans[i] = (aa == NA_INTEGER || bb == NA_INTEGER) ?	\
			NA_INTEGER : aa op bb;				\
		});							\
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE(name, a);					\
    }									\
    if (np) UNPROTECT(np);						\
    return ans

static SEXP bitwiseAnd(SEXP a, SEXP b)
{
    BIT(&, "bitwAnd");
}

static SEXP bitwiseOr(SEXP a, SEXP b)
{
    BIT(|, "bitwOr");
}

static SEXP bitwiseXor(SEXP a, SEXP b)
{
    BIT(^, "bitwXor");
}

static SEXP bitwiseShiftL(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if (isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if (!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error("%s", _("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? std::max(m, n) : 0;
	    ans = allocVector(INTSXP, mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa << bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftL", a);
    }
    if (np) UNPROTECT(np);
    return ans;
}

static SEXP bitwiseShiftR(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if (isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if (!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error("%s", _("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? std::max(m, n) : 0;
	    ans = allocVector(TYPEOF(a), mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa >> bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftR", a);
    }
    if (np) UNPROTECT(np);
    return ans;
}

attribute_hidden SEXP do_bitwise(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans = R_NilValue; /* -Wall */
    switch(PRIMVAL(op)) {
    case 1: ans = bitwiseAnd(CAR(args), CADR(args)); break;
    case 2: ans = bitwiseNot(CAR(args)); break;
    case 3: ans = bitwiseOr(CAR(args), CADR(args)); break;
    case 4: ans = bitwiseXor(CAR(args), CADR(args)); break;
    case 5: ans = bitwiseShiftL(CAR(args), CADR(args)); break;
    case 6: ans = bitwiseShiftR(CAR(args), CADR(args)); break;
    }
    return ans;
}
