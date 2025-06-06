/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025 The R Core Team.
 *  Copyright (C) 2003--2023 The R Foundation
 *  Copyright (C) 1995--1997 Robert Gentleman and Ross Ihaka
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

/** @file arithmetic.cpp
 *
 * All sorts of arithmetical and mathematical functions, from addition
 * through to Bessel functions.
 */

/* ====
   NOTE: The [dpq]<foo>() distribution functions in math2, math3, math4 are *NOT* used from R,
   ====  as [dpqr]<foo>() functions are in stats,
         and the C code wrappers are all in ../library/stats/src/distn.c  <<< keep in SYNC !!!
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// LDBL_EPSILON
#include <cfloat>
#include <limits>
#include <cerrno>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000


#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#include <Internal.h>

#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("non-numeric argument to mathematical function")

#include <Rmath.h>

#include <R_ext/Itermacros.h>

#include "arithmetic.h"

using namespace R;
using namespace CXXR;

/* Override for matherr removed for R 4.4.0 */
/* Intel compilers for Linux do have matherr, but they do not have the
   defines in math.h.  So we skip this for Intel */

typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;

#ifdef WORDS_BIGENDIAN
#define hw 0
#define lw 1
#else  /* !WORDS_BIGENDIAN */
#define hw 1
#define lw 0
#endif /* WORDS_BIGENDIAN */


static double R_ValueOfNA(void)
{
    /* The gcc (3.2.1?) shipping with Red Hat Linux 9 gets this wrong
     * without the volatile declaration. Thanks to Marc Schwartz. */
    volatile ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

/* is a value known to be a NaN also an R NA? */
attribute_hidden int R::R_NaN_is_R_NA(double x)
{
    ieee_double y;
    y.value = x;
    return (y.word[lw] == 1954);
}

int R_IsNA(double x)
{
    return std::isnan(x) && R_NaN_is_R_NA(x);
}

int R_IsNaN(double x)
{
    return std::isnan(x) && !R_NaN_is_R_NA(x);
}

/* ISNAN uses isnan, which is undefined by C++ headers
   This workaround is called only when ISNAN() is used
   in a user code in a file with __cplusplus defined */

int R_isnancpp(double x)
{
   return (std::isnan(x)!=0);
}


/* Mainly for use in packages */
int R_finite(double x)
{
#ifdef HAVE_WORKING_ISFINITE
    return std::isfinite(x);
#else
    return (!std::isnan(x) && (x != R_PosInf) && (x != R_NegInf));
#endif
}


/* Arithmetic Initialization */

attribute_hidden void R::InitArithmetic(void)
{
    R_NaInt = std::numeric_limits<int>::min();
    R_NaReal = R_ValueOfNA();
    R_NaN = std::numeric_limits<double>::quiet_NaN();
    R_PosInf = std::numeric_limits<double>::infinity();
    R_NegInf = -R_PosInf;
}


#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
/*
# ifdef __powerpc__
 // PowerPC 64 (when gcc has -mlong-double-128) fails constant folding with LDOUBLE
 // Debian Bug#946836 shows it is needed also for 32-bit ppc, not just __PPC64__
 // NB: 1 / LDBL_EPSILON has been seen to overflow on 'ppc64el ...
 ==> use  eps instead of  1 / eps  (and one multiplication more)
*/
# define c_eps LDBL_EPSILON
#else
# define c_eps DBL_EPSILON
#endif

/* Keep myfmod() and myfloor() in step */
static double myfmod(double x1, double x2)
{
    if (x2 == 0.0) return R_NaN;
    if (fabs(x2) * c_eps > 1 && R_FINITE(x1) && fabs(x1) <= fabs(x2)) {
	return
	    (fabs(x1) == fabs(x2)) ? 0 :
	    ((x1 < 0 && x2 > 0) ||
	     (x2 < 0 && x1 > 0))
	     ? x1+x2  // differing signs
	     : x1   ; // "same" signs (incl. 0)
    }
    double q = x1 / x2;
    if (R_FINITE(q) && (fabs(q) * c_eps > 1))
	warning("%s", _("probable complete loss of accuracy in modulus"));
    LDOUBLE tmp = (LDOUBLE)x1 - floor(q) * (LDOUBLE)x2;
    return (double) (tmp - floorl(tmp/x2) * x2);
}

static double myfloor(double x1, double x2)
{
    double q = x1 / x2;
    if (x2 == 0.0 || fabs(q) * c_eps > 1 || !R_FINITE(q))
	return q;
    if (fabs(q) < 1)
	return (q < 0) ? -1
	    : ((x1 < 0 && x2 > 0) ||
	       (x1 > 0 && x2 < 0) // differing signs
	       ? -1 : 0);
    LDOUBLE tmp = (LDOUBLE)x1 - floor(q) * (LDOUBLE)x2;
    return (double) (floor(q) + floorl(tmp/x2));
}

double R_pow(double x, double y) /* = x ^ y */
{
    /* squaring is the most common of the specially handled cases so
       check for it first. */
    if (y == 2.0)
	return x * x;
    if (x == 1. || y == 0.)
	return(1.);
    if (x == 0.) {
	if(y > 0.) return(0.);
	else if(y < 0) return(R_PosInf);
	else return(y); /* NA or NaN, we assert */
    }
    if (R_FINITE(x) && R_FINITE(y)) {
	/* There was a special case for y == 0.5 here, but
	   gcc 4.3.0 -g -O2 mis-compiled it.  Showed up with
	   100^0.5 as 3.162278, example(pbirthday) failed. */
#ifdef USE_POWL_IN_R_POW
	// this is used only on 64-bit Windows (so has powl).
	return powl(x, y);
#else
	return pow(x, y);
#endif
    }
    if (ISNAN(x) || ISNAN(y))
	return(x + y);
    if (!R_FINITE(x)) {
	if (x > 0)		/* Inf ^ y */
	    return (y < 0.)? 0. : R_PosInf;
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return (y < 0.) ? 0. : (myfmod(y, 2.) != 0 ? x  : -x);
	}
    }
    if (!R_FINITE(y)) {
	if (x >= 0) {
	    if (y > 0)		/* y == +Inf */
		return (x >= 1) ? R_PosInf : 0.;
	    else		/* y == -Inf */
		return (x < 1) ? R_PosInf : 0.;
	}
    }
    return R_NaN; // all other cases: (-Inf)^{+-Inf, non-int}; (neg)^{+-Inf}
}

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;

    if (n != 0) {
	if (!R_FINITE(x)) return R_POW(x, (double)n);

	bool is_neg = (n < 0);
	if (is_neg) n = -n;
	for (;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
	if (is_neg) xn = 1. / xn;
    }
    return xn;
}


/* General Base Logarithms */

static SEXP logical_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP real_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP real_binary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_binary(ARITHOP_TYPE, SEXP, SEXP, SEXP);

/* Integer arithmetic support */

/* The tests using integer comparisons are a bit faster than the tests
   using doubles, but they depend on a two's complement representation
   (but that is almost universal).  The tests that compare results to
   double's depend on being able to accurately represent all int's as
   double's.  Since int's are almost universally 32 bit that should be
   OK. */

#ifndef INT_32_BITS
/* configure checks whether int is 32 bits.  If not this code will
   need to be rewritten.  Since 32 bit ints are pretty much universal,
   we can worry about writing alternate code when the need arises.
   To be safe, we signal a compiler error if int is not 32 bits. */
# error code requires that int have 32 bits
#endif

#define INTEGER_OVERFLOW_WARNING _("NAs produced by integer overflow")

#define CHECK_INTEGER_OVERFLOW(call, ans, naflag) do {		\
	if (naflag) {						\
	    PROTECT(ans);					\
	    warningcall(call, "%s", INTEGER_OVERFLOW_WARNING);	\
	    UNPROTECT(1);					\
	}							\
    } while(0)

static R_INLINE int R_integer_plus(int lhs, int rhs, bool *pnaflag)
{
    if (lhs == NA_INTEGER || rhs == NA_INTEGER)
	return NA_INTEGER;

    if (((rhs > 0) && (lhs > (R_INT_MAX - rhs))) ||
	((rhs < 0) && (lhs < (R_INT_MIN - rhs)))) {
	if (pnaflag != NULL)
	    *pnaflag = true;
	return NA_INTEGER;
    }
    return lhs + rhs;
}

static R_INLINE int R_integer_minus(int lhs, int rhs, bool *pnaflag)
{
    if (rhs == NA_INTEGER)
        return NA_INTEGER;

    return R_integer_plus(lhs, -rhs, pnaflag);
}

#define GOODIPROD(x, y, z) ((double) (x) * (double) (y) == (z))
static R_INLINE int R_integer_times(int lhs, int rhs, bool *pnaflag)
{
    if (lhs == NA_INTEGER || rhs == NA_INTEGER)
        return NA_INTEGER;
    // This relies on the assumption that a double can represent all the
    // possible values of an integer.  This isn't true for 64-bit integers.
    static_assert(sizeof(int) <= 4,
                  "integer_times assumes 32 bit integers which isn't true on this platform");
    int z = lhs * rhs; // UBSAN will warn if this overflows (happens in bda)
    if (GOODIPROD(lhs, rhs, z) && z != NA_INTEGER)
        return z;
    else
    {
        if (pnaflag != NULL)
            *pnaflag = true;
        return NA_INTEGER;
    }
}

static R_INLINE double R_integer_divide(int lhs, int rhs)
{
    if (lhs == NA_INTEGER || rhs == NA_INTEGER)
        return NA_REAL;
    else
        return (double)lhs / (double)rhs;
}

static R_INLINE SEXP ScalarValue1(SEXP x)
{
    if (NO_REFERENCES(x))
	return x;
    else
	return allocVector(TYPEOF(x), 1);
}

static R_INLINE SEXP ScalarValue2(SEXP x, SEXP y)
{
    if (NO_REFERENCES(x))
	return x;
    else if (NO_REFERENCES(y))
	return y;
    else
	return allocVector(TYPEOF(x), 1);
}

/* Unary and Binary Operators */

attribute_hidden SEXP do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int argc;
    if (args == R_NilValue)
	argc = 0;
    else if (CDR(args) == R_NilValue)
	argc = 1;
    else if (CDDR(args) == R_NilValue)
	argc = 2;
    else
	argc = length(args);

    SEXP ans,
	arg1 = CAR(args),
	arg2 = CADR(args);
    if (ATTRIB(arg1) != R_NilValue || ATTRIB(arg2) != R_NilValue) {
        auto dgroup = DispatchGroup("Ops", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }
    else if (argc == 2) {
	/* Handle some scaler operations immediately */
	if (IS_SCALAR(arg1, REALSXP)) {
	    double x1 = SCALAR_DVAL(arg1);
	    if (IS_SCALAR(arg2, REALSXP)) {
		double x2 = SCALAR_DVAL(arg2);
		ans = ScalarValue2(arg1, arg2);
		switch (PRIMVAL(op)) {
		case PLUSOP: SET_SCALAR_DVAL(ans, x1 + x2); return ans;
		case MINUSOP: SET_SCALAR_DVAL(ans, x1 - x2); return ans;
		case TIMESOP: SET_SCALAR_DVAL(ans, x1 * x2); return ans;
		case DIVOP: SET_SCALAR_DVAL(ans, x1 / x2); return ans;
		}
	    }
	    else if (IS_SCALAR(arg2, INTSXP)) {
		int i2 = SCALAR_IVAL(arg2);
		double x2 = i2 != NA_INTEGER ? (double) i2 : NA_REAL;
		ans = ScalarValue1(arg1);
		switch (PRIMVAL(op)) {
		case PLUSOP: SET_SCALAR_DVAL(ans, x1 + x2); return ans;
		case MINUSOP: SET_SCALAR_DVAL(ans, x1 - x2); return ans;
		case TIMESOP: SET_SCALAR_DVAL(ans, x1 * x2); return ans;
		case DIVOP: SET_SCALAR_DVAL(ans, x1 / x2); return ans;
		}
	    }
	}
	else if (IS_SCALAR(arg1, INTSXP)) {
	    int i1 = SCALAR_IVAL(arg1);
	    if (IS_SCALAR(arg2, REALSXP)) {
		double x1 = i1 != NA_INTEGER ? (double) i1 : NA_REAL;
		double x2 = SCALAR_DVAL(arg2);
		ans = ScalarValue1(arg2);
		switch (PRIMVAL(op)) {
		case PLUSOP: SET_SCALAR_DVAL(ans, x1 + x2); return ans;
		case MINUSOP: SET_SCALAR_DVAL(ans, x1 - x2); return ans;
		case TIMESOP: SET_SCALAR_DVAL(ans, x1 * x2); return ans;
		case DIVOP: SET_SCALAR_DVAL(ans, x1 / x2); return ans;
		}
	    }
	    else if (IS_SCALAR(arg2, INTSXP)) {
		bool naflag = false;
		int i2 = SCALAR_IVAL(arg2);
		switch (PRIMVAL(op)) {
		case PLUSOP:
		    ans = ScalarValue2(arg1, arg2);
		    SET_SCALAR_IVAL(ans, R_integer_plus(i1, i2, &naflag));
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case MINUSOP:
		    ans = ScalarValue2(arg1, arg2);
		    SET_SCALAR_IVAL(ans, R_integer_minus(i1, i2, &naflag));
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case TIMESOP:
		    ans = ScalarValue2(arg1, arg2);
		    SET_SCALAR_IVAL(ans, R_integer_times(i1, i2, &naflag));
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case DIVOP:
		    return ScalarReal(R_integer_divide(i1, i2));
		}
	    }
	}
    }
    else if (argc == 1) {
	if (IS_SCALAR(arg1, REALSXP)) {
	    switch(PRIMVAL(op)) {
	    case PLUSOP: return arg1;
	    case MINUSOP:
		ans = ScalarValue1(arg1);
		SET_SCALAR_DVAL(ans, -SCALAR_DVAL(arg1));
		return ans;
	    }
	}
	else if (IS_SCALAR(arg1, INTSXP)) {
	    int ival;
	    switch(PRIMVAL(op)) {
	    case PLUSOP: return arg1;
	    case MINUSOP:
		ival = SCALAR_IVAL(arg1);
		ans = ScalarValue1(arg1);
		SET_SCALAR_IVAL(ans, ival == NA_INTEGER ? NA_INTEGER : -ival);
		return ans;
	    }
	}
    }

    if (argc == 2)
	return R_binary(call, op, arg1, arg2);
    else if (argc == 1)
	return R_unary(call, op, arg1);
    else
	errorcall(call, "%s", _("operator needs one or two arguments"));
    return ans;			/* never used; to keep -Wall happy */
}

#define COERCE_IF_NEEDED(v, tp) do { \
    if (TYPEOF(v) != (tp)) { \
	int __vo__ = OBJECT(v); \
	v = coerceVector(v, (tp)); \
	if (__vo__) SET_OBJECT(v, 1); \
    } \
} while (0)

#define FIXUP_NULL_AND_CHECK_TYPES(v) do { \
    switch (TYPEOF(v)) { \
    case NILSXP: v = allocVector(INTSXP,0); break; \
    case CPLXSXP: case REALSXP: case INTSXP: case LGLSXP: break; \
    default: errorcall(call, "%s", _("non-numeric argument to binary operator")); \
    } \
} while (0)

attribute_hidden SEXP R_binary(SEXP call, SEXP op, SEXP xarg, SEXP yarg)
{
    ARITHOP_TYPE oper = (ARITHOP_TYPE) PRIMVAL(op);


    GCStackRoot<> x(xarg);
    GCStackRoot<> y(yarg);

    FIXUP_NULL_AND_CHECK_TYPES(x);
    FIXUP_NULL_AND_CHECK_TYPES(y);

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    bool
	xS4 = isS4(x),
	yS4 = isS4(y),
	xarray = isArray(x),
	yarray = isArray(y),
	xts = isTs(x),
	yts = isTs(y);

#define R_ARITHMETIC_ARRAY_1_SPECIAL

#ifdef R_ARITHMETIC_ARRAY_1_SPECIAL
    /* If either x or y is a matrix with length 1 and the other is a
       vector of a different length, we want to coerce the matrix to be a vector.
       Do we want to?  We don't do it!  BDR 2004-03-06

       From 3.4.0 (Sep. 2016), this signals a warning,
       and in the future we will disable these 2 clauses,
       so it will give an error.
    */

    /* FIXME: Danger Will Robinson.
     * -----  We might be trashing arguments here.
     */
    if (xarray != yarray) {
    	if (xarray && nx==1 && ny!=1) {
	    if(ny != 0)
		warningcall(call, "%s", _(
	"Recycling array of length 1 in array-vector arithmetic is deprecated.\n\
  Use c() or as.vector() instead."));
    	    x = duplicate(x);
    	    setAttrib(x, R_DimSymbol, R_NilValue);
    	}
    	if (yarray && ny==1 && nx!=1) {
	    if(nx != 0)
		warningcall(call, "%s", _(
	"Recycling array of length 1 in vector-array arithmetic is deprecated.\n\
  Use c() or as.vector() instead."));
    	    y = duplicate(y);
    	    setAttrib(y, R_DimSymbol, R_NilValue);
    	}
    }
#endif

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
    /* need to preserve object here, as *_binary copies class attributes */
    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
	COERCE_IF_NEEDED(x, CPLXSXP);
	COERCE_IF_NEEDED(y, CPLXSXP);
	val = complex_binary(oper, x, y);
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	/* real_binary can handle REALSXP or INTSXP operand, but not LGLSXP. */
	/* Can get a LGLSXP. In base-Ex.R on 24 Oct '06, got 8 of these. */
	if (TYPEOF(x) != INTSXP) COERCE_IF_NEEDED(x, REALSXP);
	if (TYPEOF(y) != INTSXP) COERCE_IF_NEEDED(y, REALSXP);
	val = real_binary(oper, x, y);
    }
    else val = integer_binary(oper, x, y, call);

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

    if (xS4 || yS4) {   /* Only set the bit:  no method defined! */
	val = asS4(val, TRUE, TRUE); // from objects.c
    }

    return val;
}

attribute_hidden SEXP R_unary(SEXP call, SEXP op, SEXP s1)
{
    ARITHOP_TYPE operation = (ARITHOP_TYPE) PRIMVAL(op);
    switch (TYPEOF(s1)) {
    case LGLSXP:
	return logical_unary(operation, s1, call);
    case INTSXP:
	return integer_unary(operation, s1, call);
    case REALSXP:
	return real_unary(operation, s1, call);
    case CPLXSXP:
	return complex_unary(operation, s1, call);
    default:
	errorcall(call, "%s", _("invalid argument to unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP logical_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    R_xlen_t n = XLENGTH(s1);
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    SEXP names = PROTECT(getAttrib(s1, R_NamesSymbol));
    SEXP dim = PROTECT(getAttrib(s1, R_DimSymbol));
    SEXP dimnames = PROTECT(getAttrib(s1, R_DimNamesSymbol));
    if(names != R_NilValue) setAttrib(ans, R_NamesSymbol, names);
    if(dim != R_NilValue) setAttrib(ans, R_DimSymbol, dim);
    if(dimnames != R_NilValue) setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(3);

    int *pa = INTEGER(ans);
    const int *px = LOGICAL_RO(s1);

    switch (code) {
    case PLUSOP:
	for (R_xlen_t  i = 0; i < n; i++) pa[i] = px[i];
	break;
    case MINUSOP:
	for (R_xlen_t  i = 0; i < n; i++) {
	    int x = px[i];
	    pa[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	break;
    default:
	errorcall(call, "%s", _("invalid unary operator"));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP integer_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    R_xlen_t i, n;
    SEXP ans;

    switch (code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	{
	ans = NO_REFERENCES(s1) ? s1 : duplicate(s1);
	int *pa = INTEGER(ans);
	const int *px = INTEGER_RO(s1);
	n = XLENGTH(s1);
	for (i = 0; i < n; i++) {
	    int x = px[i];
	    pa[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	return ans;
	}
    default:
	errorcall(call, "%s", _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP real_unary(ARITHOP_TYPE code, SEXP s1, SEXP lcall)
{
    R_xlen_t i, n;
    SEXP ans;

    switch (code) {
    case PLUSOP: return s1;
    case MINUSOP:
	{
	ans = NO_REFERENCES(s1) ? s1 : duplicate(s1);
	double *pa = REAL(ans);
	const double *px = REAL_RO(s1);
	n = XLENGTH(s1);
	for (i = 0; i < n; i++)
	    pa[i] = -px[i];
	return ans;
	}
    default:
	errorcall(lcall, "%s", _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP integer_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2, SEXP lcall)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    int x1, x2;
    SEXP ans;
    bool naflag = false;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) n = 0; else n = (n1 > n2) ? n1 : n2;

    if (code == DIVOP || code == POWOP)
	ans = allocVector(REALSXP, n);
    else
	ans = R_allocOrReuseVector(s1, s2, INTSXP, n);
    if (n == 0) return ans;
    PROTECT(ans);

    switch (code) {
    case PLUSOP:
	{
	    int *pa = INTEGER(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    pa[i] = R_integer_plus(x1, x2, &naflag);
		});
	    if (naflag)
		warningcall(lcall, "%s", INTEGER_OVERFLOW_WARNING);
	}
	break;
    case MINUSOP:
	{
	    int *pa = INTEGER(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    pa[i] = R_integer_minus(x1, x2, &naflag);
		});
	    if (naflag)
		warningcall(lcall, "%s", INTEGER_OVERFLOW_WARNING);
	}
	break;
    case TIMESOP:
	{
	    int *pa = INTEGER(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    pa[i] = R_integer_times(x1, x2, &naflag);
		});
	    if (naflag)
		warningcall(lcall, "%s", INTEGER_OVERFLOW_WARNING);
	}
	break;
    case DIVOP:
	{
	    double *pa = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    pa[i] = R_integer_divide(x1, x2);
		});
	}
	break;
    case POWOP:
	{
	    double *pa = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    if((x1 = px1[i1]) == 1 || (x2 = px2[i2]) == 0)
			pa[i] = 1.;
		    else if (x1 == NA_INTEGER || x2 == NA_INTEGER)
			pa[i] = NA_REAL;
		    else
			pa[i] = R_POW((double) x1, (double) x2);
		});
	}
	break;
    case MODOP:
	{
	    int *pa = INTEGER(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
			pa[i] = NA_INTEGER;
		    else {
			pa[i] = /* till 0.63.2:	x1 % x2 */
			    (x1 >= 0 && x2 > 0) ? x1 % x2 :
			    (int)myfmod((double)x1,(double)x2);
		    }
		});
	}
	break;
    case IDIVOP:
	{
	    int *pa = INTEGER(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		    x1 = px1[i1];
		    x2 = px2[i2];
		    /* This had x %/% 0 == 0 prior to 2.14.1, but
		       it seems conventionally to be undefined */
		    if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
			pa[i] = NA_INTEGER;
		    else
			pa[i] = (int) floor((double)x1 / (double)x2);
		});
	}
	break;
    }
    UNPROTECT(1);

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (ans != s2 && n == n2 && ATTRIB(s2) != R_NilValue)
	copyMostAttrib(s2, ans);
    if (ans != s1 && n == n1 && ATTRIB(s1) != R_NilValue)
	copyMostAttrib(s1, ans); /* Done 2nd so s1's attrs overwrite s2's */

    return ans;
}

#define R_INTEGER(x) (double) ((x) == NA_INTEGER ? NA_REAL : (x))

static SEXP real_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    /* Note: "s1" and "s2" are protected above. */
    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);

    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) return (allocVector(REALSXP, 0));

    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = R_allocOrReuseVector(s1, s2, REALSXP, n));

    switch (code) {
    case PLUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *dx = REAL_RO(s1);
	    const double *dy = REAL_RO(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] + tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp + dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] + dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] + dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_INTEGER(px1[i1]) + px2[i2];);
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = px1[i1] + R_INTEGER(px2[i2]););
	}
	break;
    case MINUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *dx = REAL_RO(s1);
	    const double *dy = REAL_RO(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] - tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp - dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] - dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] - dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_INTEGER(px1[i1]) - px2[i2];);
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = px1[i1] - R_INTEGER(px2[i2]););
	}
	break;
    case TIMESOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *dx = REAL_RO(s1);
	    const double *dy = REAL_RO(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] * tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp * dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] * dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] * dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_INTEGER(px1[i1]) * px2[i2];);
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = px1[i1] * R_INTEGER(px2[i2]););
	}
	break;
    case DIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *dx = REAL_RO(s1);
	    const double *dy = REAL_RO(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] / tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp / dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] / dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] / dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_INTEGER(px1[i1]) / px2[i2];);
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = px1[i1] / R_INTEGER(px2[i2]););
	}
	break;
    case POWOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *dx = REAL_RO(s1);
	    const double *dy = REAL_RO(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(dx[i], tmp););
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(tmp, dy[i]););
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(dx[i], dy[i]););
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = R_POW(dx[i1], dy[i2]););
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_POW( R_INTEGER(px1[i1]), px2[i2]););
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = R_POW(px1[i1], R_INTEGER(px2[i2])););
	}
	break;
    case MODOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfmod(px1[i1], px2[i2]););
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfmod(R_INTEGER(px1[i1]), px2[i2]););
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfmod(px1[i1], R_INTEGER(px2[i2])););
	}
	break;
    case IDIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfloor(px1[i1], px2[i2]););
	}
	else if(TYPEOF(s1) == INTSXP ) {
	    double *da = REAL(ans);
	    const int *px1 = INTEGER_RO(s1);
	    const double *px2 = REAL_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfloor(R_INTEGER(px1[i1]), px2[i2]););
	}
	else if(TYPEOF(s2) == INTSXP ) {
	    double *da = REAL(ans);
	    const double *px1 = REAL_RO(s1);
	    const int *px2 = INTEGER_RO(s2);
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			       da[i] = myfloor(px1[i1], R_INTEGER(px2[i2])););
	}
	break;
    }
    UNPROTECT(1);

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (ans != s2 && n == n2 && ATTRIB(s2) != R_NilValue)
	copyMostAttrib(s2, ans);
    if (ans != s1 && n == n1 && ATTRIB(s1) != R_NilValue)
	copyMostAttrib(s1, ans); /* Done 2nd so s1's attrs overwrite s2's */

    return ans;
}


/* Mathematical Functions of One Argument */

static SEXP math1(SEXP sa, double (*f)(double), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, n;
    bool naflag;

    if (!isNumeric(sa))
	errorcall(lcall, "%s", R_MSG_NONNUM_MATH);

    n = XLENGTH(sa);
    /* coercion can lose the object bit */
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sy = NO_REFERENCES(sa) ? sa : allocVector(REALSXP, n));
    const double *a = REAL_RO(sa);
    double *y = REAL(sy);
    naflag = 0;
    for (i = 0; i < n; i++) {
	double x = a[i]; /* in case y == a */
	/* This code assumes that ISNAN(x) implies ISNAN(f(x)), so we
	   only need to check ISNAN(x) if ISNAN(f(x)) is true. */
	y[i] = f(x);
	if (ISNAN(y[i])) {
	    if (ISNAN(x))
		y[i] = x; /* make sure the incoming NaN is preserved */
	    else
		naflag = 1;
	}
    }
    /* These are primitives, so need to use the call */
    if(naflag) warningcall(lcall, "%s", R_MSG_NA);

    if (sa != sy && ATTRIB(sa) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(sy, sa);
    UNPROTECT(2);
    return sy;
}


attribute_hidden SEXP do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    {
        auto dgroup = DispatchGroup("Math", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }

    if (isComplex(CAR(args)))
	return complex_math1(call, op, args, env);

#define MATH1(x) math1(CAR(args), x, call);
    switch (PRIMVAL(op)) {
    case 1: return MATH1(floor);
    case 2: return MATH1(ceil);
    case 3: return MATH1(sqrt);
    case 4: return MATH1(sign);
	/* case 5: return MATH1(trunc); separate from 2.6.0 */

    case 10: return MATH1(exp);
    case 11: return MATH1(expm1);
    case 12: return MATH1(log1p);

    case 20: return MATH1(cos);
    case 21: return MATH1(sin);
    case 22: return MATH1(tan);
    case 23: return MATH1(acos);
    case 24: return MATH1(asin);
    case 25: return MATH1(atan);

    case 30: return MATH1(cosh);
    case 31: return MATH1(sinh);
    case 32: return MATH1(tanh);
    case 33: return MATH1(acosh);
    case 34: return MATH1(asinh);
    case 35: return MATH1(atanh);

    case 40: return MATH1(lgammafn);
    case 41: return MATH1(gammafn);

    case 42: return MATH1(digamma);
    case 43: return MATH1(trigamma);
	/* case 44: return MATH1(tetragamma);
	   case 45: return MATH1(pentagamma);
	   removed in 2.0.0 -- rather use Math2's psigamma()

	   case 46: return MATH1(Rf_gamma_cody); removed in 2.8.0
	*/
/* not yet:
    case 44: return MATH1(factorial);
*/

    case 47: return MATH1(cospi);
    case 48: return MATH1(sinpi);
    case 49: return MATH1(Rtanpi);// our own in any case

    default:
	errorcall(call, "%s", _("unimplemented real function of 1 argument"));
    }
    return R_NilValue; /* never used; to keep -Wall happy */
}

/* methods are allowed to have more than one arg */
attribute_hidden SEXP do_trunc(SEXP call, SEXP op, SEXP args, SEXP env)
{
    {
        auto dgroup = DispatchGroup("Math", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }
    // checkArity(op, args); /* is -1 in names.c */
    check1arg(args, call, "x");
    if (isComplex(CAR(args)))
	errorcall(call, "%s", _("unimplemented complex function"));
    return math1(CAR(args), trunc, call);
}

/*
   Note that this is slightly different from the do_math1 set,
   both for integer/logical inputs and what it dispatches to for complex ones.
*/

attribute_hidden SEXP do_abs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, s = R_NilValue /* -Wall */;

    checkArity(op, args);
    check1arg(args, call, "x");
    x = CAR(args);

    {
        auto dgroup = DispatchGroup("Math", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }

    if (isInteger(x) || isLogical(x)) {
	/* integer or logical ==> return integer,
	   factor was covered by Math.factor. */
	R_xlen_t n = XLENGTH(x);
	s = (NO_REFERENCES(x) && TYPEOF(x) == INTSXP) ?
	    x : allocVector(INTSXP, n);
	PROTECT(s);
	/* Note: relying on INTEGER(.) === LOGICAL(.) : */
	int *pa = INTEGER(s);
	const int *px = INTEGER_RO(x);
	for (R_xlen_t i = 0 ; i < n ; i++) {
	    int xi = px[i];
	    pa[i] = (xi == NA_INTEGER) ? xi : abs(xi);
	}
    } else if (TYPEOF(x) == REALSXP) {
	R_xlen_t n = XLENGTH(x);
	PROTECT(s = NO_REFERENCES(x) ? x : allocVector(REALSXP, n));
	double *pa = REAL(s);
	const double *px = REAL_RO(x);
	for (R_xlen_t i = 0 ; i < n ; i++)
	    pa[i] = fabs(px[i]);
    } else if (isComplex(x)) {
	SET_TAG(args, R_NilValue); /* cmathfuns want "z"; we might have "x" PR#16047 */
	return do_cmathfuns(call, op, args, env);
    } else
	errorcall(call, "%s", R_MSG_NONNUM_MATH);

    if (x != s && ATTRIB(x) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(s, x);
    UNPROTECT(1);
    return s;
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 or 2 int) */

/* math2_1 and math2_2 and related can be removed  once the byte
  compiler knows how to optimize to .External rather than
  .Internal */

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;

static SEXP math2(SEXP sa, SEXP sb, double (*f)(double, double),
		  SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *y;
    const double *a, *b;

    /* for 0-length a we want the attributes of a,
       as no recycling will occur */
#define SETUP_Math2					\
    if (!isNumeric(sa) || !isNumeric(sb))		\
	errorcall(lcall, "%s", R_MSG_NONNUM_MATH);	\
							\
    na = XLENGTH(sa);					\
    nb = XLENGTH(sb);					\
    if ((na == 0) || (nb == 0))	{			\
	PROTECT(sy = allocVector(REALSXP, 0));		\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
	UNPROTECT(1);					\
	return(sy);					\
    }							\
    n = (na < nb) ? nb : na;				\
    PROTECT(sa = coerceVector(sa, REALSXP));		\
    PROTECT(sb = coerceVector(sb, REALSXP));		\
    PROTECT(sy = allocVector(REALSXP, n));		\
    a = REAL_RO(sa);					\
    b = REAL_RO(sb);					\
    y = REAL(sy);					\
    int naflag = 0

    SETUP_Math2;

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math2					\
    if(naflag) warning("%s", R_MSG_NA);			\
    if      (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI,
		    double (*f)(double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *y;
    const double *a, *b;

    SETUP_Math2;
    int m_opt = asInteger(sI);

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, m_opt);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *y;
    const double *a, *b;

    SETUP_Math2;
    int i_1 = asInteger(sI1),
	i_2 = asInteger(sI2);

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math2;
    return sy;
} /* math2_2() */

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math2B(SEXP sa, SEXP sb, double (*f)(double, double, double *),
		   SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *y;
    const double *a, *b;
    double amax, *work;
    size_t nw;

#define besselJY_max_nu 1e7

    SETUP_Math2;

    /* allocate work array for BesselJ, BesselY large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (amax < av)
	    amax = av;
    }
    if (amax > besselJY_max_nu)
	amax = besselJY_max_nu; // and warning will happen in ../nmath/bessel_[jy].c
    CXXR::RAllocStack::Scope rscope;
    nw = 1 + (size_t)floor(amax);
    work = (double *) R_alloc(nw, sizeof(double));

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math2;

    return sy;
} /* math2B() */

#define Math2(A, FUN)	  math2(CAR(A), CADR(A), FUN, call);
#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN, call);
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call)
#define Math2B(A, FUN)   math2B(CAR(A), CADR(A), FUN, call);

attribute_hidden SEXP do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* For .Internals, fix up call so errorcall() behaves like error(). */
    if (TYPEOF(CAR(call)) == SYMSXP && INTERNAL(CAR(call)) == op)
	call = R_CurrentExpression;

    checkArity(op, args);

    if (isComplex(CAR(args)) ||
	(PRIMVAL(op) == 0 && isComplex(CADR(args))))
	return complex_math2(call, op, args, env);


    switch (PRIMVAL(op)) {

    case  0: return Math2(args, atan2);
    case 10001: return Math2(args, fround);// round(),  ../nmath/fround.c
    case 10004: return Math2(args, fprec); // signif(), ../nmath/fprec.c

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case  6: return Math2_1(args, dchisq);
    case  7: return Math2_2(args, pchisq);
    case  8: return Math2_2(args, qchisq);

    case  9: return Math2_1(args, dexp);
    case 10: return Math2_2(args, pexp);
    case 11: return Math2_2(args, qexp);

    case 12: return Math2_1(args, dgeom);
    case 13: return Math2_2(args, pgeom);
    case 14: return Math2_2(args, qgeom);

    case 15: return Math2_1(args, dpois);
    case 16: return Math2_2(args, ppois);
    case 17: return Math2_2(args, qpois);

    case 18: return Math2_1(args, dt);
    case 19: return Math2_2(args, pt);
    case 20: return Math2_2(args, qt);

    case 21: return Math2_1(args, dsignrank);
    case 22: return Math2_2(args, psignrank);
    case 23: return Math2_2(args, qsignrank);

    case 24: return Math2B(args, bessel_j_ex);
    case 25: return Math2B(args, bessel_y_ex);
    case 26: return Math2(args, psigamma);

    default:
	error(_("unimplemented real function of %d numeric arguments"), 2);
    }
    return R_NilValue;			/* never used; to keep -Wall happy */
}


// matching of (x, ...) signature.
// This signature is used for round() to support the POSIXt method.
// match for 'x' will be in the first result cell (R_MissingArg for no match).
// Any additional arguments are appended to the cell for the 'x' argument.
#define OPTIMIZE_MATCH
static R_INLINE SEXP match_round_gen_args(SEXP args, SEXP call)
{
#ifdef OPTIMIZE_MATCH
    // for now this optimization is worth while to avoid matchArgs
    // overhead in common cases
    static SEXP R_x_Symbol = NULL;
    if (R_x_Symbol == NULL)
        R_x_Symbol = install("x");
    if (args != R_NilValue &&           // at least one arg
        TAG(args) == R_NilValue &&      // first arg is not named
        TAG(CDR(args)) != R_x_Symbol && // second arg, if any, is not named 'x'
        CDDR(args) == R_NilValue)       // no mare than two arguments
        return args;
#endif
    static SEXP round_gen_formals = NULL;
    if (round_gen_formals == NULL)
        round_gen_formals = allocFormalsList2(install("x"), R_DotsSymbol);
    args = matchArgs_NR(round_gen_formals, args, call);

    // merge ... result back into args
    SEXP rest = CADR(args);
    if (rest == R_MissingArg)
        rest = R_NilValue;
    else if (TYPEOF(rest) == DOTSXP)
        SET_TYPEOF(rest, LISTSXP);
    else error("%s", _("matchArg returned something weird"));
    SETCDR(args, rest);
    return args;
}

static R_INLINE SEXP match_Math2_dflt_args(SEXP args, SEXP call)
{
#ifdef OPTIMIZE_MATCH
    // for now this optimization is worth while to avoid matchArgs
    // overhead in common cases
    if (args == R_NilValue)
        return list2(R_MissingArg, R_MissingArg);
    else if (TAG(args) == R_NilValue &&
             CDR(args) == R_NilValue) {
        SETCDR(args, CONS_NR(R_MissingArg, R_NilValue));
        return args;
    }
    else if (CDR(args) != R_NilValue &&
             CDDR(args) == R_NilValue &&
             TAG(args) == R_NilValue &&
             TAG(CDR(args)) == R_NilValue)
        return args;
#endif
    static SEXP do_Math2_dflt_formals = NULL;
    if (do_Math2_dflt_formals == NULL)
        do_Math2_dflt_formals = allocFormalsList2(install("x"),
                                                  install("digits"));
    return matchArgs_NR(do_Math2_dflt_formals, args, call);
}

/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
attribute_hidden SEXP do_Math2(SEXP call, SEXP op, SEXP args_, SEXP env)
{
    SEXP res = R_NilValue, call2;
    int is_signif = (PRIMVAL(op) == 10004);
    double dflt_digits = is_signif ? 6.0 : 0.;

    GCStackRoot<> args;
    args = evalListKeepMissing(args_, env);

    if (is_signif) {
        args = match_Math2_dflt_args(args, call);

        if (CADR(args) == R_MissingArg)
            SETCADR(args, ScalarReal(dflt_digits));
    }
    else
        args = match_round_gen_args(args, call);

    R_args_enable_refcnt(args);
    PROTECT(call2 = LCONS(CAR(call), args));

    auto dgroup = DispatchGroup("Math", call2, op, args, env);
    if (dgroup.first)
        res = dgroup.second;

    SETCDR(call2, R_NilValue); /* clear refcnt on args */
    R_try_clear_args_refcnt(args);
    UNPROTECT(1); /* call2 */

    if (!dgroup.first) {
        if (!is_signif) {
            args = match_Math2_dflt_args(args, call);

            if (CADR(args) == R_MissingArg)
                SETCADR(args, ScalarReal(dflt_digits));
        }

        if (CAR(args) == R_MissingArg)
	    R_MissingArgError_c("x", call, "MathMissingError");

        if (xlength(CADR(args)) == 0)
            errorcall(call, "%s", _("invalid second argument of length 0"));

        res = do_math2(call, op, args, env);
    }

    return res;
}

/* log{2,10}() builtins : */
attribute_hidden SEXP do_log1arg(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res = R_NilValue, tmp = R_NilValue /* -Wall */;
    GCStackRoot<> call2, args2;

    checkArity(op, args);
    check1arg(args, call, "x");

    {
        auto dgroup = DispatchGroup("Math", call, op, args, env);
        if (dgroup.first)
            return dgroup.second;
    }

    SEXP sLog = install("log");
    if(PRIMVAL(op) == 10) tmp = ScalarReal(10.0);
    if(PRIMVAL(op) == 2)  tmp = ScalarReal(2.0);

    call2 = lang3(sLog, CAR(args), tmp);
    args2 = lang2(CAR(args), tmp);
    auto dgroup = DispatchGroup("Math", call2, op, args2, env);
    if (dgroup.first) res = dgroup.second;
    if (!dgroup.first) {
	if (isComplex(CAR(args)))
	    res = complex_math2(call2, op, args2, env);
	else
	    res = math2(CAR(args), tmp, logbase, call);
    }

    return res;
}

#ifdef M_E
# define DFLT_LOG_BASE M_E
#else
# define DFLT_LOG_BASE exp(1.)
#endif

/* do_log is a primitive SPECIALSXP with internal argument
   matching. do_log_builtin is the BUILTIN version that expects
   evaluated arguments to be passed as 'args', expect that these may
   contain missing arguments.  */
attribute_hidden SEXP do_log(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = evalListKeepMissing(args, env);
    return  do_log_builtin(call, op, args, env);
}

attribute_hidden SEXP do_log_builtin(SEXP call, SEXP op, SEXP args_, SEXP env)
{
    GCStackRoot<> args(args_);
    int n = length(args);
    SEXP res = R_NilValue;

    if (n == 1 && TAG(args) == R_NilValue) {
	/* log(x) is handled here */
	SEXP x = CAR(args);
	if (x != R_MissingArg && !OBJECT(x)) {
	    if (isComplex(x))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(x, R_log, call);
	    return res;
	}
    }
    else if (n == 2 &&
	     TAG(args) == R_NilValue &&
	     (TAG(CDR(args)) == R_NilValue || TAG(CDR(args)) == R_BaseSymbol)) {
	/* log(x, y) or log(x, base = y) are handled here */
	SEXP x = CAR(args);
	SEXP y = CADR(args);
	if (x != R_MissingArg && y != R_MissingArg &&
	    !OBJECT(x) && !OBJECT(y)) {
	    if (isComplex(x) || isComplex(y))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(x, y, logbase, call);
	    return res;
	}
    }

    static SEXP do_log_formals = NULL;
    static SEXP R_x_Symbol = NULL;
    if (do_log_formals == NULL) {
	R_x_Symbol = install("x");
	do_log_formals = allocFormalsList2(R_x_Symbol, R_BaseSymbol);
    }

    if (n == 1) {
	if (CAR(args) == R_MissingArg ||
	    (TAG(args) != R_NilValue && TAG(args) != R_x_Symbol))
	    R_MissingArgError_c("x", call, "log1Error");

	auto dgroup = DispatchGroup("Math", call, op, args, env);
	if (dgroup.first) res = dgroup.second;
	if (!dgroup.first) {
	    if (isComplex(CAR(args)))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(CAR(args), R_log, call);
	}
	return res;
    }
    else {
	/* match argument names if supplied */
	/* will signal an error unless there are one or two arguments */
	/* after the match, length(args) will be 2 */
	args = matchArgs_NR(do_log_formals, args, call);

	if (CAR(args) == R_MissingArg)
	    R_MissingArgError_c("x", call, "log2Error");
	if (CADR(args) == R_MissingArg)
	    SETCADR(args, ScalarReal(DFLT_LOG_BASE));

	auto dgroup = DispatchGroup("Math", call, op, args, env);
	if (dgroup.first) res = dgroup.second;
	if (!dgroup.first) {
	    if (length(CADR(args)) == 0)
		errorcall(call, "%s", _("invalid argument 'base' of length 0"));
	    if (isComplex(CAR(args)) || isComplex(CADR(args)))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(CAR(args), CADR(args), logbase, call);
	}

	return res;
    }
}


/* Mathematical Functions of Three (Real) Arguments (plus 1 or 2 int) */

/* math3_1 and math3_2 and related can be removed once the byte
  compiler knows how to optimize to .External rather than
  .Internal */

#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	error("%s", R_MSG_NONNUM_MATH);			        \
								\
    na = XLENGTH(sa);						\
    nb = XLENGTH(sb);						\
    nc = XLENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0)) {			\
	/* for 0-length a we want the attributes of a: */	\
	PROTECT(sy = allocVector(REALSXP, 0));			\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);		\
	UNPROTECT(1);						\
	return(sy);						\
    }								\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    const double *a = REAL_RO(sa),				\
	*b = REAL_RO(sb),					\
	*c = REAL_RO(sc);					\
    y = REAL(sy);						\
    int naflag = 0

#define FINISH_Math3					\
    if(naflag) warning("%s", R_MSG_NA);			\
							\
    if      (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    UNPROTECT(4)

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *y;

    SETUP_Math3;
    int i_1 = asInteger(sI);

    MOD_ITERATE3(n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *y;

    SETUP_Math3;
    int i_1 = asInteger(sI),
	i_2 = asInteger(sJ);

    MOD_ITERATE3 (n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    return sy;
} /* math3_2 */

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math3B(SEXP sa, SEXP sb, SEXP sc,
		   double (*f)(double, double, double, double *), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *y;
    double amax, *work;
    size_t nw;

    SETUP_Math3;

    /* allocate work array for BesselI, BesselK large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (av > amax) amax = av;
    }
    CXXR::RAllocStack::Scope rscope;
    nw = 1 + (size_t)floor(amax);
    work = (double *) R_alloc(nw, sizeof(double));

    MOD_ITERATE3 (n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;

    return sy;
} /* math3B */

#define Math3_1(A, FUN)	math3_1(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call);
#define Math3_2(A, FUN) math3_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN, call)
#define Math3B(A, FUN)  math3B (CAR(A), CADR(A), CADDR(A), FUN, call);

attribute_hidden SEXP do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    switch (PRIMVAL(op)) {

    case  1:  return Math3_1(args, dbeta);
    case  2:  return Math3_2(args, pbeta);
    case  3:  return Math3_2(args, qbeta);

    case  4:  return Math3_1(args, dbinom);
    case  5:  return Math3_2(args, pbinom);
    case  6:  return Math3_2(args, qbinom);

    case  7:  return Math3_1(args, dcauchy);
    case  8:  return Math3_2(args, pcauchy);
    case  9:  return Math3_2(args, qcauchy);

    case 10:  return Math3_1(args, df);
    case 11:  return Math3_2(args, pf);
    case 12:  return Math3_2(args, qf);

    case 13:  return Math3_1(args, dgamma);
    case 14:  return Math3_2(args, pgamma);
    case 15:  return Math3_2(args, qgamma);

    case 16:  return Math3_1(args, dlnorm);
    case 17:  return Math3_2(args, plnorm);
    case 18:  return Math3_2(args, qlnorm);

    case 19:  return Math3_1(args, dlogis);
    case 20:  return Math3_2(args, plogis);
    case 21:  return Math3_2(args, qlogis);

    case 22:  return Math3_1(args, dnbinom);
    case 23:  return Math3_2(args, pnbinom);
    case 24:  return Math3_2(args, qnbinom);

    case 25:  return Math3_1(args, dnorm);
    case 26:  return Math3_2(args, pnorm);
    case 27:  return Math3_2(args, qnorm);

    case 28:  return Math3_1(args, dunif);
    case 29:  return Math3_2(args, punif);
    case 30:  return Math3_2(args, qunif);

    case 31:  return Math3_1(args, dweibull);
    case 32:  return Math3_2(args, pweibull);
    case 33:  return Math3_2(args, qweibull);

    case 34:  return Math3_1(args, dnchisq);
    case 35:  return Math3_2(args, pnchisq);
    case 36:  return Math3_2(args, qnchisq);

    case 37:  return Math3_1(args, dnt);
    case 38:  return Math3_2(args, pnt);
    case 39:  return Math3_2(args, qnt);

    case 40:  return Math3_1(args, dwilcox);
    case 41:  return Math3_2(args, pwilcox);
    case 42:  return Math3_2(args, qwilcox);

    case 43:  return Math3B(args, bessel_i_ex);
    case 44:  return Math3B(args, bessel_k_ex);

    case 45:  return Math3_1(args, dnbinom_mu);
    case 46:  return Math3_2(args, pnbinom_mu);
    case 47:  return Math3_2(args, qnbinom_mu);

    default:
	error(_("unimplemented real function of %d numeric arguments"), 3);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

/* This can be removed completely once the byte compiler knows how to
  optimize to .External rather than .Internal */

#define if_NA_Math4_set(y,a,b,c,d)				\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;

static SEXP math4(SEXP sa, SEXP sb, SEXP sc, SEXP sd,
		  double (*f)(double, double, double, double), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *y;

#define SETUP_Math4							\
    if (!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	error("%s", R_MSG_NONNUM_MATH);				        \
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0)) {		\
	/* for 0-length a we want the attributes of a: */		\
	PROTECT(sy = allocVector(REALSXP, 0));				\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);			\
	UNPROTECT(1);							\
	return(sy);							\
    }									\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    const double *a = REAL_RO(sa),					\
	*b = REAL_RO(sb),						\
	*c = REAL_RO(sc),						\
	*d = REAL_RO(sd);						\
    y = REAL(sy);							\
    int naflag = 0

    SETUP_Math4;

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math4					\
    if(naflag) warning("%s", R_MSG_NA);			\
							\
    if      (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) SHALLOW_DUPLICATE_ATTRIB(sy, sd);	\
    UNPROTECT(5)

    FINISH_Math4;

    return sy;
} /* math4() */

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)(double, double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *y;

    SETUP_Math4;
    int i_1 = asInteger(sI);

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *y;

    SETUP_Math4;
    int i_1 = asInteger(sI),
	i_2 = asInteger(sJ);

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math4;
    return sy;
} /* math4_2() */


#define Math4(A, FUN)   math4  (CAR(A), CADR(A), CADDR(A), CAD3R(A), FUN, call)
#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN, call)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN, call)


attribute_hidden SEXP do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);


    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- math4() at all! : */
    case -99: return Math4(args, (double (*)(double, double, double, double))NULL);

    case  1: return Math4_1(args, dhyper);
    case  2: return Math4_2(args, phyper);
    case  3: return Math4_2(args, qhyper);

    case  4: return Math4_1(args, dnbeta);
    case  5: return Math4_2(args, pnbeta);
    case  6: return Math4_2(args, qnbeta);
    case  7: return Math4_1(args, dnf);
    case  8: return Math4_2(args, pnf);
    case  9: return Math4_2(args, qnf);
#ifdef UNIMP
    case 10: return Math4_1(args, dtukey);
#endif
    case 11: return Math4_2(args, ptukey);
    case 12: return Math4_2(args, qtukey);
    default:
	error(_("unimplemented real function of %d numeric arguments"), 4);
    }
    return op;			/* never used; to keep -Wall happy */
}


#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e))	\
		y = NA_REAL;						\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	error("%s", R_MSG_NONNUM_MATH);				        \
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    ne = XLENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0)) { \
	/* ... TODO if(na == 0) do keep attributes of 'a' */		\
	return(allocVector(REALSXP, 0));				\
    }									\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    const double							\
	*a = REAL_RO(sa),						\
	*b = REAL_RO(sb),						\
	*c = REAL_RO(sc),						\
	*d = REAL_RO(sd),						\
	*e = REAL_RO(se);						\
    y = REAL(sy);							\
    int naflag = 0

    SETUP_Math5;

    MOD_ITERATE5 (n, na, nb, nc, nd, ne,
		  i, ia, ib, ic, id, ie, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = f(ai, bi, ci, di, ei);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math5					\
    if(naflag) warning("%s", R_MSG_NA);			\
							\
    if      (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) SHALLOW_DUPLICATE_ATTRIB(sy, sd);	\
    else if (n == ne) SHALLOW_DUPLICATE_ATTRIB(sy, se);	\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

attribute_hidden SEXP do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	error(_("unimplemented real function of %d numeric arguments"), 5);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */

/* This is used for experimenting with parallelized nmath functions -- LT */
/* Hide for now */
attribute_hidden CCODE R_get_arith_function(int which)
{
    switch (which) {
    case 1: return do_math1;
    case 2: return do_math2;
    case 3: return do_math3;
    case 4: return do_math4;
    case 11: return complex_math1;
    case 12: return complex_math2;
    default: error("%s", _("bad arith function index")); return NULL;
    }
}
