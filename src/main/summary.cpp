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

/** @file summary.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Minmax.h>
#include <CXXR/Complex.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Itermacros.h>

#include <cfloat> // for DBL_MAX

#include "duplicate.h"

using namespace std;
using namespace R;
using namespace CXXR;

#define R_MSG_type	_("invalid 'type' (%s) of argument")

	/* since INT_MIN is the NA_INTEGER value ! */
#define Int2Real(i)	((i == NA_INTEGER) ? NA_REAL : (double)i)

#ifdef DEBUG_sum
#define DbgP1(s) REprintf(s)
#define DbgP2(s,a) REprintf(s,a)
#define DbgP3(s,a,b) REprintf(s,a,b)
#else
#define DbgP1(s)
#define DbgP2(s,a)
#define DbgP3(s,a,b)
#endif

#ifdef LONG_INT // defined in Defn.h
# define isum_INT LONG_INT
static int isum(SEXP sx, isum_INT *value, bool narm, SEXP call)
{
    LONG_INT s = 0;  // at least 64-bit
    int updated = 0;
#ifdef LONG_VECTOR_SUPPORT
    int ii = R_INT_MIN; // need > 2^32 entries to overflow; checking earlier is a waste
/* NOTE: cannot use 64-bit *value to pass NA_INTEGER: that is "regular" 64bit int
 *      -> pass the NA information via return value ('updated').
 * After the first 2^32 entries, only check every 1000th time (related to GET_REGION_BUFSIZE=512 ?)
 * Assume LONG_INT_MAX >= 2^63-1 >=~ 9.223e18 >  (1000 * 9000..0L = 9 * 10^18)
 */
# define ISUM_OVERFLOW_CHECK do {					\
	if (ii++ > 1000) {						\
	    if (s > 9000000000000000L || s < -9000000000000000L) {	\
		DbgP2("|OVERFLOW triggered: s=%ld|", s);		\
		/* *value = s; no use, TODO continue from 'k' */	\
		return 42; /* was overflow, NA; now switch to irsum()*/ \
	    }								\
	    ii = 0;							\
	}								\
    } while (0)
#else
# define ISUM_OVERFLOW_CHECK do { } while(0)
#endif

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    if(!updated) updated = 1;
		    s += x[k];
		    ISUM_OVERFLOW_CHECK;
		} else if (!narm) {
		    // updated = NA_INTEGER;
		    return NA_INTEGER;
		}
	    }
	});
    *value = s;
    return updated;
#undef ISUM_OVERFLOW_CHECK
}
#else // no LONG_INT  : should never be used with a C99/C11 compiler
# define isum_INT int
static bool isum(SEXP sx, isum_INT *value, bool narm, SEXP call)
/* Version from R 3.0.0 */
{
    double s = 0.0;
    bool updated = false;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    if(!updated) updated = true;
		    s += x[k];
		} else if (!narm) {
		    if(!updated) updated = true;
		    *value = NA_INTEGER;
		    return updated;
		}
	    }
	});
    if(s > INT_MAX || s < R_INT_MIN){
	warningcall(call, "%s", _("integer overflow - use sum(as.numeric(.))"));
	*value = NA_INTEGER;
    }
    else *value = (int) s;

    return updated;
}
#endif

// Used instead of isum() for large vectors when overflow would occur:
static bool risum(SEXP sx, double *value, bool narm)
{
    LDOUBLE s = 0.0;
    bool updated = false;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    if(!updated) updated = true;
		    s += (double) x[k];
		} else if (!narm) {
		    if(!updated) updated = true;
		    *value = NA_REAL;
		    return updated;
		}
	    }
	});
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}


static bool rsum(SEXP sx, double *value, bool narm)
{
    LDOUBLE s = 0.0;
    bool updated = false;

    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (!narm || !ISNAN(x[k])) {
		    if(!updated) updated = true;
		    s += x[k];
		}
	    }
	});
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static bool csum(SEXP sx, Complex *value, bool narm)
{
    Complex *x = CXXR_COMPLEX(sx);
    R_xlen_t n = XLENGTH(sx);
    LDOUBLE sr = 0.0, si = 0.0;
    bool updated = false;

    for (R_xlen_t k = 0; k < n; k++) {
	if (!narm || (!ISNAN(x[k].r) && !ISNAN(x[k].i))) {
	    if(!updated) updated = true;
	    sr += x[k].r;
	    si += x[k].i;
	}
    }
    value->r = (double) sr;
    value->i = (double) si;

    return updated;
}

static bool imin(SEXP sx, int *value, bool narm)
{
    bool updated = false;
    int s = 0;

    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    if (!updated || s > x[k]) {
			s = x[k];
			if(!updated) updated = true;
		    }
		}
		else if (!narm) {
		    *value = NA_INTEGER;
		    return true;
		}
	    }
	});
    *value = s;
    return updated;
}

static bool rmin(SEXP sx, double *value, bool narm)
{
    double s = 0.0; /* -Wall */
    bool updated = false;

    /* s = R_PosInf; */
    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (ISNAN(x[k])) {/* Na(N) */
		    if (!narm) {
			if(!ISNA(s)) s = x[k]; /* so any NA trumps all NaNs */
			if(!updated) updated = true;
		    }
		}
		else if (!updated || x[k] < s) { /* Never true if s is NA/NaN */
		    s = x[k];
		    if(!updated) updated = true;
		}
	    }
	});
    *value = s;
    return updated;
}

static bool smin(SEXP x, SEXP *value, bool narm)
{
    SEXP s = NA_STRING; /* -Wall */
    bool updated = false;
    CXXR::RAllocStack::Scope rscope; // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) > 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = true;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return true;
	}
    }
    *value = s;

    return updated;
}

static bool imax(SEXP sx, int *value, bool narm)
{
    int s = 0 /* -Wall */;
    bool updated = false;

    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    if (!updated || s < x[k]) {
			s = x[k];
			if(!updated) updated = true;
		    }
		} else if (!narm) {
		    *value = NA_INTEGER;
		    return true;
		}
	    }
	});
    *value = s;
    return updated;
}

static bool rmax(SEXP sx, double *value, bool narm)
{
    double s = 0.0 /* -Wall */;
    bool updated = false;

    ITERATE_BY_REGION(sx, x, iii, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (ISNAN(x[k])) {/* Na(N) */
		    if (!narm) {
			if(!ISNA(s)) s = x[k]; /* so any NA trumps all NaNs */
			if(!updated) updated = true;
		    }
		}
		else if (!updated || x[k] > s) { /* Never true if s is NA/NaN */
		    s = x[k];
		    if(!updated) updated = true;
		}
	    }
	});
    *value = s;
    return updated;
}

static bool smax(SEXP x, SEXP *value, bool narm)
{
    SEXP s = NA_STRING; /* -Wall */
    bool updated = false;
    CXXR::RAllocStack::Scope rscope; // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) < 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = true;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return true;
	}
    }
    *value = s;

    return updated;
}

static bool iprod(SEXP sx, double *value, bool narm)
{
    LDOUBLE s = 1.0;
    bool updated = true;

    /**** assumes INTEGER(sx) and LOGICAL(sx) are identical!! */
    ITERATE_BY_REGION(sx, x, i, nbatch, int, INTEGER, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (x[k] != NA_INTEGER) {
		    s *= x[k];
		    if(!updated) updated = true;
		}
		else if (!narm) {
		    if(!updated) updated = true;
		    *value = NA_REAL;
		    return updated;
		}

		if(ISNAN(s)) {  /* how can this happen? */
		    *value = NA_REAL;
		    return updated;
		}
	    }
	});

    // This could over/underflow (does in package POT)
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static bool rprod(SEXP sx, double *value, bool narm)
{
    LDOUBLE s = 1.0;
    bool updated = false;

    ITERATE_BY_REGION(sx, x, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++) {
		if (!narm || !ISNAN(x[k])) {
		    if(!updated) updated = true;
		    s *= x[k];
		}
	    }
	});

    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static bool cprod(SEXP sx, Complex *value, bool narm)
{
    Complex *x = CXXR_COMPLEX(sx);
    R_xlen_t n = XLENGTH(sx);
    LDOUBLE sr = 1.0, si = 0.0;
    bool updated = false;
    for (R_xlen_t k = 0; k < n; k++) {
	if (!narm || (!ISNAN(x[k].r) && !ISNAN(x[k].i))) {
	    if(!updated) updated = true;
	    LDOUBLE tr = sr, ti = si;
	    sr = tr * x[k].r - ti * x[k].i;
	    si = tr * x[k].i + ti * x[k].r;
	}
    }
    value->r = (double) sr;
    value->i = (double) si;

    return updated;
}


attribute_hidden
SEXP R::fixup_NaRm(SEXP args)
{
    /* Need to make sure na.rm is last and exists */
    SEXP na_value = ScalarLogical(FALSE);
    bool seen_NaRm = false;
    for(SEXP a = args, prev = R_NilValue; a != R_NilValue; a = CDR(a)) {
	if(TAG(a) == R_NaRmSymbol) {
	    if(seen_NaRm)
	        error(_("formal argument \"%s\" matched by multiple actual arguments"),
		      "na.rm");
	    seen_NaRm = true;
	    if(CDR(a) == R_NilValue) return args;
	    na_value = CAR(a);
	    if(prev == R_NilValue) args = CDR(a);
	    else SETCDR(prev, CDR(a));
	}
	prev = a;
    }

    PROTECT(na_value);
    SEXP t = CONS(na_value, R_NilValue);
    UNPROTECT(1);
    PROTECT(t);
    SET_TAG(t, R_NaRmSymbol);
    if (args == R_NilValue)
	args = t;
    else {
	SEXP r = args;
	while (CDR(r) != R_NilValue) r = CDR(r);
	SETCDR(r, t);
    }
    UNPROTECT(1);
    return args;
}

/* do_summary provides a variety of data summaries
	op : 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod
 */
/* NOTE: mean() is rather different as only one arg and no na.rm, and
 * dispatch is from an R-level generic, this being a special case of
 * mean.default.
 */

static R_INLINE SEXP logical_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
	int xi = LOGICAL_ELT(x, i);
	if(xi == NA_LOGICAL)
	    return ScalarReal(R_NaReal);
	s += xi;
    }
    return ScalarReal((double) (s/n));
}

static R_INLINE SEXP integer_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    for (R_xlen_t i = 0; i < n; i++) {
	int xi = INTEGER_ELT(x, i);
	if(xi == NA_INTEGER)
	    return ScalarReal(R_NaReal);
	s += xi;
    }
    return ScalarReal((double) (s/n));
}

static R_INLINE SEXP real_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0;
    ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
	    for (R_xlen_t k = 0; k < nbatch; k++)
		s += dx[k];
	});
    bool finite_s = (R_FINITE((double) s) != 0); //isfinite returns non-zero
    if (finite_s) {
	s /= n;
	DbgP3("real_mean(): n=%g, s=%g\n", (double)n, s);
    } else { // infinite s, maybe just overflowed; try to use smaller terms:
	DbgP3("real_mean(): n=%g, infinite s=%g -- try again: ", (double)n, s);
	s = 0.;
	ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
		for (R_xlen_t k = 0; k < nbatch; k++)
		    s += dx[k]/n;
	    });
	DbgP2(" --> new s=%g\n", s);
    }
    if (finite_s && R_FINITE((double) s)) {
	LDOUBLE t = 0.0;
	ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
		for (R_xlen_t k = 0; k < nbatch; k++)
		    t += (dx[k] - s);
	    });
	s += t/n;
    }
    else if (R_FINITE((double) s)) { // was infinite: more careful
	LDOUBLE t = 0.0;
	ITERATE_BY_REGION(x, dx, i, nbatch, double, REAL, {
		for (R_xlen_t k = 0; k < nbatch; k++)
		    t += (dx[k] - s)/n;
	    });
	DbgP2(" s = s + t, t=%g\n", t);
	s += t;
    }
    return ScalarReal((double) s);
}

static R_INLINE SEXP complex_mean(SEXP x)
{
    R_xlen_t n = XLENGTH(x);
    LDOUBLE s = 0.0, si = 0.0;
    Complex *px = CXXR_COMPLEX(x);
    for (R_xlen_t i = 0; i < n; i++) {
	Complex xi = px[i];
	s += xi.r;
	si += xi.i;
    }
    s /= n; si /= n;
    if( R_FINITE((double)s) && R_FINITE((double)si) ) {
	LDOUBLE t = 0.0, ti = 0.0;
	for (R_xlen_t i = 0; i < n; i++) {
	    Complex xi = px[i];
	    t += xi.r - s;
	    ti += xi.i - si;
	}
	s += t/n; si += ti/n;
    }
    Complex val = { (double)s, (double)si };
    return ScalarComplex(val);
}

attribute_hidden SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if(PRIMVAL(op) == 1) { /* mean */
	SEXP x = CAR(args);
	switch(TYPEOF(x)) {
	case LGLSXP:  return logical_mean(x);
	case INTSXP:  return integer_mean(x);
	case REALSXP: return real_mean(x);
	case CPLXSXP: return complex_mean(x);
	default:
	    error(R_MSG_type, R_typeToChar(x));
	    return R_NilValue; // -Wall on clang 4.2
	}
    }

    SEXP ans, call2;
    /* match to foo(..., na.rm=FALSE) */
    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    R_args_enable_refcnt(args);
    SETCDR(call2, args);

    {
        auto dgroup = DispatchGroup("Summary", call2, op, args, env);
        if (dgroup.first) {
            UNPROTECT(2); /* call2, args */
            SETCDR(call2, R_NilValue); /* clear refcnt on args */
            R_try_clear_args_refcnt(args);
            return dgroup.second;
        }
    }
    UNPROTECT(1); /* call2 */
    SETCDR(call2, R_NilValue); /* clear refcnt on args */
    R_try_clear_args_refcnt(args);

#ifdef DEBUG_Summary
    REprintf("C do_summary(op%s, *): did NOT dispatch\n", PRIMNAME(op));
#endif

    ans = matchArgExact(R_NaRmSymbol, &args);
    bool narm = asBool2(ans, call);

    if (ALTREP(CAR(args)) && CDDR(args) == R_NilValue &&
	(CDR(args) == R_NilValue || TAG(CDR(args)) == R_NaRmSymbol)) {
	SEXP toret = NULL;
	SEXP vec = CAR(args);
	switch(PRIMVAL(op)) {
	case 0:
	    if(TYPEOF(vec) == INTSXP)
		toret = ALTINTEGER_SUM(vec, (Rboolean) narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_SUM(vec, (Rboolean) narm);
	    break; 
	case 2:
	    if(TYPEOF(vec) == INTSXP)
		toret = ALTINTEGER_MIN(vec, (Rboolean) narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_MIN(vec, (Rboolean) narm);
	    break;
	case 3:
	    if(TYPEOF(vec) == INTSXP)
		toret = ALTINTEGER_MAX(vec, (Rboolean) narm);
	    else if (TYPEOF(vec) == REALSXP)
		toret = ALTREAL_MAX(vec, (Rboolean) narm);
	    break;
	default:
	    break;
	}
	if(toret != NULL) {
	    UNPROTECT(1); /* args */
	    return toret;
	}
    }

    bool int_a, real_a, complex_a,
	empty = true;// <==> only zero-length arguments, or NA with na.rm=T
    int updated = 0; //
	/* updated = NA_INTEGER if encountered NA (in isum()),
	   updated != 0 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned;  */
    SEXP a;
    double tmp = 0.0, s;
    Complex ztmp, zcum={ 0.0, 0.0} /* -Wall */;
    int itmp = 0, icum = 0, warn = 0 /* dummy */;
    bool use_isum = true; // indicating if isum() should used; otherwise irsum()
    isum_INT iLtmp = (isum_INT)0, iLcum = iLtmp; // for isum() only
    SEXPTYPE ans_type;/* only INTEGER, REAL, COMPLEX or STRSXP here */

    int iop = PRIMVAL(op);
    switch(iop) {
    case 0:/* sum */
    /* we need to find out if _all_ the arguments are integer or logical
       in advance, as we might overflow before we find out.  NULL is
       documented to be the same as integer(0).
    */
	a = args;
        complex_a = real_a = false;
	while (a != R_NilValue) {
            switch(TYPEOF(CAR(a))) {
	    case INTSXP:
	    case LGLSXP:
	    case NILSXP:
		break;
	    case REALSXP:
		real_a = true;
		break;
	    case CPLXSXP:
		complex_a = true;
		break;
	    default:
		a = CAR(a);
		errorcall(call, R_MSG_type, R_typeToChar(a));
		return R_NilValue;
            }
	    a = CDR(a);
	}
        if(complex_a) {
            ans_type = CPLXSXP;
        } else if(real_a) {
            ans_type = REALSXP;
        } else {
            ans_type = INTSXP; iLcum = (isum_INT)0;
        }
	DbgP3("do_summary: sum(.. na.rm=%d): ans_type = %s\n",
	      narm, type2char(ans_type));
	zcum.r = zcum.i = 0.; icum = 0;
	break;

    case 2:/* min */
	DbgP2("do_summary: min(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_PosInf;
	icum = INT_MAX;
	break;

    case 3:/* max */
	DbgP2("do_summary: max(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_NegInf;;
	icum = R_INT_MIN;
	break;

    case 4:/* prod */
	ans_type = REALSXP;
	zcum.r = 1.;
	zcum.i = 0.;
	break;

    default:
	errorcall(call,
		  _("internal error ('op = %d' in do_summary).\t Call a Guru"),
		  iop);
	return R_NilValue;/*-Wall */
    }

    SEXP stmp = NA_STRING,
	 scum = PROTECT(NA_STRING);
    /*-- now loop over all arguments.  Do the 'op' switch INSIDE : */
    while (args != R_NilValue) {
	a = CAR(args);
	int_a = false;// int_a = true  <-->  a is INTEGER
	real_a = false;

	if(xlength(a) > 0) {
	    updated = 0;/*- GLOBAL -*/

	    switch(iop) {
	    case 2:/* min */
	    case 3:/* max */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    int_a = true;
		    if (iop == 2) updated = imin(a, &itmp, narm);
		    else	  updated = imax(a, &itmp, narm);
		    break;
		case REALSXP:
		    real_a = true;
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    if (iop == 2) updated = rmin(a, &tmp, narm);
		    else	  updated = rmax(a, &tmp, narm);
		    break;
		case STRSXP:
		    if(!empty && ans_type == INTSXP) {
			scum = StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if(!empty && ans_type == REALSXP) {
			scum = StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    if (iop == 2) updated = smin(a, &stmp, narm);
		    else updated = smax(a, &stmp, narm);
		    break;
		default:
		    errorcall(call, R_MSG_type, R_typeToChar(a));
		    return R_NilValue;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (icum == NA_INTEGER); /* NA trumps anything */
			else if (itmp == NA_INTEGER ||
			    (iop == 2 && itmp < icum) || /* min */
			    (iop == 3 && itmp > icum))   /* max */
			    icum = itmp;
		    } else if(ans_type == REALSXP) {
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNA(zcum.r)); /* NA trumps anything */
			else if (ISNAN(tmp)) {
			    if (ISNA(tmp)) zcum.r = tmp;
			    else zcum.r += tmp;/* NA or NaN */
			} else if(
			    (iop == 2 && tmp < zcum.r) ||
			    (iop == 3 && tmp > zcum.r))	zcum.r = tmp;
		    } else if(ans_type == STRSXP) {
			if(int_a)
			   stmp = StringFromInteger(itmp, &warn);
			else if(real_a)
			   stmp = StringFromReal(tmp, &warn);

			if(empty)
			    scum = stmp;
			else if (scum != NA_STRING) {
			    PROTECT(stmp);
			    if(stmp == NA_STRING ||
			       (iop == 2 && stmp != scum && Scollate(stmp, scum) < 0) ||
			       (iop == 3 && stmp != scum && Scollate(stmp, scum) > 0) )
				scum = stmp;
			    UNPROTECT(1); /* stmp */
			}
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		}/*updated*/ else {
		    /*-- in what cases does this happen here at all?
		      -- if there are no non-missing elements.
		     */
		    DbgP2(" NOT updated [!! RARE !!]: int_a=%s\n", int_a ? "TRUE" : "FALSE");
		}

		break;/*--- end of  min() / max() ---*/

	    case 0:/* sum */
		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
#ifdef LONG_INT
		    updated = (use_isum ?
			       isum(a, &iLtmp, narm, call) :
			       risum(a,  &tmp, narm));
		    DbgP2(" int|lgl: updated=%d ", updated);
		    if(updated == NA_INTEGER)
			goto na_answer;
		    else if(use_isum && updated == 42) {
			// impending integer overflow --> switch to irsum()
			use_isum = false;
			if(ans_type == INTSXP) ans_type = REALSXP;
			// re-sum() 'a' (a waste, rare; FIXME ?) :
			risum(a, &tmp, narm);
			zcum.r = (double) iLcum + tmp;
			DbgP3(" .. switching type to REAL, tmp=%g, zcum.r=%g",
			      tmp, zcum.r);
		    }
		    else if(updated) {
			// iLtmp is LONG_INT i.e. at least 64bit
			if(ans_type == INTSXP) {
			    s = (double) iLcum + (double) iLtmp;
			    if(s > INT_MAX || s < R_INT_MIN ||
			       iLtmp < -LONG_INT_MAX || LONG_INT_MAX < iLtmp) {
				ans_type = REALSXP;
				zcum.r = s;
				DbgP2(" int_1 switch: zcum.r = s = %g\n", s);
			    } else if(s < -(double)LONG_INT_MAX || (double)LONG_INT_MAX < s) {
				use_isum = false;
				ans_type = REALSXP;
				zcum.r = s;
				DbgP2(" int_2 switch: zcum.r = s = %g\n", s);
			    }
			    else {
				iLcum += iLtmp;
				DbgP3(" int_3: (iLtmp,iLcum) = (%ld,%ld)\n",
				      iLtmp, iLcum);
			    }
			} else { // dealt with NA_INTEGER already above
			    zcum.r += use_isum ? (double)iLtmp : tmp;
			    DbgP3(" dbl: (*tmp, zcum.r) = (%g,%g)\n",
				  use_isum ? (double)iLtmp : tmp, zcum.r);
			}
		    }
#else
		    updated = isum(a, &iLtmp, narm, call);
		    if(updated) {
			if(iLtmp == NA_INTEGER) goto na_answer;
			if(ans_type == INTSXP) {
			    s = (double) icum + (double) iLtmp;
			    if(s > INT_MAX || s < R_INT_MIN){
				warningcall(call,_(
				  "Integer overflow - use sum(as.numeric(.))"));
				goto na_answer;
			    }
			    else icum += iLtmp;
			} else
			    zcum.r += Int2Real(iLtmp);
		    }
#endif
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(iLcum);
		    }
		    updated = rsum(a, &tmp, narm);
		    if(updated) {
			zcum.r += tmp;
		    }
		    break;
		case CPLXSXP:
		    if(ans_type == INTSXP) {
			ans_type = CPLXSXP;
			if(!empty) zcum.r = Int2Real(iLcum);
		    } else if (ans_type == REALSXP)
			ans_type = CPLXSXP;
		    updated = csum(a, &ztmp, narm);
		    if(updated) {
			zcum.r += ztmp.r;
			zcum.i += ztmp.i;
		    }
		    break;
		default:
		    errorcall(call, R_MSG_type, R_typeToChar(a));
		    return R_NilValue;
		}

		break;/* sum() part */

	    case 4:/* prod */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		    if(TYPEOF(a) == REALSXP)
			updated = rprod(a, &tmp, narm);
		    else
			updated = iprod(a, &tmp, narm);
		    if(updated) {
			zcum.r *= tmp;
			zcum.i *= tmp;
		    }
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    updated = cprod(a, &ztmp, narm);
		    if(updated) {
			Complex z;
			z.r = zcum.r;
			z.i = zcum.i;
			zcum.r = z.r * ztmp.r - z.i * ztmp.i;
			zcum.i = z.r * ztmp.i + z.i * ztmp.r;
		    }
		    break;
		default:
		    errorcall(call, R_MSG_type, R_typeToChar(a));
		    return R_NilValue;
		}

		break;/* prod() part */

	    } /* switch(iop) */

	} else { /* len(a)=0 */
	    /* Even though this has length zero it can still be invalid,
	       e.g. list() or raw() */
	    switch(TYPEOF(a)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case NILSXP:  /* OK historically, e.g. PR#1283 */
		break;
	    case CPLXSXP:
		if (iop == 2 || iop == 3)
		    errorcall(call, R_MSG_type, R_typeToChar(a));
		return R_NilValue;
		break;
	    case STRSXP:
		if (iop == 2 || iop == 3) {
		    if (!empty && ans_type == INTSXP) {
			scum = StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if (!empty && ans_type == REALSXP) {
			scum = StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    break;
		}
	    default:
		errorcall(call, R_MSG_type, R_typeToChar(a));
		return R_NilValue;
	    }
	    if (ans_type < TYPEOF(a) && ans_type != CPLXSXP) {
		if(!empty && ans_type == INTSXP)
		    zcum.r = Int2Real(icum);
		ans_type = TYPEOF(a);
	    }
	}
	DbgP3(" .. upd.=%d, empty=%d", updated, (int)empty);
	if (empty && updated) empty = false;
	DbgP2(", new empty=%d\n", (int)empty);
	args = CDR(args);
    } /*-- while(..) loop over args */

    /*-------------------------------------------------------*/
    if (empty && (iop == 2 || iop == 3)) {
	if(ans_type == STRSXP) {
	    warningcall(call, "%s", _("no non-missing arguments, returning NA"));
	} else {
	    if (iop == 2)
		warningcall(call, "%s", _("no non-missing arguments to min; returning Inf"));
	    else
		warningcall(call, "%s", _("no non-missing arguments to max; returning -Inf"));
	    ans_type = REALSXP;
	}
    }

    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:   INTEGER(ans)[0] = (iop == 0) ? (int)iLcum : icum; break;
    case REALSXP:  REAL(ans)[0] = zcum.r; break;
    case CPLXSXP:  COMPLEX(ans)[0].r = zcum.r; COMPLEX(ans)[0].i = zcum.i;break;
    case STRSXP:   SET_STRING_ELT(ans, 0, scum); break;
    default: break;
    }
    UNPROTECT(2); /* scum, args */
    return ans;

na_answer: /* only sum(INTSXP, ...) case currently used */
    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = NA_INTEGER; break;
    case REALSXP:	REAL(ans)[0] = NA_REAL; break;
    case CPLXSXP:	COMPLEX(ans)[0].r = COMPLEX(ans)[0].i = NA_REAL; break;
    case STRSXP:        SET_STRING_ELT(ans, 0, NA_STRING); break;
    default: break;
    }
    UNPROTECT(2); /* scum, args */
    return ans;
}/* do_summary */


attribute_hidden SEXP do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a, b, prargs, call2;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    R_args_enable_refcnt(args);
    SETCDR(call2, args);

    {
        auto dgroup = DispatchGroup("Summary", call2, op, args, env);
        if (dgroup.first) {
            SETCDR(call2, R_NilValue); /* clear refcnt on args */
            R_try_clear_args_refcnt(args);
            UNPROTECT(2);
            return dgroup.second;
        }
    }
    UNPROTECT(1);
    SETCDR(call2, R_NilValue); /* clear refcnt on args */
    R_try_clear_args_refcnt(args);

    PROTECT(op = findFun(install("range.default"), env));
    PROTECT(prargs = promiseArgs(args, R_GlobalEnv));
    for (a = args, b = prargs; a != R_NilValue; a = CDR(a), b = CDR(b))
	IF_PROMSXP_SET_PRVALUE(CAR(b), CAR(a));
    ans = applyClosure(call, op, prargs, env, R_NilValue, true);
    UNPROTECT(3);
    return ans;
}

// which.min(x) : The index (starting at 1), of the first min(x) in x
// which.max(x) : The index (starting at 1), of the first max(x) in x
attribute_hidden SEXP do_first_min(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sx = CAR(args), ans;
    int nprot = 1;
    R_xlen_t i, n, indx = -1;

    checkArity(op, args);
    if (!isNumeric(sx)) {
	PROTECT(sx = coerceVector(CAR(args), REALSXP)); nprot++;
    }
    n = XLENGTH(sx);
    switch(TYPEOF(sx)) {
    case LGLSXP: // with only (TRUE, FALSE, NA) -- may be fast
    {
	int *r = LOGICAL(sx);
	if(PRIMVAL(op) == 0) { /* which.min */
	    for (i = 0; i < n; i++)
		if (r[i] == FALSE) {
		    indx = i; break; // found FALSE: done
		} else if (indx == -1 && r[i] != NA_LOGICAL) {
		    indx = i; // first TRUE
		}
	} else { /* which.max */
	    for (i = 0; i < n; i++)
		if (r[i] == TRUE) {
		    indx = i; break; // found TRUE: done
		} else if (indx == -1 && r[i] != NA_LOGICAL) {
		    indx = i; // first FALSE
		}
	}
    }
    break;

    case INTSXP:
    {
	int s, *r = INTEGER(sx);
	if(PRIMVAL(op) == 0) { /* which.min */
	    s = INT_MAX;
	    for (i = 0; i < n; i++)
		if (r[i] != NA_INTEGER && (r[i] < s || indx == -1)) {
		    s = r[i]; indx = i;
		}
	} else { /* which.max */
	    s = INT_MIN;
	    for (i = 0; i < n; i++)
		if (r[i] != NA_INTEGER && (r[i] > s || indx == -1)) {
		    s = r[i]; indx = i;
		}
	}
    }
    break;

    case REALSXP:
    {
	double s, *r = REAL(sx);
	if(PRIMVAL(op) == 0) { /* which.min */
	    s = R_PosInf;
	    for (i = 0; i < n; i++)
		if ( !ISNAN(r[i]) && (r[i] < s || indx == -1) ) {
		    s = r[i]; indx = i;
		}
	} else { /* which.max */
	    s = R_NegInf;
	    for (i = 0; i < n; i++)
		if ( !ISNAN(r[i]) && (r[i] > s || indx == -1) ) {
		    s = r[i]; indx = i;
		}
	}
    }
    default:
	break;
    } // switch()


    i = (indx != -1);
    bool large = (indx + 1) > INT_MAX;
    PROTECT(ans = allocVector(large ? REALSXP : INTSXP, i ? 1 : 0));
    if (i) {
	if(large)
	    REAL(ans)[0] = (double)indx + 1;
	else
	    INTEGER(ans)[0] = (int)indx + 1;
	if (getAttrib(sx, R_NamesSymbol) != R_NilValue) { /* preserve names */
	    SEXP ansnam;
	    PROTECT(ansnam =
		    ScalarString(STRING_ELT(getAttrib(sx, R_NamesSymbol), indx)));
	    setAttrib(ans, R_NamesSymbol, ansnam);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(nprot);
    return ans;
}


/* which(x) : indices of non-NA TRUE values in x */
attribute_hidden SEXP do_which(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP v = CAR(args);
    if (!isLogical(v))
	error("%s", _("argument to 'which' is not logical"));
    R_xlen_t len = xlength(v), i, j = 0;
    SEXP ans;
#ifdef LONG_VECTOR_SUPPORT
    if (len > R_SHORT_LEN_MAX) {
    R_xlen_t xoffset = 1; // 1 for 1-based indexing of response
    double *buf = (double *) R_alloc(len, sizeof(double));
    ITERATE_BY_REGION(v, ptr, idx, nb, int, LOGICAL, {
	    for(R_xlen_t i = 0; i < nb; i++) {
		if(ptr[i] == TRUE) {
		    buf[j] = (double)(xoffset + i); // offset has +1 built in
		    j++;
		}
	    }
	    xoffset += nb; // move to beginning of next buffer (+1 since R-based)
	});

    len = j;
    PROTECT(ans = allocVector(REALSXP, len));
    // buf has doubles in it, memcopy if we found any indices.
    if(len) memcpy(REAL(ans), buf, sizeof(double) * len);
    } else
#endif
    {
    int ioffset = 1;
    int *buf = (int *) R_alloc(len, sizeof(int));
    /* use iteration macros to be ALTREP safe and pull ptr retrieval out of tight loop */
    ITERATE_BY_REGION(v, ptr, idx, nb, int, LOGICAL, {
	    for(int i = 0; i < nb; i++) {
		if(ptr[i] == TRUE) {
		    buf[j] = ioffset + i; // offset has +1 built in
		    j++;
		}
	    }
	    ioffset += nb; // move to beginning of next buffer
	});

    len = j;
    // buf has ints in it and we're returning ints, memcopy if we found any indices;
    PROTECT(ans = allocVector(INTSXP, len));
    if(len) memcpy(INTEGER(ans), buf, sizeof(int) * len);
    }

    SEXP v_nms = getAttrib(v, R_NamesSymbol);
    if (v_nms != R_NilValue) {
	SEXP ans_nms = PROTECT(allocVector(STRSXP, len));
#ifdef LONG_VECTOR_SUPPORT
	if (TYPEOF(ans) == REALSXP)
	for (i = 0; i < len; i++) {
	    SET_STRING_ELT(ans_nms, i,
			   STRING_ELT(v_nms, (R_xlen_t)REAL(ans)[i] - 1));
	}
	else
#endif
	for (i = 0; i < len; i++) {
	    SET_STRING_ELT(ans_nms, i,
			   STRING_ELT(v_nms, (R_xlen_t)INTEGER(ans)[i] - 1));
	}
	setAttrib(ans, R_NamesSymbol, ans_nms);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}


/* op = 0 is pmin, op = 1 is pmax
   NULL and logicals are handled as if they had been coerced to integer.
 */
attribute_hidden SEXP do_pmin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    bool narm = asLogicalNoNA(CAR(args), "na.rm");
    args = CDR(args);
    if(args == R_NilValue) error("%s", _("no arguments"));
    SEXP x = CAR(args);

    SEXPTYPE anstype = TYPEOF(x);
    switch(anstype) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
	break;
    default:
	error("%s", _("invalid input type"));
    }
    SEXP a = CDR(args);
    if(a == R_NilValue) return x; /* one input */

    R_xlen_t n, len = xlength(x), /* not LENGTH, as NULL is allowed */
	i, i1; 
    for(; a != R_NilValue; a = CDR(a)) {
	x = CAR(a);
	SEXPTYPE type = TYPEOF(x);
	switch(type) {
	case NILSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case STRSXP:
	    break;
	default:
	    error("%s", _("invalid input type"));
	}
	if(type > anstype) anstype = type;
	n = xlength(x);
	if ((len > 0) ^ (n > 0)) {
	    // till 2.15.0:  error("%s", _("cannot mix 0-length vectors with others"));
	    len = 0;
	    break;
	}
	len = max(len, n);
    }
    if(anstype < INTSXP) anstype = INTSXP;
    if(len == 0) return allocVector(anstype, 0);
    /* Check for fractional recycling (added in 2.14.0) */
    for(a = args; a != R_NilValue; a = CDR(a)) {
	n = xlength(CAR(a));
	if (len % n) {
	    warning("%s", _("an argument will be fractionally recycled"));
	    break;
	}
    }

    SEXP ans = PROTECT(allocVector(anstype, len));
    switch(anstype) {
    case INTSXP:
    {
	int *r,  *ra = INTEGER(ans), tmp;
	PROTECT(x = coerceVector(CAR(args), anstype));
	r = INTEGER(x);
	n = XLENGTH(x);
	xcopyWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    x = CAR(a);
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    r = INTEGER(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(PRIMVAL(op) == 1) {
		    if( (narm && ra[i] == NA_INTEGER) ||
			(ra[i] != NA_INTEGER && tmp != NA_INTEGER
			 && tmp > ra[i]) ||
			(!narm && tmp == NA_INTEGER) )
			ra[i] = tmp;
		} else {
		    if( (narm && ra[i] == NA_INTEGER) ||
			(ra[i] != NA_INTEGER && tmp != NA_INTEGER
			 && tmp < ra[i]) ||
			(!narm && tmp == NA_INTEGER) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case REALSXP:
    {
	double *r, *ra = REAL(ans), tmp;
	PROTECT(x = coerceVector(CAR(args), anstype));
	r = REAL(x);
	n = XLENGTH(x);
	xcopyWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    r = REAL(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(PRIMVAL(op) == 1) {
		    if( (narm && ISNAN(ra[i])) ||
			(!ISNAN(ra[i]) && !ISNAN(tmp) && tmp > ra[i]) ||
			(!narm && ISNAN(tmp)) )
			ra[i] = tmp;
		} else {
		    if( (narm && ISNAN(ra[i])) ||
			(!ISNAN(ra[i]) && !ISNAN(tmp) && tmp < ra[i]) ||
			(!narm && ISNAN(tmp)) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case STRSXP:
    {
	PROTECT(x = coerceVector(CAR(args), anstype));
	n = XLENGTH(x);
	xcopyStringWithRecycle(ans, x, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    SEXP tmp, t2;
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = STRING_ELT(x, i1);
		t2 = STRING_ELT(ans, i);
		if(PRIMVAL(op) == 1) {
		    if( (narm && t2 == NA_STRING) ||
			(t2 != NA_STRING && tmp != NA_STRING && tmp != t2 && Scollate(tmp, t2) > 0) ||
			(!narm && tmp == NA_STRING) )
			SET_STRING_ELT(ans, i, tmp);
		} else {
		    if( (narm && t2 == NA_STRING) ||
			(t2 != NA_STRING && tmp != NA_STRING && tmp != t2 && Scollate(tmp, t2) < 0) ||
			(!narm && tmp == NA_STRING) )
			SET_STRING_ELT(ans, i, tmp);
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    default:
	break;
    }
    UNPROTECT(1);
    return ans;
}
