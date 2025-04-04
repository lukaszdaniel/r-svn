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

/** @file cum.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>

using namespace R;

/* Handle NaN and NA in input for a cumulative operation, preserving
   distinction between NA and NaN. */
static SEXP handleNaN(SEXP x, SEXP s)
{
    bool hasNA = false;
    bool hasNaN = false;
    double *rx = REAL(x), *rs = REAL(s);

    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	hasNaN = (hasNaN || ISNAN(rx[i]));
	hasNA = (hasNA || (hasNaN && R_IsNA(rx[i])));

	if (hasNA)
	    rs[i] = NA_REAL;
	else if (hasNaN)
	    rs[i] = R_NaN;
    }
    return s;
}

static SEXP cumsum(SEXP x, SEXP s)
{
    LDOUBLE sum = 0.;
    double *rx = REAL(x), *rs = REAL(s);
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	sum += rx[i]; /* NA and NaN propagated */
	rs[i] = (double) sum;
    }
    return ISNAN(sum) ? handleNaN(x, s) : s;
}

/* We need to ensure that overflow gives NA here */
static SEXP icumsum(SEXP x, SEXP s)
{
    int *ix = INTEGER(x), *is = INTEGER(s);
    double sum = 0.0;
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	if (ix[i] == NA_INTEGER) break;
	sum += ix[i];
	if(sum > INT_MAX || sum < 1 + INT_MIN) { /* INT_MIN is NA_INTEGER */
	    warning("%s", _("integer overflow in 'cumsum'; use 'cumsum(as.numeric(.))'"));
	    break;
	}
	is[i] = (int) sum;
    }
    return s;
}

/* For complex result: recompute once we know one of the result's {re, im} fulfills  ISNAN(.),
   (speed optimized for the case of *no* NA|NaN) : */
static SEXP chandleNaN(SEXP x, SEXP s, bool r_isN, bool i_isN)
{
    bool hasNA = false;
    bool hasNaN = false;

    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	hasNaN = hasNaN || ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i);
	hasNA = hasNA || (hasNaN && (R_IsNA(COMPLEX(x)[i].r) ||
				     R_IsNA(COMPLEX(x)[i].i)));
	if (hasNA) {
	    if(r_isN) COMPLEX(s)[i].r = NA_REAL;
	    if(i_isN) COMPLEX(s)[i].i = NA_REAL;
	} else if (hasNaN) {
	    if(r_isN) COMPLEX(s)[i].r = R_NaN;
	    if(i_isN) COMPLEX(s)[i].i = R_NaN;
	}
    }
    return s;
}

static SEXP ccumsum(SEXP x, SEXP s)
{
    Rcomplex sum;
    sum.r = 0;
    sum.i = 0;
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	sum.r += COMPLEX(x)[i].r;
	sum.i += COMPLEX(x)[i].i;
	COMPLEX(s)[i].r = sum.r;
	COMPLEX(s)[i].i = sum.i;
    }
    return (ISNAN(sum.r) || ISNAN(sum.i)) ? chandleNaN(x, s, ISNAN(sum.r), ISNAN(sum.i)) : s;
}

static SEXP cumprod(SEXP x, SEXP s)
{
    LDOUBLE prod;
    double *rx = REAL(x), *rs = REAL(s);
    prod = 1.0;
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	prod *= rx[i]; /* NA and NaN propagated */
	rs[i] = (double) prod;
    }
    return ISNAN(prod) ? handleNaN(x, s) : s;
}

static SEXP ccumprod(SEXP x, SEXP s)
{
    Rcomplex prod, tmp;
    prod.r = 1;
    prod.i = 0;
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	tmp.r = prod.r;
	tmp.i = prod.i;
	prod.r = COMPLEX(x)[i].r * tmp.r - COMPLEX(x)[i].i * tmp.i;
	prod.i = COMPLEX(x)[i].r * tmp.i + COMPLEX(x)[i].i * tmp.r;
	COMPLEX(s)[i].r = prod.r;
	COMPLEX(s)[i].i = prod.i;
    }
    return (ISNAN(prod.r) || ISNAN(prod.i)) ? chandleNaN(x, s, ISNAN(prod.r), ISNAN(prod.i)) : s;
}

static SEXP cummax(SEXP x, SEXP s)
{
    double max, *rx = REAL(x), *rs = REAL(s);
    max = R_NegInf;
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++) {
	if (ISNAN(rx[i]))
	    return handleNaN(x, s);
	else
	    max = (max > rx[i]) ? max : rx[i];
	rs[i] = max;
    }
    return s;
}

static SEXP cummin(SEXP x, SEXP s)
{
    double min, *rx = REAL(x), *rs = REAL(s);
    min = R_PosInf; /* always positive, not NA */
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++ ) {
	if (ISNAN(rx[i]))
	    return handleNaN(x, s);
	else
	    min = (min < rx[i]) ? min : rx[i];
	rs[i] = min;
    }
    return s;
}

static SEXP icummax(SEXP x, SEXP s)
{
    int *ix = INTEGER(x);
    if(ix[0] == NA_INTEGER)
	return s; // all NA
    int *is = INTEGER(s), max = ix[0];
    is[0] = max;
    for (R_xlen_t i = 1 ; i < XLENGTH(x) ; i++) {
	if(ix[i] == NA_INTEGER) break;
	is[i] = max = (max > ix[i]) ? max : ix[i];
    }
    return s;
}

static SEXP icummin(SEXP x, SEXP s)
{
    int *ix = INTEGER(x), *is = INTEGER(s);
    int min = ix[0];
    is[0] = min;
    for (R_xlen_t i = 1 ; i < XLENGTH(x) ; i++ ) {
	if(ix[i] == NA_INTEGER) break;
	is[i] = min = (min < ix[i]) ? min : ix[i];
    }
    return s;
}

/*
op = 1 is cumsum
op = 2 is cumprod
op = 3 is cummax
op = 4 is cummin
*/
attribute_hidden SEXP do_cum(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, ans;
    R_xlen_t n;
    checkArity(op, args);
    {
        auto dispatched = DispatchGroup("Math", call, op, args, env);
        if (dispatched.first)
            return dispatched.second;
    }
    if (isComplex(CAR(args))) {
	t = CAR(args);
	n = XLENGTH(t);
	PROTECT(s = allocVector(CPLXSXP, n));
	setAttrib(s, R_NamesSymbol, getAttrib(t, R_NamesSymbol));
	UNPROTECT(1);
	if(n == 0) return s;
	/* no need to initialize s, ccum* set all elements */
	switch (PRIMVAL(op) ) {
	case 1:	/* cumsum */
	    return ccumsum(t, s);
	    break;
	case 2: /* cumprod */
	    return ccumprod(t, s);
	    break;
	case 3: /* cummax */
	    errorcall(call, "%s", _("'cummax' not defined for complex numbers"));
	    break;
	case 4: /* cummin */
	    errorcall(call, "%s", _("'cummin' not defined for complex numbers"));
	    break;
	default:
	    errorcall(call, "%s", _("unknown cumxxx function"));
	}
    } else if( ( isInteger(CAR(args)) || isLogical(CAR(args)) ) &&
	       PRIMVAL(op) != 2) {
	PROTECT(t = coerceVector(CAR(args), INTSXP));
	n = XLENGTH(t);
	PROTECT(s = allocVector(INTSXP, n));
	setAttrib(s, R_NamesSymbol, getAttrib(t, R_NamesSymbol));
	if(n == 0) {
	    UNPROTECT(2); /* t, s */
	    return s;
	}
	for (R_xlen_t i = 0 ; i < n ; i++) INTEGER(s)[i] = NA_INTEGER;
	switch (PRIMVAL(op) ) {
	case 1:	/* cumsum */
	    ans = icumsum(t,s);
	    break;
	case 3: /* cummax */
	    ans = icummax(t,s);
	    break;
	case 4: /* cummin */
	    ans = icummin(t,s);
	    break;
	default:
	    errorcall(call, "%s", _("unknown cumxxx function"));
	    ans = R_NilValue;
	}
	UNPROTECT(2); /* t, s */
	return ans;
    } else {
	PROTECT(t = coerceVector(CAR(args), REALSXP));
	n = XLENGTH(t);
	PROTECT(s = allocVector(REALSXP, n));
	setAttrib(s, R_NamesSymbol, getAttrib(t, R_NamesSymbol));
	UNPROTECT(2);
	if(n == 0) return s;
	/* no need to initialize s, cum* set all elements */
	switch (PRIMVAL(op) ) {
	case 1:	/* cumsum */
	    return cumsum(t,s);
	    break;
	case 2: /* cumprod */
	    return cumprod(t,s);
	    break;
	case 3: /* cummax */
	    return cummax(t,s);
	    break;
	case 4: /* cummin */
	    return cummin(t,s);
	    break;
	default:
	    errorcall(call, "%s", _("unknown cumxxx function"));
	}
    }
    return R_NilValue; /* for -Wall */
}
