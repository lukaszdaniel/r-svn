/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 2003--2016  The R Foundation
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
 * Object Formatting
 *
 *  See ./paste.c for do_paste() , do_format() and do_formatinfo() and
 *       ./util.c for do_formatC()
 *  See ./printutils.c for general remarks on Printing and the Encode.. utils.
 *  See ./print.c  for do_printdefault, do_prmatrix, etc.
 *
 * Exports
 *	formatString
 *	formatStringS
 *	formatLogical
 *	formatLogicalS
 *	formatInteger
 *	formatIntegerS
 *	formatReal
 *	formatRealS
 *	formatComplex
 *	formatComplexS
 *
 * These  formatFOO() functions determine the proper width, digits, etc.
 *
 * formatFOOS() functions behave identically to formatFOO
 * except that they accept a SEXP rather than a data pointer
 */

/** @file format.cpp
 *
 * Object formatting.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat> /* for DBL_EPSILON */
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Defn.h>
#include <Rmath.h>
#include <Print.h>
#include <R_ext/Itermacros.h> /* for ITERATE_BY_REGION */
#ifdef formatComplex_tricky
#ifdef formatComplex_USING_signif
#include <Rcomplex.h>
#endif
#endif

using namespace R;
using namespace CXXR;

/* this is just for conformity with other types */
attribute_hidden
void R::formatRaw(const Rbyte *x, R_xlen_t n, int *fieldwidth)
{
    *fieldwidth = 2;
}

attribute_hidden
void R::formatRawS(SEXP x, R_xlen_t n, int *fieldwidth)
{
    *fieldwidth = 2;
}

namespace CXXR
{
    // Designed for use with std::accumulate():
    unsigned int stringWidth(unsigned int minwidth, SEXP string)
    {
        unsigned int width = R_print.na_width_noquote;
        if (string != NA_STRING)
            width = Rstrlen(string, false);
        return std::max(minwidth, width);
    }

    // Designed for use with std::accumulate():
    unsigned int stringWidthQuote(unsigned int minwidth, SEXP string)
    {
        unsigned int width = R_print.na_width;
        if (string != NA_STRING)
            width = Rstrlen(string, true) + 2;
        return std::max(minwidth, width);
    }
} // namespace CXXR

attribute_hidden
void R::formatString(const SEXP *x, R_xlen_t n, int *fieldwidth, int quote)
{
    int xmax = 0;

    if (quote)
    {
        for (R_xlen_t i = 0; i < n; i++)
        {
            xmax = stringWidthQuote(xmax, x[i]);
        }
    }
    else
    {
        for (R_xlen_t i = 0; i < n; i++)
        {
            xmax = stringWidth(xmax, x[i]);
        }
    }

    *fieldwidth = xmax;
}

/* currently there is no STRING_GET_REGION */

attribute_hidden
void R::formatStringS(SEXP x, R_xlen_t n, int *fieldwidth, bool quote)
{
    int xmax = 0;

    if (quote)
    {
        for (R_xlen_t i = 0; i < n; i++)
        {
            xmax = stringWidthQuote(xmax, STRING_ELT(x, i));
        }
    }
    else
    {
        for (R_xlen_t i = 0; i < n; i++)
        {
            xmax = stringWidth(xmax, STRING_ELT(x, i));
        }
    }

    *fieldwidth = xmax;
}


attribute_hidden
void Rf_formatLogical(const int *x, R_xlen_t n, int *fieldwidth)
{
    *fieldwidth = 1;
    for (R_xlen_t i = 0 ; i < n; i++) {
	if (x[i] == NA_LOGICAL) {
	    if(*fieldwidth < R_print.na_width)
		*fieldwidth = R_print.na_width;
	} else if (x[i] != 0 && *fieldwidth < 4) {
	    *fieldwidth = 4;
	} else if (x[i] == 0 && *fieldwidth < 5 ) {
	    *fieldwidth = 5;
	    break;
	    /* this is the widest it can be,  so stop */
	}
    }
}

attribute_hidden
void formatLogicalS(SEXP x, R_xlen_t n, int *fieldwidth) {
    *fieldwidth = 1;
    int tmpfieldwidth = 1;
    ITERATE_BY_REGION_PARTIAL(x, px, idx, nb, int, LOGICAL, 0, n,
			      {
				  formatLogical(px, nb, &tmpfieldwidth);
				  if (tmpfieldwidth > *fieldwidth)
				      *fieldwidth = tmpfieldwidth;
				  if (*fieldwidth == 5)
				      break;  /* break iteration loop */
			      });
    return;
}


/* needed in 2 places so rolled out into macro
   to avoid divergence
*/
#define FORMATINT_RETLOGIC do {					\
	if (naflag) *fieldwidth = R_print.na_width;		\
	else *fieldwidth = 1;					\
								\
	if (xmin < 0) {						\
	    l = IndexWidth(-xmin) + 1;	/* +1 for sign */	\
	    if (l > *fieldwidth) *fieldwidth = l;		\
	}							\
	if (xmax > 0) {						\
	    l = IndexWidth(xmax);				\
	    if (l > *fieldwidth) *fieldwidth = l;		\
	}							\
    } while(0)

attribute_hidden
void Rf_formatInteger(const int *x, R_xlen_t n, int *fieldwidth)
{
    int xmin = INT_MAX, xmax = INT_MIN, naflag = 0;
    int l;

    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] == NA_INTEGER)
	    naflag = 1;
	else {
	    if (x[i] < xmin) xmin = x[i];
	    if (x[i] > xmax) xmax = x[i];
	}
    }
    FORMATINT_RETLOGIC;
}

attribute_hidden
void formatIntegerS(SEXP x, R_xlen_t n, int *fieldwidth)
{

    int xmin = INT_MAX, xmax = INT_MIN, naflag = 0,
	sorted;
    SEXP tmpmin = NULL, tmpmax = NULL;
    /*
       min and max should be VERY cheap when sortedness
       is known, so better to call them both than loop
       through whole vector even once

       Shouldn't need to check for ALTREPness here
       because KNOWN_SORTED(sorted) will never be
       true for non-ALTREPs or "exploded" ALTREPs
    */
    sorted = INTEGER_IS_SORTED(x);
    /* if we're not formatting/printing the whole thing
       ALTINTEGER_MIN/MAX will give us the wrong thing
       anyway */
    if(n == XLENGTH(x) && KNOWN_SORTED(sorted)) {
	tmpmin = PROTECT(ALTINTEGER_MIN(x, TRUE));
	tmpmax = PROTECT(ALTINTEGER_MAX(x, TRUE));
	naflag = KNOWN_NA_1ST(sorted) ?
	    INTEGER_ELT(x, 0) == NA_INTEGER :
	    INTEGER_ELT(x, XLENGTH(x) - 1) == NA_INTEGER;
	UNPROTECT(2); /* tmpmin, tmpmax */
    }

    /*
       If we don't have min/max methods or they need to punt
       for some reason we will get NULL.

       The data are integers, so the only reason we would not
       get INTSXP answers is if we got Inf/-Inf because
       everything was NA.

       In both of the above cases we will
       do things the hard way below
    */
    if(tmpmin != NULL && tmpmax != NULL &&
       TYPEOF(tmpmin) == INTSXP && TYPEOF(tmpmax) == INTSXP) {
	int l; /* only needed here so defined locally */
	PROTECT(tmpmin);
	PROTECT(tmpmax);
	xmin = INTEGER_ELT(tmpmin, 0);
	xmax = INTEGER_ELT(tmpmax, 0);
	UNPROTECT(2); /* tmpmin, tmpmax */
	/* naflag set above */

	/* this is identical logic to what formatInteger
	   does */
	FORMATINT_RETLOGIC;
    } else {
	/*
	   no fastpass so just format using formatInteger
	   by region. No need for FORMATINT_RETLOGIC
	   here because it happens inside all the
	   formatInteger calls.
	*/
	int tmpfw = 1;
	*fieldwidth = 1;
	ITERATE_BY_REGION_PARTIAL(x, px, idx, nb, int, INTEGER, 0, n,
			  {
			      formatInteger(px, nb, &tmpfw);
			      if(tmpfw > *fieldwidth)
				  *fieldwidth = tmpfw;
			  });
    }
}

/*---------------------------------------------------------------------------
 * scientific format determination for real numbers.
 * This is time-critical code.	 It is worth optimizing.
 *
 *    nsig		digits altogether
 *    kpower+1		digits to the left of "."
 *    kpower+1+sgn	including sign
 *
 * Using GLOBAL	 R_print.digits	 -- had	 #define MAXDIG R_print.digits
*/

/*  Very likely everyone has nearbyintl now (2018), but it took until
    2012 for FreeBSD to get it, and longer for Cygwin.
*/
#if defined(HAVE_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
# ifdef HAVE_NEARBYINTL
# define R_nearbyintl nearbyintl
# elif defined(HAVE_RINTL)
# define R_nearbyintl rintl
# else
# define R_nearbyintl private_nearbyintl
LDOUBLE private_nearbyintl(LDOUBLE x)
{
    LDOUBLE x1;
    x1 = - floorl(-x + 0.5);
    x = floorl(x + 0.5);
    if (x == x1) return(x);
    else {
	/* FIXME: we should really test for floorl, also C99.
	   But FreeBSD 7.x does have it, but not nearbyintl */
        if (x/2.0 == floorl(x/2.0)) return(x); else return(x1);
    }
}
# endif
#endif

#define NB 1000
static void format_via_sprintf(double r, int d, int *kpower, int *nsig)
{
    static char buff[NB];
    int i;
    snprintf(buff, NB, "%#.*e", d - 1, r);
    *kpower = (int) strtol(buff + (d + 2), NULL, 10);
    for (i = d; i >= 2; i--)
        if (buff[i] != '0') break;
    *nsig = i;
}


#if defined(HAVE_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
static const long double tbl[] =
{
    /* Powers exactly representable with 64 bit mantissa */
    1e00, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06, 1e07, 1e08, 1e09,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27
};
#define KP_MAX 27
#else
static const double tbl[] =
{
    1e00, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06, 1e07, 1e08, 1e09,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22
};
#define KP_MAX 22
#endif

static void scientific(const double *x, int *neg, int *kpower, int *nsig, bool *roundingwidens)
{
    /* for a number x , determine
     *	neg    = 1_{x < 0}  {0/1}
     *	kpower = Exponent of 10;
     *	nsig   = min(R_print.digits, #{significant digits of alpha})
     *  roundingwidens = true iff rounding causes x to increase in width
     *
     * where  |x| = alpha * 10^kpower	and	 1 <= alpha < 10
     */
    double alpha;
    double r;
    int kp;
    int j;

    if (*x == 0.0) {
	*kpower = 0;
	*nsig = 1;
	*neg = 0;
	*roundingwidens = false;
    } else {
	if(*x < 0.0) {
	    *neg = 1; r = -*x;
	} else {
	    *neg = 0; r = *x;
	}
        if (R_print.digits >= DBL_DIG + 1) {
            format_via_sprintf(r, R_print.digits, kpower, nsig);
	    *roundingwidens = false;
            return;
        }
        kp = (int) floor(log10(r)) - R_print.digits + 1;/* r = |x|; 10^(kp + digits - 1) <= r */
#if defined(HAVE_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
        long double r_prec = r;
        /* use exact scaling factor in long double precision, if possible */
        if (abs(kp) <= KP_MAX) {
            if (kp > 0) r_prec /= tbl[kp]; else if (kp < 0) r_prec *= tbl[ -kp];
        }
#ifdef HAVE_POWL
	// powl is C99 but only added to FreeBSD in 2017.
	else
            r_prec /= powl(10.0, (long double) kp);
#else
        else if (kp <= R_dec_min_exponent)
            r_prec = (r_prec * 1e+303)/Rexp10((double)(kp+303));
        else
            r_prec /= Rexp10((double) kp);
#endif
        if (r_prec < tbl[R_print.digits - 1]) {
            r_prec *= 10.0;
            kp--;
        }
        /* round alpha to integer, 10^(digits-1) <= alpha <= 10^digits
	   accuracy limited by double rounding problem,
	   alpha already rounded to 64 bits */
        alpha = (double) R_nearbyintl(r_prec);
#else /* not using long doubles */
	double r_prec = r;
        /* use exact scaling factor in double precision, if possible */
        if (abs(kp) <= KP_MAX) {
            if (kp >= 0) r_prec /= tbl[kp]; else r_prec *= tbl[ -kp];
        }
        /* For IEC60559 1e-308 is not representable except by gradual underflow.
           Shifting by 303 allows for any potential denormalized numbers x,
           and makes the reasonable assumption that R_dec_min_exponent+303
           is in range. Representation of 1e+303 has low error.
         */
        else if (kp <= R_dec_min_exponent)
            r_prec = (r_prec * 1e+303)/Rexp10((double)(kp+303));
        else
            r_prec /= Rexp10((double)kp);
        if (r_prec < tbl[R_print.digits - 1]) {
            r_prec *= 10.0;
            kp--;
        }
        /* round alpha to integer, 10^(digits-1) <= alpha <= 10^digits */
        /* accuracy limited by double rounding problem,
	   alpha already rounded to 53 bits */
        alpha = nearbyint(r_prec);
#endif
        *nsig = R_print.digits;
        for (j = 1; j <= R_print.digits; j++) {
            alpha /= 10.0;
            if (alpha == floor(alpha)) {
                (*nsig)--;
            } else {
                break;
            }
        }
        if (*nsig == 0 && R_print.digits > 0) {
            *nsig = 1;
            kp += 1;
        }
        *kpower = kp + R_print.digits - 1;

	/* Scientific format may do more rounding than fixed format, e.g.
	   9996 with 3 digits is 1e+04 in scientific, but 9996 in fixed.
	   This happens when the true value r is less than 10^(kpower+1)
	   and would not round up to it in fixed format.
	   Here rgt is the decimal place that will be cut off by rounding */

	int rgt = R_print.digits - *kpower;
	/* bound rgt by 0 and KP_MAX */
	rgt = rgt < 0 ? 0 : rgt > KP_MAX ? KP_MAX : rgt;
	double fuzz = 0.5/(double)tbl[rgt];
	// kpower can be bigger than the table.
	*roundingwidens = (*kpower > 0 && *kpower <= KP_MAX && r < tbl[*kpower] - fuzz);
    }
}

/*
   The return values are
     w : the required field width
     d : use %w.df in fixed format, %#w.de in scientific format
     e : use scientific format if != 0, value is number of exp digits - 1

   nsmall specifies the minimum number of decimal digits in fixed format:
   it is 0 except when called from do_format.
*/

/* not hidden: used in graphics/src/plot.c */
void Rf_formatReal(const double *x, R_xlen_t n, int *w, int *d, int *e, int nsmall)
{
    bool
	naflag = false, nanflag = false,
	posinf = false, neginf = false;
    int neg = 0;
    int mnl = INT_MAX,
	mxl, rgt, mxsl, mxns;
    mxl = rgt = mxsl = mxns = INT_MIN;

    for (R_xlen_t i = 0; i < n; i++) {
	if (!R_FINITE(x[i])) {
	    if(ISNA(x[i])) naflag = true;
	    else if(ISNAN(x[i])) nanflag = true;
	    else if(x[i] > 0) posinf = true;
	    else neginf = true;
	} else {
	    int neg_i, kpower, nsig;
	    bool roundingwidens;
	    scientific(&x[i], &neg_i, &kpower, &nsig, &roundingwidens);

	    int left = kpower + 1;
	    if (roundingwidens) left--;

	    int sleft = neg_i + ((left <= 0) ? 1 : left); /* >= 1 */
	    int right = nsig - left; /* #{digits} right of '.' ( > 0 often)*/
	    if (neg_i) neg = 1;	 /* if any < 0, need extra space for sign */

	    /* Infinite precision "F" Format : */
	    if (right > rgt) rgt = right;	/* max digits to right of . */
	    if (left > mxl)  mxl = left;	/* max digits to  left of . */
	    if (left < mnl)  mnl = left;	/* min digits to  left of . */
	    if (sleft> mxsl) mxsl = sleft;	/* max left including sign(s)*/
	    if (nsig > mxns) mxns = nsig;	/* max sig digits */
	}
    }
    /* F Format: use "F" format WHENEVER we use not more space than 'E'
     *		and still satisfy 'R_print.digits' {but as if nsmall==0 !}
     *
     * E Format has the form   [S]X[.XXX]E+XX[X]
     *
     * This is indicated by setting *e to non-zero (usually 1)
     * If the additional exponent digit is required *e is set to 2
     */

    /*-- These	'mxsl' & 'rgt'	are used in F Format
     *	 AND in the	____ if(.) "F" else "E" ___   below: */
    if (R_print.digits == 0) rgt = 0;
    if (mxl < 0) mxsl = 1 + neg;  /* we use %#w.dg, so have leading zero */

    /* use nsmall only *after* comparing "F" vs "E": */
    if (rgt < 0) rgt = 0;
    int wF = mxsl + rgt + (rgt != 0);	/* width for F format */

    /*-- 'see' how "E" Exponential format would be like : */
    *e = (mxl > 100 || mnl <= -99) ? 2 /* 3 digit exponent */ : 1;
    if (mxns != INT_MIN) {
	*d = mxns - 1;
	*w = neg + (*d > 0) + *d + 4 + *e; /* width for E format */
	if (wF <= *w + R_print.scipen) { /* Fixpoint if it needs less space */
	    *e = 0;
	    if (nsmall > rgt) {
		rgt = nsmall;
		wF = mxsl + rgt + (rgt != 0);
	    }
	    *d = rgt;
	    *w = wF;
	} /* else : "E" Exponential format -- all done above */
    }
    else { /* when all x[i] are non-finite */
	*w = 0;/* to be increased */
	*d = 0;
	*e = 0;
    }
    if (naflag && *w < R_print.na_width)
	*w = R_print.na_width;
    if (nanflag && *w < 3) *w = 3;
    if (posinf && *w < 3) *w = 3;
    if (neginf && *w < 4) *w = 4;
}

attribute_hidden
void formatRealS(SEXP x, R_xlen_t n, int *w, int *d, int *e, int nsmall)
{
    /*
     *  iterate by region and just take the most extreme
     *  values across all the regions for final w, d, and e
     */
    int tmpw, tmpd, tmpe;

    *w = 0;
    *d = 0;
    *e = 0;

    ITERATE_BY_REGION_PARTIAL(x, px, idx, nb, double, REAL, 0, n,
		      {
			  formatReal(px, nb, &tmpw, &tmpd, &tmpe, nsmall);
			  if(tmpw > *w) *w = tmpw;
			  if(!*d && tmpd) *d = tmpd;
			  if(tmpe > *e) *e = tmpe;
		      });
}

/* From R 2.2.0 to 4.3.z, the number of digits applied to real and imaginary parts
   together, not separately.  Since R 4.4.0, Re(.) and Im(.) are treated seperately. */
void Rf_formatComplex(const Rcomplex *x, R_xlen_t n,
		   int *wr, int *dr, int *er, // (w,d,e) for Re(.)
		   int *wi, int *di, int *ei, // (w,d,e) for Im(.)
		   int nsmall)
{
/* format.info() for  x[1..n] for both Re & Im */
#ifdef formatComplex_tricky // R 4.3.z and earlier
    bool all_re_zero = true, all_im_zero = true,
	naflag = false,
	rnan = false, rposinf = false, rneginf = false,
	inan = false, iposinf = false;
    int neg = 0;
    int rt, mnl, mxl, mxsl, mxns, wF, i_wF;
    int i_rt, i_mnl, i_mxl, i_mxsl, i_mxns;
    rt	=  mxl =  mxsl =  mxns = INT_MIN;
    i_rt= i_mxl= i_mxsl= i_mxns= INT_MIN;
    i_mnl = mnl = INT_MAX;

    for (R_xlen_t i = 0; i < n; i++) {
	Rcomplex tmp;
#ifdef formatComplex_USING_signif
	/* Now round */
	z_prec_r(&tmp, &(x[i]), R_print.digits);
#else
 	tmp.r = x[i].r;
 	tmp.i = x[i].i;
#endif
	if(ISNA(tmp.r) || ISNA(tmp.i)) {
	    naflag = true;
	} else {
	    bool roundingwidens;
	    int left, right, sleft,
		neg_i, kpower, nsig;

	    /* real part */

	    if(!R_FINITE(tmp.r)) {
		if (ISNAN(tmp.r)) rnan = true;
		else if (tmp.r > 0) rposinf = true;
		else rneginf = true;
	    } else {
		if(x[i].r != 0) all_re_zero = false;
		scientific(&(tmp.r), &neg_i, &kpower, &nsig, &roundingwidens);

		left = kpower + 1;
		if (roundingwidens) left--;
		sleft = neg_i + ((left <= 0) ? 1 : left); /* >= 1 */
		right = nsig - left; /* #{digits} right of '.' ( > 0 often)*/
		if (neg_i) neg = 1; /* if any < 0, need extra space for sign */

		if (right > rt) rt = right;	/* max digits to right of . */
		if (left > mxl) mxl = left;	/* max digits to left of . */
		if (left < mnl) mnl = left;	/* min digits to left of . */
		if (sleft> mxsl) mxsl = sleft;	/* max left including sign(s) */
		if (nsig > mxns) mxns = nsig;	/* max sig digits */

	    }
	    /* imaginary part */

	    /* this is always unsigned */
	    /* we explicitly put the sign in when we print */

	    if(!R_FINITE(tmp.i)) {
		if (ISNAN(tmp.i)) inan = true;
		else iposinf = true;
	    } else {
		if(x[i].i != 0) all_im_zero = false;
		scientific(&(tmp.i), &neg_i, &kpower, &nsig, &roundingwidens);

		left = kpower + 1;
		if (roundingwidens) left--;
		sleft = ((left <= 0) ? 1 : left);
		right = nsig - left;

		if (right > i_rt) i_rt = right;
		if (left > i_mxl) i_mxl = left;
		if (left < i_mnl) i_mnl = left;
		if (sleft> i_mxsl) i_mxsl = sleft;
		if (nsig > i_mxns) i_mxns = nsig;
	    }
	    /* done: ; */
	}
    }

    /* see comments in formatReal() for details on this */

    /* overall format for real part	*/

    if (R_print.digits == 0) rt = 0;
    if (mxl != INT_MIN) {
	if (mxl < 0) mxsl = 1 + neg;
	if (rt < 0) rt = 0;
	wF = mxsl + rt + (rt != 0);

	*er = (mxl > 100 || mnl < -99) ? 2 : 1;
	*dr = mxns - 1;
	*wr = neg + (*dr > 0) + *dr + 4 + *er;
    } else {
	*er = 0;
	*wr = 0;
	*dr = 0;
	wF = 0;
    }

    /* overall format for imaginary part */

    if (R_print.digits == 0) i_rt = 0;
    if (i_mxl != INT_MIN) {
	if (i_mxl < 0) i_mxsl = 1;
	if (i_rt < 0) i_rt = 0;
	i_wF = i_mxsl + i_rt + (i_rt != 0);

	*ei = (i_mxl > 100 || i_mnl < -99) ? 2 : 1;
	*di = i_mxns - 1;
	*wi = (*di > 0) + *di + 4 + *ei;
    } else {
	*ei = 0;
	*wi = 0;
	*di = 0;
	i_wF = 0;
    }

    /* Now make the fixed/scientific decision */
    if(all_re_zero) {
	*er = *dr = 0;
	*wr = wF;
	if (i_wF <= *wi + R_print.scipen) {
	    *ei = 0;
	    if (nsmall > i_rt) {i_rt = nsmall; i_wF = i_mxsl + i_rt + (i_rt != 0);}
	    *di = i_rt;
	    *wi = i_wF;
	}
    } else if(all_im_zero) {
	if (wF <= *wr + R_print.scipen) {
	    *er = 0;
	    if (nsmall > rt) {rt = nsmall; wF = mxsl + rt + (rt != 0);}
	    *dr = rt;
	    *wr = wF;
	}
	*ei = *di = 0;
	*wi = i_wF;
    } else if(wF + i_wF < *wr + *wi + 2*R_print.scipen) {
	    *er = 0;
	    if (nsmall > rt) {rt = nsmall; wF = mxsl + rt + (rt != 0);}
	    *dr = rt;
	    *wr = wF;

	    *ei = 0;
	    if (nsmall > i_rt) {
		i_rt = nsmall;
		i_wF = i_mxsl + i_rt + (i_rt != 0);
	    }
	    *di = i_rt;
	    *wi = i_wF;
    } /* else scientific for both */
    if(*wr < 0) *wr = 0;
    if(*wi < 0) *wi = 0;

    /* Ensure space for Inf and NaN */
    if (rnan    && *wr < 3) *wr = 3;
    if (rposinf && *wr < 3) *wr = 3;
    if (rneginf && *wr < 4) *wr = 4;
    if (inan    && *wi < 3) *wi = 3;
    if (iposinf && *wi < 3) *wi = 3;

#else // R >= 4.4.0 : no longer "tricky"
    double
	*Re = (double *) R_alloc(n, sizeof(double)),
	*Im = (double *) R_alloc(n, sizeof(double));

# ifdef formatComplex_NA_give_NA // as previously in all S and R versions:
    bool naflag = false;
    R_xlen_t i1 = 0;
    for (R_xlen_t i = 0; i < n; i++) {
	if(ISNA(x[i].r) || ISNA(x[i].i)) {
	    naflag = true;
	} else {
	    Re[i1] =      x[i].r;
	    Im[i1] = fabs(x[i].i); // in "Re +/- Im", the '-' does not take more space
	    i1++;
	} // 0 <= i1 <= i < n; i1 == length(Re) == length(Im)
    }
    formatReal(Re, i1, wr, dr, er, nsmall);
    formatReal(Im, i1, wi, di, ei, nsmall);

    /* finally, ensure that there is space for NA */
    if (naflag && *wr+*wi+2 < R_print.na_width)
	*wr += (R_print.na_width -(*wr + *wi + 2));

# else // even simpler: can lead to extra " " e.g. for c(NA, 1+2i)
    for (R_xlen_t i = 0; i < n; i++) {
	Re[i] =      x[i].r;
	Im[i] = fabs(x[i].i); // in "Re +/- Im", the '-' does not take more space
    }
    formatReal(Re, n, wr, dr, er, nsmall);
    formatReal(Im, n, wi, di, ei, nsmall);
# endif
#endif
}



attribute_hidden
void formatComplexS(SEXP x, R_xlen_t n, int *wr, int *dr, int *er,
		   int *wi, int *di, int *ei, int nsmall)
{
/* format.info() for  x[1..n] for both Re & Im */
    int tmpwr, tmpdr, tmper, tmpwi, tmpdi, tmpei;

    *wr = 0;
    *wi = 0;
    *dr = 0;
    *di = 0;
    *er = 0;
    *ei = 0;
    ITERATE_BY_REGION_PARTIAL(x, px, idx, nb, Rcomplex, COMPLEX, 0, n,
		      {
			  formatComplex(px, nb, &tmpwr, &tmpdr, &tmper,
					&tmpwi, &tmpdi, &tmpei, nsmall);
			  if(tmpwr > *wr) *wr = tmpwr;
			  if(tmpdr && !*dr) *dr = tmpdr;
			  if(tmper > *er) *er = tmper;
			  if(tmpwi > *wi) *wi = tmpwi;
			  if(tmpdi && !*di) *di = tmpdi;
			  if(tmpei > *ei) *ei = tmpei;
		      });
}
