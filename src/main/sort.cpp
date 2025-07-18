/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2025   The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2004        The R Foundation
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

/** @file sort.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat> /* for DBL_MAX */
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Localization.h>
#include <Defn.h> /* => Utils.h with the protos from here; Rinternals.h */
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/RS.h>  /* for R_Calloc/R_Free */
#include <R_ext/Itermacros.h> /* for ITERATE_BY_REGION */

using namespace R;
using namespace CXXR;

			/*--- Part I: Comparison Utilities ---*/

static int icmp(int x, int y, bool nalast)
{
    if (x == NA_INTEGER && y == NA_INTEGER) return 0;
    if (x == NA_INTEGER)return nalast ? 1 : -1;
    if (y == NA_INTEGER)return nalast ? -1 : 1;
    if (x < y)		return -1;
    if (x > y)		return 1;
    return 0;
}

static int rcmp(double x, double y, bool nalast)
{
    int nax = ISNAN(x), nay = ISNAN(y);
    if (nax && nay)	return 0;
    if (nax)		return nalast ? 1 : -1;
    if (nay)		return nalast ? -1 : 1;
    if (x < y)		return -1;
    if (x > y)		return 1;
    return 0;
}

static int ccmp(Rcomplex x, Rcomplex y, bool nalast)
{
    int nax = ISNAN(x.r), nay = ISNAN(y.r);
				/* compare real parts */
    if (nax && nay)	return 0;
    if (nax)		return nalast ? 1 : -1;
    if (nay)		return nalast ? -1 : 1;
    if (x.r < y.r)	return -1;
    if (x.r > y.r)	return 1;
				/* compare complex parts */
    nax = ISNAN(x.i); nay = ISNAN(y.i);
    if (nax && nay)	return 0;
    if (nax)		return nalast ? 1 : -1;
    if (nay)		return nalast ? -1 : 1;
    if (x.i < y.i)	return -1;
    if (x.i > y.i)	return 1;

    return 0;		/* equal */
}

static int scmp(SEXP x, SEXP y, bool nalast)
{
    if (x == NA_STRING && y == NA_STRING) return 0;
    if (x == NA_STRING) return nalast ? 1 : -1;
    if (y == NA_STRING) return nalast ? -1 : 1;
    if (x == y) return 0;  /* same string in cache */
    return Scollate(x, y);
}

namespace CXXR
{
    bool String::Comparator::operator()(const String *l, const String *r) const
    {
        return scmp(const_cast<String *>(l), const_cast<String *>(r), m_na_last) < 0;
    }
} // namespace CXXR

// API: in Rinternals.h
Rboolean Rf_isUnsorted(SEXP x, Rboolean strictly)
{
    R_xlen_t n, i;
    int itmp = NA_INTEGER; /* this is NA_INTEGER, < all valid nonNA R integer
			   values */
    double dtmp = R_NegInf; /*  R_NegInf is min possible double	"value",
			        ***this is <= all nonNA R numeric values but
				NOT < all nonNA	values*** */
    if (!isVectorAtomic(x))
	error("%s", _("only atomic vectors can be tested to be sorted"));
    n = XLENGTH(x);
    if(n >= 2)
	switch (TYPEOF(x)) {

	    /* NOTE: x must have no NAs {is.na(.) in R};
	       hence be faster than `rcmp()', `icmp()' for these two cases */

	    /* The only difference between strictly and not is '>' vs '>='
	       but we want the if() outside the loop */

	    /* x can be an ALTREP here provided that its sortedness is unknown, 
	       so we use ITERATE_BY_REGION to get the multiple INTEGER calls
	       outside of the tight loop and be ALTREP safe. */
	case LGLSXP:
	    if (strictly) {
		ITERATE_BY_REGION(x, xptr, i, nbatch, int, LOGICAL, {
			/* itmp initialized to INT_MIN which is < all
			   valid nonNA R int values so no need to
			   exclude first iteration

			   After first iteration itmp is value at end
			   of last batch */
			if (itmp >= xptr[0]) 
			    return TRUE;
			for (R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if (xptr[k] >= xptr[k+1])
				return TRUE;
			}
			itmp = xptr[nbatch - 1];
		    });
	    } else {
		ITERATE_BY_REGION(x, xptr, i, nbatch, int, LOGICAL, {
			if (itmp > xptr[0]) //deal with batch barriers
			    return TRUE;
			for (R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if (xptr[k] > xptr[k+1])
				return TRUE;
			}
			itmp = xptr[nbatch - 1];
		    });
	    }
	    break;
	case INTSXP:
	    if(strictly) {
		ITERATE_BY_REGION(x, xptr, i, nbatch, int, INTEGER, {
			/* itmp initialized to INT_MIN which is < all
			   valid nonNA R int values so no need to
			   exclude first iteration

			   After first iteration itmp is value at end
			   of last batch */
			if(itmp >= xptr[0]) 
			    return TRUE;
			for(R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if(xptr[k] >= xptr[k+1])
				return TRUE;
			}
			itmp = xptr[nbatch - 1];
		    });
	    } else {
		ITERATE_BY_REGION(x, xptr, i, nbatch, int, INTEGER, {
			if(itmp > xptr[0]) //deal with batch barriers
			    return TRUE;
			for(R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if(xptr[k] > xptr[k+1])
				return TRUE;
			}
			itmp = xptr[nbatch - 1];
		    });
	    }
	    break;
	case REALSXP:
	    if(strictly) {
		ITERATE_BY_REGION(x, xptr, i, nbatch, double, REAL, {
			/* first element could be R_NegInf which is
			   initialization value of dtmp so don't do
			   the barrier check at i == 0 since its >=
			   here

			   After first iteration dtmp is value at end
			   of last batch */
			if(i > 0 && dtmp >= xptr[0]) //deal with batch barriers
			    return TRUE;
			for(R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if(xptr[k] >= xptr[k+1])
				return TRUE;
			}
			dtmp = xptr[nbatch - 1];
		    });
	    } else {
		/* nonstrict, first element can never be < dtmp (== R_NegInf),
		   so no need to exclude first iteration in barrier check */
		ITERATE_BY_REGION(x, xptr, i, nbatch, double, REAL, {
			if(dtmp > xptr[0]) /* deal with batch barriers, dtmp
					      is end of last batch */
			    return TRUE;
			for(R_xlen_t k = 0; k < nbatch - 1; k++) {
			    if(xptr[k] > xptr[k+1])
				return TRUE;
			}
			dtmp = xptr[nbatch - 1];
		    });
	    }
	    break;
	case CPLXSXP:
	    if(strictly) {
		for(i = 0; i+1 < n ; i++)
		    if(ccmp(COMPLEX(x)[i], COMPLEX(x)[i+1], true) >= 0)
			return TRUE;
	    } else {
		for(i = 0; i+1 < n ; i++)
		    if(ccmp(COMPLEX(x)[i], COMPLEX(x)[i+1], true) > 0)
			return TRUE;
	    }
	    break;
	case STRSXP:
	    if(strictly) {
		for(i = 0; i+1 < n ; i++)
		    if(scmp(STRING_ELT(x, i ),
			    STRING_ELT(x,i+1), true) >= 0)
			return TRUE;
	    } else {
		for(i = 0; i+1 < n ; i++)
		    if(scmp(STRING_ELT(x, i ),
			    STRING_ELT(x,i+1), true) > 0)
			return TRUE;
	    }
	    break;
	case RAWSXP: // being compatible with raw_relop() in ./relop.c
	    if(strictly) {
		for(i = 0; i+1 < n ; i++)
		    if(RAW(x)[i] >= RAW(x)[i+1])
			return TRUE;

	    } else {
		for(i = 0; i+1 < n ; i++)
		    if(RAW(x)[i] > RAW(x)[i+1])
			return TRUE;
	    }
	    break;
	default:
	    UNIMPLEMENTED_TYPE("isUnsorted", x);
	}
    return FALSE;/* sorted */
} // isUnsorted()

#define FIRST_LAST_DIFF(x, type) type##_ELT(x, 0) != type##_ELT(x, XLENGTH(x) - 1)
/* assumes no NAs, which is the case for do_isunsorted, they're removed in R code */
#define SORTED_VEC_NONCONST(x)	(XLENGTH(x) > 1 &&			\
     ((TYPEOF(x) == INTSXP && FIRST_LAST_DIFF(x, INTEGER)) ||		\
      (TYPEOF(x) == REALSXP && FIRST_LAST_DIFF(x, REAL))))

attribute_hidden SEXP do_isunsorted(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (length(args) == 2) {
	/* This allows for old calls of the form
	   .Internal(is.unsorted(x, strictly)) prior to adding the
	   na.rm argument in order to match the closure signature to
	   the signature expected by methods called in
	   DispatchOrEval. These old calls might have been captured in
	   closures or S4 generic definitions. This code inserts a
	   value for the na.rm argument in the list of evaluated
	   arguments. The first cell of args is protected when
	   do_isunsorted is called. */
	SEXP tmp = CONS_NR(R_FalseValue, CDR(args));
	SETCDR(args, tmp);
    }
    checkArity(op, args);

    SEXP ans, x = CAR(args);
    /* DispatchOrEval internal generic: is.unsorted */
    if(DispatchOrEval(call, op, "is.unsorted", args, rho, &ans, 0, 1))
	return ans;
    PROTECT(args = ans); // args evaluated now

    int sorted = UNKNOWN_SORTEDNESS;
    switch(TYPEOF(x)) {
    case INTSXP:
	sorted = INTEGER_IS_SORTED(x);
	break;
    case REALSXP:
	sorted = REAL_IS_SORTED(x);
	break;
    default:
	break;
    }

    SEXP strictlyArg = CADDR(args);
    /* right now is.unsorted only tells you if something is sorted ascending
      hopefully someday it will work for descending too */
    if(!asLogical(strictlyArg)) { /*not strict since we don't memoize that */
	if(KNOWN_INCR(sorted)) {
	    UNPROTECT(1);
	    return ScalarLogical(FALSE);
	}
	/* is.unsorted returns TRUE for vectors sorted in descending order 
	   iff the vector has more than 1 unique value */
	else if(KNOWN_DECR(sorted)) {
	    UNPROTECT(1);
	    return ScalarLogical(SORTED_VEC_NONCONST(x));
	} else if (sorted == KNOWN_UNSORTED) {
	    UNPROTECT(1);
	    return ScalarLogical(TRUE);
	}
    }

    bool strictly = asLogicalNoNA(strictlyArg, "strictly");
    if(isVectorAtomic(x)) {
	UNPROTECT(1);
	return (xlength(x) < 2) ? ScalarLogical(FALSE) :
	    ScalarLogical(isUnsorted(x, (Rboolean)strictly));
    }
    if(isObject(x)) {
	SEXP call;
	PROTECT(call = 	// R>  .gtn(x, strictly) :
		lang3(install(".gtn"), x, strictlyArg));
	ans = eval(call, rho);
	UNPROTECT(2);
	return ans;
    }
    else {
	UNPROTECT(1);
	return ScalarLogical(NA_LOGICAL);
    }
}


			/*--- Part II: Complete (non-partial) Sorting ---*/


/* SHELLsort -- corrected from R. Sedgewick `Algorithms in C'
 *		(version of BDR's lqs():*/
#define sort_body(TYPE_CMP, TYPE_PROT, TYPE_UNPROT)	\
    bool nalast = true;					\
    int i, j, h;					\
							\
    for (h = 1; h <= n / 9; h = 3 * h + 1);		\
    for (; h > 0; h /= 3)				\
	for (i = h; i < n; i++) {			\
	    v = TYPE_PROT(x[i]);			\
	    j = i;					\
	    while (j >= h && TYPE_CMP(x[j - h], v, nalast) > 0)	\
		 { x[j] = x[j - h]; j -= h; }		\
	    x[j] = v;					\
	    TYPE_UNPROT;				\
	}

void R_isort(int *x, int n)
{
    int v;
    sort_body(icmp,,)
}

void R_rsort(double *x, int n)
{
    double v;
    sort_body(rcmp,,)
}

void R_csort(Rcomplex *x, int n)
{
    Rcomplex v;
    sort_body(ccmp,,)
}

/* used in platform.c */
attribute_hidden void R::ssort(SEXP *x, int n)
{
    SEXP v;
    sort_body(scmp,PROTECT,UNPROTECT(1))
}

void rsort_with_index(double *x, int *indx, int n)
{
    double v;
    int i, j, h, iv;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && rcmp(x[j - h], v, true) > 0)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}

void Rf_revsort(double *a, int *ib, int n)
{
/* Sort a[] into descending order by "heapsort";
 * sort ib[] alongside;
 * if initially, ib[] = 1...n, it will contain the permutation finally
 */

    int l, j, ir, i;
    double ra;
    int ii;

    if (n <= 1) return;

    a--; ib--;

    l = (n >> 1) + 1;
    ir = n;

    for (;;) {
	if (l > 1) {
	    l = l - 1;
	    ra = a[l];
	    ii = ib[l];
	}
	else {
	    ra = a[ir];
	    ii = ib[ir];
	    a[ir] = a[1];
	    ib[ir] = ib[1];
	    if (--ir == 1) {
		a[1] = ra;
		ib[1] = ii;
		return;
	    }
	}
	i = l;
	j = l << 1;
	while (j <= ir) {
	    if (j < ir && a[j] > a[j + 1]) ++j;
	    if (ra > a[j]) {
		a[i] = a[j];
		ib[i] = ib[j];
		j += (i = j);
	    }
	    else
		j = ir + 1;
	}
	a[i] = ra;
	ib[i] = ii;
    }
}


attribute_hidden SEXP do_sort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity(op, args);

    bool decreasing = asRbool(CADR(args), call);
    if (CAR(args) == R_NilValue) return R_NilValue;
    if (!isVectorAtomic(CAR(args)))
	error("%s", _("only atomic vectors can be sorted"));
    if (TYPEOF(CAR(args)) == RAWSXP)
	error("%s", _("raw vectors cannot be sorted"));

    /* we need consistent behaviour here, including dropping attributes,
       so as from 2.3.0 we always duplicate. */
    PROTECT(ans = duplicate(CAR(args)));
    SET_ATTRIB(ans, R_NilValue);  /* this is never called with names */
    SET_OBJECT(ans, 0);		  /* we may have just stripped off the class */
    sortVector(ans, decreasing);
    UNPROTECT(1);
    return ans; /* wrapping with metadata happens at end of sort.int */
}

static bool fastpass_sortcheck(SEXP x, int wanted) {
    if (!KNOWN_SORTED(wanted)) 
	return false;

    int sorted = UNKNOWN_SORTEDNESS;
    bool noNA = false, done = false;

    switch(TYPEOF(x)) {
    case INTSXP:
	sorted = INTEGER_IS_SORTED(x);
	noNA = INTEGER_NO_NA(x);
	break;
    case REALSXP:
	sorted = REAL_IS_SORTED(x);
	noNA = REAL_NO_NA(x);
	break;
    default:
	/* keep sorted == UNKNOWN_SORTEDNESS */
	break;
    }

    /* we know wanted is not NA_INTEGER or 0 at this point because
       of the immediate return at the beginning for that case */
    if(!KNOWN_SORTED(sorted)) {
	done = false;
    } else if(sorted == wanted) {   
	done = true;
	/* if there are no NAs, na.last can be ignored */
    } else if(noNA && sorted * wanted > 0) {
	/* same sign, thus same direction of sort */
	done = true;
    }

    /* Increasing, usually fairly short, sequences of integers often
       arise as levels in as.factor.  A quick check here allows a fast
       return in sort.int. */
    if (! done && TYPEOF(x) == INTSXP && wanted > 0 && ! ALTREP(x)) {
	R_xlen_t len = XLENGTH(x);
	if (len > 0) {
	    int *px = INTEGER(x);
	    int last = px[0];
	    if (last != NA_INTEGER) {
		for (R_xlen_t i = 1; i < len; i++) {
		    int next = px[i];
		    if (next < last || next == NA_INTEGER)
			return false;
		    else last = next;
		}
		return true;
	    }
	}
    }

    return done;
}

static int makeSortEnum(int decr, int nalast) {

    /* passing decr = NA_INTEGER indicates UNKNOWN_SORTEDNESS. */
    if (decr == NA_INTEGER)
	return UNKNOWN_SORTEDNESS;

    if (nalast == NA_INTEGER)
	nalast = 1; //  they were/will be removed so we say they are "last"

    if (decr)
	return nalast ? SORTED_DECR : SORTED_DECR_NA_1ST;
    else /* increasing */
	return nalast ? SORTED_INCR : SORTED_INCR_NA_1ST;
}

/* .Internal(sorted_fpass(x, decr, nalast)) */
attribute_hidden SEXP do_sorted_fpass(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    int decr = asInteger(CADR(args));
    int nalast = asInteger(CADDR(args));
    int wanted = makeSortEnum(decr, nalast);
    SEXP x = PROTECT(CAR(args));
    bool wassorted = fastpass_sortcheck(x, wanted);
    UNPROTECT(1);
    return ScalarLogical(wassorted);
}


/* faster versions of shellsort, following Sedgewick (1986) */

/* c(1, 4^k +3*2^(k-1)+1) */
#ifdef LONG_VECTOR_SUPPORT
// This goes up to 2^38: extend eventually.
#define NI 20
static const R_xlen_t incs[NI + 1] = {
    274878693377L, 68719869953L, 17180065793L, 4295065601L,
    1073790977L, 268460033L, 67121153L, 16783361L, 4197377L, 1050113L,
    262913L, 65921L, 16577L, 4193L, 1073L, 281L, 77L, 23L, 8L, 1L, 0L
};
#else
#define NI 16
static const int incs[NI + 1] = {
    1073790977, 268460033, 67121153, 16783361, 4197377, 1050113,
    262913, 65921, 16577, 4193, 1073, 281, 77, 23, 8, 1, 0
};
#endif

#define sort2_body \
    for (h = incs[t]; t < NI; h = incs[++t]) \
	for (i = h; i < n; i++) { \
	    v = x[i]; \
	    j = i; \
	    while (j >= h && x[j - h] less v) { x[j] = x[j - h]; j -= h; } \
	    x[j] = v; \
	}

/* These are only called with n >= 2 */
static void R_isort2(int *x, R_xlen_t n, bool decreasing)
{
    int v;
    R_xlen_t i, j, h, t;

    if (n < 2) error("%s", _("'n >= 2' is required"));
    for (t = 0; incs[t] > n; t++);
    if(decreasing)
#define less <
	sort2_body
#undef less
    else
#define less >
	sort2_body
#undef less
}

static void R_rsort2(double *x, R_xlen_t n, bool decreasing)
{
    double v;
    R_xlen_t i, j, h, t;

    if (n < 2) error("%s", _("'n >= 2' is required"));
    for (t = 0; incs[t] > n; t++);
    if(decreasing)
#define less <
	sort2_body
#undef less
    else
#define less >
	sort2_body
#undef less
}

static void R_csort2(Rcomplex *x, R_xlen_t n, bool decreasing)
{
    Rcomplex v;
    R_xlen_t i, j, h, t;

    if (n < 2) error("%s", _("'n >= 2' is required"));
    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < NI; h = incs[++t])
	for (i = h; i < n; i++) {
	    v = x[i];
	    j = i;
	    if(decreasing)
		while (j >= h && (x[j - h].r < v.r ||
				  (x[j - h].r == v.r && x[j - h].i < v.i)))
		{ x[j] = x[j - h]; j -= h; }
	    else
		while (j >= h && (x[j - h].r > v.r ||
				  (x[j - h].r == v.r && x[j - h].i > v.i)))
		{ x[j] = x[j - h]; j -= h; }
	    x[j] = v;
	}
}

static void ssort2(SEXP *x, R_xlen_t n, bool decreasing)
{
    SEXP v;
    R_xlen_t i, j, h, t;

    if (n < 2) error("%s", _("'n >= 2' is required"));
    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < NI; h = incs[++t])
	for (i = h; i < n; i++) {
	    v = x[i];
	    j = i;
	    PROTECT(v);
	    if(decreasing)
		while (j >= h && scmp(x[j - h], v, true) < 0)
		{ x[j] = x[j - h]; j -= h; }
	    else
		while (j >= h && scmp(x[j - h], v, true) > 0)
		{ x[j] = x[j - h]; j -= h; }
	    x[j] = v;
	    UNPROTECT(1); /* v */
	}
}

/* The meat of sort.int() */
// Used in envir.c library/utils/src/io.c
void R::sortVector(SEXP s, bool decreasing)
{
    R_xlen_t n = XLENGTH(s);
    if (n >= 2 && (decreasing || isUnsorted(s, FALSE)))
	switch (TYPEOF(s)) {
	case LGLSXP:
	    R_isort2(LOGICAL(s), n, decreasing);
	    break;
	case INTSXP:
	    R_isort2(INTEGER(s), n, decreasing);
	    break;
	case REALSXP:
	    R_rsort2(REAL(s), n, decreasing);
	    break;
	case CPLXSXP:
	    R_csort2(COMPLEX(s), n, decreasing);
	    break;
	case STRSXP:
	    ssort2(STRING_PTR(s), n, decreasing);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("sortVector", s);
	}
}


			/*--- Part III: Partial Sorting ---*/

/*
   Partial sort so that x[k] is in the correct place, smaller to left,
   larger to right

   NOTA BENE:  k < n  required, and *not* checked here but in do_psort();
	       -----  infinite loop possible otherwise!
 */
#define psort_body						\
    bool nalast=true;						\
    R_xlen_t L, R, i, j;					\
								\
    for (L = lo, R = hi; L < R; ) {				\
	v = x[k];						\
	for(i = L, j = R; i <= j;) {				\
	    while (TYPE_CMP(x[i], v, nalast) < 0) i++;		\
	    while (TYPE_CMP(v, x[j], nalast) < 0) j--;		\
	    if (i <= j) { w = x[i]; x[i++] = x[j]; x[j--] = w; }\
	}							\
	if (j < k) L = i;					\
	if (k < i) R = j;					\
    }


static void iPsort2(int *x, R_xlen_t lo, R_xlen_t hi, R_xlen_t k)
{
    int v, w;
#define TYPE_CMP icmp
    psort_body
#undef TYPE_CMP
}

static void rPsort2(double *x, R_xlen_t lo, R_xlen_t hi, R_xlen_t k)
{
    double v, w;
#define TYPE_CMP rcmp
    psort_body
#undef TYPE_CMP
}

static void cPsort2(Rcomplex *x, R_xlen_t lo, R_xlen_t hi, R_xlen_t k)
{
    Rcomplex v, w;
#define TYPE_CMP ccmp
    psort_body
#undef TYPE_CMP
}


static void sPsort2(SEXP *x, R_xlen_t lo, R_xlen_t hi, R_xlen_t k)
{
    SEXP v, w;
#define TYPE_CMP scmp
    psort_body
#undef TYPE_CMP
}


/* Needed for mistaken decision to put these in the API */
void Rf_iPsort(int *x, int n, int k)
{
    iPsort2(x, 0, n-1, k);
}

void Rf_rPsort(double *x, int n, int k)
{
    rPsort2(x, 0, n-1, k);
}

void Rf_cPsort(Rcomplex *x, int n, int k)
{
    cPsort2(x, 0, n-1, k);
}

/* lo, hi, k are 0-based */
static void Psort(SEXP x, R_xlen_t lo, R_xlen_t hi, R_xlen_t k)
{
    /* Rprintf("looking for index %d in (%d, %d)\n", k, lo, hi);*/
    switch (TYPEOF(x)) {
    case LGLSXP:
	iPsort2(LOGICAL(x), lo, hi, k);
	break;
    case INTSXP:
	iPsort2(INTEGER(x), lo, hi, k);
	break;
    case REALSXP:
	rPsort2(REAL(x), lo, hi, k);
	break;
    case CPLXSXP:
	cPsort2(COMPLEX(x), lo, hi, k);
	break;
    case STRSXP:
	sPsort2(STRING_PTR(x), lo, hi, k);
	break;
    default:
	UNIMPLEMENTED_TYPE("Psort", x);
    }
}


/* Here ind are 1-based indices passed from R */
static void Psort0(SEXP x, R_xlen_t lo, R_xlen_t hi, R_xlen_t *ind, int nind)
{
    if(nind < 1 || hi - lo < 1) return;
    if(nind <= 1)
	Psort(x, lo, hi, ind[0] - 1);
    else {
    /* Look for index nearest the centre of the range */
	int This = 0;
	R_xlen_t mid = (lo+hi)/2, z;
	for(int i = 0; i < nind; i++) if(ind[i] - 1 <= mid) This = i;
	z = ind[This] - 1;
	Psort(x, lo, hi, z);
	Psort0(x, lo, z-1, ind, This);
	Psort0(x, z+1, hi, ind + This + 1, nind - This -1);
    }
}


/* FUNCTION psort(x, indices) */
attribute_hidden SEXP do_psort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args), p = CADR(args);

    if (!isVectorAtomic(x))
	error("%s", _("only atomic vectors can be sorted"));
    if(TYPEOF(x) == RAWSXP)
	error("%s", _("raw vectors cannot be sorted"));
    R_xlen_t n = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    if(!IS_LONG_VEC(x) || TYPEOF(p) != REALSXP)
	SETCADR(args, coerceVector(p, INTSXP));
    p = CADR(args);
    int nind = LENGTH(p);
    R_xlen_t *l = (R_xlen_t *) R_alloc(nind, sizeof(R_xlen_t));
    if (TYPEOF(p) == REALSXP) {
	double *rl = REAL(p);
	for (int i = 0; i < nind; i++) {
	    if (!R_FINITE(rl[i])) error("%s", _("NA or infinite index"));
	    l[i] = (R_xlen_t) rl[i];
	    if (l[i] < 1 || l[i] > n)
		error(_("'partial' index %lld outside bounds"), (long long)l[i]);
	}
    } else {
	int *il = INTEGER(p);
	for (int i = 0; i < nind; i++) {
	    if (il[i] == NA_INTEGER) error("%s", _("NA index"));
	    if (il[i] < 1 || il[i] > n)
		error(_("'partial' index %d outside bounds"), il[i]);
	    l[i] = il[i];
	}
    }
#else
    SETCADR(args, coerceVector(p, INTSXP));
    p = CADR(args);
    int nind = LENGTH(p);
    int *l = INTEGER(p);
    for (int i = 0; i < nind; i++) {
	if (l[i] == NA_INTEGER)
	    error("%s", _("NA index"));
	if (l[i] < 1 || l[i] > n)
	    error(_("'partial' index %d outside bounds"), l[i]);
    }
#endif
    SETCAR(args, duplicate(x));
    SET_ATTRIB(CAR(args), R_NilValue);  /* remove all attributes */
    SET_OBJECT(CAR(args), 0);           /* and the object bit    */
    Psort0(CAR(args), 0, n - 1, l, nind);
    return CAR(args);
}


			/*--- Part IV : Rank & Order ---*/

static bool equal(R_xlen_t i, R_xlen_t j, SEXP x, bool nalast, SEXP rho)
{
    int c = -1;

    if (isObject(x) && !isNull(rho)) { /* so never any NAs */
	/* evaluate .gt(x, i, j) */
	GCStackRoot<> si, sj, call;
	si = ScalarInteger((int)i+1);
	sj = ScalarInteger((int)j+1);
	call = lang4(install(".gt"), x, si, sj);
	c = asInteger(eval(call, rho));
    } else {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    c = icmp(LOGICAL(x)[i], LOGICAL(x)[j], nalast);
	    break;
	case INTSXP:
	    c = icmp(INTEGER(x)[i], INTEGER(x)[j], nalast);
	    break;
	case REALSXP:
	    c = rcmp(REAL(x)[i], REAL(x)[j], nalast);
	    break;
	case CPLXSXP:
	    c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j], nalast);
	    break;
	case STRSXP:
	    c = scmp(STRING_ELT(x, i), STRING_ELT(x, j), nalast);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("equal", x);
	    break;
	}
    }
    return (c == 0);
}

static bool greater(R_xlen_t i, R_xlen_t j, SEXP x, bool nalast,
		   bool decreasing, SEXP rho)
{
    int c = -1;

    if (isObject(x) && !isNull(rho)) { /* so never any NAs */
	/* evaluate .gt(x, i, j) */
	GCStackRoot<> si, sj, call;
	si = ScalarInteger((int)i+1);
	sj = ScalarInteger((int)j+1);
	call = lang4(install(".gt"), x, si, sj);
	c = asInteger(eval(call, rho));
    } else {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    c = icmp(LOGICAL(x)[i], LOGICAL(x)[j], nalast);
	    break;
	case INTSXP:
	    c = icmp(INTEGER(x)[i], INTEGER(x)[j], nalast);
	    break;
	case REALSXP:
	    c = rcmp(REAL(x)[i], REAL(x)[j], nalast);
	    break;
	case CPLXSXP:
	    c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j], nalast);
	    break;
	case STRSXP:
	    c = scmp(STRING_ELT(x, i), STRING_ELT(x, j), nalast);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("greater", x);
	    break;
	}
    }
    if (decreasing) c = -c;
    return (c > 0 || (c == 0 && j < i));
}

/* listgreater(): used as greater_sub in orderVector() in do_order(...) */
static bool listgreater(int i, int j, SEXP key, bool nalast,
		       bool decreasing)
{
    SEXP x;
    int c = -1;

    while (key != R_NilValue) {
	x = CAR(key);
	switch (TYPEOF(x)) {
	case LGLSXP:
	    c = icmp(LOGICAL(x)[i], LOGICAL(x)[j], nalast);
	    break;
	case INTSXP:
	    c = icmp(INTEGER(x)[i], INTEGER(x)[j], nalast);
	    break;
	case REALSXP:
	    c = rcmp(REAL(x)[i], REAL(x)[j], nalast);
	    break;
	case CPLXSXP:
	    c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j], nalast);
	    break;
	case STRSXP:
	    c = scmp(STRING_ELT(x, i), STRING_ELT(x, j), nalast);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("listgreater", x);
	}
	if (decreasing) c = -c;
	if (c > 0)
	    return 1;
	if (c < 0)
	    return 0;
	key = CDR(key);
    }
    if (c == 0 && i < j) return 0; else return 1;
}


#define GREATER_2_SUB_DEF(FNAME, TYPE_1, TYPE_2, CMP_FN_1, CMP_FN_2)	\
static int FNAME(int i, int j,						\
		 TYPE_1 *x, TYPE_2 *y,					\
		 bool nalast, bool decreasing)				\
{									\
    int CMP_FN_1(TYPE_1, TYPE_1, bool);					\
    int CMP_FN_2(TYPE_2, TYPE_2, bool);					\
									\
    int c = CMP_FN_1(x[i], x[j], nalast);				\
    if(c) {								\
	if (decreasing) c = -c;						\
	if (c > 0) return 1;						\
	/* else: (c < 0) */ return 0;					\
    }									\
    else {/* have a tie in x -- use  y[]: */				\
	c = CMP_FN_2(y[i], y[j], nalast);				\
	if(c) {								\
	    if (decreasing) c = -c;					\
	    if (c > 0) return 1;					\
	    /* else: (c < 0) */ return 0;				\
	}								\
	else { /* tie in both x[] and y[] : */				\
	    if (i < j) return 0;					\
	    /* else */ return 1;					\
	}								\
    }									\
}

static const int sincs[17] = {
    1073790977, 268460033, 67121153, 16783361, 4197377, 1050113,
    262913, 65921, 16577, 4193, 1073, 281, 77, 23, 8, 1, 0
};

// Needs indx set to  0:(n-1)  initially :
static void orderVector(int *indx, int n, SEXP key, bool nalast,
	    bool decreasing,
	    bool greater_sub(int, int, SEXP, bool, bool))
{
    int i, j, h, t;
    int itmp;

    if (n < 2) return;
    for (t = 0; sincs[t] > n; t++);
    for (h = sincs[t]; t < 16; h = sincs[++t]) {
	R_CheckUserInterrupt();
	for (i = h; i < n; i++) {
	    itmp = indx[i];
	    j = i;
	    while (j >= h &&
		   greater_sub(indx[j - h], itmp, key, (nalast^decreasing),
			       decreasing)) {
		indx[j] = indx[j - h];
		j -= h;
	    }
	    indx[j] = itmp;
	}
    }
}

#ifdef LONG_VECTOR_SUPPORT
static bool listgreaterl(R_xlen_t i, R_xlen_t j, SEXP key, bool nalast,
		       bool decreasing)
{
    SEXP x;
    int c = -1;

    while (key != R_NilValue) {
	x = CAR(key);
	switch (TYPEOF(x)) {
	case LGLSXP:
	    c = icmp(LOGICAL(x)[i], LOGICAL(x)[j], nalast);
	    break;
	case INTSXP:
	    c = icmp(INTEGER(x)[i], INTEGER(x)[j], nalast);
	    break;
	case REALSXP:
	    c = rcmp(REAL(x)[i], REAL(x)[j], nalast);
	    break;
	case CPLXSXP:
	    c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j], nalast);
	    break;
	case STRSXP:
	    c = scmp(STRING_ELT(x, i), STRING_ELT(x, j), nalast);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("listgreater", x);
	}
	if (decreasing) c = -c;
	if (c > 0)
	    return 1;
	if (c < 0)
	    return 0;
	key = CDR(key);
    }
    if (c == 0 && i < j) return 0; else return 1;
}

static void orderVectorl(R_xlen_t *indx, R_xlen_t n, SEXP key, bool nalast,
	     bool decreasing,
	     bool greater_sub(R_xlen_t, R_xlen_t, SEXP, bool, bool))
{
    int t;
    R_xlen_t i, j, h;
    R_xlen_t itmp;

    if (n < 2) return;
    for (t = 0; incs[t] > n; t++);
    for (h = incs[t]; t < NI; h = incs[++t]) {
	R_CheckUserInterrupt();
	for (i = h; i < n; i++) {
	    itmp = indx[i];
	    j = i;
	    while (j >= h &&
		   greater_sub(indx[j - h], itmp, key, (nalast^decreasing),
			       decreasing)) {
		indx[j] = indx[j - h];
		j -= h;
	    }
	    indx[j] = itmp;
	}
    }
}
#endif

#ifdef UNUSED
#define ORD_2_BODY(FNAME, TYPE_1, TYPE_2, GREATER_2_SUB)		\
    void FNAME(int *indx, int n, TYPE_1 *x, TYPE_2 *y,			\
	   bool nalast, bool decreasing)				\
{									\
    int t;								\
    for(t = 0; t < n; t++) indx[t] = t; /* indx[] <- 0:(n-1) */		\
    if (n < 2) return;							\
    for(t = 0; sincs[t] > n; t++);					\
    for (int h = sincs[t]; t < 16; h = sincs[++t])			\
	for (int i = h; i < n; i++) {					\
	    int itmp = indx[i], j = i;					\
	    while (j >= h &&						\
		   GREATER_2_SUB(indx[j - h], itmp, x, y,		\
				 nalast^decreasing, decreasing)) {	\
		indx[j] = indx[j - h];					\
		j -= h;							\
	    }								\
	    indx[j] = itmp;						\
	}								\
}

ORD_2_BODY(R_order2double , double, double, double2greater)
ORD_2_BODY(R_order2int    ,    int,    int,    int2greater)
ORD_2_BODY(R_order2dbl_int, double,    int, dblint2greater)
ORD_2_BODY(R_order2int_dbl,    int, double, intdbl2greater)


GREATER_2_SUB_DEF(double2greater, double, double, rcmp, rcmp)
GREATER_2_SUB_DEF(int2greater,       int,    int, icmp, icmp)
GREATER_2_SUB_DEF(dblint2greater, double,    int, rcmp, icmp)
GREATER_2_SUB_DEF(intdbl2greater,    int, double, icmp, rcmp)
#endif

#define sort2_with_index \
    for (h = sincs[t]; t < 16; h = sincs[++t]) { \
	R_CheckUserInterrupt();	 \
	for (i = lo + h; i <= hi; i++) {	 \
	    itmp = indx[i];			 \
	    j = i;						     \
	    while (j >= lo + h && less(indx[j - h], itmp)) {	     \
		indx[j] = indx[j - h]; j -= h; }		     \
	    indx[j] = itmp;					     \
	}							     \
    }


/* TODO: once LONG_VECTOR_SUPPORT and  R_xlen_t  belong to the R API,
 * ----  also add "long" versions, say,
 *    R_orderVectorl (R_xlen_t *indx, R_xlen_t n, SEXP arglist, ...)
 *    R_orderVector1l(R_xlen_t *indx, R_xlen_t n, SEXP arg,  ...)
 * to the API */

// Usage:  R_orderVector(indx, n,  Rf_lang2(x,y),  nalast, decreasing)
// In Rinternals.h
void R_orderVector(int *indx, // must be pre-allocated to length >= n
		   int n,
		   SEXP arglist, // <- e.g.  Rf_lang2(x,y)
		   Rboolean nalast, Rboolean decreasing)
{
    // idx[] <- 0:(n-1) :
    for(int i = 0; i < n; i++) indx[i] = i;
    orderVector(indx, n, arglist, nalast, decreasing, listgreater);
    return;
}

// Fast version of 1-argument case of R_orderVector()
// In Rinternals.h
void R_orderVector1(int *indx, int n, SEXP x,
		    Rboolean nalast, Rboolean decreasing)
{
    for(int i = 0; i < n; i++) indx[i] = i;
    orderVector1(indx, n, x, nalast, decreasing, R_NilValue);
}



/* Needs indx set to  0:(n-1)  initially.
   Also used by do_options and  ../gnuwin32/extra.c
   Called with rho != R_NilValue only from do_rank, when NAs are not involved.
 */
// used in envir.c options.c library/utils/src/windows/registry.c
attribute_hidden void R::orderVector1(int *indx, int n, SEXP key, bool nalast, bool decreasing,
	     SEXP rho)
{
    int c, i, j, h, t, lo = 0, hi = n-1;
    int itmp, numna = 0;
    bool *isna = NULL;
    int *ix = NULL /* -Wall */;
    double *x = NULL /* -Wall */;
    Rcomplex *cx = NULL /* -Wall */;
    SEXP *sx = NULL /* -Wall */;

    if (n < 2) return;
    switch (TYPEOF(key)) {
    case LGLSXP:
	ix = LOGICAL(key);
	break;
    case INTSXP:
	ix = INTEGER(key);
	break;
    case REALSXP:
	x = REAL(key);
	break;
    case STRSXP:
	sx = STRING_PTR(key);
	break;
    case CPLXSXP:
	cx = COMPLEX(key);
	break;
    default:  // -Wswitch
	break;
    }

    if(isNull(rho)) {
	/* First sort NAs to one end */
	isna = R_Calloc(n, bool);
	switch (TYPEOF(key)) {
	case LGLSXP:
	    for (i = 0; i < n; i++) isna[i] = (ix[i] == NA_LOGICAL);
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++) isna[i] = (ix[i] == NA_INTEGER);
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++) isna[i] = ISNAN(x[i]);
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++) isna[i] = (sx[i] == NA_STRING);
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++) isna[i] = ISNAN(cx[i].r) || ISNAN(cx[i].i);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("orderVector1", key);
	}
	for (i = 0; i < n; i++) numna += isna[i];

	if(numna)
	    switch (TYPEOF(key)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case STRSXP:
	    case CPLXSXP:
		if (!nalast) for (i = 0; i < n; i++) isna[i] = !isna[i];
		for (t = 0; sincs[t] > n; t++);
#define less(a, b) (isna[a] > isna[b] || (isna[a] == isna[b] && a > b))
		sort2_with_index
#undef less
		if (n - numna < 2) {
		    R_Free(isna);
		    return;
		}
		if (nalast) hi -= numna; else lo += numna;
		break;
	    default: Rf_error("%s", _("invalid type")); break;
	    }
    }

    /* Shell sort isn't stable, so add test on index */

    for (t = 0; sincs[t] > hi-lo+1; t++);

    if (isObject(key) && !isNull(rho)) {
/* only reached from do_rank */
#define less(a, b) greater(a, b, key, (nalast^decreasing), decreasing, rho)
	    sort2_with_index
#undef less
    } else {
	switch (TYPEOF(key)) {
	case LGLSXP:
	case INTSXP:
	    if (decreasing) {
#define less(a, b) (ix[a] < ix[b] || (ix[a] == ix[b] && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (ix[a] > ix[b] || (ix[a] == ix[b] && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case REALSXP:
	    if (decreasing) {
#define less(a, b) (x[a] < x[b] || (x[a] == x[b] && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (x[a] > x[b] || (x[a] == x[b] && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case CPLXSXP:
	    if (decreasing) {
#define less(a, b) (ccmp(cx[a], cx[b], FALSE) < 0 || (cx[a].r == cx[b].r && cx[a].i == cx[b].i && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (ccmp(cx[a], cx[b], FALSE) > 0 || (cx[a].r == cx[b].r && cx[a].i == cx[b].i && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case STRSXP:
	    if (decreasing)
#define less(a, b) (c = Scollate(sx[a], sx[b]), c < 0 || (c == 0 && a > b))
		sort2_with_index
#undef less
	    else
#define less(a, b) (c = Scollate(sx[a], sx[b]), c > 0 || (c == 0 && a > b))
		sort2_with_index
#undef less
	    break;
	default:  /* only reached from do_rank */
#define less(a, b) greater(a, b, key, (nalast^decreasing), decreasing, rho)
	    sort2_with_index
#undef less
	}
    }
    if(isna) R_Free(isna);
}

/* version for long vectors */
#ifdef LONG_VECTOR_SUPPORT
static void orderVector1l(R_xlen_t *indx, R_xlen_t n, SEXP key, bool nalast,
	      bool decreasing, SEXP rho)
{
    R_xlen_t c, i, j, h, t, lo = 0, hi = n-1;
    bool *isna = NULL;
    int numna = 0;
    int *ix = NULL /* -Wall */;
    double *x = NULL /* -Wall */;
    Rcomplex *cx = NULL /* -Wall */;
    SEXP *sx = NULL /* -Wall */;
    R_xlen_t itmp;

    if (n < 2) return;
    switch (TYPEOF(key)) {
    case LGLSXP:
	ix = LOGICAL(key);
	break;
    case INTSXP:
	ix = INTEGER(key);
	break;
    case REALSXP:
	x = REAL(key);
	break;
    case STRSXP:
	sx = STRING_PTR(key);
	break;
    case CPLXSXP:
	cx = COMPLEX(key);
	break;
    default:  // -Wswitch
	break;
    }

    if(isNull(rho)) {
	/* First sort NAs to one end */
	isna = R_Calloc(n, bool);
	switch (TYPEOF(key)) {
	case LGLSXP:
	    for (i = 0; i < n; i++) isna[i] = (ix[i] == NA_LOGICAL);
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++) isna[i] = (ix[i] == NA_INTEGER);
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++) isna[i] = ISNAN(x[i]);
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++) isna[i] = (sx[i] == NA_STRING);
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++) isna[i] = ISNAN(cx[i].r) || ISNAN(cx[i].i);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("orderVector1", key);
	}
	for (i = 0; i < n; i++) numna += isna[i];

	if(numna)
	    switch (TYPEOF(key)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case STRSXP:
	    case CPLXSXP:
		if (!nalast) for (i = 0; i < n; i++) isna[i] = !isna[i];
		for (t = 0; sincs[t] > n; t++);
#define less(a, b) (isna[a] > isna[b] || (isna[a] == isna[b] && a > b))
		sort2_with_index
#undef less
		if (n - numna < 2) {
		    R_Free(isna);
		    return;
		}
		if (nalast) hi -= numna; else lo += numna;
		break;
	    default: Rf_error("%s", _("invalid type")); break;
	    }
    }

    /* Shell sort isn't stable, so add test on index */

    for (t = 0; sincs[t] > hi-lo+1; t++);

    if (isObject(key) && !isNull(rho)) {
/* only reached from do_rank */
#define less(a, b) greater(a, b, key, (nalast^decreasing), decreasing, rho)
	    sort2_with_index
#undef less
    } else {
	switch (TYPEOF(key)) {
	case LGLSXP:
	case INTSXP:
	    if (decreasing) {
#define less(a, b) (ix[a] < ix[b] || (ix[a] == ix[b] && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (ix[a] > ix[b] || (ix[a] == ix[b] && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case REALSXP:
	    if (decreasing) {
#define less(a, b) (x[a] < x[b] || (x[a] == x[b] && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (x[a] > x[b] || (x[a] == x[b] && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case CPLXSXP:
	    if (decreasing) {
#define less(a, b) (ccmp(cx[a], cx[b], FALSE) < 0 || (cx[a].r == cx[b].r && cx[a].i == cx[b].i && a > b))
		sort2_with_index
#undef less
	    } else {
#define less(a, b) (ccmp(cx[a], cx[b], FALSE) > 0 || (cx[a].r == cx[b].r && cx[a].i == cx[b].i && a > b))
		sort2_with_index
#undef less
	    }
	    break;
	case STRSXP:
	    if (decreasing)
#define less(a, b) (c=Scollate(sx[a], sx[b]), c < 0 || (c == 0 && a > b))
		sort2_with_index
#undef less
	    else
#define less(a, b) (c=Scollate(sx[a], sx[b]), c > 0 || (c == 0 && a > b))
		sort2_with_index
#undef less
	    break;
	default:  /* only reached from do_rank */
#define less(a, b) greater(a, b, key, (nalast^decreasing), decreasing, rho)
	    sort2_with_index
#undef less
	}
    }
    if(isna) R_Free(isna);
}
#endif

/* FUNCTION order(...) */
attribute_hidden SEXP do_order(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ap, ans = R_NilValue /* -Wall */;
    int narg = 0;
    R_xlen_t n = -1;

    bool nalast = asLogicalNoNA(CAR(args), "na.last");
    args = CDR(args);
    bool decreasing = asLogicalNoNA(CAR(args), "decreasing");
    args = CDR(args);
    if (args == R_NilValue)
	return R_NilValue;

    if (isVector(CAR(args)))
	n = XLENGTH(CAR(args));
    for (ap = args; ap != R_NilValue; ap = CDR(ap), narg++) {
	if (!isVector(CAR(ap)))
	    error(_("argument %d is not a vector"), narg + 1);
	if (XLENGTH(CAR(ap)) != n)
	    error("%s", _("argument lengths differ"));
    }
    /* NB: collation functions such as Scollate might allocate */
    if (n != 0) {
	if(narg == 1) {
#ifdef LONG_VECTOR_SUPPORT
	    if (n > INT_MAX)  {
		PROTECT(ans = allocVector(REALSXP, n));
		R_xlen_t *in = (R_xlen_t *) R_alloc(n, sizeof(R_xlen_t));
		for (R_xlen_t i = 0; i < n; i++) in[i] = i;
		orderVector1l(in, n, CAR(args), nalast, decreasing,
			      R_NilValue);
		for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = in[i] + 1;
	    } else
#endif
	    {
		PROTECT(ans = allocVector(INTSXP, n));
		for (R_xlen_t i = 0; i < n; i++) INTEGER(ans)[i] = (int) i;
		orderVector1(INTEGER(ans), (int)n, CAR(args), nalast,
			     decreasing, R_NilValue);
		for (R_xlen_t i = 0; i < n; i++) INTEGER(ans)[i]++;
	    }
	} else {
#ifdef LONG_VECTOR_SUPPORT
	    if (n > INT_MAX)  {
		PROTECT(ans = allocVector(REALSXP, n));
		R_xlen_t *in = (R_xlen_t *) R_alloc(n, sizeof(R_xlen_t));
		for (R_xlen_t i = 0; i < n; i++) in[i] = i;
		orderVectorl(in, n, CAR(args), nalast, decreasing,
			     listgreaterl);
		for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = in[i] + 1;
	    } else
#endif
	    {
		PROTECT(ans = allocVector(INTSXP, n));
		for (R_xlen_t i = 0; i < n; i++) INTEGER(ans)[i] = (int) i;
		orderVector(INTEGER(ans), (int) n, args, nalast,
			    decreasing, listgreater);
		for (R_xlen_t i = 0; i < n; i++) INTEGER(ans)[i]++;
	    }
	}
	UNPROTECT(1);
	return ans;
    } else return allocVector(INTSXP, 0);
}

/* FUNCTION: rank(x, length, ties.method) */
attribute_hidden SEXP do_rank(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rank, x;
    int *ik = NULL /* -Wall */;
    double *rk = NULL /* -Wall */;
    enum {AVERAGE, MAX, MIN} ties_kind = AVERAGE;
    bool isLong = false;

    checkArity(op, args);
    x = CAR(args);
    if(TYPEOF(x) == RAWSXP && !isObject(x))
	error("%s", _("raw vectors cannot be sorted"));
    // n := sn := length(x) :
#ifdef LONG_VECTOR_SUPPORT
    SEXP sn = CADR(args);
    R_xlen_t n;
    if (TYPEOF(sn) == REALSXP)  {
	double d = REAL(sn)[0];
	if(ISNAN(d)) error("%s", _("vector size cannot be NA/NaN"));
	if(!R_FINITE(d)) error("%s", _("vector size cannot be infinite"));
	if(d > (double) R_XLEN_T_MAX) error("%s", _("vector size specified is too large"));
	n = (R_xlen_t) d;
	if (n < 0) error(_("invalid '%s' value"), "length(xx)");
    } else {
	int nn = asInteger(sn);
	if (nn == NA_INTEGER || nn < 0)
	    error(_("invalid '%s' value"), "length(xx)");
	n = nn;
    }
    isLong = (n > INT_MAX);
#else
    int n = asInteger(CADR(args));
    if (n == NA_INTEGER || n < 0)
	error(_("invalid '%s' value"), "length(xx)");
#endif
    const char *ties_str = CHAR(asChar(CADDR(args)));
    if(streql(ties_str, "average"))	ties_kind = AVERAGE;
    else if(streql(ties_str, "max"))	ties_kind = MAX;
    else if(streql(ties_str, "min"))	ties_kind = MIN;
    else error("%s", _("invalid ties.method for rank() [should never happen]"));
    if (ties_kind == AVERAGE || isLong) {
	PROTECT(rank = allocVector(REALSXP, n));
	rk = REAL(rank);
    } else {
	PROTECT(rank = allocVector(INTSXP, n));
	ik = INTEGER(rank);
    }
    if (n > 0) {
#ifdef LONG_VECTOR_SUPPORT
	if(isLong) {
	    R_xlen_t i, j, k;
	    R_xlen_t *in = (R_xlen_t *) R_alloc(n, sizeof(R_xlen_t));
	    for (i = 0; i < n; i++) in[i] = i;
	    orderVector1l(in, n, x, true, false, rho);
	    for (i = 0; i < n; i = j+1) {
		j = i;
		while ((j < n - 1) && equal(in[j], in[j + 1], x, true, rho)) j++;
		switch(ties_kind) {
		case AVERAGE:
		    for (k = i; k <= j; k++)
			rk[in[k]] = (i + j + 2) / 2.; 
		    break;
		case MAX:
		    for (k = i; k <= j; k++) rk[in[k]] = j+1;
		    break;
		case MIN:
		    for (k = i; k <= j; k++) rk[in[k]] = i+1;
		    break;
		}
	    }
	} else
#endif
	{
	    int i, j, k;
	    int *in = (int *) R_alloc(n, sizeof(int));
	    for (i = 0; i < n; i++) in[i] = i;
	    orderVector1(in, (int) n, x, true, false, rho);
	    for (i = 0; i < n; i = j+1) {
		j = i;
		while ((j < n - 1) && equal(in[j], in[j + 1], x, true, rho)) j++;
		switch(ties_kind) {
		case AVERAGE:
		    for (k = i; k <= j; k++)
			rk[in[k]] = ((double)i + j + 2) / 2.;
		    break;
		case MAX:
		    for (k = i; k <= j; k++) ik[in[k]] = j+1;
		    break;
		case MIN:
		    for (k = i; k <= j; k++) ik[in[k]] = i+1; 
		    break;
		}
	    }
	}
    }
    UNPROTECT(1);
    return rank;
}

attribute_hidden SEXP do_xtfrm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    /* DispatchOrEval internal generic: xtfrm */
    {
        auto dispatched = DispatchOrEval(call, op, "xtfrm", args, rho, false, true);
        if (dispatched.first)
            return dispatched.second;
    }
    /* otherwise dispatch the default method */
    GCStackRoot<> fn, prargs;
    fn = findFun(install("xtfrm.default"), rho);
    prargs = promiseArgs(args, R_GlobalEnv);
    IF_PROMSXP_SET_PRVALUE(CAR(prargs), CAR(args));
    SEXP ans = applyClosure(call, fn, prargs, rho, R_NilValue, true);

    return ans;

}
