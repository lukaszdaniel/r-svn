/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2025  The R Core Team.
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
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

/* The x:y  primitive calls do_colon(); do_colon() calls cross_colon() if
   both arguments are factors and seq_colon() otherwise.
 */

/** @file seq.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>  /* for DBL_EPSILON */
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/Itermacros.h>

#include "RBufferUtils.h"

using namespace R;
using namespace CXXR;

static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#define _S4_rep_keepClass
/* ==>  rep(<S4>, .) keeps class e.g., for list-like */

static SEXP cross_colon(SEXP call, SEXP s, SEXP t)
{
    SEXP a, la, ls, lt, rs, rt;
    int k, n, nls, nlt;
    char *cbuf;
    CXXR::RAllocStack::Scope rscope;

    if (length(s) != length(t))
	errorcall(call, "%s", _("unequal factor lengths"));
    n = length(s);
    ls = getAttrib(s, R_LevelsSymbol);
    lt = getAttrib(t, R_LevelsSymbol);
    nls = LENGTH(ls);
    nlt = LENGTH(lt);
    PROTECT(a = allocVector(INTSXP, n));
    PROTECT(rs = coerceVector(s, INTSXP));
    PROTECT(rt = coerceVector(t, INTSXP));
    for (int i = 0; i < n; i++) {
	int vs = INTEGER(rs)[i];
	int vt = INTEGER(rt)[i];
	if ((vs == NA_INTEGER) || (vt == NA_INTEGER))
	    INTEGER(a)[i] = NA_INTEGER;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    UNPROTECT(2);
    if (!isNull(ls) && !isNull(lt)) {
	PROTECT(la = allocVector(STRSXP, nls * nlt));
	k = 0;
	/* FIXME: possibly UTF-8 version */
	for (int i = 0; i < nls; i++) {
	    const char *vi = translateChar(STRING_ELT(ls, i));
	    size_t vs = strlen(vi);
	    for (int j = 0; j < nlt; j++) {
		const char *vj = translateChar(STRING_ELT(lt, j));
		size_t vt = strlen(vj), len = vs + vt + 2;
		cbuf = (char *) R_AllocStringBuffer(len, &cbuff);
		snprintf(cbuf, len, "%s:%s", vi, vj);
		SET_STRING_ELT(la, k, mkChar(cbuf));
		k++;
	    }
	}
	setAttrib(a, R_LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = mkString("factor"));
    setAttrib(a, R_ClassSymbol, la);
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return a;
}

/* interval at which to check interrupts */
/*   if re-enabling, consider a power of two */
/* #define NINTERRUPT 1000000U */

static SEXP seq_colon(double n1, double n2, SEXP call)
{
    double r = fabs(n2 - n1);
    if(r >= R_XLEN_T_MAX)
	errorcall(call, "%s", _("result would be too long a vector"));

    if (n1 == (R_xlen_t) n1 && n2 == (R_xlen_t) n2)
	return R_compact_intrange((R_xlen_t) n1, (R_xlen_t) n2);

    SEXP ans;
    R_xlen_t n = (R_xlen_t)(r + 1 + FLT_EPSILON);

    bool useInt = ((n1 <= INT_MAX) && (n1 == (int) n1));
    if(useInt) {
	if(n1 <= INT_MIN) /* know  n1 <= INT_MAX */
	    useInt = FALSE;
	else {
	    /* r := " the effective 'to' "  of  from:to */
	    double dn = (double) n;
	    r = n1 + ((n1 <= n2) ? dn-1 : -(dn-1));
	    if(r <= INT_MIN || r > INT_MAX) useInt = false;
	}
    }
    if (useInt) {
	if (n1 <= n2)
	    ans = R_compact_intrange((R_xlen_t) n1, (R_xlen_t)(n1 + n - 1));
	else
	    ans = R_compact_intrange((R_xlen_t) n1, (R_xlen_t)(n1 - n + 1));
    } else {
	ans = allocVector(REALSXP, n);
	if (n1 <= n2)
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = n1 + (double)i;
	    }
	else
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = n1 - (double)i;
	    }
    }
    return ans;
}

attribute_hidden SEXP do_colon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP s1 = CAR(args),
	s2 = CADR(args);
    if (inherits(s1, "factor") && inherits(s2, "factor"))
	return(cross_colon(call, s1, s2));
    double
	n1 = length(s1),
	n2 = length(s2);
    if (n1 != 1 || n2 != 1) {
	if (n1 == 0 || n2 == 0)
	    errorcall(call, "%s", _("argument of length 0"));
	char *check = getenv("_R_CHECK_LENGTH_COLON_");
	if (check ? StringTrue(check) : false) // warn by default
	    errorcall(call, "%s", _("numerical expression has length > 1"));
	else
	    warningcall(call, n_("numerical expression has %d element: only the first used",
				"numerical expression has %d elements: only the first used",
				(n1 > 1) ? (int) n1 : (int) n2),
			(n1 > 1) ? (int) n1 : (int) n2);
    }

    n1 = asReal(s1);
    n2 = asReal(s2);
    if (ISNAN(n1) || ISNAN(n2))
	errorcall(call, "%s", _("NA/NaN argument"));
    return seq_colon(n1, n2, call);
}

/* rep.int(x, times) for a vector times */
static SEXP rep2(SEXP s, SEXP ncopy)
{
    R_xlen_t i, j, nc, n;
    SEXP a, t;

#define R2_SWITCH_LOOP(it) \
    switch (TYPEOF(s)) { \
    case LGLSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = 0; j < (R_xlen_t) it[i]; j++) \
		LOGICAL(a)[n++] = LOGICAL(s)[i]; \
	} \
	break; \
    case INTSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		INTEGER(a)[n++] = INTEGER(s)[i]; \
	} \
	break; \
    case REALSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		REAL(a)[n++] = REAL(s)[i]; \
	} \
	break; \
    case CPLXSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		COMPLEX(a)[n++] = COMPLEX(s)[i]; \
	} \
	break; \
    case STRSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_STRING_ELT(a, n++, STRING_ELT(s, i)); \
	} \
	break; \
    case VECSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    SEXP elt = lazy_duplicate(VECTOR_ELT(s, i)); \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_VECTOR_ELT(a, n++, elt); \
	} \
	break; \
    case EXPRSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    SEXP elt = lazy_duplicate(XVECTOR_ELT(s, i)); \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		SET_XVECTOR_ELT(a, n++, elt); \
	} \
	break; \
    case RAWSXP: \
	for (i = 0; i < nc; i++) { \
/*	    if ((i+1) % ni == 0) R_CheckUserInterrupt();*/ \
	    for (j = (R_xlen_t) it[i]; j > 0; j--) \
		RAW(a)[n++] = RAW(s)[i]; \
	} \
	break; \
    default: \
	UNIMPLEMENTED_TYPE("rep2", s); \
    }

#ifdef LONG_VECTOR_SUPPORT
    if (TYPEOF(ncopy) != INTSXP)
#else
    if (TYPEOF(ncopy) == REALSXP)
#endif
	PROTECT(t = coerceVector(ncopy, REALSXP));
    else
	PROTECT(t = coerceVector(ncopy, INTSXP));

    nc = xlength(ncopy);
    double sna = 0;
    if (TYPEOF(t) == REALSXP)
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (ISNAN(REAL(t)[i]) || REAL(t)[i] <= -1 ||
	    REAL(t)[i] >= (double) R_XLEN_T_MAX+1.0)
	    error(_("invalid '%s' value"), "times");
	sna += (R_xlen_t) REAL(t)[i];
    }
    else
    for (i = 0; i < nc; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if (INTEGER(t)[i] == NA_INTEGER || INTEGER(t)[i] < 0)
	    error(_("invalid '%s' value"), "times");
	sna += INTEGER(t)[i];
    }
    if (sna > (double) R_XLEN_T_MAX)
	error(_("invalid '%s' value"), "times");
    R_xlen_t na = (R_xlen_t) sna;

/*    R_xlen_t ni = NINTERRUPT, ratio;
    if(nc > 0) {
	ratio = na/nc; // average no of replications
	if (ratio > 1000U) ni = 1000U;
	} */
    PROTECT(a = allocVector(TYPEOF(s), na));
    n = 0;
    if (TYPEOF(t) == REALSXP)
	R2_SWITCH_LOOP(REAL(t))
    else
	R2_SWITCH_LOOP(INTEGER(t))
    UNPROTECT(2);
    return a;
}
#undef R2_SWITCH_LOOP

/* rep_len(x, len), also used for rep.int() with scalar 'times' */
static SEXP rep3(SEXP s, R_xlen_t ns, R_xlen_t na)
{
    R_xlen_t i, j;
    SEXP a;

    PROTECT(a = allocVector(TYPEOF(s), na));

    switch (TYPEOF(s)) {
    case LGLSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(a)[i] = LOGICAL(s)[j];
	});
	break;
    case INTSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    INTEGER(a)[i] = INTEGER(s)[j];
	});
	break;
    case REALSXP:
	MOD_ITERATE1(na, ns, i, j,  {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    REAL(a)[i] = REAL(s)[j];
	});
	break;
    case CPLXSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    COMPLEX(a)[i] = COMPLEX(s)[j];
	});
	break;
    case RAWSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    RAW(a)[i] = RAW(s)[j];
	});
	break;
    case STRSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(a, i, STRING_ELT(s, j));
	});
	break;
    case VECSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(a, i, lazy_duplicate(VECTOR_ELT(s, j)));
	});
	break;
    case EXPRSXP:
	MOD_ITERATE1(na, ns, i, j, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_XVECTOR_ELT(a, i, lazy_duplicate(XVECTOR_ELT(s, j)));
	});
	break;
    default:
	UNIMPLEMENTED_TYPE("rep3", s);
    }
    UNPROTECT(1);
    return a;
}

// .Internal(rep.int(x, times))
attribute_hidden SEXP do_rep_int(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP s = CAR(args), ncopy = CADR(args);
    R_xlen_t nc;
    SEXP a;

    /* DispatchOrEval internal generic: rep.int */
    if (DispatchOrEval(call, op, "rep.int", args, rho, &a, 0, 0))
      return a;

    if (DispatchOrEval(call, op, "rep", args, rho, &a, 0, 0))
      return a;

    if (!isVector(ncopy))
	error(_("invalid type (%s) for '%s' (must be a vector)"),
	      R_typeToChar(ncopy), "times");

    if (!isVector(s) && s != R_NilValue)
	error(_("attempt to replicate an object of type '%s'"),
	      R_typeToChar(s));

    nc = xlength(ncopy); // might be 0
    if (nc == xlength(s))
	PROTECT(a = rep2(s, ncopy));
    else {
	if (nc != 1) error(_("invalid '%s' value"), "times");

	R_xlen_t ns = xlength(s);
	if (TYPEOF(ncopy) != INTSXP) {
	    double snc = asReal(ncopy);
	    if (!R_FINITE(snc) || snc <= -1. ||
		(ns > 0 && snc >= R_XLEN_T_MAX + 1.))
		error(_("invalid '%s' value"), "times");
	    nc = ns == 0 ? 1 : (R_xlen_t) snc;
	} else if ((nc = asInteger(ncopy)) == NA_INTEGER || nc < 0) // nc = 0 ok
	    error(_("invalid '%s' value"), "times");
	if ((double) nc * ns > R_XLEN_T_MAX)
	    error(_("invalid '%s' value"), "times");
	PROTECT(a = rep3(s, ns, nc * ns));
    }

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

attribute_hidden SEXP do_rep_len(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_xlen_t ns, na;
    SEXP a, s, len;

    checkArity(op, args);

    /* DispatchOrEval internal generic: rep_len */
    if (DispatchOrEval(call, op, "rep_len", args, rho, &a, 0, 0))
      return a;

    s = CAR(args);

    if (isObject(s)) {
	SEXP rep_call;
	PROTECT(rep_call = shallow_duplicate(call));
	SETCAR(rep_call, install("rep"));
	SET_TAG(CDDR(rep_call), install("length.out"));
	SET_TAG(CDR(args), install("length.out"));
	if (DispatchOrEval(rep_call, op, "rep", args, rho, &a, 0, 0)) {
	    UNPROTECT(1);
	    return a;
	}
	UNPROTECT(1);
    }

    if (!isVector(s) && s != R_NilValue)
	error("%s", _("attempt to replicate non-vector"));

    len = CADR(args);
    if(length(len) != 1)
	error(_("invalid '%s' value"), "length.out");
    if (TYPEOF(len) != INTSXP) {
	double sna = asReal(len);
	if (ISNAN(sna) || sna <= -1. || sna >= (double) R_XLEN_T_MAX + 1.)
	    error(_("invalid '%s' value"), "length.out");
	na = (R_xlen_t) sna;
    } else
	if ((na = asInteger(len)) == NA_INTEGER || na < 0) /* na = 0 ok */
	    error(_("invalid '%s' value"), "length.out");

    if (TYPEOF(s) == NILSXP && na > 0)
	error("%s", _("cannot replicate NULL to a non-zero length"));
    ns = xlength(s);
    if (ns == 0) {
	SEXP a;
	PROTECT(a = duplicate(s));
	if(na > 0) a = xlengthgets(a, na);
	UNPROTECT(1);
	return a;
    }
    PROTECT(a = rep3(s, ns, na));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

/* rep(), allowing for both times and each ;
 * -----  nt == length(times) ;  if (nt == 1)  'times' is *not* accessed  */
static SEXP rep4(SEXP x, SEXP times, R_xlen_t len, R_xlen_t each, R_xlen_t nt)
{
    SEXP a;
    R_xlen_t lx = xlength(x);
    R_xlen_t i, j, k, k2, k3, sum;

    // faster code for common special case
    if (each == 1 && nt == 1) return rep3(x, lx, len);

    PROTECT(a = allocVector(TYPEOF(x), len));

#define R4_SWITCH_LOOP(itimes)						\
    switch (TYPEOF(x)) {						\
    case LGLSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		LOGICAL(a)[k2++] = LOGICAL(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case INTSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		INTEGER(a)[k2++] = INTEGER(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case REALSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		REAL(a)[k2++] = REAL(x)[i];				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case CPLXSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		COMPLEX(a)[k2++] = COMPLEX(x)[i];			\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case STRSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		SET_STRING_ELT(a, k2++, STRING_ELT(x, i));		\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case VECSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    SEXP elt = lazy_duplicate(VECTOR_ELT(x, i));		\
	    for(k3 = 0; k3 < sum; k3++) {				\
		SET_VECTOR_ELT(a, k2++, elt);				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case EXPRSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    SEXP elt = lazy_duplicate(XVECTOR_ELT(x, i));		\
	    for(k3 = 0; k3 < sum; k3++) {				\
		SET_XVECTOR_ELT(a, k2++, elt);				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    case RAWSXP:							\
	for(i = 0, k = 0, k2 = 0; i < lx; i++) {			\
	    /*		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();*/ \
	    for(j = 0, sum = 0; j < each; j++) sum += (R_xlen_t) itimes[k++]; \
	    for(k3 = 0; k3 < sum; k3++) {				\
		RAW(a)[k2++] = RAW(x)[i];				\
		if(k2 == len) goto done;				\
	    }								\
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE("rep4", x);					\
    }

    if(nt == 1)
	switch (TYPEOF(x)) {
	case LGLSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		LOGICAL(a)[i] = LOGICAL(x)[(i/each) % lx];
	    }
	    break;
	case INTSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(a)[i] = INTEGER(x)[(i/each) % lx];
	    }
	    break;
	case REALSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(a)[i] = REAL(x)[(i/each) % lx];
	    }
	    break;
	case CPLXSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		COMPLEX(a)[i] = COMPLEX(x)[(i/each) % lx];
	    }
	    break;
	case STRSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SET_STRING_ELT(a, i, STRING_ELT(x, (i/each) % lx));
	    }
	    break;
	case VECSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SEXP elt = lazy_duplicate(VECTOR_ELT(x, (i/each) % lx));
		SET_VECTOR_ELT(a, i, elt);
	    }
	    break;
	case EXPRSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		SEXP elt = lazy_duplicate(XVECTOR_ELT(x, (i/each) % lx));
		SET_XVECTOR_ELT(a, i, elt);
	    }
	    break;
	case RAWSXP:
	    for(i = 0; i < len; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		RAW(a)[i] = RAW(x)[(i/each) % lx];
	    }
	    break;
	default:
	    UNIMPLEMENTED_TYPE("rep4", x);
	}
    else if(TYPEOF(times) == REALSXP)
	R4_SWITCH_LOOP(REAL(times))
	else
	    R4_SWITCH_LOOP(INTEGER(times))
		done:
	    UNPROTECT(1);
    return a;
}
#undef R4_SWITCH_LOOP

/* We are careful to use evalListKeepMissing here (inside
   DispatchOrEval) to avoid dropping missing arguments so e.g.
   rep(1:3,,8) matches length.out */

/* This is a primitive SPECIALSXP with internal argument matching */
attribute_hidden SEXP do_rep(SEXP call, SEXP op, SEXP args_, SEXP rho)
{
    GCStackRoot<> args(args_);
    GCStackRoot<> times(R_NilValue);
    SEXP ans, x;
    R_xlen_t i, lx, len = NA_INTEGER, each = 1, nt;
    static SEXP do_rep_formals = NULL;

    /* includes factors, POSIX[cl]t, Date */
    /* DispatchOrEval internal generic: rep */
    if (DispatchOrEval(call, op, "rep", args, rho, &ans, 0, 0))
	return ans;

    /* This has evaluated all the non-missing arguments into ans */
    args = ans;

    /* This is a primitive, and we have not dispatched to a method
       so we manage the argument matching ourselves.  We pretend this is
       rep(x, times, length.out, each, ...)
    */
    if (do_rep_formals == NULL)
	do_rep_formals = allocFormalsList5(install("x"), install("times"),
					   install("length.out"),
					   install("each"), R_DotsSymbol);
    args = matchArgs_NR(do_rep_formals, args, call);

    x = CAR(args);
    /* supported in R 2.15.x */
    if (TYPEOF(x) == LISTSXP)
	errorcall(call, "%s", _("replication of pairlists is defunct"));

    lx = xlength(x);

    if (TYPEOF(CADDR(args)) != INTSXP) {
	double slen = asReal(CADDR(args));
	if (R_FINITE(slen)) {
	    if (slen <= -1 || slen >= (double) R_XLEN_T_MAX+1.0)
		errorcall(call, _("invalid '%s' argument"), "length.out");
	    len = (R_xlen_t) slen;
	} else
	    len = NA_INTEGER;
    } else {
	len = asInteger(CADDR(args));
	if(len != NA_INTEGER && len < 0)
	    errorcall(call, _("invalid '%s' argument"), "length.out");
    }
    if(length(CADDR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

    if (TYPEOF(CADDDR(args)) != INTSXP) {
	double seach = asReal(CADDDR(args));
	if (R_FINITE(seach)) {
	    if (seach <= -1. || (lx > 0 && seach >= (double) R_XLEN_T_MAX + 1.))
		errorcall(call, _("invalid '%s' argument"), "each");
	    each = lx == 0 ? NA_INTEGER : (R_xlen_t) seach;
	} else each = NA_INTEGER;
    } else {
	each = asInteger(CADDDR(args));
	if(each != NA_INTEGER && each < 0)
	    errorcall(call, _("invalid '%s' argument"), "each");
    }
    if(length(CADDDR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"), "each");
    if(each == NA_INTEGER) each = 1;

    if(lx == 0) {
	if(len > 0 && x == R_NilValue)
	    warningcall(call, "%s", _("'x' is NULL so the result will be NULL"));
	GCStackRoot<> a;
	a = duplicate(x);
	if(len != NA_INTEGER && len > 0 && x != R_NilValue)
	    a = xlengthgets(a, len);

	return a;
    }
    if (!isVector(x))
	errorcall(call, _("attempt to replicate an object of type '%s'"),
		  R_typeToChar(x));

    /* So now we know x is a vector of positive length.  We need to
       replicate it, and its names if it has them. */

    /* First find the final length using 'times' and 'each' */
    if(len != NA_INTEGER) { /* takes precedence over times */
	nt = 1;
    } else {
	double sum = 0;
	if(CADR(args) == R_MissingArg)
	    times = ScalarInteger(1);
#ifdef LONG_VECTOR_SUPPORT
	else if(TYPEOF(CADR(args)) != INTSXP)
#else
	else if(TYPEOF(CADR(args)) == REALSXP)
#endif
	    times = coerceVector(CADR(args), REALSXP);
	else times = coerceVector(CADR(args), INTSXP);

	nt = XLENGTH(times);
	if(nt == 1) {
	    R_xlen_t it;
	    if (TYPEOF(times) == REALSXP) {
		double rt = REAL(times)[0];
		if (ISNAN(rt) || rt <= -1 || rt >= (double) R_XLEN_T_MAX+1.0)
		    errorcall(call, _("invalid '%s' argument"), "times");
		it = (R_xlen_t) rt;
	    } else {
		it = INTEGER(times)[0];
		if (it == NA_INTEGER || it < 0)
		    errorcall(call, _("invalid '%s' argument"), "times");
	    }
	    if ((double) lx * it * each > (double) R_XLEN_T_MAX)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = lx * it * each;
	} else { // nt != 1
	    if(nt != (double) lx * each)
		errorcall(call, _("invalid '%s' argument"), "times");
	    if (TYPEOF(times) == REALSXP)
		for(i = 0; i < nt; i++) {
		    double rt = REAL(times)[i];
		    if (ISNAN(rt) || rt <= -1 || rt >= (double) R_XLEN_T_MAX+1.0)
			errorcall(call, _("invalid '%s' argument"), "times");
		    sum += (R_xlen_t) rt;
		}
	    else
		for(i = 0; i < nt; i++) {
		    int it = INTEGER(times)[i];
		    if (it == NA_INTEGER || it < 0)
			errorcall(call, _("invalid '%s' argument"), "times");
		    sum += it;
		}
	    if (sum > (double) R_XLEN_T_MAX)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = (R_xlen_t) sum;
	}
    }

    if(len > 0 && each == 0)
	errorcall(call, _("invalid '%s' argument"), "each");

    GCStackRoot<> xn, res;
    xn = getAttrib(x, R_NamesSymbol);
    res = rep4(x, times, len, each, nt);

    if (xlength(xn) > 0)
	setAttrib(res, R_NamesSymbol, rep4(xn, times, len, each, nt));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	setAttrib(res, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	SET_S4_OBJECT(res);
    }
#endif

    return res;
}


/*
  This is a primitive SPECIALSXP with internal argument matching, implementing

  seq.int(from, to, by, length.out, along.with, ...)

  'along' has to be used on an unevaluated argument, and evalList
  tries to evaluate language objects.
 */
#define FEPS 1e-10
/* to match seq.default */
attribute_hidden SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue /* -Wall */, from, to, by, len, along;
    R_xlen_t i, lout = NA_INTEGER;
    bool One = (length(args) == 1); // *before* messing with args ..

    /* DispatchOrEval internal generic: seq.int */
    if (DispatchOrEval(call, op, "seq", args, rho, &ans, 0, 1))
	return ans;

    /* This is a primitive and we manage argument matching ourselves.
       We pretend this is
       seq.int(from, to, by, length.out, along.with, ...)
    */
    static SEXP do_seq_formals = NULL;
    if (do_seq_formals == NULL)
	do_seq_formals = allocFormalsList6(install("from"), install("to"),
					   install("by"), install("length.out"),
					   install("along.with"), R_DotsSymbol);
    PROTECT(args = matchArgs_NR(do_seq_formals, args, call));

    from = CAR(args); args = CDR(args);
    to   = CAR(args); args = CDR(args);
    by   = CAR(args); args = CDR(args);
    len  = CAR(args); args = CDR(args);
    along= CAR(args);
    bool
	miss_from = (from == R_MissingArg),
	miss_to   = (to   == R_MissingArg);
    if(One && !miss_from) {
	int lf = length(from);
	if(lf == 1 && (TYPEOF(from) == INTSXP || TYPEOF(from) == REALSXP)) {
	    double rfrom = asReal(from);
	    if (!R_FINITE(rfrom))
		errorcall(call, _("'%s' must be a finite number"), "from");
	    ans = seq_colon(1.0, rfrom, call);
	}
	else if (lf) // typically  seq(<vec>) , length(<vec>) >= 2
	    ans = seq_colon(1.0, (double)lf, call);
	else
	    ans = allocVector(INTSXP, 0);
	goto done;
    }
    if(along != R_MissingArg) {
	lout = XLENGTH(along);
	if(One) {
	    ans = lout ? seq_colon(1.0, (double)lout, call) : allocVector(INTSXP, 0);
	    goto done;
	}
    } else if(len != R_MissingArg && len != R_NilValue) {
	double rout = asReal(len);
	if(!R_FINITE(rout))
	    errorcall(call, _("'%s' must be a finite number"), "length.out");
	if(ISNAN(rout) || rout <= -0.5)
	    errorcall(call, "%s", _("'length.out' must be a non-negative number"));
	if(length(len) != 1)
	    warningcall(call, _("first element used of '%s' argument"),
			"length.out");
	rout = ceil(rout);
	if(rout >= R_XLEN_T_MAX)
	    errorcall(call, "%s", _("result would be too long a vector"));
	lout = (R_xlen_t) rout;
    }

    if(lout == NA_INTEGER) {
	double rfrom, rto;
	if(miss_from) rfrom = 1.0;
	else {
	    if(length(from) != 1) errorcall(call, _("'%s' must be of length 1"), "from");
	    rfrom = asReal(from);
	    if(!R_FINITE(rfrom))
		errorcall(call, _("'%s' must be a finite number"), "from");
	}
	if(miss_to) rto = 1.0;
	else {
	    if(length(to) != 1) errorcall(call, _("'%s' must be of length 1"), "to");
	    rto = asReal(to);
	    if(!R_FINITE(rto))
		errorcall(call, _("'%s' must be a finite number"), "to");
	}
	if(by == R_MissingArg)
	    ans = seq_colon(rfrom, rto, call);
	else { // 'by' specified
	    if(length(by) != 1) errorcall(call, _("'%s' must be of length 1"), "by");
	    double del = rto - rfrom;
	    if(del == 0.0 && rto == 0.0) {
		ans = to; // is *not* missing in this case
		goto done;
	    }
	    double rby = asReal(by);
	    if((rby ==  1. && del > 0.) ||
	       (rby == -1. && del < 0.)) { // --> treat as if missing (return integer)
		ans = seq_colon(rfrom, rto, call);
		goto done;
	    }
	    bool finite_del = (R_FINITE(del) != 0);
	    double n = (finite_del)
		? del/rby
		: rto/rby - rfrom/rby; /* overflow in  (to - from)  when both are finite */
	    if(!R_FINITE(n)) {
		if(del == 0.0 && rby == 0.0) {
		    ans = miss_from ? ScalarReal(rfrom) : from;
		    goto done;
		} else
		    errorcall(call, "%s", _("invalid '(to - from)/by'"));
	    }
	    // inherited from seq.default() but "fudgy"
	    if(finite_del && fabs(del)/fmax2(fabs(rto), fabs(rfrom)) < 100 * DBL_EPSILON) {
		ans = miss_from ? ScalarReal(rfrom) : from;
		goto done;
	    }
#ifdef LONG_VECTOR_SUPPORT
	    if(n > 100 * (double) INT_MAX)
#else
	    if(n > (double) INT_MAX)
#endif
		errorcall(call, "%s", _("'by' argument is much too small"));
	    if(n < - FEPS)
		errorcall(call, "%s", _("wrong sign in 'by' argument"));

	    R_xlen_t nn;
	    if((miss_from || TYPEOF(from) == INTSXP) &&
	       (miss_to   || TYPEOF(to)   == INTSXP) && TYPEOF(by) == INTSXP)
	    {
		int *ia, ifrom = miss_from ? (int)rfrom : asInteger(from),
		    iby = asInteger(by);
		/* With the current limits on integers and FEPS
		   reduced below 1/INT_MAX this is the same as the
		   next, so this is future-proofing against longer integers.
		*/
		/* as seq.default also returns integer for from + (0:n)*by
		*/
		nn = (R_xlen_t) n;
		ans = allocVector(INTSXP, nn+1);
		ia = INTEGER(ans);
		for(i = 0; i <= nn; i++)
		    ia[i] = (int)(ifrom + i * iby);
	    } else {
		nn = (int)(n + FEPS);
		ans = allocVector(REALSXP, nn+1);
		double *ra = REAL(ans);
		if(finite_del)
		    for(i = 0; i <= nn; i++)
			ra[i] = rfrom + (double)i * rby;
		else { // |from - to| is infinite, but n = (from-to)/by is not
		    rfrom /= 4.;
		    rby   /= 4.;
		    for(i = 0; i <= nn; i++) // ldexp(Y, 2) := Y * 2^2 = 4 Y
			ra[i] = ldexp(rfrom + (double)i * rby, 2);
		}
		/* Added in 2.9.0 */
		if (nn > 0)
		    if((rby > 0 && ra[nn] > rto) || (rby < 0 && ra[nn] < rto))
			ra[nn] = rto;
	    }
	}
    } else if (lout == 0) {
	ans = allocVector(INTSXP, 0);
    } else if (One) {
	ans = seq_colon(1.0, (double)lout, call);
    } else if (by == R_MissingArg) { // and  len := length.out  specified, >= 1
	double rfrom = asReal(from), rto = asReal(to), rby = 0; // -Wall
	if(miss_to)   rto   = rfrom + (double)lout - 1;
	if(miss_from) rfrom = rto   - (double)lout + 1;
	if(!R_FINITE(rfrom)) errorcall(call, _("'%s' must be a finite number"), "from");
	if(!R_FINITE(rto))   errorcall(call, _("'%s' must be a finite number"), "to");
	bool finite_del = false;
	if(lout > 2) { // only then, use 'by'
	    double nint = (double)(lout - 1);
	    if((finite_del = (R_FINITE(rby = (rto - rfrom)) != 0)))
		rby /= nint;
	    else // overflow in (to - from), nint >= 2  => finite 'by'
		rby = (rto/nint - rfrom/nint);
	}
	if(rfrom <= INT_MAX && rfrom >= INT_MIN &&
	   rto   <= INT_MAX && rto   >= INT_MIN &&
	   rfrom == (int)rfrom &&
	   (lout <= 1 || rto == (int)rto) &&
	   (lout <= 2 || rby == (int)rby)) {
	    ans = allocVector(INTSXP, lout);
	    INTEGER(ans)[0] = (int)rfrom;
	    if(lout > 1) INTEGER(ans)[lout - 1] = (int)rto;
	    if(lout > 2)
		for(i = 1; i < lout-1; i++) {
//		    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		    INTEGER(ans)[i] = (int)(rfrom + (double)i*rby);
		}
	} else {
	    ans = allocVector(REALSXP, lout);
	    REAL(ans)[0] = rfrom;
	    if(lout > 1) REAL(ans)[lout - 1] = rto;
	    if(lout > 2) {
		if(finite_del)
		    for(i = 1; i < lout-1; i++) {
// 		        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
			REAL(ans)[i] = rfrom + (double)i*rby;
		    }
		else {  // del:=(from - to) is infinite, but n = del/by is not
		    rfrom /= 4.; // or ldexp(*, -2) for speed
		    rby   /= 4.;
		    for(i = 1; i < lout-1; i++) {
// 		        if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
			REAL(ans)[i] = ldexp(rfrom + (double)i*rby, 2); // ldexp(y,2) = y * 4
		    }
		}
	    }
	}
    } else if (miss_to) {
	double rfrom = asReal(from), rby = asReal(by), rto;
	if(miss_from) rfrom = 1.0;
	if(!R_FINITE(rfrom)) errorcall(call, _("'%s' must be a finite number"), "from");
	if(!R_FINITE(rby))   errorcall(call, _("'%s' must be a finite number"), "by");
	rto = rfrom + (double)(lout-1)*rby;
	// avoid undefined behaviour by testing range before converting.
	if(rfrom <= INT_MAX && rfrom >= INT_MIN
	   &&  rto  <= INT_MAX &&  rto  >= INT_MIN
	   && rby == (int)rby && rfrom == (int)rfrom) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = (int)(rfrom + (double)i*rby);
	    }
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rfrom + (double)i*rby;
	    }
	}
    } else if (miss_from) {
	double rto = asReal(to), rby = asReal(by),
	    rfrom = rto - (double)(lout-1)*rby;
	if(!R_FINITE(rto)) errorcall(call, _("'%s' must be a finite number"), "to");
	if(!R_FINITE(rby)) errorcall(call, _("'%s' must be a finite number"), "by");
	if(rby == (int)rby && rto == (int)rto
	   && rfrom <= INT_MAX && rfrom >= INT_MIN
	   &&  rto  <= INT_MAX &&  rto  >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER(ans)[i] = (int)(rto - (double)(lout - 1 - i)*rby);
	    }
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL(ans)[i] = rto - (double)(lout - 1 - i)*rby;
	    }
	}
    } else
	errorcall(call, "%s", _("too many arguments"));

done:
    UNPROTECT(1);
    return ans;
} // do_seq()

attribute_hidden SEXP do_seq_along(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    R_xlen_t len;
    static SEXP length_op = NULL;

    /* Store the .Primitive for 'length' for DispatchOrEval to use. */
    if (length_op == NULL) {
	SEXP R_lengthSymbol = install("length");
	length_op = eval(R_lengthSymbol, R_BaseEnv);
	if (TYPEOF(length_op) != BUILTINSXP) {
	    length_op = NULL;
	    error("%s", _("'length' is not a BUILTIN"));
	}
	R_PreserveObject(length_op);
    }

    checkArity(op, args);
    check1arg(args, call, "along.with");

    /* Try to dispatch to S3 or S4 methods for 'length'.  For cases
       where no methods are defined this is more efficient than an
       unconditional callback to R */
    if (isObject(CAR(args)) &&
	DispatchOrEval(call, length_op, "length", args, rho, &ans, 0, 1)) {
	len = asInteger(ans);
    }
    else
	len = xlength(CAR(args));

    if (len == 0)
	return allocVector(INTSXP, 0);
    else
	return R_compact_intrange(1, len);
}

attribute_hidden SEXP do_seq_len(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_xlen_t len;

    checkArity(op, args);
    check1arg(args, call, "length.out");
    if(length(CAR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

 #ifdef LONG_VECTOR_SUPPORT
    double dlen = asReal(CAR(args));
    if(!R_FINITE(dlen) || dlen < 0)
	errorcall(call, "%s", _("argument must be coercible to non-negative integer"));
    if(dlen >= (double) R_XLEN_T_MAX)
    	errorcall(call, "%s", _("result would be too long a vector"));
    len = (R_xlen_t) dlen;
#else
    len = asInteger(CAR(args));
    if(len == NA_INTEGER || len < 0)
	errorcall(call, "%s", _("argument must be coercible to non-negative integer"));
#endif

    if (len == 0)
	return allocVector(INTSXP, 0);
    else
	return R_compact_intrange(1, len);
}

attribute_hidden SEXP do_sequence(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_xlen_t lengths_len, from_len, by_len, ans_len, i, i2, i3;
    int from_elt, by_elt, length, j, k, *ans_elt;
    const int *lengths_elt;
    SEXP ans, lengths, from, by;

    checkArity(op, args);

    lengths = CAR(args);
    if (!isInteger(lengths))
	error("%s", _("'lengths' is not of mode integer"));
    from = CADR(args);
    if (!isInteger(from))
	error("%s", _("'from' is not of mode integer"));
    by = CADDR(args);
    if (!isInteger(by))
	error("%s", _("'by' is not of mode integer"));

    lengths_len = Rf_length(lengths);
    from_len = Rf_length(from);
    by_len = Rf_length(by);
    if (lengths_len != 0) {
	if (from_len == 0)
	    error("%s", _("'from' has length 0, but not 'lengths'"));
	if (by_len == 0)
	    error("%s", _("'by' has length 0, but not 'lengths'"));
    }
    ans_len = 0;
    lengths_elt = INTEGER(lengths);
    for (i = 0; i < lengths_len; i++, lengths_elt++) {
	length = *lengths_elt;
	if (length == NA_INTEGER || length < 0)
	    error("%s", _("'lengths' must be a vector of non-negative integers"));
	ans_len += length;
    }
    PROTECT(ans = allocVector(INTSXP, ans_len));
    ans_elt = INTEGER(ans);
    lengths_elt = INTEGER(lengths);
    for (i = i2 = i3 = 0; i < lengths_len; i++, i2++, i3++, lengths_elt++) {
	if (i2 >= from_len)
	    i2 = 0; /* recycle */
	if (i3 >= by_len)
	    i3 = 0; /* recycle */
	length = *lengths_elt;
	from_elt = INTEGER(from)[i2];
	if (length != 0 && from_elt == NA_INTEGER) {
	    UNPROTECT(1);
	    error("%s", _("'from' contains NAs"));
	}
	by_elt = INTEGER(by)[i3];
	if (length >= 2 && by_elt == NA_INTEGER) {
	    UNPROTECT(1);
	    error("%s", _("'by' contains NAs"));
	}
	// int to = from_elt + (length - 1) * by_elt;
	for (k = 0, j = from_elt; k < length; j += by_elt, k++)
	    *(ans_elt++) = j;
    }
    UNPROTECT(1);
    return ans;
}
