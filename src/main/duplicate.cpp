/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *            (C) 2004  The R Foundation
 *  Copyright (C) 1998-2025 The R Core Team.
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

/** @file duplicate.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Localization.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/FunctionBase.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/StringVector.hpp>
#include <Defn.h>
#include <R_ext/RS.h> /* S4 bit */

#include "duplicate.h"

using namespace R;
using namespace CXXR;

/*  duplicate  -  object duplication  */

/*  Because we try to maintain the illusion of call by
 *  value, we often need to duplicate entire data
 *  objects.  There are a couple of points to note.
 *  First, duplication of list-like objects is done
 *  iteratively to prevent growth of the pointer
 *  protection stack, and second, the duplication of
 *  promises requires that the promises be forced and
 *  the value duplicated.  */

/* For memory profiling.  */
/* We want a count of calls to duplicate from outside
   which requires a wrapper function.

   I don't see how to make the wrapper go away when R_PROFILING
   is not defined, because we still need to be able to
   optionally rename duplicate() as Rf_duplicate().
*/
#ifdef R_PROFILING
static unsigned long s_duplicate_counter = (unsigned long)-1;

attribute_hidden unsigned long R::get_duplicate_counter(void)
{
    return s_duplicate_counter;
}

attribute_hidden void R::reset_duplicate_counter(void)
{
    s_duplicate_counter = 0;
}
#endif

template <CXXR::RObject::Duplication depth = CXXR::RObject::Duplication::DEEP>
SEXP CXXR_duplicate(SEXP s_)
{
    GCStackRoot<> s(s_);

#ifdef R_PROFILING
    ++s_duplicate_counter;
#endif
    SEXP t = RObject::clone(s.get(), depth);
    if (t == R_NilValue)
        return s;
    if (!(FunctionBase::isA(s) || Promise::isA(s) || Environment::isA(s)))
    {
        t->maybeTraceMemory(s);
    }

    return t;
}

// In Rinternals.h
SEXP Rf_duplicate(SEXP s)
{
    return CXXR_duplicate<CXXR::RObject::Duplication::DEEP>(s);
}

SEXP Rf_shallow_duplicate(SEXP s)
{
    return CXXR_duplicate<CXXR::RObject::Duplication::SHALLOW>(s);
}

attribute_hidden
SEXP R::Rf_lazy_duplicate(SEXP s) {
    switch (TYPEOF(s)) {
    case NILSXP:
    case SYMSXP:
    case ENVSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case WEAKREFSXP:
    case CHARSXP:
    case PROMSXP:
	break;
    case CLOSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case EXPRSXP:
    case VECSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case STRSXP:
    case OBJSXP:
	ENSURE_NAMEDMAX(s);
	break;
    default:
	UNIMPLEMENTED_TYPE("lazy_duplicate", s);
    }
    return s;
}

/*****************/

/* Detect cycles that would be created by assigning 'child' as a
   component of 's' in a complex assignment without duplicating
   'child'.  This is called quite often but almost always returns
   FALSE. Could be made more efficient, at least with partial
   inlining, but probably not worth while until it starts showing up
   significantly in profiling. Based on code from Michael Lawrence. */
attribute_hidden bool R::R_cycle_detected(SEXP s, SEXP child) {
    if (s == child) {
	switch (TYPEOF(child)) {
	case NILSXP:
	case SYMSXP:
	case ENVSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case EXTPTRSXP:
	case BCODESXP:
	case WEAKREFSXP:
	    /* it's a cycle but one that is OK */
	    return FALSE;
	default:
	return TRUE;
	}
    }
    if (ATTRIB(child) != R_NilValue) {
	if (R_cycle_detected(s, ATTRIB(child)))
	    return TRUE;
    }
    if (isPairList(child)) {
	SEXP el = child;
	while(el != R_NilValue) {
	    if (s == el || R_cycle_detected(s, CAR(el)))
		return TRUE;
	    if (ATTRIB(el) != R_NilValue && R_cycle_detected(s, ATTRIB(el)))
		return TRUE;
	    el = CDR(el);
	}
    } else if (isVectorList(child)) {
	for(int i = 0 ; i < length(child); i++)
	    if (R_cycle_detected(s, VECTOR_ELT(child, i)))
		return TRUE;
    }
    return FALSE;
}

void Rf_copyVector(SEXP s, SEXP t)
{
    SEXPTYPE sT = TYPEOF(s), tT = TYPEOF(t);
    if (sT != tT)
	error("%s", _("vector types do not match in copyVector"));
    R_xlen_t ns = XLENGTH(s), nt = XLENGTH(t);
    switch (sT) {
    case STRSXP:
	xcopyStringWithRecycle(s, t, 0, ns, nt);
	break;
    case LGLSXP:
	xcopyWithRecycle(LOGICAL(s), LOGICAL_RO(t), 0, ns, nt);
	break;
    case INTSXP:
	xcopyWithRecycle(INTEGER(s), INTEGER_RO(t), 0, ns, nt);
	break;
    case REALSXP:
	xcopyWithRecycle(REAL(s), REAL_RO(t), 0, ns, nt);
	break;
    case CPLXSXP:
	xcopyWithRecycle(COMPLEX(s), COMPLEX_RO(t), 0, ns, nt);
	break;
    case EXPRSXP:
	xcopyVectorWithRecycle(s, t, 0, ns, nt);
	break;
    case VECSXP:
	xcopyVectorWithRecycle(s, t, 0, ns, nt);
	break;
    case RAWSXP:
	xcopyWithRecycle(RAW(s), RAW_RO(t), 0, ns, nt);
	break;
    default:
	UNIMPLEMENTED_TYPE("copyVector", s);
    }
}

// In Rinternals.h, but no longer used by R
void Rf_copyListMatrix(SEXP s, SEXP t, Rboolean byrow)
{
    int nr = nrows(s), nc = ncols(s);
    R_xlen_t ns = ((R_xlen_t) nr) * nc;
    SEXP pt = t;
    if(byrow) {
	R_xlen_t NR = nr;
	GCStackRoot<> tmp;
	tmp = ListVector::create(ns);
	for (int i = 0; i < nr; i++)
	    for (int j = 0; j < nc; j++) {
		SET_VECTOR_ELT(tmp, i + j * NR, duplicate(CAR(pt)));
		pt = CDR(pt);
		if(pt == R_NilValue) pt = t;
	    }
	for (int i = 0; i < ns; i++) {
	    SETCAR(s, VECTOR_ELT(tmp, i++));
	    s = CDR(s);
	}
    }
    else {
	for (int i = 0; i < ns; i++) {
	    SETCAR(s, duplicate(CAR(pt)));
	    s = CDR(s);
	    pt = CDR(pt);
	    if(pt == R_NilValue) pt = t;
	}
    }
}

static R_INLINE SEXP VECTOR_ELT_LD(SEXP x, R_xlen_t i)
{
    return lazy_duplicate(VECTOR_ELT(x, i));
}

static R_INLINE SEXP XVECTOR_ELT_LD(SEXP x, R_xlen_t i)
{
    return lazy_duplicate(XVECTOR_ELT(x, i));
}

// In Rinternals.h
void Rf_copyMatrix(SEXP s, SEXP t, Rboolean byrow)
{
    int nr = nrows(s), nc = ncols(s);
    R_xlen_t nt = XLENGTH(t);

    if (byrow) {
	switch (TYPEOF(s)) {
	case STRSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		SET_STRING_ELT(s, didx, STRING_ELT(t, sidx));
	    break;
	case LGLSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		LOGICAL(s)[didx] = LOGICAL(t)[sidx];
	    break;
	case INTSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		INTEGER(s)[didx] = INTEGER(t)[sidx];
	    break;
	case REALSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		REAL(s)[didx] = REAL(t)[sidx];
	    break;
	case CPLXSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		COMPLEX(s)[didx] = COMPLEX(t)[sidx];
	    break;
	case EXPRSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		SET_XVECTOR_ELT(s, didx, XVECTOR_ELT_LD(t, sidx));
	    break;
	case VECSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		SET_VECTOR_ELT(s, didx, VECTOR_ELT_LD(t, sidx));
	    break;
	case RAWSXP:
	    FILL_MATRIX_BYROW_ITERATE(0, nr, nc, nt)
		RAW(s)[didx] = RAW(t)[sidx];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("copyMatrix", s);
	}
    }
    else
	copyVector(s, t);
}

#define COPY_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT) \
attribute_hidden void \
xcopy##TNAME##WithRecycle(SEXP dst, SEXP src, R_xlen_t dstart, R_xlen_t n, R_xlen_t nsrc) { \
							\
    if (nsrc >= n) { /* no recycle needed */		\
	for(R_xlen_t i = 0; i < n; i++)		\
	    SETELT(dst, dstart + i, GETELT(src, i));	\
	return;					\
    }							\
    if (nsrc == 1) {					\
	SEXP val = GETELT(src, 0);			\
	for(R_xlen_t i = 0; i < n; i++)			\
	    SETELT(dst, dstart + i, val);		\
	return;						\
    }							\
							\
    /* recycle needed */				\
    R_xlen_t sidx = 0;					\
    for(R_xlen_t i = 0; i < n; i++, sidx++) {		\
	if (sidx == nsrc) sidx = 0;			\
	SETELT(dst, dstart + i, GETELT(src, sidx));	\
    }							\
}

COPY_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT) /* xcopyStringWithRecycle */
COPY_ELT_WITH_RECYCLE(Vector, VECTOR_ELT_LD, SET_VECTOR_ELT) /* xcopyVectorWithRecycle */

#define FILL_ELT_WITH_RECYCLE(TNAME, GETELT, SETELT) \
attribute_hidden void xfill##TNAME##MatrixWithRecycle(SEXP dst, SEXP src,	\
    R_xlen_t dstart, R_xlen_t drows, R_xlen_t srows,		\
    R_xlen_t cols, R_xlen_t nsrc) {				\
								\
    FILL_MATRIX_ITERATE(dstart, drows, srows, cols, nsrc)	\
	SETELT(dst, didx, GETELT(src, sidx));			\
}

FILL_ELT_WITH_RECYCLE(String, STRING_ELT, SET_STRING_ELT) /* xfillStringMatrixWithRecycle */
FILL_ELT_WITH_RECYCLE(Vector, VECTOR_ELT, SET_VECTOR_ELT) /* xfillVectorMatrixWithRecycle */

/* For duplicating before modifying attributes duplicate_attr tries to
   wrap a larger vector object with an ALTREP wrapper, and falls back
   to duplicate or shallow_duplicate if the object can't be
   wrapped. The size threshold used seems to be reasonable but could be
   tested more extensively. */
#define WRAP_THRESHOLD 64
static SEXP duplicate_attr(SEXP x, Rboolean deep)
{
    if (isVector(x) && XLENGTH(x) >= WRAP_THRESHOLD) {
	GCStackRoot<> val;
	val = R_tryWrap(x);
	if (val != x) {
	    if (deep) {
		/* the spine has been duplicated; we could just do the values */
		SET_ATTRIB(val, duplicate(ATTRIB(val)));
	    }
	    return val;
	}
    }
    return deep ? duplicate(x) : shallow_duplicate(x);
}

attribute_hidden
SEXP R::R_shallow_duplicate_attr(SEXP x) { return duplicate_attr(x, FALSE); }
SEXP R::R_duplicate_attr(SEXP x) { return duplicate_attr(x, TRUE); }
