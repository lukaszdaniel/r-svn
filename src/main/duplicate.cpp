/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *            (C) 2004  The R Foundation
 *  Copyright (C) 1998-2015 The R Core Team.
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

#include <CXXR/ProtectStack.hpp>
#include <CXXR/FunctionBase.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <Localization.h>
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

#define COPY_TRUELENGTH(to, from) do {			\
	if (! IS_GROWABLE(from))			\
	    SET_TRUELENGTH(to, XTRUELENGTH(from));	\
    } while (0)

/* This macro pulls out the common code in copying an atomic vector.
   The special handling of the scalar case (__n__ == 1) seems to make
   a small but measurable difference, at least for some cases
   and when (as in R 2.15.x) a for() loop was used.
*/
#ifdef __APPLE__
/* it seems macOS builds did not copy >= 2^32 bytes fully */
#define DUPLICATE_ATOMIC_VECTOR(type, fun, to, from, deep) do {	\
  R_xlen_t __n__ = XLENGTH(from); \
  PROTECT(from); \
  PROTECT(to = allocVector(TYPEOF(from), __n__)); \
  if (__n__ == 1) fun(to)[0] = fun(from)[0]; \
  else { \
      R_xlen_t __this; \
      type *__to = fun(to), *__from = fun(from); \
      while(__n__ > 0) { \
	 __this = (__n__ < 1000000) ? __n__ : 1000000; \
	 memcpy(__to, __from, __this * sizeof(type));  \
	 __n__ -= __this;  __to += __this; __from += __this; \
      } \
  } \
  DUPLICATE_ATTRIB(to, from, deep);		 \
  COPY_TRUELENGTH(to, from); \
  UNPROTECT(2); \
} while (0)
#else
#define DUPLICATE_ATOMIC_VECTOR(type, fun, to, from, deep) do {	\
  R_xlen_t __n__ = XLENGTH(from); \
  PROTECT(from); \
  PROTECT(to = allocVector(TYPEOF(from), __n__)); \
  if (__n__ == 1) fun(to)[0] = fun(from)[0]; \
  else if (__n__) memcpy(fun(to), fun(from), __n__ * sizeof(type)); \
  DUPLICATE_ATTRIB(to, from, deep); \
  COPY_TRUELENGTH(to, from); \
  UNPROTECT(2); \
} while (0)
#endif

/* The following macros avoid the cost of going through calls to the
   assignment functions (and duplicate in the case of ATTRIB) when the
   ATTRIB or TAG value to be stored is R_NilValue, the value the field
   will have been set to by the allocation function */
#define DUPLICATE_ATTRIB(to, from, deep) do { \
  SEXP __a__ = ATTRIB(from); \
  if (__a__ != R_NilValue) { \
      SET_ATTRIB(to, duplicate1(__a__, deep)); \
    SET_OBJECT(to, OBJECT(from)); \
    to->setS4Object(IS_S4_OBJECT(from));  \
  } \
} while (0)

#define COPY_TAG(to, from) do { \
  SEXP __tag__ = TAG(from); \
  if (__tag__ != R_NilValue) SET_TAG(to, __tag__); \
} while (0)


/* For memory profiling.  */
/* We want a count of calls to duplicate from outside
   which requires a wrapper function.

   The original duplicate() function is now duplicate1().

   I don't see how to make the wrapper go away when R_PROFILING
   is not defined, because we still need to be able to
   optionally rename duplicate() as Rf_duplicate().
*/
static SEXP duplicate1(SEXP, Rboolean deep);

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
SEXP CXXR_duplicate(SEXP s)
{
    if (s == R_NilValue)
        return R_NilValue;
#ifdef R_PROFILING
    ++s_duplicate_counter;
#endif
    SEXP t = duplicate1(s, Rboolean(depth));
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

SEXP Rf_lazy_duplicate(SEXP s) {
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

static SEXP duplicate_child(SEXP s, Rboolean deep) {
    if (deep)
	return duplicate1(s, TRUE);
    else
	return lazy_duplicate(s);
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

static R_INLINE SEXP duplicate_list(SEXP s, Rboolean deep)
{
    if (s == R_NilValue) return R_NilValue;
    SEXP sp, vp, val;
    PROTECT(s);

    val = R_NilValue;
    for (sp = s; sp != R_NilValue; sp = CDR(sp))
	val = CONS(R_NilValue, val);

    PROTECT(val);
    for (sp = s, vp = val; sp != R_NilValue; sp = CDR(sp), vp = CDR(vp)) {
	SETCAR(vp, duplicate_child(CAR(sp), deep));
	COPY_TAG(vp, sp);
	DUPLICATE_ATTRIB(vp, sp, deep);
    }
    UNPROTECT(2);
    return val;
}

static SEXP duplicate1(SEXP s, Rboolean deep)
{
    SEXP t;
    R_xlen_t i, n;

    if (ALTREP(s)) {
	PROTECT(s); /* the methods should protect, but ... */
	SEXP ans = ALTREP_DUPLICATE_EX(s, deep);
	UNPROTECT(1);
	if (ans != NULL)
	    return ans;
    }

    switch (TYPEOF(s)) {
    case NILSXP:
    case SYMSXP:
    case ENVSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case WEAKREFSXP:
	return s;
    case CLOSXP:
	PROTECT(s);
	PROTECT(t = allocSExp(CLOSXP));
	SET_FORMALS(t, FORMALS(s));
	SET_BODY(t, BODY(s));
	SET_CLOENV(t, CLOENV(s));
	DUPLICATE_ATTRIB(t, s, deep);
	if (NOJIT(s)) SET_NOJIT(t);
	if (MAYBEJIT(s)) SET_MAYBEJIT(t);
	UNPROTECT(2);
	break;
    case LISTSXP:
	PROTECT(s);
	t = duplicate_list(s, deep);
	UNPROTECT(1);
	break;
    case LANGSXP:
	PROTECT(s);
	PROTECT(t = duplicate_list(s, deep));
	SET_TYPEOF(t, LANGSXP);
	DUPLICATE_ATTRIB(t, s, deep);
	UNPROTECT(2);
	break;
    case DOTSXP:
	PROTECT(s);
	PROTECT(t = duplicate_list(s, deep));
	SET_TYPEOF(t, DOTSXP);
	DUPLICATE_ATTRIB(t, s, deep);
	UNPROTECT(2);
	break;
    case CHARSXP:
	return s;
	break;
    case EXPRSXP:
    case VECSXP:
	n = XLENGTH(s);
	PROTECT(s);
	PROTECT(t = allocVector(TYPEOF(s), n));
	for(i = 0 ; i < n ; i++)
	    SET_VECTOR_ELT(t, i, duplicate_child(VECTOR_ELT(s, i), deep));
	DUPLICATE_ATTRIB(t, s, deep);
	COPY_TRUELENGTH(t, s);
	UNPROTECT(2);
	break;
    case LGLSXP: DUPLICATE_ATOMIC_VECTOR(int, LOGICAL, t, s, deep); break;
    case INTSXP: DUPLICATE_ATOMIC_VECTOR(int, INTEGER, t, s, deep); break;
    case REALSXP: DUPLICATE_ATOMIC_VECTOR(double, REAL, t, s, deep); break;
    case CPLXSXP: DUPLICATE_ATOMIC_VECTOR(Rcomplex, COMPLEX, t, s, deep); break;
    case RAWSXP: DUPLICATE_ATOMIC_VECTOR(Rbyte, RAW, t, s, deep); break;
    case STRSXP:
	/* direct copying and bypassing the write barrier is OK since
	   t was just allocated and so it cannot be older than any of
	   the elements in s.  LT */
	DUPLICATE_ATOMIC_VECTOR(SEXP, STRING_PTR, t, s, deep);
	break;
    case PROMSXP:
	return s;
	break;
    case OBJSXP:
	PROTECT(s);
	PROTECT(t = R_allocObject());
	DUPLICATE_ATTRIB(t, s, deep);
	UNPROTECT(2);
	break;
    default:
	UNIMPLEMENTED_TYPE("duplicate", s);
	t = s;/* for -Wall */
    }
    if(TYPEOF(t) == TYPEOF(s) ) { /* surely it only makes sense in this case*/
	SET_OBJECT(t, OBJECT(s));
	t->setS4Object(IS_S4_OBJECT(s));
    }
    return t;
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
	xcopyWithRecycle(LOGICAL(s), LOGICAL(t), 0, ns, nt);
	break;
    case INTSXP:
	xcopyWithRecycle(INTEGER(s), INTEGER(t), 0, ns, nt);
	break;
    case REALSXP:
	xcopyWithRecycle(REAL(s), REAL(t), 0, ns, nt);
	break;
    case CPLXSXP:
	xcopyWithRecycle(COMPLEX(s), COMPLEX(t), 0, ns, nt);
	break;
    case EXPRSXP:
	xcopyVectorWithRecycle(s, t, 0, ns, nt);
	break;
    case VECSXP:
	xcopyVectorWithRecycle(s, t, 0, ns, nt);
	break;
    case RAWSXP:
	xcopyWithRecycle(RAW(s), RAW(t), 0, ns, nt);
	break;
    default:
	UNIMPLEMENTED_TYPE("copyVector", s);
    }
}

// In Rinternals.h
void Rf_copyListMatrix(SEXP s, SEXP t, Rboolean byrow)
{
    int nr = nrows(s), nc = ncols(s);
    R_xlen_t ns = ((R_xlen_t) nr) * nc;
    SEXP pt = t;
    if(byrow) {
	R_xlen_t NR = nr;
	SEXP tmp = PROTECT(allocVector(STRSXP, ns));
	for (int i = 0; i < nr; i++)
	    for (int j = 0; j < nc; j++) {
		SET_STRING_ELT(tmp, i + j * NR, duplicate(CAR(pt)));
		pt = CDR(pt);
		if(pt == R_NilValue) pt = t;
	    }
	for (int i = 0; i < ns; i++) {
	    SETCAR(s, STRING_ELT(tmp, i++));
	    s = CDR(s);
	}
	UNPROTECT(1);
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
	SEXP val = R_tryWrap(x);
	if (val != x) {
	    if (deep) {
		PROTECT(val);
		/* the spine has been duplicated; we could just do the values */
		SET_ATTRIB(val, duplicate(ATTRIB(val)));
		UNPROTECT(1); /* val */
	    }
	    return val;
	}
    }
    return deep ? duplicate(x) : shallow_duplicate(x);
}

SEXP R_shallow_duplicate_attr(SEXP x) { return duplicate_attr(x, FALSE); }
SEXP R_duplicate_attr(SEXP x) { return duplicate_attr(x, TRUE); }
