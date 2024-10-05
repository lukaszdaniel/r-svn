/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2023  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/* Internal header, not installed */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in inlined.c) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   See http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

#ifndef __cplusplus
#error Rinlinedfuns.h can only be included in C++ files
#endif

#ifdef COMPILING_R
/* defined only in inlined.c: this emits standalone code there */
# define INLINE_FUN
# define HIDDEN attribute_hidden
#else
/* This section is normally only used for versions of gcc which do not
   support C99 semantics.  __GNUC_STDC_INLINE__ is defined if
   GCC is following C99 inline semantics by default: we
   switch R's usage to the older GNU semantics via attributes.
   Do this even for __GNUC_GNUC_INLINE__ to shut up warnings in 4.2.x.
   __GNUC_STDC_INLINE__ and __GNUC_GNU_INLINE__ were added in gcc 4.2.0.
*/
/* object files will not contain definitions of functions declared
   "extern inline" in gnu90 inline mode */
# if defined(__GNUC_STDC_INLINE__) || defined(__GNUC_GNU_INLINE__)
#  define INLINE_FUN extern __attribute__((gnu_inline)) inline
# else
#  define INLINE_FUN extern R_INLINE
# endif
# define HIDDEN
#endif /* ifdef COMPILING_R */

#include <cstring> /* for strlen, strcmp */

#ifdef TESTING_WRITE_BARRIER
using namespace R;
#endif

#ifdef __cplusplus
extern "C" {
#endif
/* define inline-able functions */
#if defined(TESTING_WRITE_BARRIER) || defined(COMPILING_R)
# define STRICT_TYPECHECK
# define CATCH_ZERO_LENGTH_ACCESS
#endif


#if defined(USE_RINTERNALS) || defined(COMPILING_R)
/* inline version of CAR to support immediate bindings */
INLINE_FUN SEXP CAR(SEXP e)
{
    if (BNDCELL_TAG(e))
	error("bad binding access");
    return CAR0(e);
}
#else
SEXP CAR(SEXP e);
#endif

#ifdef STRICT_TYPECHECK
/*HIDDEN*/ INLINE_FUN void CHKVEC(SEXP x) {
    switch (TYPEOF(x)) {
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case WEAKREFSXP:
	break;
    default:
	error("cannot get data pointer of '%s' objects", R_typeToChar(x));
    }
}
#else
# define CHKVEC(x) do {} while(0)
#endif

INLINE_FUN void *DATAPTR(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return R::ALTVEC_DATAPTR(x);
#ifdef CATCH_ZERO_LENGTH_ACCESS
    /* Attempts to read or write elements of a zero length vector will
       result in a segfault, rather than read and write random memory.
       Returning NULL would be more natural, but Matrix seems to assume
       that even zero-length vectors have non-NULL data pointers, so
       return (void *) 1 instead. Zero-length CHARSXP objects still
       have a trailing zero byte so they are not handled. */
    else if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
	return (void *) 1;
#endif
    else
	return STDVEC_DATAPTR(x);
}

INLINE_FUN const void *DATAPTR_RO(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return R::ALTVEC_DATAPTR_RO(x);
    else
	return STDVEC_DATAPTR(x);
}

INLINE_FUN const void *DATAPTR_OR_NULL(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return R::ALTVEC_DATAPTR_OR_NULL(x);
    else
	return STDVEC_DATAPTR(x);
}

#ifdef STRICT_TYPECHECK
# define CHECK_VECTOR_LGL(x) do {				\
	if (TYPEOF(x) != LGLSXP) error("bad LGLSXP vector");	\
    } while (0)
# define CHECK_VECTOR_INT(x) do {				\
	if (! (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP))	\
	    error("bad INTSXP vector");				\
    } while (0)
# define CHECK_VECTOR_REAL(x) do {				\
	if (TYPEOF(x) != REALSXP) error("bad REALSXP vector");	\
    } while (0)
# define CHECK_VECTOR_CPLX(x) do {				\
	if (TYPEOF(x) != CPLXSXP) error("bad CPLXSXP vector");	\
    } while (0)
# define CHECK_VECTOR_RAW(x) do {				\
	if (TYPEOF(x) != RAWSXP) error("bad RAWSXP vector");	\
    } while (0)
#else
# define CHECK_VECTOR_LGL(x) do { } while(0)
# define CHECK_VECTOR_INT(x) do { } while(0)
# define CHECK_VECTOR_REAL(x) do { } while(0)
# define CHECK_VECTOR_CPLX(x) do { } while(0)
# define CHECK_VECTOR_RAW(x) do { } while(0)
#endif

INLINE_FUN const int *LOGICAL_OR_NULL(SEXP x) {
    CHECK_VECTOR_LGL(x);
    return (const int *) (ALTREP(x) ? R::ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

INLINE_FUN const int *INTEGER_OR_NULL(SEXP x) {
    CHECK_VECTOR_INT(x);
    return (const int *) (ALTREP(x) ? R::ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

INLINE_FUN const double *REAL_OR_NULL(SEXP x) {
    CHECK_VECTOR_REAL(x);
    return (const double *) (ALTREP(x) ? R::ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

INLINE_FUN const Rcomplex *COMPLEX_OR_NULL(SEXP x) {
    CHECK_VECTOR_CPLX(x);
    return (const Rcomplex *) (ALTREP(x) ? R::ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

INLINE_FUN const Rbyte *RAW_OR_NULL(SEXP x) {
    CHECK_VECTOR_RAW(x);
    return (const Rbyte *) (ALTREP(x) ? R::ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x));
}

INLINE_FUN R_xlen_t XLENGTH_EX(SEXP x)
{
    return ALTREP(x) ? R::ALTREP_LENGTH(x) : STDVEC_LENGTH(x);
}

INLINE_FUN R_xlen_t XTRUELENGTH(SEXP x)
{
    return ALTREP(x) ? R::ALTREP_TRUELENGTH(x) : STDVEC_TRUELENGTH(x);
}

/*HIDDEN*/ INLINE_FUN int LENGTH_EX(SEXP x, const char *file, int line)
{
    if (x == R_NilValue) return 0;
    R_xlen_t len = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    if (len > R_SHORT_LEN_MAX)
	R::R_BadLongVector(x, file, line);
#endif
    return (int) len;
}

#ifdef STRICT_TYPECHECK
# define CHECK_STDVEC_LGL(x) do {				\
	CHECK_VECTOR_LGL(x);					\
	if (ALTREP(x)) error("bad standard LGLSXP vector");	\
    } while (0)
# define CHECK_STDVEC_INT(x) do {				\
	CHECK_VECTOR_INT(x);					\
	if (ALTREP(x)) error("bad standard INTSXP vector");	\
    } while (0)
# define CHECK_STDVEC_REAL(x) do {				\
	CHECK_VECTOR_REAL(x);					\
	if (ALTREP(x)) error("bad standard REALSXP vector");	\
    } while (0)
# define CHECK_STDVEC_CPLX(x) do {				\
	CHECK_VECTOR_CPLX(x);					\
	if (ALTREP(x)) error("bad standard CPLXSXP vector");	\
    } while (0)
# define CHECK_STDVEC_RAW(x) do {				\
	CHECK_VECTOR_RAW(x);					\
	if (ALTREP(x)) error("bad standard RAWSXP vector");	\
    } while (0)

# define CHECK_SCALAR_LGL(x) do {				\
	CHECK_STDVEC_LGL(x);					\
	if (XLENGTH(x) != 1) error("bad LGLSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_INT(x) do {				\
	CHECK_STDVEC_INT(x);					\
	if (XLENGTH(x) != 1) error("bad INTSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_REAL(x) do {				\
	CHECK_STDVEC_REAL(x);					\
	if (XLENGTH(x) != 1) error("bad REALSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_CPLX(x) do {				\
	CHECK_STDVEC_CPLX(x);					\
	if (XLENGTH(x) != 1) error("bad CPLXSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_RAW(x) do {				\
	CHECK_STDVEC_RAW(x);					\
	if (XLENGTH(x) != 1) error("bad RAWSXP scalar");	\
    } while (0)

# define CHECK_BOUNDS_ELT(x, i) do {			\
	if (i < 0 || i > XLENGTH(x))			\
	    error("subscript out of bounds");		\
    } while (0)

# define CHECK_VECTOR_LGL_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_LGL(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_INT_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_INT(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_REAL_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_REAL(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_CPLX_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_CPLX(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_RAW_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_RAW(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
#else
# define CHECK_STDVEC_LGL(x) do { } while(0)
# define CHECK_STDVEC_INT(x) do { } while(0)
# define CHECK_STDVEC_REAL(x) do { } while(0)
# define CHECK_STDVEC_CPLX(x) do { } while(0)
# define CHECK_STDVEC_RAW(x) do { } while(0)

# define CHECK_SCALAR_LGL(x) do { } while(0)
# define CHECK_SCALAR_INT(x) do { } while(0)
# define CHECK_SCALAR_REAL(x) do { } while(0)
# define CHECK_SCALAR_CPLX(x) do { } while(0)
# define CHECK_SCALAR_RAW(x) do { } while(0)

# define CHECK_VECTOR_LGL_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_INT_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_REAL_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_CPLX_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_RAW_ELT(x, i) do { } while(0)
#endif

/*HIDDEN*/ INLINE_FUN int *LOGICAL0(SEXP x) {
    CHECK_STDVEC_LGL(x);
    return (int *) STDVEC_DATAPTR(x);
}

namespace R {
HIDDEN INLINE_FUN int SCALAR_LVAL(SEXP x) {
    CHECK_SCALAR_LGL(x);
    return LOGICAL0(x)[0];
}

HIDDEN INLINE_FUN void SET_SCALAR_LVAL(SEXP x, int v) {
    CHECK_SCALAR_LGL(x);
    LOGICAL0(x)[0] = v;
}
} // namespace R

/*HIDDEN*/ INLINE_FUN int *INTEGER0(SEXP x) {
    CHECK_STDVEC_INT(x);
    return (int *) STDVEC_DATAPTR(x);
}

namespace R {
HIDDEN INLINE_FUN int SCALAR_IVAL(SEXP x) {
    CHECK_SCALAR_INT(x);
    return INTEGER0(x)[0];
}

/*HIDDEN*/ INLINE_FUN void SET_SCALAR_IVAL(SEXP x, int v) {
    CHECK_SCALAR_INT(x);
    INTEGER0(x)[0] = v;
}
} // namespace R

/*HIDDEN*/ INLINE_FUN double *REAL0(SEXP x) {
    CHECK_STDVEC_REAL(x);
    return (double *) STDVEC_DATAPTR(x);
}

namespace R {
HIDDEN INLINE_FUN double SCALAR_DVAL(SEXP x) {
    CHECK_SCALAR_REAL(x);
    return REAL0(x)[0];
}

/*HIDDEN*/ INLINE_FUN void SET_SCALAR_DVAL(SEXP x, double v) {
    CHECK_SCALAR_REAL(x);
    REAL0(x)[0] = v;
}
} // namespace R

/*HIDDEN*/ INLINE_FUN Rcomplex *COMPLEX0(SEXP x) {
    CHECK_STDVEC_CPLX(x);
    return (Rcomplex *) STDVEC_DATAPTR(x);
}

namespace R {
HIDDEN INLINE_FUN Rcomplex SCALAR_CVAL(SEXP x) {
    CHECK_SCALAR_CPLX(x);
    return COMPLEX0(x)[0];
}

/*HIDDEN*/ INLINE_FUN void SET_SCALAR_CVAL(SEXP x, Rcomplex v) {
    CHECK_SCALAR_CPLX(x);
    COMPLEX0(x)[0] = v;
}
} // namespace R

/*HIDDEN*/ INLINE_FUN Rbyte *RAW0(SEXP x) {
    CHECK_STDVEC_RAW(x);
    return (Rbyte *) STDVEC_DATAPTR(x);
}

namespace R {
HIDDEN INLINE_FUN Rbyte SCALAR_BVAL(SEXP x) {
    CHECK_SCALAR_RAW(x);
    return RAW0(x)[0];
}

/*HIDDEN*/ INLINE_FUN void SET_SCALAR_BVAL(SEXP x, Rbyte v) {
    CHECK_SCALAR_RAW(x);
    RAW0(x)[0] = v;
}
} // namespace R

INLINE_FUN SEXP ALTREP_CLASS(SEXP x) { return CLASS(x); }

INLINE_FUN SEXP R_altrep_data1(SEXP x) { return DATA1(x); }
INLINE_FUN SEXP R_altrep_data2(SEXP x) { return DATA2(x); }
INLINE_FUN void R_set_altrep_data1(SEXP x, SEXP v) { SET_DATA1(x, v); }
INLINE_FUN void R_set_altrep_data2(SEXP x, SEXP v) { SET_DATA2(x, v); }

INLINE_FUN int INTEGER_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_INT_ELT(x, i);
    return ALTREP(x) ? R::ALTINTEGER_ELT(x, i) : INTEGER0(x)[i];
}

INLINE_FUN void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_INT_ELT(x, i);
    if (ALTREP(x)) R::ALTINTEGER_SET_ELT(x, i, v);
    else INTEGER0(x)[i] = v;
}

INLINE_FUN int LOGICAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    return ALTREP(x) ? R::ALTLOGICAL_ELT(x, i) : LOGICAL0(x)[i];
}

INLINE_FUN void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    if (ALTREP(x)) R::ALTLOGICAL_SET_ELT(x, i, v);
    else LOGICAL0(x)[i] = v;
}

INLINE_FUN double REAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    return ALTREP(x) ? R::ALTREAL_ELT(x, i) : REAL0(x)[i];
}

INLINE_FUN void SET_REAL_ELT(SEXP x, R_xlen_t i, double v)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    if (ALTREP(x)) R::ALTREAL_SET_ELT(x, i, v);
    else REAL0(x)[i] = v;
}

INLINE_FUN Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    return ALTREP(x) ? R::ALTCOMPLEX_ELT(x, i) : COMPLEX0(x)[i];
}

INLINE_FUN void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    if (ALTREP(x)) R::ALTCOMPLEX_SET_ELT(x, i, v);
    else COMPLEX0(x)[i] = v;
}

INLINE_FUN Rbyte RAW_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    return ALTREP(x) ? R::ALTRAW_ELT(x, i) : RAW0(x)[i];
}

INLINE_FUN void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    if (ALTREP(x)) R::ALTRAW_SET_ELT(x, i, v);
    else RAW0(x)[i] = v;
}

#if !defined(COMPILING_R) && !defined(COMPILING_MEMORY_C) &&	\
    !defined(TESTING_WRITE_BARRIER)
/* if not inlining use version in memory.c with more error checking */
INLINE_FUN SEXP STRING_ELT(SEXP x, R_xlen_t i) {
    if (ALTREP(x))
	return R::ALTSTRING_ELT(x, i);
    else {
	SEXP *ps = (SEXP *) STDVEC_DATAPTR(x);
	return ps[i];
    }
}
#else
SEXP STRING_ELT(SEXP x, R_xlen_t i);
#endif

#ifdef INLINE_PROTECT
LibExtern std::vector<SEXP> R_PPStack;

INLINE_FUN SEXP Rf_protect(SEXP s)
{
    R_CHECK_THREAD;
	R_PPStack.push_back(s);
    return s;
}

INLINE_FUN void Rf_unprotect(unsigned int l)
{
    R_CHECK_THREAD;
#ifdef PROTECT_PARANOID
    if (R_PPStack.size() < l)
        R_signal_unprotect_error();
#endif
    while (l--)
    {
        R_PPStack.pop_back();
    }
}

INLINE_FUN void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    protect(s);
    *pi = R_PPStack.size() - 1;
}

INLINE_FUN void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    if (i >= R_PPStack.size() || i < 0)
	R::R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}
#endif /* INLINE_PROTECT */

/* from dstruct.c */

/*  length - length of objects  */

/* TODO: a  Length(.) {say} which is  length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.c
*/
INLINE_FUN R_len_t Rf_length(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
	int i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return R::Rf_envlength(s);
    default:
	return 1;
    }
}

INLINE_FUN R_xlen_t Rf_xlength(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return XLENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
	// it is implausible this would be >= 2^31 elements, but allow it
	R_xlen_t i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return R::Rf_envxlength(s);
    default:
	return 1;
    }
}

/* regular allocVector() as a special case of allocVector3() with no custom allocator */
INLINE_FUN SEXP Rf_allocVector(SEXPTYPE type, R_xlen_t length)
{
    return allocVector3(type, length, NULL);
}

/* from list.c */
/* Return a dotted pair with the given CAR and CDR. */
/* The (R) TAG slot on the cell is set to NULL. */


/* Get the i-th element of a list */
INLINE_FUN SEXP Rf_elt(SEXP list, int i)
{
    SEXP result = list;

    if ((i < 0) || (i > length(list)))
	return R_NilValue;
    else
	for (int j = 0; j < i; j++)
	    result = CDR(result);

    return CAR(result);
}


/* Return the last element of a list */
INLINE_FUN SEXP Rf_lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue) {
	result = list;
	list = CDR(list);
    }
    return result;
}


/* Shorthands for creating small lists */

INLINE_FUN SEXP Rf_list1(SEXP s)
{
    return CONS(s, R_NilValue);
}


INLINE_FUN SEXP Rf_list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, list1(t));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP Rf_list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = CONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP Rf_list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = CONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = CONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = CONS(s, list5(t, u, v, w, x));
    UNPROTECT(1);
    return s;
}

/* Destructive list append : See also ``append'' */

INLINE_FUN SEXP Rf_listAppend(SEXP s, SEXP t)
{
    if (s == R_NilValue)
	return t;
    SEXP r = s;
    while (CDR(r) != R_NilValue)
	r = CDR(r);
    SETCDR(r, t);
    return s;
}


/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/* Return a (language) dotted pair with the given car and cdr */

INLINE_FUN SEXP Rf_lcons(SEXP car, SEXP cdr)
{
    SEXP e = CONS(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

INLINE_FUN SEXP Rf_lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

INLINE_FUN SEXP Rf_lang2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = LCONS(s, list1(t));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = LCONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = LCONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = LCONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP Rf_lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = LCONS(s, list5(t, u, v, w, x));
    UNPROTECT(1);
    return s;
}

/* from util.c */

/* Check to see if the arrays "x" and "y" have the identical extents */

HIDDEN INLINE_FUN Rboolean Rf_conformable(SEXP x, SEXP y)
{
    int n;
    PROTECT(x = getAttrib(x, R_DimSymbol));
    y = getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = length(x)) != length(y))
	return FALSE;
    for (int i = 0; i < n; i++)
	if (INTEGER(x)[i] != INTEGER(y)[i])
	    return FALSE;
    return TRUE;
}

/* NOTE: R's inherits() is based on inherits3() in ../main/objects.c
 * Here, use char / CHAR() instead of the slower more general translateChar()
 */
INLINE_FUN Rboolean Rf_inherits(SEXP s, const char *name)
{
    if (OBJECT(s)) {
	SEXP klass = getAttrib(s, R_ClassSymbol);
	int nclass = length(klass);
	for (int i = 0; i < nclass; i++) {
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), name))
		return TRUE;
	}
    }
    return FALSE;
}

INLINE_FUN Rboolean Rf_isValidString(SEXP x)
{
    return (Rboolean) (TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP);
}

/* non-empty ("") valid string :*/
INLINE_FUN Rboolean Rf_isValidStringF(SEXP x)
{
    return (Rboolean) (isValidString(x) && CHAR(STRING_ELT(x, 0))[0]);
}

HIDDEN INLINE_FUN Rboolean Rf_isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP) {
	const char *str = CHAR(PRINTNAME(s));
	if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str)-1] == '%')
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean Rf_isFunction(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == CLOSXP ||
	    TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean Rf_isPrimitive(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean Rf_isList(SEXP s)
{
    return (Rboolean) (s == R_NilValue || TYPEOF(s) == LISTSXP);
}


INLINE_FUN Rboolean Rf_isNewList(SEXP s)
{
    return (Rboolean) (s == R_NilValue || TYPEOF(s) == VECSXP);
}

INLINE_FUN Rboolean Rf_isPairList(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean Rf_isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean Rf_isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	return TRUE;
    default: /* including NULL */
	return FALSE;
    }
}

INLINE_FUN Rboolean Rf_isVector(SEXP s)/* === isVectorList() or isVectorAtomic() */
{
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean Rf_isDataFrame(SEXP s)
{
    if (OBJECT(s)) {
	SEXP klass = getAttrib(s, R_ClassSymbol);
	for (int i = 0; i < length(klass); i++)
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), "data.frame")) return TRUE;
    }
    return FALSE;
}
/* keep available under old name for now */
INLINE_FUN Rboolean isFrame(SEXP s) { return isDataFrame(s); }


/* DIFFERENT than R's  is.language(.) in ../main/coerce.c [do_is(), case 301:]
 *                                    which is   <=>  SYMSXP || LANGSXP || EXPRSXP */
INLINE_FUN Rboolean Rf_isLanguage(SEXP s)
{
    return (Rboolean) (s == R_NilValue || TYPEOF(s) == LANGSXP);
}

INLINE_FUN Rboolean Rf_isMatrix(SEXP s)
{
    if (isVector(s)) {
	SEXP t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
	if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean Rf_isArray(SEXP s)
{
    if (isVector(s)) {
	SEXP t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
	if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean Rf_isTs(SEXP s)
{
    return (Rboolean) (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue);
}


INLINE_FUN Rboolean Rf_isInteger(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == INTSXP && !inherits(s, "factor"));
}

INLINE_FUN Rboolean Rf_isFactor(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == INTSXP  && inherits(s, "factor"));
}

INLINE_FUN int Rf_nlevels(SEXP f)
{
    if (!isFactor(f))
	return 0;
    return LENGTH(getAttrib(f, R_LevelsSymbol));
}

/* Is an object of numeric type. */
/* FIXME:  the LGLSXP case should be excluded here
 * (really? in many places we affirm they are treated like INTs)*/

INLINE_FUN Rboolean Rf_isNumeric(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inherits(s,"factor")) return FALSE;
    case LGLSXP:
    case REALSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/** Is an object "Numeric" or  complex */
INLINE_FUN Rboolean Rf_isNumber(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inherits(s,"factor")) return FALSE;
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/* As from R 2.4.0 we check that the value is allowed. */
INLINE_FUN SEXP Rf_ScalarLogical(int x)
{
    LibExtern SEXP R_LogicalNAValue, R_TrueValue, R_FalseValue;
    if (x == NA_LOGICAL) return R_LogicalNAValue;
    else if (x != 0) return R_TrueValue;
    else return R_FalseValue;
}

INLINE_FUN SEXP Rf_ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, 1);
    R::SET_SCALAR_IVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP Rf_ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, 1);
    R::SET_SCALAR_DVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP Rf_ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, 1);
    R::SET_SCALAR_CVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP Rf_ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    UNPROTECT(1);
    return ans;
}

INLINE_FUN SEXP Rf_ScalarRaw(Rbyte x)
{
    SEXP ans = allocVector(RAWSXP, 1);
    R::SET_SCALAR_BVAL(ans, x);
    return ans;
}

/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */
/* BUT it does not exclude 0! */

INLINE_FUN Rboolean Rf_isVectorizable(SEXP s)
{
    if (s == R_NilValue) return TRUE;
    else if (isNewList(s)) {

	R_xlen_t n = XLENGTH(s);
	for (R_xlen_t i = 0 ; i < n; i++)
	    if (!isVector(VECTOR_ELT(s, i)) || XLENGTH(VECTOR_ELT(s, i)) > 1)
		return FALSE;
	return TRUE;
    }
    else if (isList(s)) {
	for ( ; s != R_NilValue; s = CDR(s))
	    if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1) return FALSE;
	return TRUE;
    }
    else return FALSE;
}


/**
 * Create a named vector of type TYP
 *
 * @example const char *nms[] = {"xi", "yi", "zi", ""};
 *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
 *
 * @param TYP a vector SEXP type (e.g. REALSXP)
 * @param names names of list elements with null string appended
 *
 * @return (pointer to a) named vector of type TYP
 */
INLINE_FUN SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
{
    R_xlen_t n;

    for (n = 0; strlen(names[n]) > 0; n++) {}
    SEXP ans = PROTECT(allocVector(TYP, n));
    SEXP nms = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++)
	SET_STRING_ELT(nms, i, mkChar(names[i]));
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

/* from gram.y */

/* short cut for  ScalarString(mkChar(s)) : */
INLINE_FUN SEXP Rf_mkString(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, (R_xlen_t)1));
    SET_STRING_ELT(t, (R_xlen_t)0, mkChar(s));
    UNPROTECT(1);
    return t;
}

/* index of a given C string in (translated) R string vector  */
HIDDEN INLINE_FUN int Rf_stringPositionTr(SEXP string, const char *translatedElement) {

    int slen = LENGTH(string);

    const void *vmax = vmaxget();
    for (int i = 0 ; i < slen; i++) {
	bool found = !strcmp(translateChar(STRING_ELT(string, i)),
				  translatedElement);
	vmaxset(vmax);
        if (found)
            return i;
    }
    return -1; /* not found */
}

#ifdef __cplusplus
} // extern "C"
#endif

namespace R {
/* duplicate RHS value of complex assignment if necessary to prevent cycles */
HIDDEN INLINE_FUN SEXP R_FixupRHS(SEXP x, SEXP y)
{
    if ( y != R_NilValue && MAYBE_REFERENCED(y) ) {
	if (R::R_cycle_detected(x, y)) {
#ifdef WARNING_ON_CYCLE_DETECT
	    warning("cycle detected");
	    R_cycle_detected(x, y);
#endif
	    y = duplicate(y);
	}
	else ENSURE_NAMEDMAX(y);
    }
    return y;
}
} // namespace R

#endif /* R_INLINES_H_ */
