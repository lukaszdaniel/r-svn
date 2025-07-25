/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2023  The R Core Team
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
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 */

/** @file subset.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/Complex.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/String.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>

using namespace R;
using namespace CXXR;

/* JMC convinced MM that this was not a good idea: */
#undef _S4_subsettable


static R_INLINE SEXP VECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = NAMEDMAX.
       Duplicating might be safer/more consistent (fix bug reported by
       Radford Neal; similar to PR15098) */
    SEXP val = VECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	ENSURE_NAMEDMAX(val);
    return val;
}

static R_INLINE SEXP XVECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = NAMEDMAX.
       Duplicating might be safer/more consistent (fix bug reported by
       Radford Neal; similar to PR15098) */
    SEXP val = XVECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	ENSURE_NAMEDMAX(val);
    return val;
}

/* ExtractSubset allocates "result" and does the transfer of elements
   from "x" to "result" according to the integer/real subscripts given
   in "indx".

   The EXTRACT_SUBSET_LOOP macro allows the branches based on index
   type and vector type to happen outside the loop.

   This could avoid using data pointers, but there is little point as
   currently the subscript code forces allocation.
*/

#define EXTRACT_SUBSET_LOOP(STDCODE, NACODE) do { \
	if (TYPEOF(indx) == INTSXP) {		  \
	    const int *pindx = INTEGER_RO(indx);  \
	    for (i = 0; i < n; i++) {		  \
		ii = pindx[i];			  \
		if (0 < ii && ii <= nx) {	  \
		    ii--;			  \
		    STDCODE;			  \
		}				  \
		else /* out of bounds or NA */	  \
		    NACODE;			  \
	    }					  \
	}					  \
	else {					  \
	    const double *pindx = REAL_RO(indx);  \
	    for (i = 0; i < n; i++) {		  \
		double di = pindx[i];		  \
		ii = (R_xlen_t) (di - 1);	  \
		if (R_FINITE(di) &&		  \
		    0 <= ii && ii < nx)		  \
		    STDCODE;			  \
		else				  \
		    NACODE;			  \
	    }					  \
	}					  \
    } while (0)

NORET static void errorcallNotSubsettable(SEXP x, SEXP call)
{
    GCStackRoot<> cond;
    cond = R_makeNotSubsettableError(x, call);
    R_signalErrorCondition(cond, call);
}

NORET static void errorcallMissingSubs(SEXP x, SEXP call)
{
    if (call == R_NilValue)
	call = R_CurrentExpression;
    GCStackRoot<> cond;
    cond = R_makeMissingSubscriptError(x, call);
    R_signalErrorCondition(cond, call);
}


attribute_hidden SEXP R::ExtractSubset(SEXP x, SEXP indx, SEXP call)
{
    if (x == R_NilValue)
	return x;

    SEXP result;

    if (ALTREP(x)) {
	result = ALTVEC_EXTRACT_SUBSET(x, indx, call);
	if (result != NULL)
	    return result;
    }

    R_xlen_t i, ii, n, nx;
    n = XLENGTH(indx);
    nx = xlength(x);
    SEXPTYPE mode = TYPEOF(x);

    /* protect allocation in case _ELT operations need to allocate */
    PROTECT(result = allocVector(mode, n));
    switch(mode) {
    case LGLSXP:
	EXTRACT_SUBSET_LOOP(LOGICAL0(result)[i] = LOGICAL_ELT(x, ii),
			    LOGICAL0(result)[i] = NA_LOGICAL);
	break;
    case INTSXP:
	EXTRACT_SUBSET_LOOP(INTEGER0(result)[i] = INTEGER_ELT(x, ii),
			    INTEGER0(result)[i] = NA_INTEGER);
	break;
    case REALSXP:
	EXTRACT_SUBSET_LOOP(REAL0(result)[i] = REAL_ELT(x, ii),
			    REAL0(result)[i] = NA_REAL);
	break;
    case CPLXSXP:
	{
	    Complex NA_CPLX = { NA_REAL, NA_REAL };
	    EXTRACT_SUBSET_LOOP(COMPLEX0(result)[i] = COMPLEX_ELT(x, ii),
				COMPLEX0(result)[i] = NA_CPLX);
	}
	break;
    case STRSXP:
	EXTRACT_SUBSET_LOOP(SET_STRING_ELT(result, i, STRING_ELT(x, ii)),
			    SET_STRING_ELT(result, i, NA_STRING));
	break;
    case VECSXP:
	EXTRACT_SUBSET_LOOP(SET_VECTOR_ELT(result, i,
					   VECTOR_ELT_FIX_NAMED(x, ii)),
			    SET_VECTOR_ELT(result, i, R_NilValue));
	break;
    case EXPRSXP:
	EXTRACT_SUBSET_LOOP(SET_XVECTOR_ELT(result, i,
					   XVECTOR_ELT_FIX_NAMED(x, ii)),
			    SET_XVECTOR_ELT(result, i, R_NilValue));
	break;
    case RAWSXP:
	EXTRACT_SUBSET_LOOP(RAW0(result)[i] = RAW_ELT(x, ii),
			    RAW0(result)[i] = (Rbyte) 0);
	break;
    case LISTSXP:
	/* cannot happen: pairlists are coerced to lists */
    case LANGSXP:
	/* cannot happen: LANGSXPs are coerced to lists */
    default:
	errorcallNotSubsettable(x, call);
    }
    UNPROTECT(1); /* result */
    return result;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP s, SEXP call)
{
    if (x == R_NilValue)
	    return R_NilValue;
    if (s == R_MissingArg) return duplicate(x);

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    PROTECT(s);
    if (ATTRIB(s) != R_NilValue) { /* pretest to speed up simple case */
	SEXP dim = getAttrib(x, R_DimSymbol);
	if (isMatrix(s) && isArray(x) && ncols(s) == length(dim)) {
	    if (isString(s)) {
		SEXP dnames = PROTECT(GetArrayDimnames(x));
 		s = strmat2intmat(s, dnames, call, x);
		UNPROTECT(2); /* dnames, s */
		PROTECT(s);
	    }
	    if (isInteger(s) || isReal(s)) {
		s = mat2indsub(dim, s, call, x);
		UNPROTECT(1);
		PROTECT(s);
	    }
	}
    }

    SEXPTYPE mode = TYPEOF(x);
    switch (mode)
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
        break;
    default:
        Rf_errorcall(call, _("object of type '%s' is not subsettable in CXXR"), Rf_type2char(mode));
    }

    /* Convert to a vector of integer subscripts */
    /* in the range 1:length(x). */
    R_xlen_t stretch = 1;
    SEXP indx = PROTECT(makeSubscript(x, s, &stretch, call));

    /* Allocate the result. */


    SEXP result = PROTECT(ExtractSubset(x, indx, call));
    if (mode == VECSXP || mode == EXPRSXP)
	/* we do not duplicate the values when extracting the subset,
	   so to be conservative mark the result as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(result);

    // Fix attributes:
    if (result != R_NilValue) {
	SEXP attrib, nattrib;
	if (
	    ((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
	    ( /* here we might have an array.  Use row names if 1D */
		isArray(x) && length(getAttrib(x, R_DimNamesSymbol)) == 1 &&
		(attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
		(attrib = GetRowNames(attrib)) != R_NilValue
		)
	    ) {
	    PROTECT(attrib);
	    PROTECT(nattrib = ExtractSubset(attrib, indx, call));
	    setAttrib(result, R_NamesSymbol, nattrib);
	    UNPROTECT(2); /* attrib, nattrib */
	}
	if ((attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
	    TYPEOF(attrib) == VECSXP) {
	    PROTECT(nattrib = ExtractSubset(attrib, indx, call));
	    setAttrib(result, R_SrcrefSymbol, nattrib);
	    UNPROTECT(1);
	}
	/* FIXME:  this is wrong, because the slots are gone, so result is an invalid object of the S4 class! JMC 3/3/09 */
#ifdef _S4_subsettable
	if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	    SET_S4_OBJECT(result);
	}
#endif
    }
    UNPROTECT(3);
    return result;
}

NORET static void errorcallOutOfBounds(SEXP x, int subscript,
				       R_xlen_t index, SEXP call)
{
    GCStackRoot<> sindex, cond;
    sindex = ScalarReal((double) index);
    cond = R_makeOutOfBoundsError(x, subscript, sindex, call, NULL);
    R_signalErrorCondition(cond, call);
}

NORET static void errorcallOutOfBoundsSEXP(SEXP x, int subscript,
					   SEXP sindex, SEXP call)
{
    GCStackRoot<> cond;
    cond = R_makeOutOfBoundsError(x, subscript, sindex, call, NULL);
    R_signalErrorCondition(cond, call);
}

// in ./subscript.c :
SEXP int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call);

/* The MATRIX_SUBSET_LOOP macro allows the branches based on index
   type and vector type to happen outside the loop. Running through
   the indices in column-major order also improves cache locality. */

#define MATRIX_SUBSET_LOOP(STDCODE, NACODE) do {		\
	for (j = 0; j < ncs; j++) {				\
	    jj = psc[j];					\
	    if (jj != NA_INTEGER) {				\
		if (jj < 1 || jj > nc)				\
		    errorcallOutOfBounds(x, 0, jj, call);	\
		jj--;						\
	    }							\
	    for (i = 0; i < nrs; i++) {				\
		ii = psr[i];					\
		if (ii != NA_INTEGER) {				\
		    if (ii < 1 || ii > nr)			\
			errorcallOutOfBounds(x, 1, ii, call);	\
		    ii--;					\
		}						\
		ij = i + j * nrs;				\
		if (ii == NA_INTEGER || jj == NA_INTEGER)	\
		    NACODE;					\
		else {						\
		    iijj = ii + jj * nr;			\
		    STDCODE;					\
		}						\
	    }							\
	}							\
    } while (0)

static SEXP MatrixSubset(SEXP x, SEXP s, SEXP call, int drop)
{
    SEXP attr, result, sr, sc, dim;
    int nr, nc, nrs, ncs;
    R_xlen_t i, j, ii, jj, ij, iijj;

    nr = nrows(x);
    nc = ncols(x);

    /* Note that "s" is protected on entry. */
    /* The following ensures that pointers remain protected. */
    dim = getAttrib(x, R_DimSymbol);

    sr = SETCAR (s, int_arraySubscript(0, CAR(s),  dim, x, call));
    sc = SETCADR(s, int_arraySubscript(1, CADR(s), dim, x, call));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    /* Check this does not overflow: currently only possible on 32-bit */
    if ((double)nrs * (double)ncs > (double) R_XLEN_T_MAX)
	error("%s", _("dimensions would exceed maximum size of array"));
    PROTECT(sr);
    PROTECT(sc);
    result = allocVector(TYPEOF(x), (R_xlen_t) nrs * (R_xlen_t) ncs);
    const int *psr = INTEGER_RO(sr);
    const int *psc = INTEGER_RO(sc);
    PROTECT(result);
    switch(TYPEOF(x)) {
    case LGLSXP:
	MATRIX_SUBSET_LOOP(LOGICAL0(result)[ij] = LOGICAL_ELT(x, iijj),
			   LOGICAL0(result)[ij] = NA_LOGICAL);
	break;
    case INTSXP:
	MATRIX_SUBSET_LOOP(INTEGER0(result)[ij] = INTEGER_ELT(x, iijj),
			   INTEGER0(result)[ij] = NA_INTEGER);
	break;
    case REALSXP:
	MATRIX_SUBSET_LOOP(REAL0(result)[ij] = REAL_ELT(x, iijj),
			   REAL0(result)[ij] = NA_REAL);
	break;
    case CPLXSXP:
	{
	    Complex NA_CPLX = { NA_REAL, NA_REAL };
	    MATRIX_SUBSET_LOOP(COMPLEX0(result)[ij] = COMPLEX_ELT(x, iijj),
			       COMPLEX0(result)[ij] = NA_CPLX);
	}
	break;
    case STRSXP:
	MATRIX_SUBSET_LOOP(SET_STRING_ELT(result, ij, STRING_ELT(x, iijj)),
			   SET_STRING_ELT(result, ij, NA_STRING));
	break;
    case VECSXP:
	MATRIX_SUBSET_LOOP(SET_VECTOR_ELT(result, ij,
					  VECTOR_ELT_FIX_NAMED(x, iijj)),
			   SET_VECTOR_ELT(result, ij, R_NilValue));
	break;
    case EXPRSXP:
	MATRIX_SUBSET_LOOP(SET_XVECTOR_ELT(result, ij,
					  VECTOR_ELT_FIX_NAMED(x, iijj)),
			   SET_XVECTOR_ELT(result, ij, R_NilValue));
	break;
    case RAWSXP:
	MATRIX_SUBSET_LOOP(RAW0(result)[ij] = RAW_ELT(x, iijj),
			   RAW0(result)[ij] = (Rbyte) 0);
	break;
    default:
	errorcall(call, "%s", _("matrix subscripting not handled for this type"));
	break;
    }

    if(nrs >= 0 && ncs >= 0) {
	PROTECT(attr = allocVector(INTSXP, 2));
	INTEGER0(attr)[0] = nrs;
	INTEGER0(attr)[1] = ncs;
	if(!isNull(getAttrib(dim, R_NamesSymbol)))
	    setAttrib(attr, R_NamesSymbol, getAttrib(dim, R_NamesSymbol));
	setAttrib(result, R_DimSymbol, attr);
	UNPROTECT(1);
    }

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.	 Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    if (nrs >= 0 && ncs >= 0) {
	SEXP dimnames, dimnamesnames, newdimnames;
	dimnames = getAttrib(x, R_DimNamesSymbol);
	PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
	if (!isNull(dimnames)) {
	    PROTECT(newdimnames = allocVector(VECSXP, 2));
	    if (TYPEOF(dimnames) == VECSXP) {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(VECTOR_ELT(dimnames, 0), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(VECTOR_ELT(dimnames, 1), sc, call));
	    }
	    else {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(CAR(dimnames), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(CADR(dimnames), sc, call));
	    }
	    setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(result, R_DimNamesSymbol, newdimnames);
	    UNPROTECT(1); /* newdimnames */
	}
	UNPROTECT(1); /* dimnamesnames */
    }
    /*  Probably should not do this:
    copyMostAttrib(x, result); */
    if (drop)
	DropDims(result);
    UNPROTECT(3);
    return result;
}

static R_INLINE R_xlen_t findASubIndex(R_xlen_t k, const int * const *subs,
				       const int *indx, const int *pxdims,
				       const R_xlen_t *offset,
				       SEXP call)
{
    R_xlen_t ii = 0;
    for (int j = 0; j < k; j++) {
	int jj = subs[j][indx[j]];
	if (jj == NA_INTEGER)
	    return NA_INTEGER;
	ii += (jj - 1) * offset[j];
    }
    return ii;
}

#define ARRAY_SUBSET_LOOP(STDCODE, NACODE) do {			\
	for (R_xlen_t i = 0; i < n; i++) {			\
	    R_xlen_t ii = findASubIndex(k, subs, indx,		\
					pxdims, offset, call);	\
	    if (ii != NA_INTEGER)				\
		STDCODE;					\
	    else						\
		NACODE;						\
	    if (n > 1) {					\
		int j = 0;					\
		while (++indx[j] >= bound[j]) {			\
		    indx[j] = 0;				\
		    j = (j + 1) % k;				\
		}						\
	    }							\
	}							\
    } while (0)

static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
    int k;
    SEXPTYPE mode;
    SEXP dimnames, dimnamesnames, p, q, r, result, xdims;
    CXXR::RAllocStack::Scope rscope;

    mode = TYPEOF(x);
    xdims = getAttrib(x, R_DimSymbol);
    k = length(xdims);
    const int *pxdims = INTEGER_RO(xdims);

    /* k is now the number of dims */
    const int **subs = (const int**)R_alloc(k, sizeof(int*));
    int *indx = (int*)R_alloc(k, sizeof(int));
    int *bound = (int*)R_alloc(k, sizeof(int));
    R_xlen_t *offset = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));

    /* Construct a vector to contain the returned values. */
    /* Store its extents. */

    R_xlen_t n = 1;
    r = s;
    for (int i = 0; i < k; i++) {
	SETCAR(r, int_arraySubscript(i, CAR(r), xdims, x, call));
	bound[i] = LENGTH(CAR(r));
	n *= bound[i];
	r = CDR(r);
    }

    r = s;
    for (int i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER_RO(CAR(r));
	r = CDR(r);
    }
    offset[0] = 1;
    for (int i = 1; i < k; i++)
	offset[i] = offset[i - 1] * pxdims[i - 1];

    /* range check on indices -- the lower bound check may not be needed */
    for (int i = 0; i < k; i++)
	for (int j = 0; j < bound[i]; j++) {
	    int jj = subs[i][j];
	    if (jj > pxdims[i] ||
		/* this should not be needed as indices reaching this
		   point should be positive or NA_INTEGER, which is
		   negative */
		(jj < 1 && jj != NA_INTEGER))
		errorcallOutOfBounds(x, i, jj, call);
	}

    /* Transfer the subset elements from "x" to "a". */
    PROTECT(result = allocVector(mode, n));
    switch (mode) {
    case LGLSXP:
	ARRAY_SUBSET_LOOP(LOGICAL0(result)[i] = LOGICAL_ELT(x, ii),
			  LOGICAL0(result)[i] = NA_LOGICAL);
	break;
    case INTSXP:
	ARRAY_SUBSET_LOOP(INTEGER0(result)[i] = INTEGER_ELT(x, ii),
			  INTEGER0(result)[i] = NA_INTEGER);
	break;
    case REALSXP:
	ARRAY_SUBSET_LOOP(REAL0(result)[i] = REAL_ELT(x, ii),
			  REAL0(result)[i] = NA_REAL);
	break;
    case CPLXSXP:
	{
	    Complex NA_CPLX = { NA_REAL, NA_REAL };
	    ARRAY_SUBSET_LOOP(COMPLEX0(result)[i] = COMPLEX_ELT(x, ii),
			      COMPLEX0(result)[i] = NA_CPLX);
	}
	break;
    case STRSXP:
	ARRAY_SUBSET_LOOP(SET_STRING_ELT(result, i, STRING_ELT(x, ii)),
			  SET_STRING_ELT(result, i, NA_STRING));
	break;
    case VECSXP:
	ARRAY_SUBSET_LOOP(SET_VECTOR_ELT(result, i,
					 VECTOR_ELT_FIX_NAMED(x, ii)),
			  SET_VECTOR_ELT(result, i, R_NilValue));
	break;
    case EXPRSXP:
	ARRAY_SUBSET_LOOP(SET_XVECTOR_ELT(result, i,
					 VECTOR_ELT_FIX_NAMED(x, ii)),
			  SET_XVECTOR_ELT(result, i, R_NilValue));
	break;
    case RAWSXP:
	ARRAY_SUBSET_LOOP(RAW0(result)[i] = RAW_ELT(x, ii),
			  RAW0(result)[i] = (Rbyte) 0);
	break;
    default:
	errorcall(call, "%s", _("array subscripting not handled for this type"));
	break;
    }

    SEXP new_dim = PROTECT(allocVector(INTSXP, k));
    for (int i = 0 ; i < k ; i++)
	INTEGER0(new_dim)[i] = bound[i];
    if(!isNull(getAttrib(xdims, R_NamesSymbol)))
	setAttrib(new_dim, R_NamesSymbol, getAttrib(xdims, R_NamesSymbol));
    setAttrib(result, R_DimSymbol, new_dim);
    UNPROTECT(1); // new_dim

    /* The array elements have been transferred. */
    /* Now we need to transfer the attributes. */
    /* Most importantly, we need to subset the */
    /* dimnames of the returned value. */

    dimnames = getAttrib(x, R_DimNamesSymbol);
    PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
    if (dimnames != R_NilValue) {
	int j = 0;
	PROTECT(xdims = allocVector(VECSXP, k));
	if (TYPEOF(dimnames) == VECSXP) {
	    r = s;
	    for (int i = 0; i < k ; i++) {
		if (bound[i] > 0) {
		  SET_VECTOR_ELT(xdims, j++,
			ExtractSubset(VECTOR_ELT(dimnames, i), CAR(r), call));
		} else { /* 0-length dims have NULL dimnames */
		    SET_VECTOR_ELT(xdims, j++, R_NilValue);
		}
		r = CDR(r);
	    }
	}
	else {
	    p = dimnames;
	    q = xdims;
	    r = s;
	    for (int i = 0 ; i < k; i++) {
		SETCAR(q, ExtractSubset(CAR(p), CAR(r), call));
		p = CDR(p);
		q = CDR(q);
		r = CDR(r);
	    }
	}
	setAttrib(xdims, R_NamesSymbol, dimnamesnames);
	setAttrib(result, R_DimNamesSymbol, xdims);
	UNPROTECT(1); /* xdims */
    }
    /* This was removed for matrices in 1998
       copyMostAttrib(x, result); */
    if (drop)
	DropDims(result);
    UNPROTECT(2); /* dimnamesnames, result */
    return result;
}


/* Returns and removes a named argument from argument list args.
   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NilValue is returned.
 */
static SEXP ExtractArg(SEXP args, SEXP arg_sym)
{
    SEXP arg, prev_arg;
    bool found = false;

    for (arg = prev_arg = args; arg != R_NilValue; arg = CDR(arg)) {
	if (TAG(arg) == arg_sym) {
	    if (arg == prev_arg) /* found at head of args */
		args = CDR(args);
	    else
		SETCDR(prev_arg, CDR(arg));
	    found = true;
	    break;
	}
	else  prev_arg = arg;
    }
    return found ? CAR(arg) : R_NilValue;
}

/* Extracts the drop argument, if present, from the argument list.
   The object being subsetted must be the first argument. */
static void ExtractDropArg(SEXP el, bool *drop)
{
    *drop = asLogical(ExtractArg(el, R_DropSymbol));
    if (*drop == NA_LOGICAL) *drop = 1;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used
 */
static int ExtractExactArg(SEXP args)
{
    SEXP argval = ExtractArg(args, R_ExactSymbol);
    if (isNull(argval)) return 1; /* Default is true as from R 2.7.0 */
    int exact = asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}

/* Version of DispatchOrEval for "[" and friends that speeds up simple cases.
   Also defined in subassign.c */
static R_INLINE std::pair<bool, RObject *> R_DispatchOrEvalSP(SEXP call, SEXP op, const char *generic, SEXP args_,
    SEXP rho)
{
    SEXP ans = R_NilValue;
    GCStackRoot<> args(args_);
    SEXP prom = NULL;
    if (args != R_NilValue && CAR(args) != R_DotsSymbol) {
        GCStackRoot<> x;
        x = eval(CAR(args), rho);
        INCREMENT_LINKS(x);
        if (!OBJECT(x)) {
            ans = CONS_NR(x, evalListKeepMissing(CDR(args), rho));
            DECREMENT_LINKS(x);
            return std::pair<bool, RObject *>(false, ans);
        }
        prom = R_mkEVPROMISE_NR(CAR(args), x);
        args = CONS(prom, CDR(args));
    }
    auto dispatched = DispatchOrEval(call, op, generic, args, rho, false, false);
    if (prom) DECREMENT_LINKS(PRVALUE(prom));
    return dispatched;
}

static R_INLINE bool R_DispatchOrEvalSP(SEXP call, SEXP op, const char *generic, SEXP args,
		    SEXP rho, SEXP *ans)
{
    std::pair<bool, RObject *> result = R_DispatchOrEvalSP(call, op, generic, args, rho);

    *ans = result.second;
    return result.first;
}

/* The "[" subset operator.
 * This provides the most general form of subsetting. */

attribute_hidden SEXP do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is an */
    /* appropriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall through */
    /* to the generic code below.  Note that evaluation */
    /* retains any missing argument indicators. */

    /* DispatchOrEval internal generic: [ */
    if (R_DispatchOrEvalSP(call, op, "[", args, rho, &ans)) {
/*     if(DispatchAnyOrEval(call, op, "[", args, rho, &ans, 0, 0)) */
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return ans;
    }

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    return do_subset_dflt(call, op, ans, rho);
}

static R_INLINE R_xlen_t scalarIndex(SEXP s)
{
    if (ATTRIB(s) == R_NilValue) {
	if (IS_SCALAR(s, INTSXP)) {
	    int ival = SCALAR_IVAL(s);
	    if (ival != NA_INTEGER)
		return ival;
	    else return -1;
	}
	else if (IS_SCALAR(s, REALSXP)) {
	    double rval = SCALAR_DVAL(s);
	    // treat infinite indices as NA, like asInteger
	    if (R_FINITE(rval))
		return (R_xlen_t) rval;
	    else return -1;
	}
	else return -1;
    }
    else return -1;
}

// called from (R `[` => ) do_subset, but also from R .subset() :
attribute_hidden SEXP do_subset_dflt(SEXP call, SEXP op, SEXP args_, SEXP rho)
{
    /* By default we drop extents of length 1 */

    /* Handle cases of extracting a single element from a simple vector
       or matrix directly to improve speed for these simple cases. */
    GCStackRoot<> args(args_);
    SEXP x = CAR(args);
    SEXP cdrArgs = CDR(args);
    SEXP cddrArgs = CDR(cdrArgs);
    if (cdrArgs != R_NilValue && cddrArgs == R_NilValue &&
	TAG(cdrArgs) == R_NilValue) {
	/* one index, not named */
	if (ATTRIB(x) == R_NilValue) {
	    SEXP s = CAR(cdrArgs);
	    R_xlen_t i = scalarIndex(s);
	    switch (TYPEOF(x)) {
	    case REALSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarReal( REAL_ELT(x, i-1) );
		break;
	    case INTSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarInteger( INTEGER_ELT(x, i-1) );
		break;
	    case LGLSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarLogical( LOGICAL_ELT(x, i-1) );
		break;
//	    do the more rare cases as well, since we've already prepared everything:
	    case CPLXSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarComplex( COMPLEX_ELT(x, i-1) );
		break;
	    case RAWSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarRaw( RAW_ELT(x, i-1) );
		break;
	    default: break;
	    }
	}
    }
    else if (cddrArgs != R_NilValue && CDR(cddrArgs) == R_NilValue &&
	     TAG(cdrArgs) == R_NilValue && TAG(cddrArgs) == R_NilValue) {
	/* two indices, not named */
	SEXP attr = ATTRIB(x);
	if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
	    /* only attribute of x is 'dim' */
	    SEXP dim = CAR(attr);
	    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {
		/* x is a matrix */
		SEXP si = CAR(cdrArgs);
		SEXP sj = CAR(cddrArgs);
		R_xlen_t i = scalarIndex(si);
		R_xlen_t j = scalarIndex(sj);
		int nrow = INTEGER_ELT(dim, 0);
		int ncol = INTEGER_ELT(dim, 1);
		if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		    /* indices are legal scalars */
		    R_xlen_t k = i - 1 + nrow * (j - 1);
		    switch (TYPEOF(x)) {
		    case REALSXP:
			if (k < XLENGTH(x))
			    return ScalarReal( REAL_ELT(x, k) );
			break;
		    case INTSXP:
			if (k < XLENGTH(x))
			    return ScalarInteger( INTEGER_ELT(x, k) );
			break;
		    case LGLSXP:
			if (k < XLENGTH(x))
			    return ScalarLogical( LOGICAL_ELT(x, k) );
			break;
		    case CPLXSXP:
			if (k < XLENGTH(x))
			    return ScalarComplex( COMPLEX_ELT(x, k) );
			break;
		    case RAWSXP:
			if (k < XLENGTH(x))
			    return ScalarRaw( RAW_ELT(x, k) );
			break;
		    default: break;
		    }
		}
	    }
	}
    }

    bool drop = 1;
    ExtractDropArg(args, &drop);

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */
    /* FIXME: replace the test by isNull ... ? */

    if (x == R_NilValue) {
	return x;
    }

    SEXP subs = CDR(args);
    int nsubs = length(subs); /* Will be short */
    SEXPTYPE type = TYPEOF(x);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    GCStackRoot<> ax(x);
    if (isVector(x))
    {
    }
    else if (type == LISTSXP || type == LANGSXP) { // *not* <DOTSXP>[]  :
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	if (ndim > 1) {
	    ax = allocArray(VECSXP, dim);
	    setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    ax = allocVector(VECSXP, length(x));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
	int i = 0;
	for (SEXP px = x; px != R_NilValue; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else errorcallNotSubsettable(x, call);

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    GCStackRoot<> ans;
    if (nsubs < 2) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	ans = VectorSubset(ax, (nsubs == 1 ? CAR(subs) : R_MissingArg),
				   call);
	/* one-dimensional arrays went through here, and they should
	   have their dimensions dropped only if the result has
	   length one and drop == TRUE
	*/
	if (ndim == 1) {
	    int len = length(ans);
	    if(!drop || len > 1) {
		// must grab these before the dim is set.
		GCStackRoot<> nm, attr;
		nm = getAttrib(ans, R_NamesSymbol);
		attr = ScalarInteger(length(ans));
		if(!isNull(getAttrib(dim, R_NamesSymbol)))
		    setAttrib(attr, R_NamesSymbol, getAttrib(dim, R_NamesSymbol));
		setAttrib(ans, R_DimSymbol, attr);
		SEXP attrib;
		if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
		    /* reinstate dimnames, include names of dimnames */
		    GCStackRoot<> nattrib;
		    nattrib = duplicate(attrib);
		    SET_VECTOR_ELT(nattrib, 0, nm);
		    setAttrib(ans, R_DimNamesSymbol, nattrib);
		    setAttrib(ans, R_NamesSymbol, R_NilValue);
		}
	    }
	}
    } else {
	if (nsubs != length(getAttrib(x, R_DimSymbol)))
	    errorcall(call, "%s", _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, subs, call, drop);
	else
	    ans = ArraySubset(ax, subs, call, drop);
    }

    /* Note: we do not coerce back to pair-based lists. */
    /* They are "defunct" in this version of R. */

    if (type == LANGSXP) {
	ax = ans;
	ans = allocLang(LENGTH(ax));
	if (LENGTH(ax) > 0) {
	    int i = 0;
	    for (SEXP px = ans; px != R_NilValue; px = CDR(px))
		SETCAR(px, VECTOR_ELT(ax, i++));
	    setAttrib(ans, R_DimSymbol,      getAttrib(ax, R_DimSymbol));
	    setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
	    setAttrib(ans, R_NamesSymbol,    getAttrib(ax, R_NamesSymbol));
	    RAISE_NAMED(ans, NAMED(ax)); /* PR#7924 */
	}
    }
    else {
    }
    if (ATTRIB(ans) != R_NilValue) { /* remove probably erroneous attr's */
	setAttrib(ans, R_TspSymbol, R_NilValue);
#ifdef _S4_subsettable
	if (!IS_S4_OBJECT(x))
#endif
	    setAttrib(ans, R_ClassSymbol, R_NilValue);
    }

    return ans;
}


/* The [[ subset operator.  It needs to be fast. */
/* The arguments to this call are evaluated on entry. */

attribute_hidden SEXP do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is */
    /* an appropriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    /* DispatchOrEval internal generic: [[ */
    if (R_DispatchOrEvalSP(call, op, "[[", args, rho, &ans)) {
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return ans;
    }

    /* Method dispatch has failed. */
    /* We now run the generic internal code. */

    return do_subset2_dflt(call, op, ans, rho);
}

attribute_hidden SEXP do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, dimnames, indx, subs, x;
    int ndims, nsubs;
    bool drop = 1;
    R_xlen_t offset = 0;

    PROTECT(args);
    ExtractDropArg(args, &drop);
    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    int exact = ExtractExactArg(args);
    int pok = (exact == -1) ? exact : ! exact;

    x = CAR(args);

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    subs = CDR(args);
    if(0 == (nsubs = length(subs)))
	errorcall(call, "%s", _("no index specified"));

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */
    if (x == R_NilValue) {
	UNPROTECT(1); /* args */
	if(CAR(subs) == R_MissingArg)
	    errorcallMissingSubs(x, call);
	// else
	return x;
    }

    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    if(nsubs > 1 && nsubs != ndims)
	errorcall(call, "%s", _("incorrect number of subscripts"));

    /* code to allow classes to extend environment */
    if(TYPEOF(x) == OBJSXP) {
	SEXP xs = x;
	x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	    errorcall(call, _("this %s class is not subsettable"),
		      IS_S4_OBJECT(xs) ? "S4" : "object");
    }
    PROTECT(x);

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
	if( nsubs != 1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	    errorcall(call, "%s", _("wrong arguments for subsetting an environment"));
	ans = R_findVarInFrame(x, installTrChar(STRING_ELT(CAR(subs), 0)));
	if( TYPEOF(ans) == PROMSXP ) {
	    PROTECT(ans);
	    ans = eval(ans, R_GlobalEnv);
	    UNPROTECT(1); /* ans */
	} else ENSURE_NAMEDMAX(ans);

	UNPROTECT(2); /* args, x */
	if(ans == R_UnboundValue)
	    return R_NilValue;
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return ans;
    }

    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	errorcallNotSubsettable(x, call);

#ifndef SWITCH_TO_REFCNT
    int named_x;
    named_x = NAMED(x);  /* x may change below; save this now.  See PR#13411 */
#endif

    if (nsubs == 1) { /* vector indexing */
	SEXP thesub = CAR(subs);
	int len = length(thesub);

	if (len > 1) {
#ifdef SWITCH_TO_REFCNT
	    if (IS_GETTER_CALL(call)) {
		/* This is (most likely) a getter call in a complex
		   assighment so we duplicate as needed. The original
		   x should have been duplicated if it might be
		   shared, but might get additional references before
		   it arrives here. */
		if (MAYBE_SHARED(x)) {
		    x = shallow_duplicate(x);
		    UNPROTECT(1); /* old x */
		    PROTECT(x);
		}
		x = vectorIndex(x, thesub, 0, len-1, pok, call, TRUE);
	    }
	    else
		x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
#else
	    x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
	    named_x = NAMED(x);
#endif
	    UNPROTECT(1); /* x */
	    PROTECT(x);
	}

	SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol));
	offset = get1index(thesub, xnames,
			   xlength(x), pok, len > 1 ? len-1 : -1, call);
	UNPROTECT(1); /* xnames */
	if (offset < 0 || offset >= xlength(x)) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(2); /* args, x */
		return R_NilValue;
	    }
	    else errorcallOutOfBoundsSEXP(x, -1, thesub, call);
	}
    } else { /* nsubs == ndims >= 2 : matrix|array indexing */
	/* Here we use the fact that: */
	/* CAR(R_NilValue) = R_NilValue */
	/* CDR(R_NilValue) = R_NilValue */

	int ndn; /* Number of dimnames. Unlikely to be anything but
		    0 or nsubs, but just in case... */

	PROTECT(indx = allocVector(INTSXP, nsubs));
	int *pindx = INTEGER(indx);
	const int *pdims = INTEGER_RO(dims);
	dimnames = getAttrib(x, R_DimNamesSymbol);
	ndn = length(dimnames);
	for (int i = 0; i < nsubs; i++) {
	    SEXP thesub = CAR(subs);
	    pindx[i] = (int)
		get1index(thesub,
			  (i < ndn) ? VECTOR_ELT(dimnames, i) : R_NilValue,
			  pindx[i], pok, -1, call);
	    subs = CDR(subs);
	    if (pindx[i] < 0 || pindx[i] >= pdims[i])
		errorcallOutOfBoundsSEXP(x, i, thesub, call);
	}
	offset = 0;
	for (int i = (nsubs - 1); i > 0; i--)
	    offset = (offset + pindx[i]) * pdims[i - 1];
	offset += pindx[0];
	UNPROTECT(1); /* indx */
    }

    if(isPairList(x)) {
#ifdef LONG_VECTOR_SUPPORT
	if (offset > R_SHORT_LEN_MAX)
	    error("%s", _("invalid subscript for pairlist"));
#endif
	ans = CAR(nthcdr(x, (int) offset));
#ifndef SWITCH_TO_REFCNT
	RAISE_NAMED(ans, named_x);
#endif
    } else if(isVectorList(x)) {
	/* did unconditional duplication before 2.4.0 */
	if (TYPEOF(x) == EXPRSXP) {
	    ans = XVECTOR_ELT(x, offset);
	} else
	    ans = VECTOR_ELT(x, offset);
#ifndef SWITCH_TO_REFCNT
	RAISE_NAMED(ans, named_x);
#endif
    } else {
	ans = PROTECT(allocVector(TYPEOF(x), 1));
	switch (TYPEOF(x)) {
	case LGLSXP:
	    LOGICAL0(ans)[0] = LOGICAL_ELT(x, offset);
	    break;
	case INTSXP:
	    INTEGER0(ans)[0] = INTEGER_ELT(x, offset);
	    break;
	case REALSXP:
	    REAL0(ans)[0] = REAL_ELT(x, offset);
	    break;
	case CPLXSXP:
	    COMPLEX0(ans)[0] = COMPLEX_ELT(x, offset);
	    break;
	case STRSXP:
	    SET_STRING_ELT(ans, 0, STRING_ELT(x, offset));
	    break;
	case RAWSXP:
	    RAW0(ans)[0] = RAW_ELT(x, offset);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("do_subset2", x);
	}
	UNPROTECT(1); /* ans */
    }
    UNPROTECT(2); /* args, x */
    return ans;
}

attribute_hidden SEXP R::dispatch_subset2(SEXP x, R_xlen_t i, SEXP call, SEXP rho)
{
    static SEXP bracket_op = NULL;
    GCStackRoot<> args;
    SEXP x_elt;
    if (isObject(x)) {
        if (bracket_op == NULL)
            bracket_op = R_Primitive("[[");
        args = list2(x, ScalarReal((double)i + 1));
        x_elt = do_subset2(call, bracket_op, args, rho);
    } else {
      // FIXME: throw error if not a list
        x_elt = VECTOR_ELT(x, i);
    }
    return x_elt;
}

enum pmatch {
    NO_MATCH,
    EXACT_MATCH,
    PARTIAL_MATCH
};

/* A helper to partially match tags against a candidate.
   Tags are always in the native charset.
 */
/* Returns: */
static enum pmatch pstrmatch(SEXP target, SEXP input, size_t slen)
{
    const char *st = "";
    const char *si = "";
    CXXR::RAllocStack::Scope rscope;

    if (target == R_NilValue)
	return NO_MATCH;

    switch (TYPEOF(target)) {
    case SYMSXP:
	st = CHAR(PRINTNAME(target));
	break;
    case CHARSXP:
	st = translateChar(target);
	break;
    default: // -Wswitch
	break;
    }
    si = translateChar(input);
    if (si[0] != '\0' && streqln(st, si, slen)) {
	return (strlen(st) == slen) ? EXACT_MATCH : PARTIAL_MATCH;
    } else {
	return NO_MATCH;
    }
}

attribute_hidden SEXP R::fixSubset3Args(SEXP call, SEXP args, SEXP env, SEXP* syminp)
{
    GCStackRoot<> input;
    SEXP nlist;

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */

    input = allocVector(STRSXP, 1);
    nlist = CADR(args);
    if (TYPEOF(nlist) == PROMSXP)
	nlist = eval(nlist, env);
    if (isSymbol(nlist)) {
	if (syminp != NULL)
	    *syminp = nlist;
	SET_STRING_ELT(input, 0, PRINTNAME(nlist));
    } else if (isString(nlist) ) {
	if (LENGTH(nlist) != 1)
	    error("%s", _("invalid subscript length"));
	SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
    }
    else {
	errorcall(call,_("invalid subscript type '%s'"),
		  R_typeToChar(nlist));
	return R_NilValue; /*-Wall*/
    }

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when nlist was */
    /* ..., as in PR#8718 */

    args = shallow_duplicate(args);
    SETCADR(args, input);

    return args;
}

/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
attribute_hidden SEXP do_subset3(SEXP call, SEXP op, SEXP args_, SEXP env)
{
    SEXP ans;
    GCStackRoot<> args(args_);

    checkArity(op, args);
    args = fixSubset3Args(call, args, env, NULL);

    /* If the first argument is an object and there is */
    /* an appropriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    /* DispatchOrEval internal generic: $ */
    if (R_DispatchOrEvalSP(call, op, "$", args, env, &ans)) {
	if (NAMED(ans))
	    ENSURE_NAMEDMAX(ans);
	return ans;
    }
    GCStackRoot<> ansrt(ans);
    ansrt = R_subset3_dflt(CAR(ansrt), STRING_ELT(CADR(args), 0), call);
    return ansrt;
}

/* also used in eval.c */
attribute_hidden SEXP R::R_subset3_dflt(SEXP x_, SEXP input_, SEXP call)
{
    GCStackRoot<> y;
    GCStackRoot<> input(input_);
    GCStackRoot<> x(x_);

    /* Optimisation to prevent repeated recalculation */
    size_t slen = strlen(translateChar(input));
    /* The mechanism to allow a class extending "environment" */
    if (IS_S4_OBJECT(x) && TYPEOF(x) == OBJSXP) {
	x = R_getS4DataSlot(x, ANYSXP);
	if (x == R_NilValue)
	    errorcall(call, "%s", _("$ operator not defined for this S4 class"));
    }

    // isPairList(x)  but *not* <DOTSXP> :
    if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP || TYPEOF(x) == NILSXP) {
	SEXP xmatch = R_NilValue;
	int havematch;
	havematch = 0;
	for (SEXP y = x ; y != R_NilValue ; y = CDR(y)) {
	    switch (pstrmatch(TAG(y), input, slen)) {
	    case EXACT_MATCH:
		y = CAR(y);
		RAISE_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		xmatch = y;
#ifdef SWITCH_TO_REFCNT
		if (IS_GETTER_CALL(call))
		    MARK_NOT_MUTABLE(y);
#endif
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if (havematch == 1) { /* unique partial match */
	    if (R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = TAG(xmatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = translateChar(target);
		    break;
		default:
		    break;
		}
		warningcall(call, _("partial match of '%s' to '%s'"),
			    translateChar(input), st);
	    }
	    y = CAR(xmatch);
	    RAISE_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (isVectorList(x)) {
	R_xlen_t imatch = -1;
	SEXP nlist = getAttrib(x, R_NamesSymbol);

	R_xlen_t n = xlength(nlist);
	int havematch = 0;
	for (R_xlen_t i = 0 ; i < n ; i = i + 1) {
	    switch (pstrmatch(STRING_ELT(nlist, i), input, slen)) {
	    case EXACT_MATCH:
		y = VECTOR_ELT(x, i);
		RAISE_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		if (havematch == 1) {
		    /* partial matches can cause aliasing in eval.c:evalseq
		       This is overkill, but alternative ways to prevent
		       the aliasing appear to be even worse */
		    y = VECTOR_ELT(x,i);
#ifdef SWITCH_TO_REFCNT
		    if (IS_GETTER_CALL(call))
			MARK_NOT_MUTABLE(y);
#else
		    ENSURE_NAMEDMAX(y);
#endif
		    SET_VECTOR_ELT(x,i,y);
		}
		imatch = i;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if (havematch == 1) { /* unique partial match */
	    if (R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = STRING_ELT(nlist, imatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = translateChar(target);
		    break;
		default:
		    break;
		}
		warningcall(call, _("partial match of '%s' to '%s'"),
			    translateChar(input), st);
	    }
	    y = VECTOR_ELT(x, imatch);
	    RAISE_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (isEnvironment(x)) {
	y = R_findVarInFrame(x, installTrChar(input));
	if (TYPEOF(y) == PROMSXP) {
	    y = eval(y, R_GlobalEnv);
	}
	if (y != R_UnboundValue) {
	    if (NAMED(y))
		ENSURE_NAMEDMAX(y);
	    else RAISE_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (isVectorAtomic(x)) {
	errorcall(call, "%s", _("$ operator is invalid for atomic vectors"));
    }
    else /* e.g. a function */
	errorcallNotSubsettable(x, call);

    return R_NilValue;
}
