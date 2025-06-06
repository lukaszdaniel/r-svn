/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2017   The R Core Team.
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

/* **********************************************************************
 * === This was 'sort()' in  gamfit's  mysort.f  [or sortdi() in sortdi.f ] :
 * was at end of  modreg/src/ppr.f
 * Translated by f2c (version 20010821) and f2c-clean,v 1.9 2000/01/13 13:46:53
 * then manually by Martin Maechler
*/

/** @file qsort.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/ProtectStack.hpp>
#include <Localization.h>
#include <Defn.h> /* => Utils.h with the protos from here */
#include <Internal.h>
#include <Rmath.h>

#include <R_ext/RS.h>

using namespace R;

#ifdef LONG_VECTOR_SUPPORT
static void R_qsort_R(double *v, double *I, size_t i, size_t j);
static void R_qsort_int_R(int *v, double *I, size_t i, size_t j);
#endif

/* R function  qsort(x, index.return) */
attribute_hidden SEXP do_qsort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, sx;
    double *vx = NULL;
    int *ivx = NULL;

    checkArity(op, args);
    x = CAR(args);
    if (!isNumeric(x))
	error("%s", _("argument is not a numeric vector"));
    bool x_real= (TYPEOF(x) == REALSXP);
    bool x_int = (!x_real && (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP));
    PROTECT(sx = (x_real || x_int) ? duplicate(x) : coerceVector(x, REALSXP));
    SET_ATTRIB(sx, R_NilValue);
    SET_OBJECT(sx, 0);
    bool indx_ret = asLogical(CADR(args));
    R_xlen_t n = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    bool isLong = (n > INT_MAX);
#endif
    if (x_int) ivx = INTEGER(sx); else vx = REAL(sx);
    if (indx_ret) {
	SEXP ans, ansnames, indx;
	/* answer will have x = sorted x , ix = index :*/
	PROTECT(ans = allocVector(VECSXP, 2));
	PROTECT(ansnames = allocVector(STRSXP, 2));
#ifdef LONG_VECTOR_SUPPORT
	if (isLong) {
	    PROTECT(indx = allocVector(REALSXP, n));
	    double *ix = REAL(indx);
	    for (R_xlen_t i = 0; i < n; i++) ix[i] = (double) (i+1);
	    // do not need to sort 0-length array
	    if (n > 0) {
		if(x_int) R_qsort_int_R(ivx, ix, 1, n);
		else R_qsort_R(vx, ix, 1, n);
	    }
	} else
#endif
	{
	    PROTECT(indx = allocVector(INTSXP, n));
	    int *ix = INTEGER(indx);
	    int nn = (int) n;
	    for (int i = 0; i < nn; i++) ix[i] = i+1;
	    // do not need to sort 0-length array
	    if (nn > 0) {
		if (x_int) R_qsort_int_I(ivx, ix, 1, nn);
		else R_qsort_I(vx, ix, 1, nn);
	    }
	}

	SET_VECTOR_ELT(ans, 0, sx);
	SET_VECTOR_ELT(ans, 1, indx);
	SET_STRING_ELT(ansnames, 0, mkChar("x"));
	SET_STRING_ELT(ansnames, 1, mkChar("ix"));
	setAttrib(ans, R_NamesSymbol, ansnames);
	UNPROTECT(4);
	return ans;
    } else { // not indx_ret
	// do not need to sort 0-length array
	if (n > 0) {
	    if (x_int)
		R_qsort_int(ivx, 1, n);
	    else
		R_qsort(vx, 1, n);
	}

	UNPROTECT(1);
	return sx;
    }
}


/* These are exposed in Utils.h and are misguidedly in the API */
void F77_SUB(qsort4)(double *v, int *indx, int *ii, int *jj)
{
    R_qsort_I(v, indx, *ii, *jj);
}

void F77_SUB(qsort3)(double *v, int *ii, int *jj)
{
    R_qsort(v, *ii, *jj);
}

//  sort with index : --------------------------
#define qsort_Index
#define INTt int
#define INDt int

#define NUMERIC double
void R_qsort_I(double *v, int *I, int i, int j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int_I(int *v, int *I, int i, int j)
#include "qsort-body.cpp"
#undef NUMERIC

#undef INTt
#undef INDt

#ifdef LONG_VECTOR_SUPPORT
#define INDt double
#define NUMERIC double
static void R_qsort_R(double *v, double *I, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
static void R_qsort_int_R(int *v, double *I, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC
#undef INDt
#endif // LONG_VECTOR_SUPPORT

//  sort withOUT index : -----------------------
#undef qsort_Index

#define NUMERIC double
void R_qsort(double *v, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC

#define NUMERIC int
void R_qsort_int(int *v, size_t i, size_t j)
#include "qsort-body.cpp"
#undef NUMERIC
