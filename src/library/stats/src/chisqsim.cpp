/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2020  The R Core Team.
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

#include <cmath>
#include <CXXR/ProtectStack.hpp>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Random.h>
#include "stats.h" // for rcont2

/* Driver routine to call RCONT2 from R, B times.
   Calculates the Pearson chi-squared for each generated table.

   Mostly here for historical reasons now that we have r2dtable().
*/

static void chisqsim(int nrow, int ncol, const int nrowt[], const int ncolt[], int n,
	 int B, const double expected[],
	 // modified :
	 int *observed, double *fact, int *jwork, double *results)
{
    /* Calculate log-factorials.  fact[i] = lgamma(i+1) */
    fact[0] = fact[1] = 0.;
    for (int i = 2; i <= n; i++)
	fact[i] = fact[i - 1] + log(i);

    GetRNGstate();

    for (int iter = 0; iter < B; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate chi-squared value from the random table. */
	double chisq = 0.;
	for (int j = 0; j < ncol; ++j) {
	    for (int i = 0, ii = j * nrow; i < nrow;  i++, ii++) {
		double
		    e = expected[ii],
		    o = observed[ii];
		chisq += (o - e) * (o - e) / e;
	    }
	}
	results[iter] = chisq;
    }

    PutRNGstate();

    return;
}

/* Driver routine to call RCONT2 from R, B times.
   Calculates the log probability for each generated table.

   Mostly here for historical reasons now that we have r2dtable().
*/

static void fisher_sim(int nrow, int ncol, const int nrowt[], const int ncolt[], int n,
	   int B,
	   // modified :
	   int *observed, double *fact,
	   int *jwork, double *results)
{
    /* Calculate log-factorials.  fact[i] = lgamma(i+1) */
    fact[0] = fact[1] = 0.;
    for (int i = 2; i <= n; i++)
	fact[i] = fact[i - 1] + log(i);

    GetRNGstate();

    for (int iter = 0; iter < B; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate log-prob value from the random table. */
	double ans = 0.;
	for (int j = 0; j < ncol; ++j) {
	    for (int i = 0, ii = j * nrow; i < nrow;  i++, ii++)
		ans -= fact[observed[ii]];
	}
	results[iter] = ans;
    }

    PutRNGstate();

    return;
}

SEXP Fisher_sim(SEXP sr, SEXP sc, SEXP sB)
{
    sr = PROTECT(coerceVector(sr, INTSXP));
    sc = PROTECT(coerceVector(sc, INTSXP));
    int nr = LENGTH(sr), nc = LENGTH(sc), B = asInteger(sB);
    int n = 0, *isr = INTEGER(sr);
    for (int i = 0; i < nr; i++) n += isr[i];
    int *observed = (int *) R_alloc(nr * nc, sizeof(int));
    double *fact = (double *) R_alloc(n+1, sizeof(double));
    int *jwork = (int *) R_alloc(nc, sizeof(int));
    SEXP ans = PROTECT(allocVector(REALSXP, B));
    fisher_sim(nr, nc, isr, INTEGER(sc), n, B,
	       observed, fact,
	       jwork, REAL(ans));
    UNPROTECT(3);
    return ans;
}

SEXP chisq_sim(SEXP sr, SEXP sc, SEXP sB, SEXP E)
{
    sr = PROTECT(coerceVector(sr, INTSXP));
    sc = PROTECT(coerceVector(sc, INTSXP));
    E = PROTECT(coerceVector(E, REALSXP));
    int nr = LENGTH(sr), nc = LENGTH(sc), B = asInteger(sB);
    int n = 0, *isr = INTEGER(sr);
    for (int i = 0; i < nr; i++) n += isr[i];
    int *observed = (int *) R_alloc(nr * nc, sizeof(int));
    double *fact = (double *) R_alloc(n+1, sizeof(double));
    int *jwork = (int *) R_alloc(nc, sizeof(int));
    SEXP ans = PROTECT(allocVector(REALSXP, B));
    chisqsim(nr, nc, isr, INTEGER(sc), n, B, REAL(E), observed, fact,
	     jwork, REAL(ans));
    UNPROTECT(4);
    return ans;
}
