/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2024  The R Core Team
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
 */

/* ansari.c
   Compute the exact distribution of the Ansari-Bradley test statistic.
   */

#include <cmath>  // for floor
#include <cstring>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/RealVector.hpp>
#include <R.h>
#include <Rmath.h>		/* uses choose() */
#include <Rinternals.h>
#include "stats.h"
#include "localization.h"

using namespace CXXR;

// @note Caller has to manage the R_alloc stack
static double ***w_init(int m, int n)
{
    double ***w = (double ***) R_alloc(m + 1, sizeof(double **));
    if (m+1)
	memset(w, '\0', (m+1) * sizeof(double**));
    for (int i = 0; i <= m; i++) {
	w[i] = (double**) R_alloc(n + 1, sizeof(double *));
	if (n+1)
	    memset(w[i], '\0', (n+1) * sizeof(double*));
    }
    return(w);
}

// @note Caller has to manage the R_alloc stack
static double cansari(int k, int m, int n, double ***w)
{
    int l, u;

    l = (m + 1) * (m + 1) / 4;
    u = l + m * n / 2;

    if ((k < l) || (k > u))
	return(0);

    if (w[m][n] == 0) {
	w[m][n] = (double *) R_alloc(u + 1, sizeof(double));
	if (u+1)
	    memset(w[m][n], '\0', (u + 1) * sizeof(double));
	for (int i = 0; i <= u; i++)
	    w[m][n][i] = -1;
    }

    if (w[m][n][k] < 0) {
	if (m == 0)
	    w[m][n][k] = (k == 0);
	else if (n == 0)
	    w[m][n][k] = (k == l);
	else
	    w[m][n][k] = cansari(k, m, n - 1, w)
		+ cansari(k - (m + n + 1) / 2, m - 1, n, w);
    }

    return w[m][n][k];
}

// @note Caller has to manage the R_alloc stack
static void pansari(int len, double *Q, double *P, int m, int n)
{
    int l, u;
    double c, p, q;
    double ***w;

    w = w_init(m, n);
    l = (m + 1) * (m + 1) / 4;
    u = l + m * n / 2;
    c = choose(m + n, m);
    for (int i = 0; i < len; i++) {
	q = floor(Q[i] + 1e-7);
	if (q < l)
	    P[i] = 0;
	else if (q > u)
	    P[i] = 1;
	else {
	    p = 0;
	    for (int j = l; j <= q; j++) p += cansari(j, m, n, w);
	    P[i] = p / c;
	}
    }
}

// @note Caller has to manage the R_alloc stack
static void qansari(int len, double *P, double *Q, int m, int n)
{
    int l, u;
    double c, p, xi;
    double ***w;

    w = w_init(m, n);
    l = (m + 1) * (m + 1) / 4;
    u = l + m * n / 2;
    c = choose(m + n, m);
    for (int i = 0; i < len; i++) {
	xi = P[i];
	if(xi < 0 || xi > 1)
	    error("%s", _("probabilities outside [0,1] in qansari()"));
	if(xi == 0)
	    Q[i] = l;
	else if(xi == 1)
	    Q[i] = u;
	else {
	    p = 0.;
	    int q = 0;
	    for (;;) {
		p += cansari(q, m, n, w) / c;
		if (p >= xi) break;
		q++;
	    }
	    Q[i] = q;
	}
    }
}

SEXP pAnsari(SEXP q_, SEXP sm, SEXP sn)
{
    CXXR::RAllocStack::Scope rscope; // pansari
    GCStackRoot<> p, q(q_);
    int m = asInteger(sm), n = asInteger(sn);
    q = RealVector::coerce(q);
    int len = LENGTH(q);
    p = RealVector::create(len);
    pansari(len, REAL(q), REAL(p), m, n);

    return p;
}

SEXP qAnsari(SEXP p_, SEXP sm, SEXP sn)
{
    CXXR::RAllocStack::Scope rscope; // qansari
    GCStackRoot<> p(p_), q;
    int m = asInteger(sm), n = asInteger(sn);
    p = RealVector::coerce(p);
    int len = LENGTH(p);
    q = RealVector::create(len);
    qansari(len, REAL(p), REAL(q), m, n);

    return q;
}
