/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2020  The R Core Team
 *  Copyright (C) 2004	      The R Foundation
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

#include "nmath.h"
#include "dpq.h"

double qnchisq(double p, double df, double ncp, int lower_tail, int log_p)
{
    constexpr double accu = 1e-13;
    constexpr double racc = 4*DBL_EPSILON;
    /* these two are for the "search" loops, can have less accuracy: */
    constexpr double Eps = 1e-11; /* must be > accu */
    constexpr double rEps= 1e-10; /* relative tolerance ... */

    double ux, lx, ux0, nx, pp;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(df) || ISNAN(ncp))
	return p + df + ncp;
#endif
    if (!R_FINITE(df)) ML_WARN_return_NAN;

    /* Was
     * df = floor(df + 0.5);
     * if (df < 1 || ncp < 0) ML_WARN_return_NAN;
     */
    if (df < 0 || ncp < 0) ML_WARN_return_NAN;

    R_Q_P01_boundaries(p, 0, ML_POSINF);

    pp = R_D_qIv(p); // exp(p) iff log_p
    if(pp > 1 - DBL_EPSILON)
	return lower_tail ? ML_POSINF : 0.0; // early under/over flow  iff log_p (FIXME)

    /* Invert pnchisq(.) :
     * 1. finding an upper and lower bound */
    {
       /* This is Pearson's (1959) approximation,
          which is usually good to 4 figs or so.  */
	double b, c, ff;
	b = (ncp*ncp)/(df + 3*ncp);
	c = (df + 3*ncp)/(df + 2*ncp);
	ff = (df + 2 * ncp)/(c*c);
	ux = b + c * qchisq(p, ff, lower_tail, log_p);
	if(ux <= 0.) ux = 1;
	ux0 = ux;
    }

    if(!lower_tail && ncp >= 80) {
	/* in this case, pnchisq() works via lower_tail = TRUE */
	if(pp < 1e-10) ML_WARNING(ME_PRECISION, "qnchisq");
	p = /* R_DT_qIv(p)*/ log_p ? -expm1(p) : (0.5 - (p) + 0.5);
	lower_tail = TRUE;
    } else {
	p = pp;
    }

    pp = fmin2(1 - DBL_EPSILON, p * (1 + Eps));
    if(lower_tail) {
        for(; ux < DBL_MAX &&
		pnchisq_raw(ux, df, ncp, Eps, rEps, 10000, TRUE, FALSE) < pp;
	    ux *= 2);
	pp = p * (1 - Eps);
        for(lx = fmin2(ux0, DBL_MAX);
	    lx > DBL_MIN &&
		pnchisq_raw(lx, df, ncp, Eps, rEps, 10000, TRUE, FALSE) > pp;
	    lx *= 0.5);
    }
    else {
        for(; ux < DBL_MAX &&
		pnchisq_raw(ux, df, ncp, Eps, rEps, 10000, FALSE, FALSE) > pp;
	    ux *= 2);
	pp = p * (1 - Eps);
        for(lx = fmin2(ux0, DBL_MAX);
	    lx > DBL_MIN &&
		pnchisq_raw(lx, df, ncp, Eps, rEps, 10000, FALSE, FALSE) < pp;
	    lx *= 0.5);
    }

    /* 2. interval (lx,ux)  halving : */
    if(lower_tail) {
	do {
	    nx = 0.5 * (lx + ux);
	    if (pnchisq_raw(nx, df, ncp, accu, racc, 100000, TRUE, FALSE) > p)
		ux = nx;
	    else
		lx = nx;
	}
	while ((ux - lx) / nx > accu);
    } else {
	do {
	    nx = 0.5 * (lx + ux);
	    if (pnchisq_raw(nx, df, ncp, accu, racc, 100000, FALSE, FALSE) < p)
		ux = nx;
	    else
		lx = nx;
	}
	while ((ux - lx) / nx > accu);
    }
    return 0.5 * (ux + lx);
}
