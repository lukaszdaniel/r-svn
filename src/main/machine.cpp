/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2022 The R Core Team
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

/** @file machine.cpp
 *
 * @note Formerly part of platform.cpp
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <cfloat> // -> FLT_RADIX

#ifdef  USE_INTERNAL_MKTIME
// for R_time_t
# include "datetime.h"
#else
// for time_t
# include <ctime>
#endif

using namespace R;

/* Machine Constants */

namespace
{
    template <typename DTYPE = double>
    void machar_generic(int *ibeta, int *it, int *irnd, int *ngrd, int *machep, int *negep,
        int *iexp, int *minexp, int *maxexp,
        DTYPE *eps, DTYPE *epsneg, DTYPE *xmin, DTYPE *xmax)
    {
        volatile DTYPE a, b, beta, betain, betah, one,
            t, temp, tempa, temp1, two, y, z, zero;
        int i, iz, j, k, mx, nxres;

        one = 1;
        two = one + one;
        zero = one - one;

        /* determine ibeta, beta ala malcolm. */
        a = one; // a = <large> = 9.0072e+15 for 'double' is used later
        do
        {
            a = a + a;
            temp = a + one;
            temp1 = temp - a;
        } while (temp1 - one == zero);
#ifdef _no_longer___did_overflow_ // on IBM PowerPPC ('Power 8')
        int itemp;
        b = one;
        do
        {
            b = b + b;
            temp = a + b;
            itemp = (int)(temp - a);
        } while (itemp == 0);
        *ibeta = itemp;
#else
        *ibeta = (int)FLT_RADIX;
#endif
        beta = *ibeta;

        /* determine it, irnd */

        *it = 0;
        b = one;
        do
        {
            *it = *it + 1;
            b = b * beta;
            temp = b + one;
            temp1 = temp - b;
        } while (temp1 - one == zero);
        *irnd = 0;
        betah = beta / two;
        temp = a + betah;
        if (temp - a != zero)
            *irnd = 1;
        tempa = a + beta;
        temp = tempa + betah;
        if (*irnd == 0 && temp - tempa != zero)
            *irnd = 2;

        /* determine negep, epsneg */

        *negep = *it + 3;
        betain = one / beta;
        a = one;
        for (i = 1; i <= *negep; i++)
            a = a * betain;
        b = a;
        for (;;)
        {
            temp = one - a;
            if (temp - one != zero)
                break;
            a = a * beta;
            *negep = *negep - 1;
        }
        *negep = -*negep;
        *epsneg = a;
        if (*ibeta != 2 && *irnd != 0)
        {
            a = (a * (one + a)) / two;
            temp = one - a;
            if (temp - one != zero)
                *epsneg = a;
        }

        /* determine machep, eps */

        *machep = -*it - 3;
        a = b;
        for (;;)
        {
            temp = one + a;
            if (temp - one != zero)
                break;
            a = a * beta;
            *machep = *machep + 1;
        }
        *eps = a;
        temp = tempa + beta * (one + *eps);
        if (*ibeta != 2 && *irnd != 0)
        {
            a = (a * (one + a)) / two;
            temp = one + a;
            if (temp - one != zero)
                *eps = a;
        }

        /* determine ngrd */

        *ngrd = 0;
        temp = one + *eps;
        if (*irnd == 0 && temp * one - one != zero)
            *ngrd = 1;

        /* determine iexp, minexp, xmin */

        /* loop to determine largest i and k = 2**i such that */
        /*        (1/beta) ** (2**(i)) */
        /* does not underflow. */
        /* exit from loop is signaled by an underflow. */

        i = 0;
        k = 1;
        z = betain;
        t = one + *eps;
        nxres = 0;
        for (;;)
        {
            y = z;
            z = y * y;

            /* check for underflow here */

            a = z * one;
            temp = z * t;
            if (a + a == zero || std::abs(z) >= y)
                break;
            temp1 = temp * betain;
            if (temp1 * beta == z)
                break;
            i = i + 1;
            k = k + k;
        }
        if (*ibeta != 10)
        {
            *iexp = i + 1;
            mx = k + k;
        }
        else
        {
            /* this segment is for decimal machines only */

            *iexp = 2;
            iz = *ibeta;
            while (k >= iz)
            {
                iz = iz * *ibeta;
                iexp = iexp + 1;
            }
            mx = iz + iz - 1;
        }
        do
        {
            /* loop to determine minexp, xmin */
            /* exit from loop is signaled by an underflow */

            *xmin = y;
            y = y * betain;

            /* check for underflow here */

            a = y * one;
            temp = y * t;
            if (a + a == zero || std::abs(y) >= *xmin)
                goto L10;
            k = k + 1;
            temp1 = temp * betain;
        } while (temp1 * beta != y);
        nxres = 3;
        *xmin = y;
    L10:
        *minexp = -k;

        /* determine maxexp, xmax */

        if (mx <= k + k - 3 && *ibeta != 10)
        {
            mx = mx + mx;
            *iexp = *iexp + 1;
        }
        *maxexp = mx + *minexp;

        /* adjust irnd to reflect partial underflow */

        *irnd = *irnd + nxres;

        /* adjust for ieee-style machines */

        if (*irnd == 2 || *irnd == 5)
            *maxexp = *maxexp - 2;

        /* adjust for non-ieee machines with partial underflow */

        if (*irnd == 3 || *irnd == 4)
            *maxexp = *maxexp - *it;

        /* adjust for machines with implicit leading bit in binary */
        /* significand, and machines with radix point at extreme */
        /* right of significand. */

        i = *maxexp + *minexp;
        if (*ibeta == 2 && i == 0)
            *maxexp = *maxexp - 1;
        if (i > 20)
            *maxexp = *maxexp - 1;
        if (a != y)
            *maxexp = *maxexp - 2;
        *xmax = one - *epsneg;
        if (*xmax * one != *xmax)
            *xmax = one - beta * *epsneg;
        *xmax = *xmax / (beta * beta * beta * *xmin);
        i = *maxexp + *minexp + 3;
        if (i > 0)
            for (j = 1; j <= i; j++)
            {
                if (*ibeta == 2)
                    *xmax = *xmax + *xmax;
                if (*ibeta != 2)
                    *xmax = *xmax * beta;
            }
    }
} // anonymous namespace

attribute_hidden void Init_R_Machine(SEXP rho)
{
    machar_generic(&R_AccuracyInfo.ibeta,
	   &R_AccuracyInfo.it,
	   &R_AccuracyInfo.irnd,
	   &R_AccuracyInfo.ngrd,
	   &R_AccuracyInfo.machep,
	   &R_AccuracyInfo.negep,
	   &R_AccuracyInfo.iexp,
	   &R_AccuracyInfo.minexp,
	   &R_AccuracyInfo.maxexp,
	   &R_AccuracyInfo.eps,
	   &R_AccuracyInfo.epsneg,
	   &R_AccuracyInfo.xmin,
	   &R_AccuracyInfo.xmax);

    R_dec_min_exponent = (int) floor(log10(R_AccuracyInfo.xmin)); /* smallest decimal exponent */

    /*
#ifdef HAVE_LONG_DOUBLE
# define MACH_SIZE 18+10
#else
# define MACH_SIZE 18
#endif
    */
    int MACH_SIZE = 19;
    if (sizeof(LDOUBLE) > sizeof(double)) MACH_SIZE += 10;

    SEXP ans = PROTECT(allocVector(VECSXP, MACH_SIZE)),
	 nms = PROTECT(allocVector(STRSXP, MACH_SIZE));

    SET_STRING_ELT(nms, 0, mkChar("double.eps"));
    SET_VECTOR_ELT(ans, 0, ScalarReal(R_AccuracyInfo.eps));

    SET_STRING_ELT(nms, 1, mkChar("double.neg.eps"));
    SET_VECTOR_ELT(ans, 1, ScalarReal(R_AccuracyInfo.epsneg));

    SET_STRING_ELT(nms, 2, mkChar("double.xmin"));
    SET_VECTOR_ELT(ans, 2, ScalarReal(R_AccuracyInfo.xmin));

    SET_STRING_ELT(nms, 3, mkChar("double.xmax"));
    SET_VECTOR_ELT(ans, 3, ScalarReal(R_AccuracyInfo.xmax));

    SET_STRING_ELT(nms, 4, mkChar("double.base"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(R_AccuracyInfo.ibeta));

    SET_STRING_ELT(nms, 5, mkChar("double.digits"));
    SET_VECTOR_ELT(ans, 5, ScalarInteger(R_AccuracyInfo.it));

    SET_STRING_ELT(nms, 6, mkChar("double.rounding"));
    SET_VECTOR_ELT(ans, 6, ScalarInteger(R_AccuracyInfo.irnd));

    SET_STRING_ELT(nms, 7, mkChar("double.guard"));
    SET_VECTOR_ELT(ans, 7, ScalarInteger(R_AccuracyInfo.ngrd));

    SET_STRING_ELT(nms, 8, mkChar("double.ulp.digits"));
    SET_VECTOR_ELT(ans, 8, ScalarInteger(R_AccuracyInfo.machep));

    SET_STRING_ELT(nms, 9, mkChar("double.neg.ulp.digits"));
    SET_VECTOR_ELT(ans, 9, ScalarInteger(R_AccuracyInfo.negep));

    SET_STRING_ELT(nms, 10, mkChar("double.exponent"));
    SET_VECTOR_ELT(ans, 10, ScalarInteger(R_AccuracyInfo.iexp));

    SET_STRING_ELT(nms, 11, mkChar("double.min.exp"));
    SET_VECTOR_ELT(ans, 11, ScalarInteger(R_AccuracyInfo.minexp));

    SET_STRING_ELT(nms, 12, mkChar("double.max.exp"));
    SET_VECTOR_ELT(ans, 12, ScalarInteger(R_AccuracyInfo.maxexp));

    SET_STRING_ELT(nms, 13, mkChar("integer.max"));
    SET_VECTOR_ELT(ans, 13, ScalarInteger(INT_MAX));

    SET_STRING_ELT(nms, 14, mkChar("sizeof.long"));
    SET_VECTOR_ELT(ans, 14, ScalarInteger(SIZEOF_LONG));

    SET_STRING_ELT(nms, 15, mkChar("sizeof.longlong"));
    SET_VECTOR_ELT(ans, 15, ScalarInteger(SIZEOF_LONG_LONG));

    SET_STRING_ELT(nms, 16, mkChar("sizeof.longdouble"));
#ifdef HAVE_LONG_DOUBLE
    SET_VECTOR_ELT(ans, 16, ScalarInteger(SIZEOF_LONG_DOUBLE));
#else
    SET_VECTOR_ELT(ans, 16, ScalarInteger(0));
#endif

    SET_STRING_ELT(nms, 17, mkChar("sizeof.pointer"));
    SET_VECTOR_ELT(ans, 17, ScalarInteger(sizeof(SEXP)));

    SET_STRING_ELT(nms, 18, mkChar("sizeof.time_t"));
#ifdef  USE_INTERNAL_MKTIME
    SET_VECTOR_ELT(ans, 18, ScalarInteger(sizeof(R_time_t)));
#else
    SET_VECTOR_ELT(ans, 18, ScalarInteger(sizeof(time_t)));
#endif
/* This used to be just
#ifdef HAVE_LONG_DOUBLE
   but platforms can have the type and it be identical to double
   (as on ARM).  So do the same as capabilities("long.double")
*/
#ifdef HAVE_LONG_DOUBLE
    if (sizeof(LDOUBLE) > sizeof(double)) {
	static struct {
	    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
	    long double eps, epsneg, xmin, xmax;
	} R_LD_AccuracyInfo;
	
	machar_generic<long double>(&R_LD_AccuracyInfo.ibeta,
		  &R_LD_AccuracyInfo.it,
		  &R_LD_AccuracyInfo.irnd,
		  &R_LD_AccuracyInfo.ngrd,
		  &R_LD_AccuracyInfo.machep,
		  &R_LD_AccuracyInfo.negep,
		  &R_LD_AccuracyInfo.iexp,
		  &R_LD_AccuracyInfo.minexp,
		  &R_LD_AccuracyInfo.maxexp,
		  &R_LD_AccuracyInfo.eps,
		  &R_LD_AccuracyInfo.epsneg,
		  &R_LD_AccuracyInfo.xmin,
		  &R_LD_AccuracyInfo.xmax);
#define PT1 19
	SET_STRING_ELT(nms, PT1+0, mkChar("longdouble.eps"));
	SET_VECTOR_ELT(ans, PT1+0, ScalarReal((double) R_LD_AccuracyInfo.eps));

	SET_STRING_ELT(nms, PT1+1, mkChar("longdouble.neg.eps"));
	SET_VECTOR_ELT(ans, PT1+1, ScalarReal((double) R_LD_AccuracyInfo.epsneg));
	SET_STRING_ELT(nms, PT1+2, mkChar("longdouble.digits"));
	SET_VECTOR_ELT(ans, PT1+2, ScalarInteger(R_LD_AccuracyInfo.it));

	SET_STRING_ELT(nms, PT1+3, mkChar("longdouble.rounding"));
	SET_VECTOR_ELT(ans, PT1+3, ScalarInteger(R_LD_AccuracyInfo.irnd));

	SET_STRING_ELT(nms, PT1+4, mkChar("longdouble.guard"));
	SET_VECTOR_ELT(ans, PT1+4, ScalarInteger(R_LD_AccuracyInfo.ngrd));

	SET_STRING_ELT(nms, PT1+5, mkChar("longdouble.ulp.digits"));
	SET_VECTOR_ELT(ans, PT1+5, ScalarInteger(R_LD_AccuracyInfo.machep));

	SET_STRING_ELT(nms, PT1+6, mkChar("longdouble.neg.ulp.digits"));
	SET_VECTOR_ELT(ans, PT1+6, ScalarInteger(R_LD_AccuracyInfo.negep));

	SET_STRING_ELT(nms, PT1+7, mkChar("longdouble.exponent"));
	SET_VECTOR_ELT(ans, PT1+7, ScalarInteger(R_LD_AccuracyInfo.iexp));

	SET_STRING_ELT(nms, PT1+8, mkChar("longdouble.min.exp"));
	SET_VECTOR_ELT(ans, PT1+8, ScalarInteger(R_LD_AccuracyInfo.minexp));

	SET_STRING_ELT(nms, PT1+9, mkChar("longdouble.max.exp"));
	SET_VECTOR_ELT(ans, PT1+9, ScalarInteger(R_LD_AccuracyInfo.maxexp));

    }
#endif

    setAttrib(ans, R_NamesSymbol, nms);
    defineVar(install(".Machine"), ans, rho);
    UNPROTECT(2);
}

