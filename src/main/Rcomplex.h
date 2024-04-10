/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-2018	    The R Core Team
 *  Copyright (C) 2005		    The R Foundation
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

#ifndef R_RCOMPLEX_H
#define R_RCOMPLEX_H

#include <complex>

/* GCC has problems with header files on e.g. Solaris.
   That OS defines the imaginary type, but GCC does not.
   Probably needed elsewhere, e.g. AIX, HP-UX (PR#15083)
   And use on Win32/64 suppresses warnings.
   The warning was also seen on macOS 10.5, but not later.
*/
#define I std::complex<double>(0,1)


/*
   Note: this could use the C11 CMPLX() macro.
   As could mycpow, z_tan and some of the substitutes.
 */
inline static std::complex<double> toC99(const Rcomplex *x) {
    std::complex<double> val(x->r, x->i);
    return val;
}

inline static void SET_C99_COMPLEX(Rcomplex *x, R_xlen_t i, std::complex<double> value)
{
    Rcomplex *ans = x + i;
    ans->r = value.real();
    ans->i = value.imag();
}

attribute_hidden void z_prec_r(Rcomplex *r, const Rcomplex *x, double digits);

#endif /* R_RCOMPLEX_H */
