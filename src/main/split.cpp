/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006-2016 The R Core Team
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
 */

/** @file split.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Itermacros.h>

using namespace R;
using namespace CXXR;

attribute_hidden SEXP do_split(SEXP call, SEXP op, SEXP args, SEXP env)
{
    GCStackRoot<> counts, vec;
    SEXP nmj;

    checkArity(op, args);

    SEXP x = CAR(args);
    SEXP f = CADR(args);
    if (!isVector(x))
	error("%s", _("first argument must be a vector"));
    if (!isFactor(f))
	error("%s", _("second argument must be a factor"));
    int nlevs = nlevels(f);
    R_xlen_t nfac = XLENGTH(f);
    R_xlen_t nobs = XLENGTH(x);
    if (nfac <= 0 && nobs > 0)
	error("%s", _("group length is 0 but data length > 0"));
    if (nfac > 0 && (nobs % nfac) != 0)
	warning("%s", _("data length is not a multiple of split variable"));
    SEXP nm = getAttrib(x, R_NamesSymbol);
    bool have_names = (nm != R_NilValue);

#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(x))
# define _L_INTSXP_ REALSXP
# define _L_INTEG_  REAL
# define _L_int_    R_xlen_t
# include "split-incl.cpp"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_
    else
#endif

# define _L_INTSXP_ INTSXP
# define _L_INTEG_  INTEGER
# define _L_int_    int
# include "split-incl.cpp"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_

    setAttrib(vec, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));

    return vec;
}
