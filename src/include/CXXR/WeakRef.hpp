/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file WeakRef.hpp
 * @brief Class CXXR::WeakRef and associated C interface.
 */

#ifndef WEAKREF_HPP
#define WEAKREF_HPP

#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>

extern "C"
{
    using R_CFinalizer_t = void (*)(SEXP);
}

namespace CXXR
{
} // namespace CXXR

namespace R
{
    bool RunFinalizers(void);
} // namespace R

extern "C"
{
    SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);

    SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);

    SEXP R_WeakRefKey(SEXP w);

    SEXP R_WeakRefValue(SEXP w);

    void R_RunWeakRefFinalizer(SEXP x);

    void R_RunExitFinalizers(void);

    void R_RunPendingFinalizers(void);

    void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);

    void R_RegisterFinalizer(SEXP s, SEXP fun);

    void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);

    void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
}

#endif /* WEAKREF_HPP */
