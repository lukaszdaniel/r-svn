/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Closure.cpp
 *
 * @brief Implementation of class Closure and associated C
 * interface.
 */

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Closure.hpp>
#include <R_ext/Error.h>
#include <Localization.h>

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &BODYptr = BODY;
        const auto &CLOENVptr = CLOENV;
        const auto &FORMALSptr = FORMALS;
        const auto &RSTEPptr = RSTEP;
        const auto &SET_CLOENVptr = SET_CLOENV;
        const auto &SET_RSTEPptr = SET_RSTEP;
    } // namespace ForceNonInline

    Closure *Closure::create(SEXP formal_args, SEXP body, SEXP env)
    {
        switch (TYPEOF(body))
        {
        case CLOSXP:
        case BUILTINSXP:
        case SPECIALSXP:
        case DOTSXP:
        case ANYSXP:
            Rf_error("%s", _("invalid body argument for 'function'"));
            break;
        default:
            break;
        }
        GCStackRoot<> formalsrt(formal_args);
        GCStackRoot<> bodyrt(body);
        GCStackRoot<> rhort(env);

        return new Closure(formal_args, body, env == R_NilValue ? R_GlobalEnv : env);
    }

    Closure::Closure(SEXP formal_args, SEXP body, SEXP env) : FunctionBase(CLOSXP)
    {
        u.closxp.m_formals = formal_args;
        u.closxp.m_body = body;
        u.closxp.m_env = env;
    }

    const char *Closure::typeName() const
    {
        return staticTypeName();
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

