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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Environment.hpp>
#include <Rinternals.h>

using namespace CXXR;

namespace R
{
    SEXP R_NewHashedEnv(SEXP enclos, int size);
}

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENCLOSptr = ENCLOS;
        const auto &ENVFLAGSptr = ENVFLAGS;
        const auto &HASHTABptr = HASHTAB;
        const auto &isEnvironmentptr = Rf_isEnvironment;
        const auto &FRAMEptr = FRAME;
        // const auto &ENV_RDEBUGptr = ENV_RDEBUG;
        const auto &SET_ENCLOSptr = SET_ENCLOS;
        const auto &SET_ENVFLAGSptr = SET_ENVFLAGS;
        const auto &SET_FRAMEptr = SET_FRAME;
        // const auto &SET_ENV_RDEBUGptr = SET_ENV_RDEBUG;
        const auto &SET_HASHTABptr = SET_HASHTAB;
    } // namespace ForceNonInline

    Environment *Environment::create(SEXP frame, SEXP enclosing_env, SEXP hashtab)
    {
        GCStackRoot<> framert(frame);
        GCStackRoot<> envrt(enclosing_env);
        GCStackRoot<> hashtabrt(hashtab);

        return new Environment(frame, enclosing_env, hashtab);
    }

    SEXP Environment::empty()
    {
        return R_EmptyEnv;
    }

    SEXP Environment::base()
    {
        return R_BaseEnv;
    }

    SEXP Environment::global()
    {
        return R_GlobalEnv;
    }

    SEXP Environment::baseNamespace()
    {
        return R_BaseNamespace;
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

