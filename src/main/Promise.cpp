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

/** @file Promise.cpp
 *
 * @brief Implementation of class Promise and associated C
 * interface.
 */

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Symbol.hpp>
#include <Defn.h> // for IMMEDIATE_PROMISE_VALUES

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &PRCODEptr = PRCODE;
        const auto &PRENVptr = PRENV;
        const auto &PRSEENptr = PRSEEN;
        const auto &PRVALUEptr = PRVALUE;
        const auto &SETPRSEENptr = R::SET_PRSEEN;
        const auto &SET_PRVALUEptr = SET_PRVALUE;
    } // namespace ForceNonInline

    const char *Promise::typeName() const
    {
        return staticTypeName();
    }

    Promise *Promise::create(SEXP val, SEXP expr, SEXP env)
    {
        GCStackRoot<> valrt(val);
        GCStackRoot<> exprrt(expr);
        GCStackRoot<> envrt(env);

        return new Promise(val, expr, env);
    }

    RObject *Promise::value()
    {
#ifdef IMMEDIATE_PROMISE_VALUES
        if (hasUnexpandedValue())
            return R::R_expand_promise_value(this);
#endif
        return u.promsxp.m_value;
    }

    void Promise::setValue(RObject *val)
    {
#ifdef IMMEDIATE_PROMISE_VALUES
        if (hasUnexpandedValue())
        {
            u.promsxp.m_value.reset();
            markExpanded();
        }
#endif
        u.promsxp.m_value.retarget(this, val);
        // if (val != Symbol::unboundValue())
        //     u.promsxp.m_env = nullptr;
    }

    bool Promise::evaluated() const
    {
#ifdef IMMEDIATE_PROMISE_VALUES
        return (hasUnexpandedValue() || u.promsxp.m_value != R_UnboundValue);
#endif
        return (u.promsxp.m_value != R_UnboundValue);
        // return u.promsxp.m_env == R_NilValue;
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

