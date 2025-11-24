/*
 *  R : A Computer Language for Statistical Data Analysis
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/StackChecker.hpp>
#include <Defn.h>
#include <Localization.h>

using namespace std;
using namespace R;

namespace CXXR
{

    namespace
    {
        const unsigned int R_MIN_EXPRESSIONS_OPT = 25;
        const unsigned int R_MAX_EXPRESSIONS_OPT = 500000;
    } // namespace

    unsigned int StackChecker::s_depth = 0;              // R_EvalDepth, Evaluation recursion depth
    unsigned int StackChecker::s_depth_limit = 5000;     // R_Expressions_keep, options(expressions)
    unsigned int StackChecker::s_depth_threshold = 5000; // R_Expressions, options(expressions)

    void StackChecker::checkAvailableStackSpace(size_t required_bytes)
    {
        if ((intptr_t)R_CStackLimit == -1)
            return;

        char dummy;
        intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

        if (INTPTR_MAX - usage < (intptr_t)required_bytes)
            /* addition would overflow, this is definitely too much */
            handleStackSpaceExceeded(INTPTR_MAX);

        /* do it this way, as some compilers do usage + extra
           in unsigned arithmetic */
        usage += required_bytes;
        if (usage > ((intptr_t)R_CStackLimit))
        {
            handleStackSpaceExceeded(usage);
        }
    }

    void StackChecker::setDepthLimit(unsigned int depth)
    {
        if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
        {
            Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
                     R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
        }
        s_depth_threshold = s_depth_limit = depth;
    }

    void StackChecker::restoreCStackLimit()
    {
        /* if we are in the process of handling a C stack overflow we need
           to restore the C stack limit before the jump */
        if (R_OldCStackLimit != 0)
        {
            R_CStackLimit = R_OldCStackLimit;
            R_OldCStackLimit = 0;
        }
    }

    // Implementation of handleStackSpaceExceeded() is in errors.cpp.
    // Implementation of handleStackDepthExceeded() is in errors.cpp.

    DisableStackCheckingScope::DisableStackCheckingScope()
    {
        m_previous_limit = StackChecker::depthLimit();
        StackChecker::setDepthLimit(R_MAX_EXPRESSIONS_OPT);

        m_previous_stack_limit = R_CStackLimit;
        R_CStackLimit = -1;
    }

    DisableStackCheckingScope::~DisableStackCheckingScope()
    {
        StackChecker::setDepthLimit(m_previous_limit);
        R_CStackLimit = m_previous_stack_limit;
    }

} // namespace CXXR
