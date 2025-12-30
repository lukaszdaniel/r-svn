/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file StackChecker.hpp
 *
 * @brief Class CXXR::StackChecker.
 * @brief Class CXXR::IncrementStackDepthScope;
 * @brief Class CXXR::DisableStackCheckingScope;
 */

#ifndef CXXR_STACKDEPTH_HPP
#define CXXR_STACKDEPTH_HPP

#include <cstdint>
#include <cstdio>

namespace CXXR
{

    class IncrementStackDepthScope;
    class DisableStackCheckingScope;

    class StackChecker
    {
    public:
        // Stack depth is in 'evaluator frames' (which is fairly loosely
        // defined).  In rho, it includes non-inlined function calls (including
        // builtins) and promise evaluations.

        /** @brief (Not for general use.)
         *
         * Used in context.cpp to save the evaluation depth
         * associated with an RCNTXT.  Also used in do_Cstack_info()
         * in platform.cpp.
         *
         * @return The current evaluation depth.
         */
        static unsigned int depth()
        {
            return s_depth;
        }

        /** @brief Maximum depth of R expression nesting.
         *
         * @return The current maximum nesting depth (disregarding any
         * extra depth that may have been introduced by extraDepth()
         * ).
         */
        static unsigned int depthLimit()
        {
            return s_depth_limit;
        }

        static unsigned int depthThreshold()
        {
            return s_depth_threshold;
        }

        /** @brief (Not for general use.)
         *
         * @param on If true, an increase is applied to the
         *          permissible depth of nested evaluations to allow
         *          error reporting to be carried out.  If false, any
         *          such extra depth currently in force is removed.
         */
        static void extraDepth(bool on)
        {
            s_depth_threshold = s_depth_limit + (on ? 500 : 0);
        }

        /** @brief (Not for general use.)
         *
         * Used in context.cpp to restore the evaluation depth
         * associated with an RCNTXT.  Also used in main.cpp to reset
         * the evaluation depth to zero.
         *
         * @param depth The required depth.
         */
        static void setDepth(unsigned int depth)
        {
            s_depth = depth;
        }

        /** @brief Set maximum depth of R expression nesting.
         *
         * @param depth The required maximum nesting depth.  If the
         *          supplied value lies outside the permissible range,
         *          an error is reported and the nesting depth is left
         *          unchanged.
         *
         * @return The maximum nesting depth previously in force.
         */
        static void setDepthLimit(unsigned int depth);

        // Check that there is sufficient stack space for <required_bytes>
        // plus some additional allowance.
        // Throws an exception otherwise.
        static void checkAvailableStackSpace(std::size_t required_bytes = 0);

        static void restoreCStackLimit();

    // private:
        friend class IncrementStackDepthScope;
        friend class DisableStackCheckingScope;

        static void handleStackDepthExceeded() __attribute__((noreturn));
        static void handleStackSpaceExceeded(intptr_t usage) __attribute__((noreturn));

        static unsigned int s_depth;           // Current depth of expression evaluation
        static unsigned int s_depth_threshold; // An error will be
                                               // reported if s_depth exceeds this
                                               // value.  s_depth_threshold is normally
                                               // equal to s_depth_limit, but may be
                                               // temporarily increased above s_depth_limit
                                               // to allow error reporting.
        static unsigned int s_depth_limit;     // The value (controlled
                                               // by the 'expressions' R option) to
                                               // which s_depth_threshold is set except
                                               // during error reporting.
        StackChecker() = delete;
    };

    class IncrementStackDepthScope
    {
    public:
        IncrementStackDepthScope()
        {
            StackChecker::s_depth++;
            /* We need to explicit set a NULL call here to circumvent attempts
               to deparse the call in the error-handler */
            if (StackChecker::s_depth > StackChecker::s_depth_threshold)
            {
                /* This bump of R_Expressions doesn't really work in many
                   cases since jumps (e.g. from explicit return() calls or in
                   UseMethod dispatch) reset this. Something more
                   sophisticated might work, but also increase the risk of a C
                   stack overflow. LT */
                StackChecker::extraDepth(true);
                StackChecker::handleStackDepthExceeded();
            }
            StackChecker::checkAvailableStackSpace();
        }

        ~IncrementStackDepthScope()
        {
            // Safety measure in case someone sets s_depth = 0
            if (StackChecker::s_depth > 0) StackChecker::s_depth--;
        }

    private:
        IncrementStackDepthScope(const IncrementStackDepthScope &) = delete;
        IncrementStackDepthScope &operator=(const IncrementStackDepthScope &) = delete;
    };

    /*
     * This class disables the normal stack checking to allow error reporting
     * and stack unwinding to proceed.
     */
    class DisableStackCheckingScope
    {
    public:
        DisableStackCheckingScope();
        ~DisableStackCheckingScope();

    private:
        unsigned int m_previous_limit;
        uintptr_t m_previous_stack_limit;

        DisableStackCheckingScope(const DisableStackCheckingScope &) = delete;
        DisableStackCheckingScope &operator=(const DisableStackCheckingScope &) = delete;
    };
} // namespace CXXR

#endif // CXXR_STACKDEPTH_HPP
