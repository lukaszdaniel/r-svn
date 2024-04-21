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
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Evaluator.cpp
 *
 * Implementation of class Evaluator.
 */

#include <CXXR/Evaluator.hpp>
#include <Defn.h> // for R_interrupts_suspended and R_interrupts_pending

using namespace CXXR;

namespace CXXR
{
    bool Evaluator::s_visible = false; // R_Visible

    unsigned int Evaluator::s_countdown = 1000;
    unsigned int Evaluator::s_countdown_start = 1000; // was 100 before 2.8.0
    Evaluator *Evaluator::s_current = nullptr;
    bool Evaluator::s_profiling = false; // R_Profiling
    bool Evaluator::s_bc_active = false; // R_BCIntActive

    void Evaluator::maybeCheckForUserInterrupts()
    {
        if (--s_countdown == 0)
        {
            checkForUserInterrupts();
        }
    }

    void Evaluator::enableResultPrinting(bool on)
    {
        s_visible = on;
    }

    bool Evaluator::resultPrinted()
    {
        return s_visible;
    }

    bool Evaluator::interruptsSuspended()
    {
        return R_interrupts_suspended;
    }

    void Evaluator::setInterruptsSuspended(bool on)
    {
        R_interrupts_suspended = on;
    }

    /** @brief Are any user interrupts currently pending?
     *
     * If user interrupts are attempted while user interrupts are
     * suspended, this is set non-zero.  The interrupt is then
     * services when the period of suspension ends.
     */
    bool Evaluator::interruptsPending()
    {
        return R_interrupts_pending;
    }

    void Evaluator::setInterruptsPending(bool on)
    {
        R_interrupts_pending = on;
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

extern "C"
{
}
