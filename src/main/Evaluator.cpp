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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/Evaluator.hpp>
#include <CXXR/StackChecker.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/WeakRef.hpp> // for R_RunPendingFinalizers
#include <R_ext/Utils.h> // for R_CheckUserInterrupt
#include <Defn.h> // for R_interrupts_suspended and R_interrupts_pending

using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &evalptr = Rf_eval;
    } // namespace ForceNonInline

    bool Evaluator::s_visible = false; // R_Visible

    unsigned int Evaluator::s_countdown = 1000;
    unsigned int Evaluator::s_countdown_start = 1000; // was 100 before 2.8.0
    Evaluator *Evaluator::s_current = nullptr;
    bool Evaluator::s_profiling = false;               // R_Profiling
    bool Evaluator::s_bc_active = false;               // R_BCIntActive
    GCRoot<> Evaluator::s_current_expression(R_NilValue); // R_CurrentExpr

    // Implementation of Evaluator::evaluate() is in eval.cpp (for the time being)

    void Evaluator::maybeCheckForUserInterrupts()
    {
        if (--s_countdown == 0)
        {
            checkForUserInterrupts();
        }
    }

    void Evaluator::checkForUserInterrupts()
    {
        R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
        /* finalizers are run here since this should only be called at
           points where running arbitrary code should be safe */
        R_RunPendingFinalizers();
#endif
        s_countdown = s_countdown_start;
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

    bool Evaluator::isSelfEvaluated(SEXP e)
    {
        switch (TYPEOF(e))
        {
        case NILSXP:
        case LISTSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case STRSXP:
        case CPLXSXP:
        case RAWSXP:
        case OBJSXP:
        case SPECIALSXP:
        case BUILTINSXP:
        case ENVSXP:
        case CLOSXP:
        case VECSXP:
        case EXTPTRSXP:
        case WEAKREFSXP:
        case EXPRSXP:
            return true;
        default:
            return false;
        }
    }

    /* Return value of "e" evaluated in "rho". */

    /* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
    SEXP Evaluator::evaluate(SEXP e, SEXP rho)
    {
        enableResultPrinting(true);

        /* this is needed even for self-evaluating objects or something like
           'while (TRUE) NULL' will not be interruptable */
        maybeCheckForUserInterrupts();

        /* handle self-evaluating objects with minimal overhead */
        if (isSelfEvaluated(e))
        {
        /* Make sure constants in expressions are NAMED before being
           used as values.  Setting NAMED to NAMEDMAX makes sure weird calls
           to replacement functions won't modify constants in
           expressions.  */
            ENSURE_NAMEDMAX(e);
            return e;
        }

        bool bcintactivesave = Evaluator::bcActive();
        Evaluator::enableBCActive(false);

        if (TYPEOF(e) != PROMSXP) // non-environment check only matters for non-Promise objects
        {
            if (!rho)
                Rf_error("'rho' cannot be C NULL: detected in C-level eval");
            if (!Environment::isA(rho))
                Rf_error("'rho' must be an environment not %s: detected in C-level eval",
                    R::R_typeToChar(rho));
        }

        /* Save the current srcref context. */

        SEXP srcrefsave = R_Srcref;

        /* The use of IncrementStackDepthScope below is necessary because of the
           possibility of non-local returns from evaluation.  Without this
           an "expression too complex error" is quite likely. */

        IncrementStackDepthScope scope;

#ifdef Win32
    /* This is an inlined version of Rwin_fpset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
# if (defined(__i386) || defined(__x86_64))
        __asm__("fninit");
# elif defined(__aarch64__)
        __asm__ volatile("msr fpcr, %0" : : "r"(0LL));
# else
        _fpreset();
# endif
#endif

        Environment *env = SEXP_downcast<Environment *>(rho);
        SEXP tmp = e->evaluate(env);

        R_Srcref = srcrefsave;
        Evaluator::enableBCActive(bcintactivesave);
        return tmp;
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****
