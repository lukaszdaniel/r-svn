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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Evaluator.hpp
 *
 * @brief Class CXXR::Evaluator
 */

#ifndef EVALUATOR_HPP
#define EVALUATOR_HPP

#include <CXXR/RTypes.hpp>
#include <CXXR/GCRoot.hpp>

namespace CXXR
{
    /** @brief Framework for R command evaluation.
     *
     * An object of this class provides a framework within which
     * evaluation of R commands (i.e. top-level R expressions) takes
     * place, and provides various housekeeping services to control
     * and support that evaluation.  Evaluator objects conceptually
     * form a stack, so they must be destroyed in the reverse order of
     * creation: it is therefore recommended that they always be
     * declared on the processor stack, i.e. as C++ automatic
     * variables.  The Evaluator at the top of the stack is the \e
     * current Evaluator.
     *
     * If an error occurs during R expression evaluation, the
     * resulting exception is caught within the scope of the current
     * Evaluator; the relevant code will then typically request
     * another R command (if interactive), or terminate the scope of
     * the current Evaluator, thus popping it off the Evaluator stack,
     * and return control to code within the scope of the next
     * Evaluator down.
     *
     * Each Evaluator object contains a nested sequence of zero or
     * more Contexts.  A newly created Context is added to the
     * sequence belonging to the current Evaluator.  It is therefore
     * an error to declare a Context object outside the scope of any
     * Evaluator object.
     *
     * @note Evaluator objects fulfil many of the roles of TOPLEVEL
     * contexts within CR (which do not exist within rho).  Note that
     * in rho, each Evaluator has its own singly-linked list of
     * Context objects (starting with the innermost); there is no
     * overall list of Context objects spanning different Evaluator
     * objects.
     */
    class Evaluator
    {
    public:
        class RContext;

        Evaluator()
            : m_next(s_current), m_innermost_context(nullptr)
        {
            s_current = this;
        }

        ~Evaluator()
        {
            s_current = m_next;
        }

        /** @brief The current Evaluator.
         *
         * @return Pointer to the current (innermost) Evaluator.
         */
        static Evaluator *current()
        {
            return s_current;
        }

        /** @brief (Not for general use.)
         *
         * This function is for use by the profiling code in eval.cpp
         * to record whether profiling is currently enabled.
         *
         * @param on true iff printing is required.
         */
        static void enableProfiling(bool on)
        {
            s_profiling = on;
        }

        /** @brief (Not for general use.)
         *
         * This function is for use by the bcEval in eval.cpp
         * to record whether BC evaluation is in progress.
         *
         * @param on true iff printing is required.
         */
        static void enableBCActive(bool on)
        {
            s_bc_active = on;
        }

        /** @brief Specify whether the result of top-level expression
         * be printed.
         *
         * @param on true iff printing is required.
         */
        static void enableResultPrinting(bool on);

        /** @brief Evaluate RObject in a specified Environment.
         *
         * For most types of RObject, this simply returns a pointer to
         * the RObject itself.
         *
         * @param object Pointer to the RObject to be evaluated.  May
         *          be null, in which case the function returns a null
         *          pointer.
         *
         * @param env Pointer to the environment in which evaluation
         *          is to take place.  May be null only if \a object
         *          is null.
         *
         * @return Pointer to the result of evaluation.
         */
        static SEXP evaluate(SEXP object, SEXP env);

        static bool isSelfEvaluated(SEXP e);

        /** @brief Innermost Context belonging to this Evaluator.
         *
         * @return Pointer to the innermost Context belonging to this
         * Evaluator.
         */
        RContext *innermostContext() const
        {
            return m_innermost_context;
        }

        /** @brief Next Evaluator down the stack
         *
         * @return Pointer to the next Evaluator down the stack, or a
         * null pointer if this is the innermost Evaluator (i.e. the
         * one at the top of the stack).
         */
        Evaluator *next() const
        {
            return m_next;
        }

        /** @brief Is profiling currently enabled?
         *
         * @return true iff profiling is currently in progress.
         */
        static bool profiling()
        {
            return s_profiling;
        }

        /** @brief Is bcEval currently active?
         *
         * @return true iff bcEval is currently in progress.
         */
        static bool bcActive()
        {
            return s_bc_active;
        }

        /** @brief Is the result of top-level expression evaluation
         * printed?
         *
         * @return true iff it is currently specified that the result
         * of a top-level R expression evaluation should be printed.
         */
        static bool resultPrinted();

        /** @brief Check for user interrupts.
         */
        static void maybeCheckForUserInterrupts();

        /** @brief Check for user interrupts.
         */
        static void checkForUserInterrupts();

        /** @brief Are interrupts currently suspended?
         */
        static bool interruptsSuspended();

        static void setInterruptsSuspended(bool on);

        /** @brief Are any user interrupts currently pending?
         *
         * If user interrupts are attempted while user interrupts are
         * suspended, this is set non-zero.  The interrupt is then
         * services when the period of suspension ends.
         */
        static bool interruptsPending();

        static void setInterruptsPending(bool on);

        static GCRoot<> s_current_expression;
#define R_CurrentExpr CXXR::Evaluator::s_current_expression

    public: // private:
        /** @brief Print expression value?
         *
         * If s_visible is true when the evaluation of a top-level R
         * expression completes, the value of the expression is printed.
         */
        static bool s_visible;
        static unsigned int s_countdown;       // Number of calls of
                                               // Evaluator::evaluate() to go before a
                                               // check is made for user interrupts.
        static unsigned int s_countdown_start; // Value from which
                                               // s_countdown starts counting down
        static Evaluator *s_current;           // The current (innermost) Evaluator
        static bool s_profiling;               // True iff profiling enabled
        static bool s_bc_active;               // bcEval called more recently than eval

        Evaluator *m_next; // Next Evaluator down the stack
        RContext *m_innermost_context; // Innermost Context belonging
                                       // to this Evaluator
    };
} // namespace CXXR

namespace R
{
    /** @brief Is a Symbol missing within an Environment?
     *
     * @param symbol Pointer to the Symbol whose missing status is
     * required.
     *
     * @param rho Pointer to the Environment in whose Frame \a symbol
     *          is to be sought.
     *
     * @return A non-zero value iff \a symbol is missing in the Frame
     * of \a rho.
     *
     * @note For more information, refer to the code, which is
     * surprisingly complicated.
     */
    bool R_isMissing(SEXP symbol, SEXP rho);
} // namespace R

extern "C"
{
    /** @brief Are any user interrupts currently pending?
     *
     * If user interrupts are attempted while user interrupts are
     * suspended, this is set non-zero.  The interrupt is then
     * services when the period of suspension ends.
     */
    // extern bool R_interrupts_pending;

    /** @brief Are interrupts currently suspended?
     */
    // extern bool R_interrupts_suspended;

    /** @brief Evaluate an object in a specified Environment.
     *
     * @param e Pointer (possibly null) to the object to be evaluated.
     *
     * @param rho Pointer to an Environment (checked unless \a e is null).
     *
     * @return Pointer to the result of evaluating \a e in \a rho, or
     * a null pointer if \a e is null.
     */
    SEXP Rf_eval(SEXP e, SEXP rho);
} // extern "C"

#endif /* EVALUATOR_HPP */
