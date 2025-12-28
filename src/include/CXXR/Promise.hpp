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

/** @file Promise.hpp
 * @brief Class CXXR::Promise and associated C interface.
 */

#ifndef PROMISE_HPP
#define PROMISE_HPP

#include <CXXR/RObject.hpp>

namespace CXXR
{
    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     */
    class Promise: public RObject
    {
    public:
        static Promise *create(SEXP val = R_NilValue, SEXP expr = R_NilValue, SEXP env = R_NilValue);

        /** @brief Is an RObject a Promise?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a Promise.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == PROMSXP;
        }

        /** @brief Access the environment of the Promise.
         *
         * @return Pointer to the environment of the Promise.  This
         * will be a null pointer after the promise has been
         * evaluated.
         */
        RObject *environment() const
        {
            return m_env;
        }

        /** @brief Force the Promise.
         *
         * i.e. evaluate the value generator of the Promise within the
         * Environment of the Promise.  Following this, the
         * environment pointer is set null, thus possibly allowing the
         * Environment to be garbage-collected.
         *
         * If this function is used on a Promise that has already been
         * forced, it simply returns the previously computed value.
         *
         * @return The value of the Promise, i.e. the result of
         * evaluating the value generator.
         */
        RObject *force();

        /** @brief RObject to be evaluated by the Promise.
         *
         * @return const pointer to the RObject to be evaluated by
         * the Promise.
         */
        const RObject *valueGenerator() const
        {
            return u.promsxp.m_expr;
        }

        /** @brief Set value of the Promise.
         *
         * Once the value is set to something other than
         * Symbol::unboundValue(), the environment pointer is
         * set null.
         *
         * @param val Value to be associated with the Promise.
         *
         * @todo Should be private (or removed entirely), but currently
         * still used in saveload.cpp.
         */
        void setValue(RObject *val);

        void setEnvironment(RObject *val)
        {
            m_env.retarget(this, val);
        }

        void setValueGenerator(RObject *val)
        {
            u.promsxp.m_expr.retarget(this, val);
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "promise";
        }

        /** @brief Access the value of a Promise.
         *
         * @return pointer to the value of the Promise, or to
         * Symbol::unboundValue() if it has not yet been evaluated.
         */
        RObject *value();

        /** @brief Has this promise been evaluated yet?
         */
        bool evaluated() const;

        bool hasUnexpandedValue() const
        {
            return sxpinfo.m_binding_tag != NILSXP;
        }

        void markExpanded()
        {
            sxpinfo.m_binding_tag = NILSXP;
        }

        // Virtual function of RObject:
        const char *typeName() const override;

    public:
        // GCEdge<> m_expr;
        GCEdge<> m_env;

    protected:
        // Virtual functions of GCNode:
        void visitReferents(const_visitor *v) const override;
        void detachReferents() override;

    private:
        Promise(SEXP val, SEXP expr, SEXP env): RObject(PROMSXP)
        {
            u.promsxp.m_value = val;
            u.promsxp.m_expr = expr;
            m_env = env;
        }

        // Declared private to ensure that Promise objects are
        // allocated only using 'new':
        ~Promise() {}

        // Not (yet) implemented.  Declared to prevent
        // compiler-generated versions:
        Promise(const Promise &) = delete;
        Promise &operator=(const Promise &) = delete;
    };
} // namespace CXXR

namespace R
{
    /**
     * @param x Pointer to a CXXR::Promise.
     *
     * @return Evaluation status of the CXXR::Promise.
     *
     * @deprecated Will need to be fixed.
     */
    int (PRSEEN)(SEXP x);

    /**
     * @param x Pointer to a CXXR::Promise.
     *
     * @deprecated Will need to be fixed.
     */
    void (SET_PRSEEN)(SEXP x, int v);

    /** @brief Create a CXXR::Promise object.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the CXXR::Promise.
     *
     * @param env CXXR::Environment in which \a expr is to be evaluated.
     */
    SEXP mkPROMISE(SEXP expr, SEXP env);

    /** @brief Create a CXXR::Promise object which has already been evaluated.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the CXXR::Promise.
     *
     * @param value CXXR::RObject which is the value of the promise.
     */
    SEXP R_mkEVPROMISE(SEXP expr, SEXP value);
} // namespace R

extern "C"
{
    /** @brief Access the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise.
     */
    SEXP (PRCODE)(SEXP x);

    /** @brief Access the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the environment in which the CXXR::Promise
     *         is to be  evaluated.  Set to a null pointer when the
     *         CXXR::Promise has been evaluated.
     */
    SEXP (PRENV)(SEXP x);

    /** @brief Access the value of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the value of the CXXR::Promise, or to
     *         R_UnboundValue if it has not yet been evaluated..
     */
    SEXP (PRVALUE)(SEXP x);

    /** @brief Set the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the environment in which the expression is to
     *          be evaluated.
     *
     * @todo Probably ought to be private or done in the constructor.
     */
    void SET_PRENV(SEXP x, SEXP v);

    /** @brief Set the value of a CXXR::Promise.
     *
     * Once the value is set to something other than R_UnboundValue,
     * the environment pointer is set null.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the value to be assigned to the CXXR::Promise.
     *
     * @todo Replace this with a method call to evaluate the CXXR::Promise.
     */
    void SET_PRVALUE(SEXP x, SEXP v);

    /** @brief Set the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise.
     */
    void SET_PRCODE(SEXP x, SEXP v);
} // extern "C"

#endif // PROMISE_HPP
