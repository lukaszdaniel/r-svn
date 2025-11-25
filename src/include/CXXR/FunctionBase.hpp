/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file FunctionBase.hpp
 *
 * @brief Class CXXR::FunctionBase and associated C interface functions.
 */

#ifndef FUNCTIONBASE_HPP
#define FUNCTIONBASE_HPP

#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>

namespace CXXR
{
    /** @brief Base class for function types.
     */
    class FunctionBase: public RObject
    {
    public:
        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(function type)";
        }

        /** @brief Is an RObject a FunctionBase?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a FunctionBase.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == CLOSXP || st == BUILTINSXP || st == SPECIALSXP;
        }

    protected:
        // Virtual functions of GCNode:
        void visitReferents(const_visitor *v) const override;
        void detachReferents() override;

        /**
         * @param stype Required type of the FunctionBase.
         */
        explicit FunctionBase(SEXPTYPE stype): RObject(stype)
        {
        }

        // Declared protected to ensure that FunctionBase objects are
        // allocated only using 'new':
        ~FunctionBase() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        FunctionBase(const FunctionBase &);
        FunctionBase &operator=(const FunctionBase &);
    };
} // namespace CXXR

namespace R
{
    /** @brief Get function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     *         null pointer.
     */
    int (RTRACE)(SEXP x);

    /** @brief Set function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @param v The desired tracing status: non-zero if tracing is
     *          required.
     */
    void (SET_RTRACE)(SEXP x, int v);

    /** @brief Query debugging status.
     *
     * @param x Pointer to a CXXR::FunctionBase object.
     *
     * @return \c true if debugging is set, i.e. evaluations of the
     *         function should run under the browser.
     *
     * @note In CXXR, RDEBUG() is applicable only to FunctionBase; use
     * ENV_RDEBUG() to query the debugging (single-stepping) state
     * for environments.
     */
    int (RDEBUG)(SEXP x);

    /** @brief Set the debugging state of a CXXR::FunctionBase object.
     *
     * @param x Pointer to a CXXR::FunctionBase object (checked).
     *
     * @param v The new debugging state.
     *
     * @note In CXXR, SET_RDEBUG() is applicable only to FunctionBase; use
     * SET_ENV_RDEBUG() to set the debugging (single-stepping) state
     * for environments.
     */
    void (SET_RDEBUG)(SEXP x, int v);
} // namespace R

extern "C"
{
    /** @brief Is function BuiltIn or Special?
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return \c true if CXXR::ROject is BuiltIn or Special.
     */
    Rboolean Rf_isPrimitive(SEXP s);

    /** @brief Is function Closure or Primitive?
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return \c true if CXXR::ROject is Closure or Primitive.
     */
    Rboolean Rf_isFunction(SEXP s);
} // extern "C"

#endif // FUNCTIONBASE_HPP
