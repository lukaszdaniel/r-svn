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

/** @file Closure.hpp
 * @brief Class CXXR::Closure and associated C interface.
 */

#ifndef CLOSURE_HPP
#define CLOSURE_HPP

#include <CXXR/FunctionBase.hpp>

namespace CXXR
{
   /** @brief Class representing a functional programming closure.
    *
    * A closure associates a function definition (the body) with a
    * list of formal arguments and an environment.  In evaluating the
    * function, non-local variables within the function definition
    * are interpreted by reference to the specified environment (and
    * its enclosing environments).
    */
    class Closure : public FunctionBase
    {
    public:
        /** @brief Is an RObject a Closure?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a Closure.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == CLOSXP;
        }

    private:
        // Declared private to ensure that Closure objects are
        // allocated only using 'new':
        ~Closure() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        Closure(const Closure &);
        Closure &operator=(const Closure &);
    };
} // namespace CXXR

namespace R
{
    /** @brief Create a CXXR::Closure object.
     *
     * @param formal_args Pointer to a CXXR::PairList (checked) of
     *          formal arguments.
     *
     * @param body Pointer to the body of the CXXR::Closure.  This must be
     *          either a null pointer or a pointer to an object of one
     *          of the following types: LISTSXP, LANGSXP, SYMSXP,
     *          EXPRSXP, VECSXP or BCODESXP (checked).
     *
     * @param env pointer to the CXXR::Environment (checked) in which the
     *          closure is to be evaluated.
     *
     * @return pointer to the created closure object.
     *
     * @note This is called by function() {}, where an invalid
     *       body should be impossible. When called from
     *       other places (eg do_asfunction) they
     *       should do this checking in advance.
     */
    SEXP mkCLOSXP(SEXP formal_args, SEXP body, SEXP env);

    /** @brief Get the JIT state
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x is not meant to be JIT-compiled.  Returns false if \a x
     * is nullptr.
     */
    int NOJIT(SEXP x);

    /** @brief Can this object be JIT-compiled?
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x can be JIT-compiled.  Returns false if \a x
     * is nullptr.
     */
    int MAYBEJIT(SEXP x);

    /** @brief Do not allow JIT compilation for this object
     *
     * @param x Pointer to \c RObject.
     */
    void SET_NOJIT(SEXP x);

    /** @brief Mark object as available for JIT compilation
     *
     * @param x Pointer to \c RObject.
     */
    void SET_MAYBEJIT(SEXP x);

    /** @brief Remove availabilty flag for JIT compilation
     *
     * @param x Pointer to \c RObject.
     */
    void UNSET_MAYBEJIT(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Access the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the body of \a x.
     */
    SEXP BODY(SEXP x);

    /** @brief Access the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the environment of x.
     */
    SEXP CLOENV(SEXP x);

    /** @brief Access formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @return Pointer to the formal argument list of \a x.
     */
    SEXP FORMALS(SEXP x);

    /** @brief Set the formal arguments of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the formal argument list.
     */
    void SET_FORMALS(SEXP x, SEXP v);

    /** @brief Set the body of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the body of this CXXR::Closure.
     */
    void SET_BODY(SEXP x, SEXP v);

    /** @brief Replace the environment of a CXXR::Closure.
     *
     * @param x Pointer to a CXXR::Closure object (checked).
     *
     * @param v Pointer to the environment now to be
     *          considered as the environment of this CXXR::Closure.
     *          A null pointer is not permissible (not checked).
     */
    void SET_CLOENV(SEXP x, SEXP v);

    /** @brief Get debugging state
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x is in debugging state.  Returns false if \a x
     * is nullptr.
     */
    int RSTEP(SEXP x);

    /** @brief Set debugging state
     *
     * @param x Pointer to \c RObject.
     */
    void SET_RSTEP(SEXP x, int v);
} // extern "C"

#endif /* CLOSURE_HPP */
