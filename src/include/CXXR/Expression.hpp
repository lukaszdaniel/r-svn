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

/** @file Expression.hpp
 * @brief Class CXXR::Expression and associated C interface.
 */

#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include <CXXR/ConsCell.hpp>

namespace CXXR
{
    /** @brief Singly linked list representing an R expression.
     *
     * R expression, represented as a LISP-like singly-linked list,
     * each element containing pointers to a 'car' object and to a
     * 'tag' object, as well as a pointer to the next element of the
     * list.  (Any of these pointers may be null.)  A Expression
     * object is considered to 'own' its car, its tag, and all its
     * successors.
     *
     * Most expressions should be represented using the CachingExpression class,
     * instead of this, as it has better performance.  This class is primarily
     * useful for expressions that are only evaluated once, where the function
     * is known to be a primitive and for SET_TYPEOF.
     */
    class Expression : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        Expression(SEXP cr, SEXP tl, SEXP tg) : ConsCell(LANGSXP, cr, tl, tg)
        {
        }

        /** @brief Is an RObject an Expression?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is an Expression.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == LANGSXP;
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "language";
        }

        // Virtual functions of RObject:
        const char *typeName() const override;

    private:
        // Declared private to ensure that Expression objects are
        // allocated only using 'new':
        ~Expression() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        Expression(const Expression &);
        Expression &operator=(const Expression &);
    };
} // namespace CXXR

namespace R
{
    /** @brief Print expression.
     *
     * @note Formely defined in main.cpp and eval.cpp.
     */
    void PrintCall(SEXP call, SEXP rho);
} // namespace R

extern "C"
{
    /** @brief Create a CXXR::Expression with a specified car and tail.
     *
     * This function protects its arguments from the garbage collector.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_lcons(SEXP cr, SEXP tl);

    /** @note Language based list constructs.  These are identical to the list
     * constructs, but the results can be evaluated.
     */
    SEXP Rf_lang1(SEXP s);
    SEXP Rf_lang2(SEXP s, SEXP t);
    SEXP Rf_lang3(SEXP s, SEXP t, SEXP u);
    SEXP Rf_lang4(SEXP s, SEXP t, SEXP u, SEXP v);
    SEXP Rf_lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w);
    SEXP Rf_lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x);

    /** @brief Is an object "language" expression?
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if s is a null pointer or a language expression.
     *
     * @note DIFFERENT than R's  is.language(.) in ../main/coerce.cpp [do_is(), case 301:]
     *                                    which is   <=>  SYMSXP || LANGSXP || EXPRSXP
     */
    Rboolean Rf_isLanguage(SEXP s);
} // extern "C"

#endif /* EXPRESSION_HPP */
