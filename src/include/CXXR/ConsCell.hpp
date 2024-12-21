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

/** @file ConsCell.hpp
 * @brief Class CXXR::ConsCell and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, this
 * file also includes the definition of class CXXR::PairList.
 *
 * This file includes C functions for examining and setting the CAR
 * and TAG of a CXXR::ConsCell; functions for examining and setting
 * the CDR, and other operations accessing the tail of the list, are
 * to be found in PairList.hpp.
 */

#ifndef CONSCELL_HPP
#define CONSCELL_HPP

#include <CXXR/RObject.hpp>

namespace CXXR
{
    /** @brief Element of a singly linked list.
     *
     * Element of a LISP-like singly-linked list, containing pointers
     * to a 'car' object (this is LISP terminology, and has nothing to
     * do with automobiles) and to a 'tag' object, as well as a
     * pointer to the next element of the list, which must be of the
     * derived type PairList.  (Any of these pointers may be null.)
     *
     * @note This class is used as a base class to implement CR's
     * LISTSXP, LANGSXP, DOTSXP.
     * Because what these SEXPTYPEs have in common is implementation
     * rather than meaning in the application domain, canons of
     * object-oriented design would argue against their publicly
     * inheriting from a common base class.  Without doing this,
     * however, it would have been difficult efficiently to implement
     * functions such as CAR(), which are ubiquitous in the CR code.
     */
    class ConsCell : public RObject
    {
    public:
        ConsCell(SEXPTYPE stype, SEXP cr, SEXP tl, SEXP tg) : RObject(stype)
        {
            u.listsxp.m_car = cr;
            u.listsxp.m_tail = tl;
            u.listsxp.m_tag = tg;
        }

        /** @brief Is an RObject a ConsCell?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a ConsCell.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == LISTSXP || st == LANGSXP || st == DOTSXP;
        }

    protected:
        // Declared protected to ensure that ConsCell objects are
        // allocated only using 'new':
        ~ConsCell() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        ConsCell(const ConsCell &);
        ConsCell &operator=(const ConsCell &);
    };
} // namespace CXXR

namespace R
{
    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    void SET_MISSING(SEXP x, unsigned int v);

    SEXP CONS_NR(SEXP a, SEXP b);
    SEXPTYPE BNDCELL_TAG(SEXP cell);
    void SET_BNDCELL_TAG(SEXP cell, SEXPTYPE val);
    double BNDCELL_DVAL(SEXP cell);
    int BNDCELL_IVAL(SEXP cell);
    int BNDCELL_LVAL(SEXP cell);
} // namespace R

extern "C"
{
    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    int MISSING(SEXP x);

    /** @brief Get tag of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    SEXP TAG(SEXP e);

    /** @brief Set the tag of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new tag of
     *          the CXXR::ConsCell.
     */
    void SET_TAG(SEXP x, SEXP y);

    /** @brief Create an object of a type derived from CXXR::ConsCell.
     *
     * The object is created with null car, tag and tail pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);

    /** @brief Obtain the n-th element of CXXR::ConsCell.
     *
     * @param s Pointer to a CXXR::ConsCell (checked).
     *
     * @param n n-th element to be extracted from CXXR:ConsCell
     *
     * @return Pointer to the n-th element.
     */
    SEXP Rf_nthcdr(SEXP s, int n);
} // extern "C"

#endif /* CONSCELL_HPP */
