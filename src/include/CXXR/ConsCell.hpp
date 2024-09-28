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
