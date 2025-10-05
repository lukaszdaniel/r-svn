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

/** @file ListVector.hpp
 * @brief Class CXXR::ListVector and associated C interface.
 *
 * (ListVector implements VECSXP.)
 */

#ifndef LISTVECTOR_HPP
#define LISTVECTOR_HPP

#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief General vector of GCEdge<RObject>.
     */
    using ListVector = FixedVector<GCEdge<>, VECSXP>;
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
    /** @brief Set element of CXXR::ListVector.
     *
     * @param x Pointer to a CXXR::ListVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @param v Pointer, possibly null, to CXXR::RObject representing the
     *          new value.
     *
     * @return The new value \a v.
     */
    SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);

    /** @brief Examine element of CXXR::ListVector.
     *
     * @param x Non-null pointer to a CXXR::ListVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @return The value of the \a i 'th element.
     */
    SEXP VECTOR_ELT(SEXP x, R_xlen_t i);
    Rboolean Rf_isNewList(SEXP s);
} // extern "C"

#endif // LISTVECTOR_HPP
