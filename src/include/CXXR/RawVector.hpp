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

/** @file RawVector.hpp
 * @brief Class CXXR::RawVector and associated C interface.
 */

#ifndef RAWVECTOR_HPP
#define RAWVECTOR_HPP

#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief Vector of 'raw bytes'.
     */
    using RawVector = FixedVector<Rbyte, RAWSXP>;
} // namespace CXXR

namespace R
{
    Rbyte SCALAR_BVAL(SEXP x);
    void SET_SCALAR_BVAL(SEXP x, Rbyte v);
} // namespace R

extern "C"
{
    /** @brief Is this a raw vector?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a raw vector.
     */
    Rboolean Rf_isRaw(SEXP s);

    /**
     * @param x Pointer to a CXXR::RawVector (i.e. a RAWSXP).  An error is
     *          generated if \a x is not a non-null pointer to a
     *          CXXR::RawVector.
     *
     * @return Pointer to element 0 of \a x.
     */
    Rbyte *(RAW)(SEXP x);

    /**
     * @param x Pointer to a CXXR::RawVector (i.e. a RAWSXP).  An error is
     *          generated if \a x is not a non-null pointer to a
     *          CXXR::RawVector.
     *
     * @return Pointer to constant element 0 of \a x.
     */
    const Rbyte *(RAW_RO)(SEXP x);
    Rbyte *RAW0(SEXP x);

    const Rbyte *RAW_OR_NULL(SEXP x);
    Rbyte RAW_ELT(SEXP x, R_xlen_t i);
    void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v);
    SEXP Rf_ScalarRaw(Rbyte x);
} // extern "C"

#endif // RAWVECTOR_HPP
