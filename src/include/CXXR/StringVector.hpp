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

/** @file StringVector.hpp
 * @brief Class CXXR::StringVector and associated C interface.
 *
 * (StringVector implements STRSXP.)
 */

#ifndef STRINGVECTOR_HPP
#define STRINGVECTOR_HPP

#include <CXXR/String.hpp>
#include <CXXR/FixedVector.hpp>

namespace CXXR
{
    /** @brief Vector of strings.
     *
     * Note that the <tt>StringVector(size_type)</tt> constructor will
     * fill the constructed vector with blank strings rather than
     * with NULL.
     */
    using StringVector = FixedVector<GCEdge<String>, STRSXP>;
} // namespace CXXR

namespace R
{
    void ssort(CXXR::StringVector *sv, int n);
} // namespace R

extern "C"
{
    /** @brief Is this a string vector?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a string vector.
     */
    Rboolean Rf_isString(SEXP s);

    /** @brief Set element of CXXR::StringVector.
     *
     * @param x Non-null pointer to a CXXR::StringVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @param v Non-null pointer to CXXR::String representing the new value.
     */
    void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);

    /** @brief Examine element of a CXXR::StringVector.
     *
     * @param x Non-null pointer to a CXXR::StringVector.  An error is
     *          raised if \a x is not a pointer to a CXXR::StringVector.
     *
     * @param i Index of the required element.  There is no bounds checking.
     *
     * @return Pointer to extracted \a i 'th element.
     */
    SEXP STRING_ELT(SEXP x, R_xlen_t i);

    /**
     * @param x Pointer to a CXXR::StringVector; an error is raised if \a x
     *          is not a pointer to a CXXR::StringVector.
     *
     * @return Pointer to the start of \a x 's data, interpreted (riskily)
     *         as an array of CXXR::String*.
     *
     * @deprecated This function puts the integrity of the write barrier
     * at the mercy of callers.  It is deliberately not made visible
     * to C code.
     */
    SEXP *(STRING_PTR)(SEXP x);

    /**
     * @param x Pointer to a CXXR::StringVector; an error is raised if \a x
     *          is not a pointer to a CXXR::StringVector.
     *
     * @return Constant pointer to the start of \a x 's data, interpreted (riskily)
     *         as an array of CXXR::String*.
     *
     * @deprecated This function puts the integrity of the write barrier
     * at the mercy of callers.  It is deliberately not made visible
     * to C code.
     */
    const SEXP *(STRING_PTR_RO)(SEXP x);

    /** @brief Obtaing index of a string vector
     *
     * @return index of a given C string in (translated) R string vector
     */
    int Rf_stringPositionTr(SEXP string, const char *translatedElement);

    Rboolean Rf_isValidString(SEXP x);

    /* non-empty ("") valid string :*/
    Rboolean Rf_isValidStringF(SEXP x);
    SEXP Rf_ScalarString(SEXP x);
} // extern "C"

#endif // STRINGVECTOR_HPP
