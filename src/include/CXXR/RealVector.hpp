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

/** @file RealVector.hpp
 * @brief Class CXXR::RealVector and associated C interface.
 */

#ifndef REALVECTOR_HPP
#define REALVECTOR_HPP

#include <CXXR/ElementTraits.hpp>
#include <CXXR/FixedVector.hpp>
#include <R_ext/Arith.h> // for NA_REAL

namespace CXXR
{
    /** @brief Vector of real numbers.
     */
    using RealVector = FixedVector<double, REALSXP>;

    template <>
    struct VectorTypeFor<double>
    {
        typedef RealVector type;
    };

    // Template specializations of ElementTraits:
    namespace ElementTraits
    {
        template <>
        struct MustConstruct<double>: public std::false_type
        {
        };

        template <>
        struct MustDestruct<double>: public std::false_type
        {
        };

        template <>
        inline const double &NAFunc<double>::operator()() const
        {
            static double s_na = NA_REAL;
            return s_na;
        }

        template <>
        inline bool IsNA<double>::operator()(const double &t) const
        {
            return R_IsNA(t);
        }

        template <>
        inline bool IsNaOrNaN<double>::operator()(const double &t) const
        {
            return std::isnan(t);
        }
    } // namespace ElementTraits
} // namespace CXXR

namespace R
{
    double SCALAR_DVAL(SEXP x);
    void SET_SCALAR_DVAL(SEXP x, double v);
    double *REAL0(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Is this a real vector?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a real vector.
     */
    Rboolean Rf_isReal(SEXP s);

    /**
     * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          RealVector.
     *
     * @return Pointer to element 0 of \a x.
     */
    double *(REAL)(SEXP x);

    /**
     * @param x Pointer to an \c RealVector (i.e. an R numeric vector).
     *          An error is generated if \a x is not a non-null pointer to an \c
     *          RealVector.
     *
     * @return Pointer to constant element 0 of \a x.
     */
    const double *(REAL_RO)(SEXP x);
    const double *REAL_OR_NULL(SEXP x);
    double REAL_ELT(SEXP x, R_xlen_t i);
    void SET_REAL_ELT(SEXP x, R_xlen_t i, double v);
    SEXP Rf_ScalarReal(double x);
} // extern "C"

#endif // REALVECTOR_HPP
