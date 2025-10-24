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

/** @file Complex.hpp
 * @brief Class CXXR::Complex.
 */

#ifndef CXXR_COMPLEX_HPP
#define CXXR_COMPLEX_HPP

#include <complex>
#include <CXXR/ElementTraits.hpp>
#include <R_ext/Complex.h>
#include <R_ext/Arith.h> // for NA_REAL

namespace CXXR
{
    /** @brief CXXR's extension of CR's Rcomplex.
     *
     * This class is a wrapper around the C struct Rcomplex defined by
     * CR.
     *
     * @note Backwards compatibility requires that <tt>sizeof(Complex)
     * == sizeof(Rcomplex)</tt>.
     */
    struct Complex: public Rcomplex
    {
        /** @brief Default constructor.
         *
         * Leaves data fields uninitialised.
         */
        Complex()
        {
        }

        /** @brief Primary constructor.
         *
         * @param rl Real part.
         *
         * @param im Imaginary part.
         */
        Complex(double rl, double im = 0.0)
        {
            r = rl;
            i = im;
        }

        explicit Complex(const std::complex<double> &z): Complex(z.real(), z.imag()) {}
        explicit Complex(const Rcomplex &z): Complex(z.r, z.i) {}

        /** @brief Assignment from T.
         *
         * @param rhs Value to be assigned.
         *
         * @return Reference to this object.
         *
         * @note std::complex has its own specialization.
         */
        template <typename T>
        Complex &operator=(const T &rhs)
        {
            r = rhs;
            i = 0;
            return *this;
        }

        Complex operator+(const Complex &obj)
        {
            Complex res;
            res.r = r + obj.r;
            res.i = i + obj.i;
            return res;
        }

        Complex operator-(const Complex &obj)
        {
            Complex res;
            res.r = r - obj.r;
            res.i = i - obj.i;
            return res;
        }

        Complex operator*(const Complex &obj)
        {
            std::complex<double> lhs(r, i);
            std::complex<double> rhs(obj.r, obj.i);
            return Complex(lhs * rhs);
        }

        Complex operator/(const Complex &obj)
        {
            std::complex<double> lhs(r, i);
            std::complex<double> rhs(obj.r, obj.i);
            return Complex(lhs / rhs);
        }

        bool operator==(const Complex &rhs)
        {
            return (r == rhs.r) && (i == rhs.i);
        }

        bool operator!=(const Complex &rhs)
        {
            return !(*this == rhs);
        }

        Complex &operator+=(const Complex &y)
        {
            r += y.r;
            i += y.i;
            return *this;
        }

        Complex &operator-=(const Complex &y)
        {
            r -= y.r;
            i -= y.i;
            return *this;
        }

        Complex &operator*=(const Complex &y)
        {
            *this = *this * y;
            return *this;
        }

        Complex &operator/=(const Complex &y)
        {
            *this = *this / y;
            return *this;
        }

        friend std::ostream &operator<<(std::ostream &, const Complex &z);
    };

    // Template specializations of ElementTraits:
    namespace ElementTraits
    {
        template <>
        struct MustConstruct<Complex>: public std::false_type
        {
        };

        template <>
        struct MustDestruct<Complex>: public std::false_type
        {
        };

        template <>
        inline const Complex &NAFunc<Complex>::operator()() const
        {
            static Complex s_na(NA_REAL, NA_REAL);
            return s_na;
        }

        template <>
        inline bool IsNA<Complex>::operator()(const Complex &c) const
        {
            return isNA(c.r) || isNA(c.i);
        }

        template <>
        inline bool IsNaOrNaN<Complex>::operator()(const Complex &c) const
        {
            return isNaOrNaN(c.r) || isNaOrNaN(c.i);
        }
    } // namespace ElementTraits
} // namespace CXXR

#endif // CXXR_COMPLEX_HPP
