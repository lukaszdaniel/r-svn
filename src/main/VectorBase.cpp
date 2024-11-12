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
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
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

/** @file VectorBase.cpp
 *
 * @brief Implementation of class VectorBase and related functions.
 */

#include <CXXR/Logical.hpp>
#include <CXXR/Complex.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/VectorBase.hpp>
#include <Defn.h> // for ForceNonInline

using namespace R;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
#ifdef TESTING_WRITE_BARRIER
        const auto &STDVEC_LENGTHptr = R::STDVEC_LENGTH;
        const auto &STDVEC_TRUELENGTHptr = R::STDVEC_TRUELENGTH;
        const auto &SETALTREPptr = R::SETALTREP;
#endif
        const auto &SET_TRUELENGTHptr = SET_TRUELENGTH;
        const auto &ALTREPptr = ALTREP;
    } // namespace ForceNonInline

    namespace
    {
        inline R_size_t getVecSizeInVEC(VectorBase *s)
        {
            if (IS_GROWABLE(s))
                SET_STDVEC_LENGTH(s, XTRUELENGTH(s));

            R_size_t size;
            switch (TYPEOF(s))
            { /* get size in bytes */
            case CHARSXP:
                size = (XLENGTH(s) + 1) * sizeof(char);
                break;
            case RAWSXP:
                size = XLENGTH(s) * sizeof(Rbyte);
                break;
            case LGLSXP:
                size = XLENGTH(s) * sizeof(Logical);
                break;
            case INTSXP:
                size = XLENGTH(s) * sizeof(int);
                break;
            case REALSXP:
                size = XLENGTH(s) * sizeof(double);
                break;
            case CPLXSXP:
                size = XLENGTH(s) * sizeof(Complex);
                break;
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
                size = XLENGTH(s) * sizeof(SEXP);
                break;
            default:
                size = 0;
            }
            return BYTE2VEC(size);
        }
    }

    VectorBase::~VectorBase()
    {
        if (u.vecsxp.m_data)
        {
            R_size_t n_doubles = getVecSizeInVEC(this);
            MemoryBank::deallocate(u.vecsxp.m_data, n_doubles * sizeof(double), sxpinfo.gccls);
        }
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

