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
#include <Localization.h>
#include <Defn.h> // for ForceNonInline

#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
#endif

using namespace R;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &STDVEC_LENGTHptr = R::STDVEC_LENGTH;
        const auto &STDVEC_TRUELENGTHptr = R::STDVEC_TRUELENGTH;
        const auto &SETALTREPptr = R::SETALTREP;
        const auto &SET_TRUELENGTHptr = SET_TRUELENGTH;
        const auto &ALTREPptr = ALTREP;
    } // namespace ForceNonInline

    namespace
    {
        inline R_size_t getVecSizeInBytes(VectorBase *s)
        {
#ifdef PROTECTCHECK
            if (s->sexptype() == FREESXP)
            {
                s->sxpinfo.type = SEXPTYPE(s->sxpinfo.gp);
            }
#endif
            if (IS_GROWABLE(s))
            {
                s->u.vecsxp.m_length = s->truelength();
                s->sxpinfo.scalar = (s->u.vecsxp.m_length == 1);
            }

            R_size_t size = 0;
            R_size_t n_elem = s->size();
            switch (TYPEOF(s))
            { /* get size in bytes */
            case CHARSXP:
                size = (n_elem + 1) * sizeof(char);
                break;
            case RAWSXP:
                size = n_elem * sizeof(Rbyte);
                break;
            case LGLSXP:
                size = n_elem * sizeof(Logical);
                break;
            case INTSXP:
                size = n_elem * sizeof(int);
                break;
            case REALSXP:
                size = n_elem * sizeof(double);
                break;
            case CPLXSXP:
                size = n_elem * sizeof(Complex);
                break;
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
                size = n_elem * sizeof(SEXP);
                break;
            default:
                size = 0;
            }
            return size;
        }
    }

    VectorBase::~VectorBase()
    {
        if (u.vecsxp.m_data)
        {
            R_size_t databytes = getVecSizeInBytes(this);
            MemoryBank::deallocate(u.vecsxp.m_data, databytes, sxpinfo.gccls);
        }
    }

    // The error messages here match those used by CR (as of 3.0.2),
    // not including the malformed unit abbreviations.
    void VectorBase::tooBig(size_type bytes)
    {
        if (bytes > Giga)
            Rf_errorcall(R_NilValue,
                      _("cannot allocate vector of size %0.1f %s"),
                      bytes / Giga, "GB");
        if (bytes > Mega)
            Rf_errorcall(R_NilValue,
                      _("cannot allocate vector of size %0.1f %s"),
                      bytes / Mega, "MB");
        else
            Rf_errorcall(R_NilValue,
                      _("cannot allocate vector of size %0.f %s"),
                      bytes / Kilo, "KB");
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

