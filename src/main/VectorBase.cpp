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

#define USE_RINTERNALS // always use macro versions

#include <CXXR/Logical.hpp>
#include <CXXR/Complex.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/BadObject.hpp>
#include <Localization.h>
#include <Defn.h> // for ForceNonInline

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

    VectorBase::VectorBase(SEXPTYPE stype, size_type n_elem, R_allocator_t *allocator)
        : RObject(stype)
    {
        R_size_t actual_size = 0; // in bytes
        switch (stype)
        {
        case RAWSXP:
            actual_size = n_elem * sizeof(Rbyte);
            break;
        case CHARSXP:
            actual_size = (n_elem + 1) * sizeof(char);
            break;
        case LGLSXP:
            actual_size = n_elem * sizeof(Logical);
            break;
        case INTSXP:
            actual_size = n_elem * sizeof(int);
            break;
        case REALSXP:
            actual_size = n_elem * sizeof(double);
            break;
        case CPLXSXP:
            actual_size = n_elem * sizeof(Complex);
            break;
        case STRSXP:
        case EXPRSXP:
        case VECSXP:
            actual_size = n_elem * sizeof(SEXP);
            break;
        default:
            error(_("unsupported type '%s'"), R::sexptype2char(stype));
            break;
        }

        if (actual_size >= R_SIZE_T_MAX)
        {
            VectorBase::tooBig(actual_size);
        }

        SET_EXT_ALLOCATOR(this, (allocator != nullptr));
    }

    VectorBase::~VectorBase()
    {
    }

    void VectorBase::detachReferents()
    {
        RObject::detachReferents();
    }

    void VectorBase::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
    }

    // The error messages here match those used by CR (as of 3.0.2),
    // not including the malformed unit abbreviations.
    void VectorBase::tooBig(size_type bytes)
    {
        if (bytes > Giga)
            Rf_errorcall(R_NilValue, _("cannot allocate vector of size %0.1f %s"), bytes / Giga, "GB");
        else if (bytes > Mega)
            Rf_errorcall(R_NilValue, _("cannot allocate vector of size %0.1f %s"), bytes / Mega, "MB");
        else
            Rf_errorcall(R_NilValue, _("cannot allocate vector of size %0.f %s"), bytes / Kilo, "KB");
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

