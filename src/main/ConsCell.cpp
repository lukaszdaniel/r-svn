/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file ConsCell.cpp
 *
 * @brief Class CXXR::ConsCell and associated C interface.
 */

#include <CXXR/ConsCell.hpp>
#include <R_ext/Error.h>
#include <Defn.h> // for ASSIGNMENT_PENDING, SET_ASSIGNMENT_PENDING
#include <Rinternals.h> // for ForceNonInline

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &CAD4Rptr = CAD4R;
        const auto &CADDDRptr = CADDDR;
        const auto &CADDRptr = CADDR;
        const auto &CADRptr = CADR;
        const auto &CDARptr = CDAR;
        const auto &CDDRptr = CDDR;
        const auto &CDDDRptr = CDDDR;
        const auto &CDRptr = CDR;
    } // namespace ForceNonInline

    RObject *ConsCell::car() const
    {
        if (hasUnexpandedValue())
            Rf_error("bad binding access: %d", underlyingType());
        return u.listsxp.m_car;
    }

    bool ConsCell::assignmentPending() const
    {
        return ASSIGNMENT_PENDING(this);
    }

    void ConsCell::setAssignmentPending(bool on)
    {
        SET_ASSIGNMENT_PENDING(this, on);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

