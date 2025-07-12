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
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file ProtectStack.cpp
 *
 * Implementation of class ProtectStack and associated C
 * interface.
 */

#include <CXXR/ProtectStack.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &protectptr = Rf_protect;
        const auto &unprotectptr = Rf_unprotect;
        const auto &unprotect_ptrptr = Rf_unprotect_ptr;
        const auto &ProtectWithIndexptr = R_ProtectWithIndex;
        const auto &Reprotectptr = R_Reprotect;
    } // namespace ForceNonInline

    std::vector<SEXP> ProtectStack::s_stack;

    void ProtectStack::restoreSize(size_t new_size)
    {
        if (new_size > s_stack.size())
            throw std::out_of_range("ProtectStack::restoreSize: requested size greater than current size.");
        s_stack.resize(new_size);
    }

    void ProtectStack::initialize(size_t initial_capacity)
    {
        s_stack.reserve(initial_capacity);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****
