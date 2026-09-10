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

#include <algorithm>
#include <stdexcept>
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
    size_t ProtectStack::s_reserved_capacity = 0;
    ProtectStack::Scope *ProtectStack::s_innermost_scope = nullptr;

    void ProtectStack::Scope::nestingError()
    {
        throw std::runtime_error("Fatal error: ProtectStack::Scope objects must be destroyed in reverse order of creation");
    }

    void ProtectStack::initialize(size_t initial_capacity)
    {
        static bool s_initialized = false;
        if (s_initialized)
        {
            throw std::runtime_error("ProtectStack is already initialized.");
        }
        s_initialized = true;

        s_stack.reserve(initial_capacity);
        s_reserved_capacity = initial_capacity;
    }

    void ProtectStack::restoreSize(size_t new_size)
    {
        if (new_size > s_stack.size())
            throw std::out_of_range("ProtectStack::restoreSize: requested size greater than current size.");
        s_stack.resize(new_size);
    }

    std::pair<bool, unsigned int> ProtectStack::isProtected(RObject *node)
    {
        auto it = std::find_if(ProtectStack::s_stack.rbegin(),
            ProtectStack::s_stack.rend(),
            [&](SEXP q) { return q == node; });
        if (it == ProtectStack::s_stack.rend())
            return std::pair(false, 0);

        unsigned int index = R_PPStackTop - 1 - (it - ProtectStack::s_stack.rbegin());
        return std::pair(true, index);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    CXXR::ProtectStack::restoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return CXXR::ProtectStack::size();
}
