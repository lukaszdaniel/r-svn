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

/** @file RAllocStack.cpp
 *
 * Implementation of class RAllocStack and related functions.
 */

#include <stdexcept>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/MemoryBank.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &vmaxgetptr = vmaxget;
        const auto &vmaxsetptr = vmaxset;
    } // namespace ForceNonInline

    unsigned int RAllocStack::SchwarzCounter::s_count = 0;
    RAllocStack::Stack *RAllocStack::s_stack = nullptr; // usually up to 5 elements
    RAllocStack::Scope *RAllocStack::s_innermost_scope = nullptr;

    void RAllocStack::Scope::nestingError()
    {
        throw std::runtime_error("Fatal error: RAllocStack::Scope objects must be destroyed in reverse order of creation");
    }

    void *RAllocStack::allocate(std::size_t sz)
    {
        void *block = MemoryBank::allocate(sz);
        try
        {
            s_stack->emplace(sz, block);
        }
        catch (...)
        {
            MemoryBank::deallocate(block, sz);
            throw;
        }
        return block;
    }

    void RAllocStack::initialize()
    {
        if (s_stack)
        {
            throw std::runtime_error("RAllocStack is already initialized.");
        }

        s_stack = new Stack();
    }

    void RAllocStack::restoreSize(std::size_t new_size)
    {
        if (new_size > s_stack->size())
            throw std::out_of_range("RAllocStack::restoreSize: requested size greater than current size.");

#ifndef NDEBUG
        if (s_innermost_scope && new_size < s_innermost_scope->startSize())
            throw std::out_of_range("RAllocStack::restoreSize: requested size too small for current scope.");
#endif
        trim(new_size);
    }

    void RAllocStack::trim(std::size_t new_size)
    {
        while (s_stack->size() > new_size)
        {
            Pair &top = s_stack->top();
            MemoryBank::deallocate(top.second, top.first);
            s_stack->pop();
        }
    }
} // namespace CXXR

// ***** C interface *****
