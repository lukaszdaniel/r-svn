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
    // std::unique_ptr<RAllocStack::Stack> RAllocStack::s_stack; // usually up to 5 elements
    RAllocStack::Stack *RAllocStack::s_stack = nullptr;
    RAllocStack::Scope *RAllocStack::s_innermost_scope = nullptr;

    void *RAllocStack::allocate(size_t sz)
    {
        Pair pr(sz, MemoryBank::allocate(sz));
        s_stack->push(pr);
        return s_stack->top().second;
    }

    void RAllocStack::initialize()
    {
        if (s_stack)
        {
            throw std::runtime_error("RAllocStack is already initialized.");
        }

        s_stack = new Stack(); // std::make_unique<Stack>();
    }

    void RAllocStack::restoreSize(size_t new_size)
    {
        if (new_size > s_stack->size())
            throw std::out_of_range("RAllocStack::restoreSize: requested size greater than current size.");
#define NDEBUG // for as long as R_restore_globals() uses vmaxset()
#ifndef NDEBUG
        if (s_innermost_scope && new_size < s_innermost_scope->startSize())
            throw std::out_of_range("RAllocStack::restoreSize: requested size too small for current scope.");
#endif
        trim(new_size);
    }

    void RAllocStack::trim(size_t new_size)
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
