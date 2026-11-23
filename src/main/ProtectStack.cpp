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
#include <CXXR/RContext.hpp>
#include <Localization.h>
#include <R_ext/Error.h>
#include <Defn.h> // for R_CHECK_THREAD

#define PP_REDZONE_SIZE 1000L

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

#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
    std::vector<std::pair<RObject *, RCNTXT *>> ProtectStack::s_stack;
#else
    std::vector<SEXP> ProtectStack::s_stack;
#endif
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

        // ProtectStack plus a red zone to avoid stack overflow
        s_stack.reserve(initial_capacity + PP_REDZONE_SIZE);
        s_reserved_capacity = initial_capacity + PP_REDZONE_SIZE;
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
#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
            [&](const auto &entry) { return entry.first == node; });
#else
            [&](SEXP q) { return q == node; });
#endif
        if (it == ProtectStack::s_stack.rend())
            return std::pair(false, 0);

        unsigned int index = R_PPStackTop - 1 - (it - ProtectStack::s_stack.rbegin());
        return std::pair(true, index);
    }

    /* "protect" push a single argument onto R_PPStack */

    /* In handling a stack overflow we have to be careful not to use
       PROTECT. error("protect(): stack overflow") would call deparse1,
       which uses PROTECT and segfaults.*/

    /* However, the traceback creation in the normal error handler also
       does a PROTECT, as does the jumping code, at least if there are
       cleanup expressions to handle on the way out.  So for the moment
       we'll allocate a slightly larger PP stack and only enable the added
       red zone during handling of a stack overflow error.  LT */

    NORET void ProtectStack::R_signal_protect_error(void)
    {
        size_t oldpps = size();
        const size_t R_RealPPStackSize = reservedCapacity();

        RCNTXT cntxt(CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
            R_NilValue, R_NilValue);

        try {
        /* condition is pre-allocated and protected with R_PreserveObject */
            SEXP cond = R::R_getProtectStackOverflowError();

            if (size() < R_RealPPStackSize) {
                s_stack.resize(R_RealPPStackSize);
                /* allow calling handlers */
                R::R_signalErrorCondition(cond, R_NilValue);
            }

            /* calling handlers at this point might produce a C stack
               overflow/SEGFAULT so treat them as failed and skip them */
            R::R_signalErrorConditionEx(cond, R_NilValue, TRUE);
        }
        catch (...) {
            s_stack.resize(oldpps);
            throw;
        }
        endcontext(&cntxt); /* not reached */
    }

    NORET void ProtectStack::R_signal_unprotect_error(void)
    {
        error(n_("unprotect(): only %td protected item",
            "unprotect(): only %td protected items", R_PPStackTop),
            R_PPStackTop);
    }

    unsigned int ProtectStack::protect_(SEXP s)
    {
        R_CHECK_THREAD;
        if (size() + PP_REDZONE_SIZE >= reservedCapacity())
            R_signal_protect_error();

#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
        s_stack.push_back(std::make_pair(s, R_GlobalContext));
#else
        s_stack.push_back(s);
#endif
        return s_stack.size() - 1;
    }

        /* "unprotect" pop argument list from top of R_PPStack */

    void ProtectStack::unprotect_(unsigned int count)
    {
        R_CHECK_THREAD;
        size_t sz = s_stack.size();
        if (sz < count)
            R_signal_unprotect_error();
#ifndef NDEBUG
        if (s_innermost_scope && sz - count < s_innermost_scope->startSize())
            throw std::logic_error("ProtectStack::unprotect: too many unprotects in this scope.");
#endif

#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
        for (unsigned int i = 0; i < count; ++i)
        {
            if (s_stack[sz - 1 - i].second != R_GlobalContext)
            {
                throw std::logic_error("ProtectStack::unprotect: not in same context as the corresponding call of protect().");
            }
        }
#endif
        s_stack.resize(sz - count);
    }

    /* "Rf_unprotect_ptr" remove pointer from somewhere in R_PPStack */

    void ProtectStack::unprotectPtr(SEXP s)
    {
        R_CHECK_THREAD;
#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
        auto rit = std::find_if(s_stack.rbegin(), s_stack.rend(),
            [&](const auto &entry) { return s == entry.first; });
#else
        auto rit = std::find_if(s_stack.rbegin(), s_stack.rend(),
            [&](SEXP q) { return s == q; });
#endif
        if (rit == s_stack.rend())
            throw std::invalid_argument("ProtectStack::unprotectPtr: pointer not found.");

        // See Josuttis p.267 for the need for -1.
        auto it = rit.base() - 1;
#ifndef NDEBUG
        if (s_innermost_scope &&
            static_cast<std::size_t>(it - s_stack.begin()) < s_innermost_scope->startSize())
            throw std::logic_error("ProtectStack::unprotectPtr: pointer protected outside this scope.");
#endif
#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
        const auto index = static_cast<std::size_t>(it - s_stack.begin());
        if (s_stack[index].second != R_GlobalContext)
            throw std::logic_error("ProtectStack::unprotectPtr: not in same context as the corresponding call of protect().");
#endif
        s_stack.erase(it);
    }

    NORET void ProtectStack::R_signal_reprotect_error(PROTECT_INDEX i)
    {
        error(n_("R_Reprotect: only %td protected item, can't reprotect index %d",
            "R_Reprotect: only %td protected items, can't reprotect index %d",
            R_PPStackTop),
            R_PPStackTop, i);
    }

    void ProtectStack::reprotect(SEXP s, PROTECT_INDEX i)
    {
        R_CHECK_THREAD;
        if (i >= s_stack.size())
            R_signal_reprotect_error(i);
#ifndef NDEBUG
        if (s_innermost_scope && i < s_innermost_scope->startSize())
            throw std::logic_error("ProtectStack::reprotect: pointer protected outside this scope.");
#endif

#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
        if (s_stack[i].second != R_GlobalContext)
            throw std::logic_error("ProtectStack::reprotect: not in same context as the corresponding call of protect().");
        s_stack[i] = std::make_pair(s, R_GlobalContext);
#else
        s_stack[i] = s;
#endif
    }

    void ProtectStack::visitRoots(GCNode::const_visitor *v)
    {
        for (auto &el : s_stack)
        {
#ifdef CXXR_CHECK_PROTECT_CONTEXT_MATCH
            if (el.first != R_NilValue) (*v)(el.first);
#else
            if (el != R_NilValue) (*v)(el);
#endif
        }
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
