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

/** @file NodeStack.cpp
 *
 * Implementation of class NodeStack.
 */

#include <iostream>
#include <algorithm>
#include <CXXR/NodeStack.hpp>

namespace CXXR
{
    // ***** NodeStack *****

    void NodeStack::Scope::nestingError()
    {
        std::cerr << "Fatal error: NodeStack::Scope objects must be destroyed in reverse order of creation\n";
        abort();
    }

    NodeStack::NodeStack(size_t initial_capacity)
        : m_deferred_protected_count(0), m_reserved_capacity(initial_capacity), m_protected_count(0), m_innermost_scope(nullptr)
    {
        m_vector.reserve(initial_capacity);
        m_R_BCNodeStackTop = m_vector.data();
    }

    void NodeStack::pop(unsigned int count)
    {
#ifndef NDEBUG
        size_t sz = size();
        if (count > sz)
            throw std::out_of_range("NodeStack::pop(): count greater than current stack size.");
        if (m_innermost_scope && sz - count < m_innermost_scope->startSize())
            throw std::logic_error("NodeStack::unprotect: too many unprotects in this scope.");
#endif
        m_R_BCNodeStackTop -= count;
    }

    void NodeStack::protectAll()
    {
        m_deferred_protected_count = std::distance(m_vector.data(), m_R_BCNodeStackTop);
    }

    void NodeStack::retarget(RObject *node, size_t index)
    {
        retarget(node_t(0, node), index);
    }

    void NodeStack::retarget(node_t node, size_t index)
    {
#ifndef NDEBUG
        if (index >= m_vector.size())
            throw std::out_of_range("NodeStack::retarget(): index out of range.");
#endif
        if (index < m_protected_count)
        {
#ifdef TESTING_WRITE_BARRIER
            Rf_warning("changing stack value below current protected count");
#endif
            retarget_aux(m_vector[index], node);
        }

        m_vector[index] = node;
    }

    void NodeStack::inclnk_stack(size_t top)
    {
        m_deferred_protected_count = top;
    }

    void NodeStack::inclnk_stack_commit(void)
    {
        if (m_protected_count < m_deferred_protected_count) {
            node_t *base = m_vector.data() + m_protected_count;
            node_t *top = m_vector.data() + m_deferred_protected_count;
            for (node_t *p = base; p < top; p++) {
                if (p->tag == RAWMEM_TAG || p->tag == CACHESZ_TAG)
                    p += p->u.ival;
                else if (p->tag == 0)
                    GCNode::incRefCount(p->u.sxpval);
            }
            m_protected_count = m_deferred_protected_count;
        }
    }

    void NodeStack::declnk_stack(size_t base)
    {
        if (base < m_protected_count)
        {
            std::vector<node_t>::iterator start = m_vector.begin() + std::ptrdiff_t(base);
            std::vector<node_t>::iterator end = m_vector.begin() + std::ptrdiff_t(m_protected_count);
            for (std::vector<node_t>::iterator p = start; p != end; ++p)
            {
                if (p->tag == RAWMEM_TAG || p->tag == CACHESZ_TAG)
                    p += p->u.ival;
                else if (p->tag == 0)
                    GCNode::decRefCount(p->u.sxpval);
            }
            m_protected_count = base;
        }
        m_deferred_protected_count = base;
    }

    void NodeStack::retarget_aux(node_t oldnode, node_t newnode)
    {
        if (newnode.tag == 0)
            GCNode::incRefCount(newnode.u.sxpval);
        if (oldnode.tag == 0)
            GCNode::decRefCount(oldnode.u.sxpval);
    }

    void NodeStack::resize_aux(size_t new_size)
    {
        m_vector.resize(m_protected_count);
        while (m_vector.size() > new_size)
        {
            node_t node = m_vector.back();
            if (node.tag == 0)
                GCNode::decRefCount(node.u.sxpval);
            m_vector.pop_back();
        }
        m_protected_count = new_size;
        m_deferred_protected_count = new_size;
    }

    void NodeStack::visitRoots(GCNode::const_visitor *v)
    {
        for (node_t *sp = m_vector.data(); sp < m_R_BCNodeStackTop; sp++) {
            if (sp->tag == RAWMEM_TAG)
                sp += sp->u.ival;
            else if ((sp->tag == NILSXP || IS_PARTIAL_SXP_TAG(sp->tag)) && sp->u.sxpval != R_NilValue)
                (*v)(sp->u.sxpval);
        }
    }
} // namespace CXXR
