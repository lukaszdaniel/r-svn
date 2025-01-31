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

/** @file GCRoot.cpp
 *
 * Implementation of class GCRootBase.
 */

#include <CXXR/GCRoot.hpp>

namespace CXXR
{
    // Force generation of non-inline embodiments of functions in the C
    // interface:
    namespace ForceNonInline
    {
    } // namespace ForceNonInline

    GCRootBase *GCRootBase::s_list_head = nullptr;

    GCRootBase::GCRootBase(const GCNode *node)
    {
        m_next = s_list_head;
        m_prev = nullptr;
        if (m_next)
        {
            m_next->m_prev = this;
        }
        s_list_head = this;

        m_pointer = node;
    }

    void GCRootBase::visitRoots(GCNode::const_visitor *v)
    {
        for (GCRootBase *node = s_list_head; node; node = node->m_next)
        {
            if (node->ptr())
                (*v)(node->ptr());
        }
    }
} // namespace CXXR

// ***** C interface *****
