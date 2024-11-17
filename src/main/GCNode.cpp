/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include <stdexcept>
#include <CXXR/GCNode.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>

namespace CXXR
{
    size_t GCNode::s_num_nodes = 0;
    std::unique_ptr<struct GCNode::R_GenHeap_t> GCNode::s_R_GenHeap;

    HOT_FUNCTION void *GCNode::operator new(size_t bytes)
    {
        return memset(MemoryBank::allocate(bytes), 0, bytes);
    }

    void GCNode::operator delete(void *pointer, size_t bytes)
    {
        MemoryBank::deallocate(pointer, bytes, static_cast<GCNode *>(pointer)->sxpinfo.gccls);
    }

    GCNode::GCNode() : sxpinfo(NILSXP), m_next(this), m_prev(this)
    {
    }

    GCNode::GCNode(SEXPTYPE stype) : sxpinfo(stype), m_next(this), m_prev(this)
    {
        ++s_num_nodes;
        R_GenHeap->m_New->splice(this);
    }

    GCNode::~GCNode()
    {
        unsnap();
        --s_num_nodes;
    }

    void initializeMemorySubsystem()
    {
        static bool initialized = false;
        if (!initialized)
        {
            GCNode::initialize();
            GCStackRootBase::initialize();
            ProtectStack::initialize();

            initialized = true;
        }
    }

    void GCNode::cleanup()
    {
    }

    void GCNode::initialize()
    {
        if (s_R_GenHeap)
        {
            throw std::runtime_error("R_GenHeap is already initialized.");
        }
        s_R_GenHeap = std::make_unique<struct GCNode::R_GenHeap_t>();

        for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++)
        {
            R_GenHeap->m_Old[gen] = std::make_unique<GCNode>(/* OldPeg constructor */);
#ifndef EXPEL_OLD_TO_NEW
            R_GenHeap->m_OldToNew[gen] = std::make_unique<GCNode>(/* OldToNewPeg constructor */);
#endif

            R_GenHeap->m_OldCount[gen] = 0;
        }
        R_GenHeap->m_New = std::make_unique<GCNode>(/* NewPeg constructor */);
    }
} // namespace CXXR
