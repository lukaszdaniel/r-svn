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

#include <iostream>
#include <stdexcept>
#include <CXXR/GCNode.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/ProtectStack.hpp>
// #include <CXXR/RAllocStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#ifdef PROTECTCHECK
#include <CXXR/BadObject.hpp>
#endif

namespace CXXR
{
    unsigned int GCNode::SchwarzCounter::s_count = 0;
    size_t GCNode::s_num_nodes = 0;
    std::unique_ptr<CXXR::GCNode> GCNode::s_Old[1 + GCManager::numOldGenerations()];
#ifndef EXPEL_OLD_TO_NEW
    std::unique_ptr<CXXR::GCNode> GCNode::s_OldToNew[1 + GCManager::numOldGenerations()];
#endif
    unsigned int GCNode::s_gencount[1 + GCManager::numOldGenerations()];
    unsigned int GCNode::s_next_gen[1 + GCManager::numOldGenerations()];

    HOT_FUNCTION void *GCNode::operator new(size_t bytes)
    {
        return memset(MemoryBank::allocate(bytes), 0, bytes);
    }

    void GCNode::operator delete(void *pointer, size_t bytes)
    {
        MemoryBank::deallocate(pointer, bytes);
    }

    GCNode::GCNode(): sxpinfo(NILSXP), m_next(this), m_prev(this)
    {
    }

    GCNode::GCNode(SEXPTYPE stype): sxpinfo(stype), m_next(this), m_prev(this)
    {
        ++s_num_nodes;
        ++s_gencount[0];
        s_New->splice(this);
    }

    GCNode::~GCNode()
    {
        unsnap();
        --s_gencount[generation()];
        --s_num_nodes;
    }

    void GCNode::Ager::operator()(const GCNode *node)
    {
        if (node->generation() < m_mingen) // node is younger than the minimum age required
        {
            --s_gencount[node->generation()];
            node->sxpinfo.m_gcgen = m_mingen;
            s_Old[m_mingen]->splice(node);
            ++s_gencount[m_mingen];
            node->visitReferents(this);
        }
    }

    void GCNode::CountingMarker::operator()(const GCNode *node)
    {
        if (node->isMarked() || node->generation() > maxgen())
        {
            return;
        }

        Marker::operator()(node);
        ++m_marks_applied;
    }

/* This macro should help localize where a FREESXP node is encountered
   in the GC */
#ifdef PROTECTCHECK
#define CHECK_FOR_FREE_NODE(s) { \
    if (s->sxpinfo.type == FREESXP && !GCManager::gc_inhibit_release()) \
	BadObject::register_bad_object(s, __FILE__, __LINE__); \
}
#else
#define CHECK_FOR_FREE_NODE(s)
#endif

    void GCNode::Marker::operator()(const GCNode *node)
    {
        if (node->isMarked())
        {
            return;
        }

        CHECK_FOR_FREE_NODE(node);
        if (node->generation() < m_maxgen) // node is below the number of generations to be collected
        {
            node->sxpinfo.m_mark = true;
            node->visitReferents(this);
        }
    }

    void GCNode::OldToNewChecker::operator()(const GCNode *node)
    {
        if (node && (node->generation() < m_mingen)) // node is younger than the minimum gen
        {
            std::cerr << "GCNode: old to new reference found (node's gen = " << node->generation() << ", mingen = " << m_mingen << ").\n";
            abort();
        }
    }

    void initializeMemorySubsystem()
    {
        static bool initialized = false;
        if (!initialized)
        {
            GCNode::initialize();
            GCStackRootBase::initialize();
            ProtectStack::initialize();
            // RAllocStack::initialize();

            initialized = true;
        }
    }

    void GCNode::cleanup()
    {
    }

    void GCNode::initialize()
    {
        static bool s_initialized = false;
        if (s_initialized)
        {
            throw std::runtime_error("R_GenHeap is already initialized.");
        }
        s_initialized = true;

        for (unsigned int gen = 0; gen < GCNode::numGenerations(); ++gen)
        {
            s_Old[gen] = std::make_unique<GCNode>(/* Peg constructor */);
#ifndef EXPEL_OLD_TO_NEW
            s_OldToNew[gen] = std::make_unique<GCNode>(/* OldToNew peg constructor */);
#endif
            s_gencount[gen] = 0;
            s_next_gen[gen] = gen + 1;
        }
        s_next_gen[GCNode::numOldGenerations()] = GCNode::numOldGenerations();
    }
} // namespace CXXR
