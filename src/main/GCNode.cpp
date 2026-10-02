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
    siv::Vector<CXXR::GCNode *> GCNode::s_Old[1 + GCManager::numOldGenerations()];
#ifndef EXPEL_OLD_TO_NEW
    siv::Vector<CXXR::GCNode *> GCNode::s_OldToNew[1 + GCManager::numOldGenerations()];
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

    GCNode::GCNode(SEXPTYPE stype): sxpinfo(stype)
    {
        moveToGeneration(0);
        ++s_num_nodes;
        ++s_gencount[0];
    }

    GCNode::~GCNode()
    {
#ifndef EXPEL_OLD_TO_NEW
        if (m_in_old_to_new_list)
            s_OldToNew[m_current_gen_list].erase(m_ID);
        else
#endif
            s_Old[m_current_gen_list].erase(m_ID);
        --s_gencount[generation()];
        --s_num_nodes;
    }

    void GCNode::moveToGeneration(unsigned int generation) const
    {
        const siv::ID new_id = s_Old[generation].emplace_back(const_cast<GCNode *>(this));
        if (m_ID != ID_NOT_SET)
        {
#ifndef EXPEL_OLD_TO_NEW
            if (m_in_old_to_new_list)
                s_OldToNew[m_current_gen_list].erase(m_ID);
            else
#endif
                s_Old[m_current_gen_list].erase(m_ID);
        }
        m_current_gen_list = generation;
        m_ID = new_id;
        m_in_old_to_new_list = false;
    }

#ifndef EXPEL_OLD_TO_NEW
    void GCNode::moveToOldToNew(const GCNode *node)
    {
        const unsigned int generation = node->generation();
        const siv::ID new_id = s_OldToNew[generation].emplace_back(const_cast<GCNode *>(node));
        if (node->m_ID != ID_NOT_SET)
        {
            if (node->m_in_old_to_new_list)
                s_OldToNew[node->m_current_gen_list].erase(node->m_ID);
            else
                s_Old[node->m_current_gen_list].erase(node->m_ID);
        }
        node->m_current_gen_list = generation;
        node->m_ID = new_id;
        node->m_in_old_to_new_list = true;
    }
#endif

    void GCNode::Ager::operator()(const GCNode *node)
    {
        if (node->generation() < m_mingen) // node is younger than the minimum age required
        {
            --s_gencount[node->generation()];
            node->sxpinfo.m_gcgen = m_mingen;
            node->moveToGeneration(m_mingen);
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
        if (node->generation() < m_maxgen) // node generation falls into generations to be collected
        {
            node->sxpinfo.m_mark = true;
            node->moveToGeneration(node->generation());
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

    void GCNode::propagateAges(unsigned int max_generation)
    {
#ifndef EXPEL_OLD_TO_NEW
    /* eliminate old-to-new references in generations to collect by
       transferring referenced nodes to referring generation */
        for (unsigned int gen = 1; gen < max_generation; gen++) {
            Ager ager(gen);
            while (!s_OldToNew[gen].empty()) {
                GCNode *s = s_OldToNew[gen].getDataAt(s_OldToNew[gen].size() - 1);
                s->visitReferents(&ager);
                s->moveToGeneration(gen);
            }
        }
#endif
    }

    void GCNode::sweep(unsigned int max_generation)
    {
        static unsigned int s_sweeps_since_shrink = 0;
        if (++s_sweeps_since_shrink == 64)
        {
            s_sweeps_since_shrink = 0;
            for (unsigned int gen = 0; gen < numGenerations(); ++gen)
            {
                s_Old[gen].shrink_to_fit();
#ifndef EXPEL_OLD_TO_NEW
                s_OldToNew[gen].shrink_to_fit();
#endif
            }
        }

        while (!s_New.empty())
        {
            GCNode *s = s_New.getDataAt(s_New.size() - 1);
            s->detachReferents();
            delete s;
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
            s_Old[gen].reserve(1'000'000);
            s_gencount[gen] = 0;
            s_next_gen[gen] = gen + 1;
        }
        s_next_gen[GCNode::numOldGenerations()] = GCNode::numOldGenerations();
    }
} // namespace CXXR
