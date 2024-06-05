/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCNode.hpp
 * @brief Class CXXR::GCNode.
 */

#ifndef GCNODE_HPP
#define GCNODE_HPP

#include <CXXR/config.hpp>

#include <CXXR/RTypes.hpp>
#include <CXXR/SEXPTYPE.hpp>

#define NAMED_BITS 16

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */

#ifdef SWITCH_TO_REFCNT
# define REFCNTMAX ((1 << NAMED_BITS) - 1)
#endif

namespace CXXR
{
    /* Flags */

    struct sxpinfo_struct {
        sxpinfo_struct(SEXPTYPE stype = NILSXP): type(stype), scalar(false), obj(false),
            alt(false), gp(0), m_mark(false), debug(false),
            trace(false), m_refcnt_enabled(true), m_rstep(false), m_gcgen(0),
            gccls(0), m_refcnt(0), m_binding_tag(NILSXP), extra(0)
        {
        }

        SEXPTYPE type : TYPE_BITS;
        unsigned int scalar : 1;
        unsigned int obj : 1;
        unsigned int alt : 1;
        unsigned int gp : 16;
        unsigned int m_mark : 1;
        unsigned int debug : 1;
        unsigned int trace : 1;  /* functions and memory tracing */
        unsigned int m_refcnt_enabled : 1;  /* used on closures and when REFCNT is defined */
        unsigned int m_rstep : 1;
        unsigned int m_gcgen : 1;  /* old generation number */
        unsigned int gccls : 3;  /* node class */
        unsigned int m_refcnt : NAMED_BITS;
        SEXPTYPE m_binding_tag : TYPE_BITS; /* used for immediate bindings */
        unsigned int extra : 4; /* unused bits */
    }; /*		    Tot: 64 bits, 1 double */

    /* The standard node structure consists of a header followed by the
       node data. */
    class GCNode
    {
    public:
        GCNode(SEXPTYPE stype = NILSXP): sxpinfo(stype), m_next(nullptr), m_prev(nullptr), m_attrib(nullptr)
        {
        }

        /** @brief Decrement the reference count.
         *
         */
        static void decRefCount(GCNode *node)
        {
            if (node && (node->sxpinfo.m_refcnt > 0 && node->sxpinfo.m_refcnt < REFCNTMAX))
                --(node->sxpinfo.m_refcnt);
        }

        /** @brief Increment the reference count.
         *
         */
        static void incRefCount(GCNode *node)
        {
            if (node && (node->sxpinfo.m_refcnt < REFCNTMAX))
                ++(node->sxpinfo.m_refcnt);
        }

        unsigned int getRefCount() const
        {
            return sxpinfo.m_refcnt;
        }

        void markNotMutable()
        {
            sxpinfo.m_refcnt = REFCNTMAX;
        }

        // virtual ~GCNode() {}

        /** @brief Initiate a garbage collection.
         *
         * @param num_old_gens The number of old generations to
         * collect.  Must be strictly smaller than numGenerations().
         */
        static void gc(unsigned int num_old_gens_to_collect);

        /** @brief Prevent old-to-new references.
         *
         * This is the first stage of garbage collection. It
         * propagates the generation recursively
         * through the node graph.
         *
         */
        static void propagateAges(unsigned int max_generation);

        /** @brief Carry out the mark phase of garbage collection.
         */
        static void mark(unsigned int max_generation);

        /** @brief Carry out the sweep phase of garbage collection.
         *
         */
        static void sweep();

        /** @brief Number of GCNode objects in existence.
         *
         * @return the number of GCNode objects currently in
         * existence.
         */
        static size_t numNodes() { return s_num_nodes; }

        // 2 old + 1 new
        static constexpr unsigned int numGenerations() { return s_num_old_generations + 1; }

        /** sxpinfo allocates one bit for the old generation count, so only 1
         * or 2 is allowed
         */
        static constexpr int numOldGenerations() { return s_num_old_generations; }

        unsigned int generation() const { return sxpinfo.m_gcgen; }

        bool isMarked() const { return sxpinfo.m_mark; }

        GCNode *next() const { return m_next; }

        GCNode *prev() const { return m_prev; }

        /** @brief Unsnap this node from its list
         *
         */
        void unsnap()
        {
            link(prev(), next());
            link(this, this);
        }

        // Make t the successor of s:
        static void link(GCNode *s, GCNode *t)
        {
            s->m_next = t;
            t->m_prev = s;
        }

        /** @brief Transfer a node so as to precede this node.
         *
         * @param s Pointer to node to be moved, which may be in the
         * same (circularly linked) list as '*this', or in a different
         * list.  It is permissible for \e s to point to what is already
         * the predecessor of '*this', in which case the function
         * amounts to a no-op.  It is also permissible for \e s to point
         * to '*this' itself; beware however that in that case the
         * function will detach '*this' from its current list, and turn
         * it into a singleton list.
         */
        void splice(GCNode *s)
        {
            // Doing things in this order is innocuous if s is already
            // this node's predecessor:
            link(s->prev(), s->next());
            link(prev(), s);
            link(s, this);
        }

        /** @brief Transfer a sublist so as to precede this node.
         *
         * @param beg Pointer to the first node in the sublist to be
         * moved.  The sublist may be a sublist of the same (circularly
         * linked) list of which '*this' forms a part, or of another
         * list.  Note however that in the former case, the sublist to
         * be moved must not contain '*this'.
         *
         * @param end Pointer to the successor of the last node of the
         * sublist to be moved.  It is permissible for it be identical
         * to beg, or to point to '*this': in either case the function
         * amounts to a no-op.
         */
        void splice(GCNode *beg, GCNode *end)
        {
            if (beg != end) {
                GCNode *last = end->prev();
                link(beg->prev(), end);
                link(prev(), beg);
                link(last, this);
            }
        }

        mutable struct sxpinfo_struct sxpinfo;
        GCNode *m_next;
        GCNode *m_prev;

        RObject *m_attrib;

        static size_t s_num_nodes; // Number of nodes in existence
        /* sxpinfo allocates one bit for the old generation count, so only 1
           or 2 is allowed */
        static constexpr unsigned int s_num_old_generations = 2;
    };
} // namespace CXXR

#endif /* GCNODE_HPP */
