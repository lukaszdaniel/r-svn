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

        void clear()
        {
            type = NILSXP;
            scalar = false;
            obj = false;
            alt = false;
            gp = 0;
            m_mark = false;
            debug = false;
            trace = false;
            m_refcnt_enabled = true;
            m_rstep = false;
            m_gcgen = 0;
            gccls = 0;
            m_refcnt = 0;
            m_binding_tag = NILSXP;
            extra = 0;
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
        unsigned int gccls : 1;  /* was external allocator used? */
        unsigned int m_refcnt : NAMED_BITS;
        SEXPTYPE m_binding_tag : TYPE_BITS; /* used for immediate bindings */
        unsigned int extra : 6; /* unused bits */
    }; /*		    Tot: 64 bits, 1 double */

    /** @brief Base class for objects managed by the garbage collector.
     *
     * Abstract base class for all objects managed by the garbage
     * collector.
     *
     * \par Derived class checklist:
     * Classes derived directly or indirectly from GCNode should do
     * the following to ensure the integrity of the garbage collection
     * scheme:
     * <ol>
     * <li>Explicitly declare a destructor (even if it does nothing)
     * as either protected or (if no further derivation from the class
     * is envisaged) private.  This ensures that objects of classes
     * derived from GCNode can be created only using 'new'.</li>
     *
     * <li>If the derived class contains any pointers or references to
     * other objects derived from GCNode, these should be encapsulated
     * within an object of the templated class GCEdge, and the class
     * should reimplement the methods detachReferents() and visitReferents()
     * appropriately.  (This does not necessarily apply to pointers
     * where other logic ensures that the pointer does not outlive the
     * thing pointed to.)</li>
     * </ol>
     *
     * \par Infant immunity:
     * While a GCNode or an object of a class derived from GCNode is
     * under construction, it is effectively immune from the garbage
     * collector.  Not only does this greatly simplify the coding of
     * the constructors themselves, it also means that in implementing
     * the virtual method visitReferents(), it is not necessary to
     * consider the possibility that the garbage collector will invoke
     * this method for a node whose construction is not yet complete.
     *
     * \par
     * The method expose() is used to end this immunity once
     * construction of an object is complete.  However, there appears
     * to be no clean and general way in C++ of calling expose() \e
     * exactly when construction is complete.  Consequently, a node
     * N's infant immunity will in fact continue until one of the
     * following events occurs:
     * <ul>
     *
     * <li>The method expose() is called explicitly for N, or for a
     * node that refers to N (and so on recursively).</li>
     *
     * <li>N is visited by a \b GCNode::Ager object (as part of write
     * barrier enforcement).  This will happen if a node that is
     * already exposed to the garbage collector is modified so that it
     * refers to N (or a node that refers to N, and so on
     * recursively).</li>
     *
     * <li>A pointer to N (or a node that refers to N, and so on
     * recursively) is specified in the constructor of a GCRoot
     * object, and the optional argument \a expose is set to
     * true.</li>
     *
     * <li>N is designated as the key, value or R finalizer of a weak
     * reference object.</li>
     *
     * <li>N is itself a weak reference object (in which case it is
     * exposed to garbage collection during construction).</li>
     *
     * </ul>
     *
     * \par
     * It is the responsibility of any code that creates an object of
     * a class derived from GCNode to ensure that, under normal
     * operation, the object is in due course exposed to the garbage
     * collector.  However, if exceptions occur, the static member
     * function slaughterInfants() can be used to delete \e all the
     * nodes currently enjoying infant immunity.
     *
     * \par Nested <tt>new</tt>:
     * Consider the following code to create a PairList of two
     * elements, with \c first as the 'car' of the first element and
     * \c second as the 'car' of the second element:
     * \code
     * CXXR::GCStackRoot<CXXR::PairList>
     *   pl2(new CXXR::PairList(first, new CXXR::PairList(second)));
     * \endcode
     * Is this code sound?  You might suppose that there is a risk
     * that the second element of the list will be garbage-collected
     * when \c new is invoked to allocate space for the first element.
     * But this is not so: at this stage, the second element will
     * still enjoy infant immunity.
     *
     * \par
     * However, a potential problem arises from a different quarter.
     * Suppose that the \c new to allocate space for the second
     * element succeeds, but that the \c new to allocate space for the
     * first element fails because of shortage of memory, and throws
     * <tt>std::bad_alloc</tt>.  Then the second element will not be
     * exposed to the garbage collector, and the space it occupies is
     * potentially lost.  An easy workaround is for the handler that
     * catches the exception to invoke slaughterInfants().
     *
     * @note Because this base class is used purely for housekeeping
     * by the garbage collector, and does not contribute to the
     * 'meaning' of an object of a derived class, all of its data
     * members are mutable.
     *
     * @todo The (private) cleanup() method needs to address the
     * possibility that derived classes may have destructors that
     * release some external resource (e.g. a lock).  Maybe a garbage
     * collection without a 'mark' phase would do the trick.
     */
    class GCNode
    {
    public:
        /** @brief Consturctor used for creating pegs.
         *
         */
        GCNode() : sxpinfo(NILSXP), m_next(this), m_prev(this)
        {
        }

        /** @brief Main GCNode Consturctor.
         *
         */
        GCNode(SEXPTYPE stype) : GCNode()
        {
            sxpinfo.type = stype;
            ++s_num_nodes;
        }

        /** @brief Allocate memory.
         *
         * Allocates memory for a new object of a class derived from
         * GCNode.
         *
         * @param bytes Number of bytes of memory required.
         *
         * @return Pointer to the allocated memory block.
         *
         * @note This function will often carry out garbage collection
         * of some kind before allocating memory.  However, no
         * garbage collection will be performed if at least one
         * GCInhibitor object is in existence.
         */
        static void *operator new(size_t bytes) HOT_FUNCTION;

        /** @brief Placement new for GCNode.
         */
        static void *operator new(size_t, void *where)
        {
            return where;
        }

        /** @brief Deallocate memory
         *
         * Deallocate memory previously allocated by operator new.
         *
         * @param p Pointer to the allocated memory block.
         *
         * @param bytes Size in bytes of the memory block, as
         * requested when the block was allocated.
         */
        static void operator delete(void *p, size_t bytes);

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

        virtual ~GCNode()
        {
            --s_num_nodes;
        }

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
        static constexpr unsigned int numOldGenerations() { return s_num_old_generations; }

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

        static size_t s_num_nodes; // Number of nodes in existence
        /* sxpinfo allocates one bit for the old generation count, so only 1
           or 2 is allowed */
        static constexpr unsigned int s_num_old_generations = 2;
    };
} // namespace CXXR

namespace R
{
    bool (REFCNT_ENABLED)(SEXP x);
    void (DECREMENT_REFCNT)(SEXP x);
    void (INCREMENT_REFCNT)(SEXP x);
    void (DISABLE_REFCNT)(SEXP x);
    void (ENABLE_REFCNT)(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Is this node marked?
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x is marked to prevent GC of this node.  Returns false if \a x
     * is nullptr.
     */
    int (MARK)(SEXP x);
    int (REFCNT)(SEXP x);
    void (MARK_NOT_MUTABLE)(SEXP x);
} // extern "C"

#endif /* GCNODE_HPP */
