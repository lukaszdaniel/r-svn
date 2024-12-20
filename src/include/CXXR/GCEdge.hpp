/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file GCEdge.hpp
 *
 * @brief Templated class CXXR::GCEdge.
 */

#ifndef GCEDGE_HPP
#define GCEDGE_HPP

#include <CXXR/GCNode.hpp>

namespace CXXR
{
    class RObject;

    /** @brief Directed edge in the graph whose nodes are GCNode objects.
     *
     * This class encapsulates a pointer from one GCNode to another,
     * and carries out housekeeping required by the garbage collection
     * scheme.  The class name reflects the fact that these objects
     * represent directed edges in the directed graph with the GCNode
     * objects as its nodes.
     *
     * Whenever an object of a type derived from GCNode needs to refer
     * to another such object, it should do so by containing a GCEdge
     * object, rather than by containing a pointer or reference
     * directly.
     *
     * @tparam T GCNode or a type publicly derived from GCNode.  This
     *          may be qualified by const, so for example a const
     *          String* may be encapsulated in a GCEdge using the type
     *          GCEdge<const String>.
     */
    template <class T = RObject>
    class GCEdge
    {
    public:
        using type = T;

        /** @brief Default constructor.
         *
         * @note When shouldn't I specify the target in the constructor?
         * Suppose that <tt>Foo</tt>, Bar and \c Baz are all classes
         * derived from GCNode, and that a \c Foo object in effect
         * 'contains' a \c Bar and a <tt>Baz</tt>.  If it were
         * possibly to initialize a GCEdge in its constructor, it
         * would be tempting to implement the \c Foo constructor as
         * follows:
         * <pre>
         * Foo()
         *      : m_edge1(new Bar), m_edge2(new Baz)
         * {}
         * </pre>
         * But now consider what would happen if the call <tt>new
         * Bar</tt> resulted in a garbage collection.  Then the
         * visitReferents() function of the object under construction
         * may be called before the field <tt>m_edge2</tt> has been
         * initialized, i.e. when it still contains junk, and this
         * will result in undefined behaviour, probably a program
         * crash.  This bug would remain latent until a garbage
         * collection happened at precisely this point.
         */
        GCEdge(T *val = nullptr) : m_target(val)
        {
            check_complete_type();
            GCNode::incRefCount(m_target);
        }

        // explicit GCEdge(T* target) is intentionally not defined here.
        //
        // This prevents object initializers of the form
        //     Foo::Foo() : m_edge(expression_that_might_gc()) { ... }
        // In that case, the expression is executed while the edge is
        // uninitialized.  If it causes a garbage collection, the GC's mark
        // routine will attempt to follow the uninitialized edge, causing
        // errors.
        // Object initializers should be written as:
        //     Foo::Foo() { m_edge = expression_that_might_gc(); ... }
        // which properly initialized the edge prior to doing the allocation.

        ~GCEdge()
        {
            check_complete_type();
            GCNode::decRefCount(m_target);
        }

        GCEdge<T> &operator=(const GCEdge<T> &source)
        {
            retarget(source);
            return *this;
        }

        GCEdge<T> &operator=(T *newtarget)
        {
            retarget(newtarget);
            return *this;
        }

        T *operator->() const { return get(); }

        /** @brief Extract the encapsulated pointer.
         *
         * @return The encapsulated pointer.
         */
        operator T *() const { return get(); }

        /** @brief Access the current target pointer.
         *
         * @return Pointer to the current target (if any) of the edge.
         */
        T *get() const { return m_target; }

        /**
         * @brief Detach the current target from the edge.
         *
         * The reference count of the target is decremented, and the target is set to nullptr.
         */
        void detach()
        {
            check_complete_type();
            GCNode::decRefCount(m_target);
            m_target = nullptr;
        }

        /** @brief Redirect the edge to a new target.
         *
         * Increments the reference count of the new target and decrements the count
         * of the old target. No action is taken if the target remains the same.
         *
         * @param parent The parent node responsible for the new reference.
         *
         * @param newtarget The new target to point to.
         */
        void retarget(const GCNode *parent, T *newtarget)
        {
            check_complete_type();
            if (m_target == newtarget)
                return;

            T *oldtarget = m_target;
            m_target = newtarget;

            if (parent->refCountEnabled())
            {
                GCNode::incRefCount(newtarget);
                GCNode::decRefCount(oldtarget);
            }
        }

        /** @brief Redirect the edge to a new target.
         *
         * Increments the reference count of the new target and decrements the count
         * of the old target. No action is taken if the target remains the same.
         *
         * This variant takes into account the ASSIGNMENT_PENDING flag.
         *
         * @param parent The parent node responsible for the new reference.
         *
         * @param newtarget The new target to point to.
         */
        void retarget2(const GCNode *parent, T *newtarget)
        {
            check_complete_type();
            if (m_target == newtarget)
                return;
            T *oldtarget = m_target;
            m_target = newtarget;
            if (parent->refCountEnabled())
            {
                GCNode::incRefCount(newtarget);
                if (parent->sxpinfo.gp & (1 << 11) /*ASSIGNMENT_PENDING(parent)*/)
                {
                    parent->sxpinfo.gp &= ~(1 << 11);
                    // SET_ASSIGNMENT_PENDING(parent, FALSE);
                }
                else
                    GCNode::decRefCount(oldtarget);
            }
        }

    private:
        T *m_target;

        /** @brief Redirect the edge to point to a new target.
         *
         * This method updates the edge to point to a new target and manages
         * the reference counts appropriately.
         *
         * @param newtarget Pointer to the object to which reference is now
         *           to be made.
         */
        void retarget(T *newtarget)
        {
            check_complete_type();
            if (m_target == newtarget)
                return;

            GCNode::incRefCount(newtarget);
            T *oldtarget = m_target;
            m_target = newtarget;
            GCNode::decRefCount(oldtarget);
        }

        /** @brief Disable array access for this class.
         */
        T &operator[](size_t) const = delete;

        /** @brief Ensure the type T is complete.
         */
        static void check_complete_type()
        {
            static_assert(sizeof(T) >= 0, "T must be a complete type");
        }
    };
} // namespace CXXR

#endif // GCEDGE_HPP
