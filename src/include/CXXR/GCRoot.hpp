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

/** @file GCRoot.hpp
 *
 * @brief Templated class CXXR::GCRoot and its untemplated base class
 * CXXR::GCRootBase.
 */

#ifndef GCROOT_HPP
#define GCROOT_HPP

#include <CXXR/GCNode.hpp>

namespace CXXR
{
    /** @brief Untemplated base class for GCRoot.
     *
     * The preferred method for C++ code to protect a GCNode
     * from the garbage collector is to use the templated class
     * GCRoot, of which this is the untemplated base class, or class
     * GCStackRoot.
     */
    class GCRootBase
    {
    public:
        /** @brief Conduct a const visitor to all 'root' GCNode objects.
         *
         * Conduct a GCNode::const_visitor object to each root GCNode
         * and each node on the C pointer protection stack.
         *
         * @param v Pointer to the const_visitor object.
         */
        static void visitRoots(GCNode::const_visitor *v);

    // protected:
        /** @brief Primary constructor.
         *
         * @param node Pointer, possibly null, to the node to be protected.
         */
        GCRootBase(const GCNode *node);

        /** @brief Copy constructor.
         *
         * @param source Pattern for the copy.
         */
        GCRootBase(const GCRootBase &source): GCRootBase(source.ptr()) {}

        ~GCRootBase()
        {
            unlink();
        }

        GCRootBase &operator=(const GCRootBase &source)
        {
            const GCNode *newnode = source.ptr();
            m_pointer = newnode;
            return *this;
        }

        /** @brief Change the node protected by this GCRootBase.
         *
         * @param node Pointer to the node now to be protected, or a
         * null pointer.
         */
        void retarget(const GCNode *node)
        {
            m_pointer = node;
        }

        /** @brief Access the encapsulated pointer.
         *
         * @return the GCNode pointer encapsulated by this object.
         */
        const GCNode *ptr() const
        {
            return m_pointer;
        }

    // private:
        const GCNode *m_pointer;

        // GCRoots form an intrusive doubly-linked list.  This structure
        // requires only constant initialization and doesn't allocate memory at
        // all.  This allows static creation of global GCRoots when needed.
        GCRootBase *m_next;
        GCRootBase *m_prev;

        static GCRootBase *s_list_head;

        void unlink()
        {
            if (m_next)
            {
                m_next->m_prev = m_prev;
            }
            if (m_prev)
            {
                m_prev->m_next = m_next;
            }
            else
            {
                s_list_head = m_next;
            }
        }
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCRoot object exists,
     * the GCNode that it points to will not be garbage collected.
     *
     * This class performs a similar function to GCStackRoot, but is
     * intended for variables that are not allocated on the stack.
     * Unlike GCStackRoot objects, there is no requirement that GCRoot
     * objects be destroyed in the reverse order of their creation;
     * the price of this is that there is a slightly greater time overhead
     * to construction and destruction.
     *
     * It is safe to declare a GCRoot at file or namespace scope.
     *
     * @tparam T GCNode or a type publicly derived from GCNode.  This
     *          may be qualified by const, so for example a const
     *          String* may be encapsulated in a GCRoot using the type
     *          GCRoot<const String>.
     */
    template <class T = RObject>
    class GCRoot: public GCRootBase
    {
    public:
        /**
         * @param node Pointer the node to be pointed to, and
         *          protected from the garbage collector, or a null
         *          pointer.
         */
        GCRoot(T *node = nullptr): GCRootBase(node) { check_complete_type(); }

        /** @brief Copy constructor.
         *
         * The constructed GCRoot will protect the same GCNode as
         * source.  (There is probably no reason to use this
         * constructor.)
         */
        GCRoot(const GCRoot &source): GCRootBase(source) {}

        /**
         * This will cause this GCRoot to protect the same GCNode as
         * is protected by source.  (There is probably no reason to
         * use this method.)
         */
        GCRoot &operator=(const GCRoot &source)
        {
            GCRootBase::operator=(source);
            return *this;
        }

        /**
         * This will cause this GCRoot to point to and protect node,
         * instead of the node (if any) it currently points to and
         * protects.
         *
         * @param node Pointer to the GCNode that is now to be pointed
         *          to and protected from the garbage collector.
         */
        GCRoot &operator=(T *node)
        {
            check_complete_type();
            GCRootBase::retarget(node);
            return *this;
        }

        /** @brief Access member via encapsulated pointer.
         *
         * @return the pointer currently encapsulated by the node.
         */
        T *operator->() const
        {
            return get();
        }

        /** @brief Dereference the encapsulated pointer.
         *
         * @return a reference to the object pointed to by the
         * encapsulated pointer.  The effect is undefined if this
         * object encapsulates a null pointer.
         */
        T &operator*() const
        {
            return *get();
        }

        /** @brief Implicit conversion to encapsulated pointer type.
         *
         * @return the pointer currently encapsulated by the node.
         * The pointer is of type \a T* const to prevent its use as
         * an lvalue, the effect of which would probably not be what
         * the programmer wanted.
         */
        operator T *() const
        {
            return get();
        }

        /** @brief Access the encapsulated pointer.
         *
         * @return the pointer currently encapsulated by the node.
         */
        T *get() const
        {
            check_complete_type();
            return static_cast<T *>(const_cast<GCNode *>(ptr()));
        }

    private:
        // A GC root is a pointer, not an array.
        T &operator[](size_t) const = delete;

        static void check_complete_type()
        {
            static_assert(sizeof(T) >= 0, "T must be a complete type");
        }
    };
} // namespace CXXR

// For hashing, simply hash the encapsulated pointer:
namespace std
{
    template <class T>
    struct hash<CXXR::GCRoot<T>>
    {
        std::size_t operator()(const CXXR::GCRoot<T> &gcrt) const
        {
            std::hash<T *> make_hash;
            return make_hash(gcrt);
        }
    };
} // namespace std

extern "C"
{
    /* ***** C interface ***** */

    /** @brief Protect object against garbage collection.
     *
     * This is intended for long-term protection, for which PROTECT()
     * etc. would be inappropriate.
     *
     * @param object Pointer to the object to be preserved.  It is
     *          permissible for this to be a null pointer.
     */
    void R_PreserveObject(SEXP object);

    /** @brief Remove object's protection against garbage collection.
     *
     * @param object Pointer to the object whose protection is to be
     *          removed.  It is permissible (but pointless) for this
     *          to be a pointer to an object that is not currently
     *          protected by R_PreserveObject(), but in that case
     *          R_ReleaseObject() has no effect.
     */
    void R_ReleaseObject(SEXP object);
} // extern "C"

#endif // GCROOT_HPP
