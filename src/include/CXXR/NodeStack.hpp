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

/** @file NodeStack.hpp
 *
 * @brief Class CXXR::NodeStack.
 */

#ifndef NODESTACK_HPP
#define NODESTACK_HPP

#include <vector>
#include <iterator> // for std::distance
#include <CXXR/RTypes.hpp>
#include <CXXR/RObject.hpp>
#include <R_ext/Error.h> // for NORET

namespace CXXR
{
    /** @brief Class implementing a stack of node_t.
     *
     * This class is not intended for general use.  It is currently
     * used in class ProtectStack and in the bytecode interpreter.
     *
     * Note that it is necessary for GCNode::gclite() to call the
     * protectAll() method of every NodeStack in existence before it
     * starts to delete nodes with zero references counts.
     */
    class NodeStack
    {
    public:
        /** @brief Typed Stack for the ByteCode virtual machine
         *
         * The byte code engine uses a typed stack. The typed stack's entries
         * consist of a tag and a union. An entry can represent a standard
         * SEXP value (tag = 0) or an unboxed scalar value.  For now real,
         * integer, and logical values are supported. It would in principle be
         * possible to support complex scalars and short scalar strings, but
         * it isn't clear if this is worth while.
         *
         * In addition to unboxed values the typed stack can hold partially
         * evaluated or incomplete allocated values. For now this is only used
         * for holding a short representation of an integer sequence as produce
         * by the colon operator, seq_len, or seq_along, and as consumed by
         * compiled 'for' loops. This could be used more extensively in the
         * future, though the ALTREP framework may be a better choice.
         *
         * Allocating memory on the stack is also supported; this is currently
         * used for jump buffers.
         */
        struct node_t {
            unsigned int tag;
            bool flags;
            union {
                int ival;
                int lval;
                double dval;
                SEXP sxpval;
            } u;

            node_t(unsigned int tg, SEXP val): tag(tg), flags(false)
            {
                u.sxpval = val;
            }

            node_t(unsigned int tg = 0, int val = 0): tag(tg), flags(false)
            {
                u.ival = val;
            }

            node_t(unsigned int tg, double val): tag(tg), flags(false)
            {
                u.dval = val;
            }
        };

        /** @brief Object constraining lifetime of NodeStack entries.
         *
         * Scope objects must be declared on the processor stack
         * (i.e. as C++ automatic variables).  Each Scope is
         * associated with a particular NodeStack object.  Any entry
         * pushed onto that NodeStack object during the lifetime of
         * the Scope object will be automatically popped off when that
         * lifetime comes to an end, i.e. when the Scope object itself
         * goes out of scope.
         */
        class Scope
        {
        public:
            /** @brief Constructor
             *
             * @param stack Non-null pointer to the NodeStack object
             *    with which this Scope is to be associated.
             */
            Scope(NodeStack *stack)
                : m_nodestack(stack),
                  m_next_scope(stack->m_innermost_scope),
                  m_saved_size(m_nodestack->size())
            {
                m_saved_protected_count = stack->m_protected_count;
                stack->m_innermost_scope = this;
            }

            ~Scope()
            {
#ifndef NDEBUG
                if (this != m_nodestack->m_innermost_scope)
                    nestingError();
#endif
                m_nodestack->resize(m_saved_size);
                m_nodestack->declnk_stack(m_saved_protected_count);
                m_nodestack->m_innermost_scope = m_next_scope;
            }

        private:
            friend class NodeStack;

            NodeStack *m_nodestack;
            Scope *m_next_scope;
            size_t m_saved_size;
            size_t m_saved_protected_count;

            /** @brief NodeStack size at construction.
             *
             * @return The size of the NodeStack at the time this
             * Scope object was constructed.  The NodeStack will be
             * restored to this size by the Scope destructor.
             */
            size_t startSize() const
            {
                return m_saved_size;
            }

            static void nestingError();
        };

        /** @brief Constructor.
         *
         * @param initial_capacity The initial capacity of the
         *          NodeStack to be created.  The capacity will be
         *          increased as necessary, so the value of this
         *          parameter is not critical.
         */
        NodeStack(size_t initial_capacity);

        ~NodeStack()
        {
            resize(0);
        }

        /** @brief Pointer to 'one beyond the end' of the NodeStack.
         *
         * @return A pointer to the node_t one beyond the end of
         * the node stack.  This performs a function similar to
         * R_BCNodeStackTop in CR.
         *
         * @note Used in ByteCode stack.
         */
        node_t *end()
        {
            // return &(*(m_vector.begin() + m_vector.size()));
            return m_R_BCNodeStackTop;
        }

        /** @brief Pop pointers from the NodeStack.
         *
         * If this function is executed within a NodeStack::Scope
         * pertaining to this NodeStack, the number of elements popped
         * must balance a corresponding number of push() operations
         * previously executed within the innermost such scope.  This
         * is checked unless the class is compiled with NDEBUG.
         *
         * @param count Number of cells to be popped.  Must not be
         *          larger than the current size of the C pointer
         *          protection stack (checked unless compiled with
         *          NDEBUG).
         */
        void pop(unsigned int count = 1);

        /** @brief Ensure GC protection of all nodes.
         *
         * This function ensures that all RObjects pointed to from the
         * NodeStack are protected from garbage collection.
         */
        void protectAll();

        /** @brief Push a node_t object onto the stack.
         *
         * @param node node_t object.
         */
        void push_node(node_t node)
        {
            node_t *__ntop__ = m_R_BCNodeStackTop + 1;
            if (size() + 1 > m_reserved_capacity) nodeStackOverflow();
            __ntop__[-1] = node;
            m_R_BCNodeStackTop = __ntop__;
        }

        /** @brief Push a node pointer onto the NodeStack.
         *
         * @param node Pointer, possibly null, to the node to be
         *          pushed onto the NodeStack.
         *
         * @return Index of the stack cell thus created, counting from
         *         zero.
         */
        size_t push(RObject *node)
        {
            // CHECK_SET_BELOW_PROT(m_R_BCNodeStackTop);
            push_node(node_t(NILSXP, node));
            return std::distance(m_vector.data(), m_R_BCNodeStackTop);
        }

        /** @brief Duplicate first value on the stack.
         *
         * @note Used in ByteCode stack.
         */
        void push_dup()
        {
            push_node(m_R_BCNodeStackTop[-1]);
        }

        /** @brief Duplicate second value on the stack.
         *
         * @note Used in ByteCode stack.
         */
        void push_dup2nd()
        {
            push_node(m_R_BCNodeStackTop[-2]);
        }

        /** @brief Duplicate third value on the stack.
         *
         * @note Used in ByteCode stack.
         */
        void push_dup3rd()
        {
            push_node(m_R_BCNodeStackTop[-3]);
        }

        /** @brief Change the target of a pointer on the PPS.
         *
         * Change the node that a particular cell in the C pointer
         * protection stack protects.  As a consistency check, it is
         * required that the retarget takes place within the same
         * NodeStack::Scope as the corresponding protect.
         *
         * @param node Pointer to the node now to be protected from
         *          the garbage collector by the designated stack
         *          cell.  (Not necessarily a different node from the
         *          one currently protected.)
         *
         * @param index Index (as returned by protect() ) of the stack
         *          cell to be retargeted to node.  Must be less than
         *          the current size of the C pointer protection
         *          stack (checked).
         */
        void retarget(node_t node, size_t index);
        void retarget(RObject *node, size_t index);

        /** @brief Modify size of NodeStack.
         *
         * @param new_size The required size.  If larger than the
         *          current size, the added cells will contain null
         *          pointers.  If smaller than the current size, then
         *          pointers are popped off the NodeStack to bring its
         *          size down to \a new_size.
         */
        void resize(size_t new_size)
        {
            if (new_size >= m_protected_count)
            {
                m_vector.resize(new_size, node_t());
                if (new_size < m_deferred_protected_count)
                    m_deferred_protected_count = new_size;
            }
            else
                resize_aux(new_size);
        }
        void resize_cr(size_t new_size)
        {
            m_R_BCNodeStackTop = m_vector.data() + new_size;
        }

        /** @brief Current size of NodeStack.
         *
         * @return the number of pointers currently on the NodeStack.
         */
        size_t size()
        {
            // return m_vector.size();
            return std::distance(m_vector.data(), m_R_BCNodeStackTop);
        }

        /** @brief pop and return the top element of the stack.
         *
         * The stack must not be empty; this is checked unless the
         * class is compiled with NDEBUG.
         *
         * @return the pointer previously at the top of the stack.
         */
        RObject *topnpop();

        /** @brief Set the new value for protection top.
         *
         * @param top New value for protection top.
         *
         * @note This function increases ref count up to protection top.
         *
         * @note Used in ByteCode stack.
         */
        void inclnk_stack(size_t top);
        void inclnk_stack_commit(void);

        /** @brief Decrease ref count for pending objects.
         *
         * @param base Value from which decrease of ref count
         * should start.
         *
         * @note This function decreases ref count from base up to
         * committed protection.
         *
         * @note Used in ByteCode stack.
         */
        void declnk_stack(size_t base);

        size_t protectedCount() const
        {
            return m_protected_count;
        }

        size_t deferredprotectedCount() const
        {
            return m_deferred_protected_count;
        }

        size_t reservedCapacity() const
        {
            return m_reserved_capacity;
        }

        /** @brief Conduct a const visitor via the NodeStack.
         *
         * Conduct a GCNode::const_visitor object to each node_t
         * pointed to by the NodeStack.
         *
         * @param v Pointer to the const_visitor object.
         */
        void visitRoots(GCNode::const_visitor *v);

        std::vector<node_t> m_vector;
        size_t m_deferred_protected_count;
        node_t *m_R_BCNodeStackTop;
        size_t m_reserved_capacity;
        size_t m_protected_count; // The nodes (if any) pointed to
                                  // (*m_vector)[0] through (*m_vector)[m_protected_count - 1]
                                  // will have had their reference counts increased by this
                                  // class.  Stack entries beyond this (if any) will not yet
                                  // have had this protection applied.

        Scope *m_innermost_scope;

        // Helper function for retarget(), handling the case where
        // 'index' is within the protected range:
        static void retarget_aux(node_t oldnode, node_t newnode)
            HOT_FUNCTION;

        // Helper function for trim(), handling the case where the trim
        // cuts down into protected nodes:
        void resize_aux(size_t new_size) HOT_FUNCTION;

#define R_BCNodeStackBase ByteCode::s_nodestack->m_vector.data()
// Note that this macro name uses 'Top' in the sense of the C++ standard
// library end(), i.e. one past the current top element of the stack,
// not in the way that CR uses R_BCNodeStackEnd, which relates to the
// end of allocated storage.
#define R_BCNodeStackTop ByteCode::s_nodestack->m_R_BCNodeStackTop
#define R_BCNodeStackEnd ByteCode::s_nodestack->reservedCapacity()

        NORET static void nodeStackOverflow(void);
    };

    using R_bcstack_t = NodeStack::node_t;

#define PARTIALSXP_MASK (~255)
#define IS_PARTIAL_SXP_TAG(x) ((x) & PARTIALSXP_MASK)
#define RAWMEM_TAG 254
#define CACHESZ_TAG 253

// this produces an initialized structure as a _compound literal_
#ifdef __cplusplus
inline R_bcstack_t SEXP_TO_STACKVAL(SEXP x)
{
    R_bcstack_t node;
    node.tag = 0;
    node.u.sxpval = x;
    return node;
}
#else
#define SEXP_TO_STACKVAL(x) ((R_bcstack_t) { .tag = 0, .u.sxpval = (x) })
#endif
} // namespace CXXR

namespace R
{
    void R_BCProtReset(size_t ptop);
}

#endif // NODESTACK_HPP
