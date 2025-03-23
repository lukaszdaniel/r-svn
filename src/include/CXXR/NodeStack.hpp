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

#include <memory>
#include <iterator> // for std::distance
#include <CXXR/RTypes.hpp>

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
        }

        /** @brief Current size of NodeStack.
         *
         * @return the number of pointers currently on the NodeStack.
         */
        size_t size() const
        {
            return std::distance(m_R_BCNodeStackBase.get(), m_R_BCNodeStackTop);
        }

        std::unique_ptr<node_t[]> m_R_BCNodeStackBase;
        node_t *m_R_BCProtTop;
        node_t *m_R_BCNodeStackTop;
        node_t *m_R_BCNodeStackEnd;
        node_t *m_R_BCProtCommitted;
#define R_BCNodeStackBase ByteCode::s_nodestack->m_R_BCNodeStackBase.get()
#define R_BCProtTop ByteCode::s_nodestack->m_R_BCProtTop
#define R_BCNodeStackTop ByteCode::s_nodestack->m_R_BCNodeStackTop
#define R_BCNodeStackEnd ByteCode::s_nodestack->m_R_BCNodeStackEnd
#define R_BCProtCommitted ByteCode::s_nodestack->m_R_BCProtCommitted
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
    void R_BCProtReset(CXXR::R_bcstack_t *);
}

#endif // NODESTACK_HPP
