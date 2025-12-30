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

/** @file ProtectStack.hpp
 *
 * @brief Class CXXR::ProtectStack and associated C interface.
 *
 * CXXR::ProtectStack encapsulates the functionality of the CR pointer
 * protection stack.
 *
 * See the paragraph 'Caller Protects' in the description of class
 * CXXR::GCStackRoot for recommended coding policy.
 */

#ifndef PROTECTSTACK_HPP
#define PROTECTSTACK_HPP

#include <utility>
#include <vector>
#include <cstddef>
#include <CXXR/NodeStack.hpp>

namespace CXXR
{
    /** @brief Class implementing CR's 'pointer protection stack'.
     *
     * All members of this class are static.
     */
    class ProtectStack
    {
    public:
        /** @brief Restore PPS to a previous size.
         *
         * Restore the C pointer protection stack to a previous size by
         * popping elements off the top.
         *
         * @param new_size The size to which the stack is to be
         *          restored.  Must not be greater than the current
         *          size.
         *
         * @note In future this method will probably cease to be
         * available, since the use of the ProtectStack::Scope class
         * is preferable.
         */
        static void restoreSize(size_t new_size);

        /** @brief Current size of PPS.
         *
         * @return the current size of the C pointer protection stack.
         *
         * @note This method is intended for use in conjunction with
         * restoreSize(), and may cease to be public in future.
         */
        static size_t size()
        {
            return s_stack.size();
        }
#define R_PPStackTop ProtectStack::size()

        /** @brief Push a node pointer onto the PPS.
         *
         * Push a node pointer onto the C pointer protection stack.
         *
         * @param node Pointer to the node to be protected from the
         *          garbage collector.
         *
         * @return Index of the stack cell thus created, for
         *          subsequent use with reprotect().
         */
        static unsigned int protect_(SEXP node);

        /** @brief Change the target of a pointer on the PPS.
         *
         * Change the node that a particular cell in the C pointer
         * protection stack protects.  As a consistency check, it is
         * required that the reprotect takes place within the same
         * ProtectStack::Scope as the corresponding protect.
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
        static void reprotect(SEXP node, unsigned int index);

        /** @brief Pop pointers from the PPS.
         *
         * Pop cells from the C pointer protection stack.  As a
         * consistency check, it is required that the unprotect takes
         * place within the same ProtectStack::Scope as the
         * corresponding protect.
         *
         * @param count Number of cells to be popped.  Must not be
         *          larger than the current size of the C pointer
         *          protection stack.
         */
        static void unprotect_(unsigned int count = 1);

        /**
         * Removes from the C pointer protection stack the uppermost
         * stack cell containing a pointer to a specified node, and
         * drops all the stack cells above it by one place.
         *
         * @param node Pointer to the node whose cell is to be removed
         *          from the C pointer protection stack.
         *
         * @deprecated Utterly.
         */
        static void unprotectPtr(SEXP node);

        static void initialize(size_t initial_capacity = 50000);

        static std::vector<SEXP> s_stack;
#define R_PPStack CXXR::ProtectStack::s_stack

        // Initialize the static data members:
        friend void initializeMemorySubsystem();

        ProtectStack() = delete;
    };
} // namespace CXXR

namespace R
{
    /**
     * Check that the C pointer protection stack has the expected size,
     * and print a warning if not.
     *
     * @param op Operation being performed.
     *
     * @param save The expected size of the pointer protection stack.
     *
     * @todo A warning seems too mild a response in this eventuality.
     */
    void check_stack_balance(SEXP op, size_t save);

    std::pair<bool, unsigned int> Rf_isProtected(SEXP s);
} // namespace R

extern "C"
{
    /* ***** C interface ***** */

    typedef unsigned int PROTECT_INDEX;

    /** @brief Push a node pointer onto the C pointer protection stack.
     *
     * Push a node pointer onto the C pointer protection stack, and
     * record the index of the resulting stack cell (for subsequent
     * use with R_Reprotect).
     *
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     *
     * @param iptr Pointer to a location in which the stack cell index
     *          is to be stored.
     */
    inline void R_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr)
    {
        *iptr = CXXR::ProtectStack::protect_(node);
    }

    /** @brief Retarget a cell in the C pointer protection stack.
     *
     * Change the node that a particular cell in the C pointer
     * protection stack protects.  As a consistency check, it is
     * required that the reprotect takes place within the same
     * ProtectStack::Scope as the original protect.
     *
     * @param node Pointer to the node now to be protected from
     *          the garbage collector by the designated stack
     *          cell.  (Not necessarily a different node from the
     *          one currently protected.)
     *
     * @param index Index (as returned by R_ProtectWithIndex() ) of
     *          the stack cell to be retargeted to node.  Must be less
     *          than the current size of the C pointer protection
     *          stack.
     */
    inline void R_Reprotect(SEXP node, PROTECT_INDEX index)
    {
        CXXR::ProtectStack::reprotect(node, index);
    }

    /** @brief Push a node pointer onto the C pointer protection stack.
     *
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     * @return a copy of \a node .
     */
    inline SEXP Rf_protect(SEXP node)
    {
        CXXR::ProtectStack::protect_(node);
        return node;
    }

    /** @brief Pop cells from the C pointer protection stack.
     *
     * As a consistency check, it is required that the unprotect takes
     * place within the same ProtectStack::Scope as the corresponding
     * protects.
     *
     * @param count Number of cells to be popped.  Must not be
     *          larger than the current size of the C pointer
     *          protection stack.
     */
    inline void Rf_unprotect(unsigned int count)
    {
        CXXR::ProtectStack::unprotect_(count);
    }

    /** @brief Remove entry from pointer protection stack.
     *
     * Removes from the C pointer protection stack the uppermost stack
     * cell containing a pointer to a specified node, and drops all
     * the stack cells above it by one place.
     *
     * @param node Pointer to the node whose cell is to be removed
     *          from the C pointer protection stack.
     *
     * @deprecated Utterly.
     */
    inline void Rf_unprotect_ptr(SEXP node)
    {
        CXXR::ProtectStack::unprotectPtr(node);
    }
} // extern "C"

#endif // PROTECTSTACK_HPP
