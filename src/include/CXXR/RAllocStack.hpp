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

/** @file RAllocStack.hpp
 * @brief Function R_alloc() and kin.
 */

#ifndef RALLOCSTACK_HPP
#define RALLOCSTACK_HPP

#include <R_ext/Memory.h>

namespace CXXR
{
    /** @brief Class for implementing R_alloc() and kindred functions.
     *
     * This class has only static members.  It implements a stack of
     * pointers to blocks of memory.
     */
    class RAllocStack
    {
    public:
        /** @brief Object constraining lifetime of R_alloc() blocks.
         *
         * Scope objects must be declared on the processor stack
         * (i.e. as C++ automatic variables).  Any R_alloc() block
         * allocated during the lifetime of a Scope object will be
         * automatically deallocated when that lifetime comes to an
         * end, i.e. when the Scope object itself goes out of scope.
         */
        class Scope
        {
        public:
            Scope()
                : m_next_scope(RAllocStack::s_innermost_scope),
                m_saved_vmax(vmaxget())
            {
                RAllocStack::s_innermost_scope = this;
            }

            ~Scope()
            {
                // if (vmaxget() != m_saved_vmax)
                    vmaxset(m_saved_vmax);
                RAllocStack::s_innermost_scope = m_next_scope;
            }

            /** @brief RAllocStack size at construction.
             *
             * @return The size of the RAllocStack at the time this
             * Scope object was constructed.  The RAllocStack will be
             * restored to this size by the Scope destructor.
             */
            void *startSize() const
            {
                return m_saved_vmax;
            }

        private:
            Scope *m_next_scope;
            void *m_saved_vmax;
        };

    private:
        static Scope *s_innermost_scope;

        RAllocStack() = delete;
    };
} // namespace CXXR

extern "C"
{
    /* ***** C interface ***** */

    /** @brief Allocate a block of memory.
     *
     * This function is provided primarily for the use of code called
     * from the R <tt>.C</tt> function.  It will allocate a block of
     * memory that will automatically be reclaimed by R at the end of
     * the <tt>.C</tt> call.
     *
     * @param num_elts Number of data items to be accommodated in the
     *          block.
     *
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          <tt>sizeof(char)</tt>) of each data item.  Must be
     *          non-negative.
     *
     * @return Pointer to the start of the memory block.
     *
     * @note The signed type of \a elt_size is anomalous, but is part
     * of the R API.
     */
    char *R_alloc(size_t num_elts, int elt_size);

    long double *R_allocLD(size_t num_elts);

    /** @brief Allocate a block of memory, and initialize it to zero.
     *
     * This is part of the S compatibility interface.  It does the
     * same thing as R_alloc(), except that the memory block is
     * guaranteed to be initialized to zero.
     *
     * @param num_elts Number of data items to be accommodated in the
     *          block.  Must be non-negative.
     *
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          <tt>sizeof(char)</tt>) of each data item.  Must be
     *          non-negative.
     *
     * @return Pointer to the start of the memory block.
     */
    char *S_alloc(long num_elts, int elt_size);

    /** @brief Reallocate a block of memory.
     *
     * This is part of the S compatibility interface, and is used when
     * it is decided that a block of memory previously allocated by
     * S_alloc() or S_realloc() needs to be expanded.  It allocates a
     * new block of memory, copies across the previous contents, and
     * zeroes any additional elements.
     *
     * @param prev_block Pointer to a block of memory previously
     *          allocated by S_alloc() or S_realloc().
     *
     * @param new_sz New number of elements (>= 0) to be
     *          accommodated.
     *
     * @param old_sz Number of elements contained in prev_block.
     *
     * @param elt_size Size in bytes (strictly, as a multiple of
     *          sizeof(char)) of each data item.  Must be
     *          non-negative.
     *
     * @return Pointer to the start of the newly allocated memory
     *         block.  If \a new_sz \c <= \a old_sz, the function does
     *         not allocate a new block, and simply returns \a
     *         prev_block.
     */
    char *S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size);
} // extern "C"

#endif // RALLOCSTACK_HPP
