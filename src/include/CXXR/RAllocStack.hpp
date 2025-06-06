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
 *
 * Defines functions R_alloc() and related functions, and the class
 * CXXR::RAllocStack which is used to implement them.
 */

#ifndef RALLOCSTACK_HPP
#define RALLOCSTACK_HPP

#include <stack>
#include <vector>
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
        /** @brief Schwarz counter.
         *
         * The Schwarz counter (see for example Stephen C. Dewhurst's
         * book 'C++ Gotchas') is a programming idiom to ensure that a
         * class (including particularly its static members) is
         * initialized before any client of the class requires to use
         * it, and that on program exit the class's static resources
         * are not cleaned up prematurely (e.g. while the class is
         * still in use by another class's static members).  Devices
         * such as this are necessitated by the fact that the standard
         * does not prescribe the order in which objects of file and
         * global scope in different compilation units are
         * initialized: it only specifies that the order of
         * destruction must be the reverse of the order of
         * initialization.
         *
         * This is achieved by the unusual stratagem of including the
         * \e definition of a lightweight data item within this header
         * file.  This data item is of type MemoryBank::SchwarzCounter, and is
         * declared within an anonymous namespace.  Each file that
         * <tt>\#include</tt>s this header file will therefore include
         * a definition of a SchwarzCounter object, and this definition
         * will precede any data definitions within the enclosing file
         * that depend on class MemoryBank.  Consequently, the SchwarzCounter
         * object will be constructed before any data objects of the
         * client file.  The constructor of SchwarzCounter is so defined
         * that when the first such object is created, the class MemoryBank
         * will itself be initialized.
         *
         * Conversely, when the program exits, data items within each
         * client file will have their destructors invoked before the
         * file's SchwarzCounter object has its destructor invoked.  This
         * SchwarzCounter destructor is so defined that only when the last
         * SchwarzCounter object is destroyed is the MemoryBank class itself
         * cleaned up.
         */
        class SchwarzCounter
        {
        public:
            SchwarzCounter()
            {
                if (!s_count++)
                    RAllocStack::initialize();
            }

            ~SchwarzCounter()
            {
                if (!--s_count)
                    RAllocStack::cleanup();
            }

        private:
            static unsigned int s_count;
        };

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
                  m_saved_size(RAllocStack::size())
            {
                RAllocStack::s_innermost_scope = this;
            }

            ~Scope()
            {
                if (RAllocStack::size() != m_saved_size)
                    RAllocStack::trim(m_saved_size);
                RAllocStack::s_innermost_scope = m_next_scope;
            }

            /** @brief RAllocStack size at construction.
             *
             * @return The size of the RAllocStack at the time this
             * Scope object was constructed.  The RAllocStack will be
             * restored to this size by the Scope destructor.
             */
            size_t startSize() const
            {
                return m_saved_size;
            }

        private:
            Scope *m_next_scope;
            size_t m_saved_size;
        };

        /** @brief Allocate a new block of memory.
         *
         * The block will be aligned on a multiple of
         * <tt>sizeof(double)</tt>.
         * @param sz The required size in bytes (strictly, as a
         *           multiple of <tt>sizeof(char)</tt>), of the memory block.
         * @return Pointer to the start of the memory block.
         */
        static void *allocate(size_t sz);

        /** @brief Restore stack to a previous size.
         *
         * Restore the stack to a previous size by popping elements
         * off the top.
         *
         * @param new_size The size to which the stack is to be
         *          restored.  Must not be greater than the current
         *          size.
         *
         * @note In future this method may cease to be available,
         * since the use of the RAllocStack::Scope class is
         * preferable.
         */
        static void restoreSize(size_t new_size);

        /** @brief Current size of stack.
         *
         * @return the current size of the stack.
         *
         * @note This method is intended for use in conjunction with
         * restoreSize(), and may cease to be public in future.
         */
        static size_t size()
        {
            return s_stack->size();
        }

    private:
        friend class GCNode;
        using Pair = std::pair<size_t, void *>;
        using Stack = std::stack<Pair, std::vector<Pair>>;
        static Stack *s_stack;
        static Scope *s_innermost_scope;

        // Clean up static data at end of run (called by
        // RAllocStack::SchwarzCtr destructor):
        static void cleanup()
        {
            // delete s_stack;
        }

        // Initialize the static data members:
        friend void initializeMemorySubsystem();
        // Initialize static data (called by RAllocStack::SchwarzCtr constructor):
        static void initialize();

        // Pop entries off the stack to reduce its size to new_size,
        // which must be no greater than the current size.
        static void trim(size_t new_size);

        RAllocStack() = delete;
    };
} // namespace CXXR

namespace
{
    // CXXR::SchwarzCounter<CXXR::RAllocStack> rallocstack_schwarz_ctr;
    CXXR::RAllocStack::SchwarzCounter rallocstack_schwarz_ctr;
}

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

    /** @brief Allocate a block of memory.
     *
     * This is a wrapper round R_alloc() for the use of former CR code
     * that is now treated as C++.  It differs from R_alloc() in
     * returning void* rather than char*, thus allowing conversion to
     * the required pointer type to be achieved by static_cast rather
     * than reinterpret_cast.
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
     * @deprecated For use only as described above.  The function is
     * not accessible from C, and new C++ code should use C++ memory
     * allocation mechanisms (i.e. new/delete).
     */
    inline void *CXXR_alloc(size_t num_elts, int elt_size)
    {
        return static_cast<void *>(R_alloc(num_elts, elt_size));
    }

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

    /** @brief Number of memory blocks allocated.
     *
     * @return The current number of blocks allocated via R_alloc(),
     *         S_alloc() and S_realloc(), coerced into a char* for
     *         compatibility with CR.  (This function and vmaxset()
     *         are declared in the R.h API.)
     *
     * @note C++ code should preferably use the CXXR::RAllocStack::Scope
     *       class instead.  It is possible that in
     *       the future this function will always return a null pointer.
     */
    inline void *vmaxget(void)
    {
        return reinterpret_cast<void *>(CXXR::RAllocStack::size());
        // return static_cast<char *>(nullptr) + CXXR::RAllocStack::size();
    }

    /** @brief Reclaims memory blocks.
     *
     * @param stack_sizep A value previously returned by a call to
     *          vmaxget().  vmaxset() will reclaim the memory
     *          from all blocks allocated via R_alloc(),
     *          S_alloc() and S_realloc() subsequent to that call of
     *          vmaxget().
     *
     * @deprecated For expert use only.  C++ code should preferably
     *             use the CXXR::RAllocStack::Scope class instead.  It is possible
     *             that in the future this function will become a no-op.
     */
    inline void vmaxset(const void *stack_sizep)
    {
        size_t stack_size = size_t(static_cast<const char *>(stack_sizep) - static_cast<const char *>(nullptr));
        CXXR::RAllocStack::restoreSize(stack_size);
    }
} // extern "C"

#endif // RALLOCSTACK_HPP
