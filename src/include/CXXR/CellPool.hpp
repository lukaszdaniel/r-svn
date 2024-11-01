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

/** @file CellPool.hpp
 *
 * @brief Class CXXR::CellPool.
 */

#ifndef CELLPOOL_HPP
#define CELLPOOL_HPP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstddef>
#include <memory>
#include <new>
#include <vector>
#include <forward_list>
#include <iostream>
#include <algorithm>
#include <cstdlib>
#include <stdexcept>
#include <CXXR/RTypes.hpp>

// TODO: Similar predefines also in memory.cpp
// TODO: Move to a separate header.
#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
#ifdef HAVE_VALGRIND_MEMCHECK_H
#include "valgrind/memcheck.h"
#endif
#endif

namespace CXXR
{
    /** @brief Class to manage a pool of memory cells of a fixed size.
     *
     * This class, based closely on Item 10 of Scott Meyers' 'Effective
     * C++ (2nd edition)' manages a collection of memory cells of a
     * specified size, and is intended as a back-end to implementations of
     * operator new and operator delete to enable the allocation and
     * deallocation of small objects quickly.
     *
     * Class CellPool operates a last-in-first-out allocation
     * policy; that is to say, if there are cells that have been
     * deallocated and not yet reallocated, the next one to be
     * reallocated will be the one that was most recently
     * deallocated.  This makes for efficient utilisation of the
     * processor caches.
     */
    class CellPool
    {
    public:
        /** @brief Constructor.
         *
         * Note that CellPool objects must be initialized by calling
         * initialize() before being used.
         */
        CellPool() : m_cells_allocated(0)
        {
#if VALGRIND_LEVEL >= 2
            VALGRIND_CREATE_MEMPOOL(this, 0, 0);
#endif
        }

        /** @brief Destructor
         *
         * It is up to the user to check that any cells allocated from
         * the pool have been freed before this destructor is
         * invoked.  (Although the destructor could check this for
         * itself and issue an error message, this message would
         * probably be a nuisance if it occurred during program shutdown.)
         */
        ~CellPool();

        /** @brief Allocate a cell from the pool.
         *
         * @return a pointer to the allocated cell.
         *
         * @throws bad_alloc if a cell cannot be allocated.
         */
        void *allocate()
        {
            if (m_free_cells.empty())
            {
                seekMemory();
            }
            // check();

            void *cell = m_free_cells.front();
            m_free_cells.pop_front();
            ++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
            VALGRIND_MEMPOOL_ALLOC(this, cell, cellSize());
#endif
            // check();
            return cell;
        }

        /** @brief Allocate a cell 'from stock'.
         *
         * Allocate a cell from the heap, provided it can be allocated
         * 'from stock'.  Can be useful when called from other inlined
         * functions in that it doesn't throw any exceptions.
         *
         * @return a pointer to the allocated cell, or 0 if the cell
         * cannot be allocated from the current memory superblocks.
         */
        void *easyAllocate() noexcept
        {
            if (m_free_cells.empty())
                return nullptr;
            // check();
            void *c = m_free_cells.front();
            m_free_cells.pop_front();
            ++m_cells_allocated;
#if VALGRIND_LEVEL >= 2
            VALGRIND_MEMPOOL_ALLOC(this, c, cellSize());
#endif
            // check();
            return c;
        }

        /** @brief Deallocate a cell
         *
         * @param p Pointer to a block of memory previously allocated
         * from this pool, or a null pointer (in which case method
         * does nothing).
         */
        void deallocate(void *p)
        {
            if (!p)
                return;
#ifdef DEBUG_RELEASE_MEM
            checkAllocatedCell(p);
#endif
#if VALGRIND_LEVEL >= 2
            VALGRIND_MEMPOOL_FREE(this, p);
#endif
            // check();
            m_free_cells.emplace_front(new (p) char(cellSize()));
            --m_cells_allocated;
            // check();
        }

        /** @brief Initialize the CellPool.
         *
         * This function must be called exactly once for each
         * CellPool, before any allocation is made from it.
         *
         * @param dbls_per_cell (must be >= 1). Size of cells,
         *         expressed as a multiple of sizeof(double).  For
         *         example, if you require cells large enough to
         *         contain one double, put dbls_per_cell as 1.  (NB:
         *         cells can contain anything, not just doubles; we
         *         work in doubles because these are likely to have
         *         the most stringent address alignment requirements.)
         *
         * @param cells_per_superblock (must be >= 1).  Memory for cells is
         *         obtained from the main heap in 'superblocks'
         *         sufficient to contain this many cells.
         */
        void initialize(size_t dbls_per_cell, size_t cells_per_superblock);

        /** @brief Size of cells.
         *
         * @return the size of each cell in bytes (well, strictly as a
         * multiple of sizeof(char)).
         */
        size_t cellSize() const { return m_admin->m_cell_size; }

        /** @brief Number of cells allocated from this CellPool.
         *
         * @return the number of cells currently allocated from this
         * pool.
         */
        size_t cellsAllocated() const { return m_cells_allocated; }

        /**
         * @return The size in bytes of the superblocks from which
         *         cells are allocated.
         */
        size_t superblockSize() const { return m_admin->m_superblock_size; }

        /** @brief Integrity check.
         *
         * Aborts the program with an error message if the object is
         * found to be internally inconsistent.
         *
         * @return true, if it returns at all.  The return value is to
         * facilitate use with \c assert.
         */
        void check() const;

        /** @brief Reorganise list of free cells within the CellPool.
         *
         * This is done with a view to increasing the probability that
         * successive allocations will lie within the same cache line
         * or (less importantly nowadays) memory page.
         */
        void defragment();

    private:
        // We put data fields that are used relatively rarely in a
        // separate data structure stored on the heap, so that an
        // array of CellPool objects, as used in MemoryBank, can be as
        // compact as possible.
        struct Admin
        {
            const size_t m_cell_size;
            const size_t m_cells_per_superblock;
            const size_t m_superblock_size;
            std::vector<void *> m_superblocks;

            /**
             * @param dbls_per_cell (must be >= 1). Size of cells,
             *         expressed as a multiple of sizeof(double).  For
             *         example, if you require cells large enough to
             *         contain one double, put dbls_per_cell as 1.  (NB:
             *         cells can contain anything, not just doubles; we
             *         work in doubles because these are likely to have
             *         the most stringent address alignment requirements.)
             *
             * @param cells_per_superblock (must be >= 1).  Memory for cells is
             *         obtained from the main heap in 'superblocks'
             *         sufficient to contain this many cells.
             */
            Admin(size_t dbls_per_cell, size_t cells_per_superblock)
                : m_cell_size(dbls_per_cell * sizeof(double)),
                  m_cells_per_superblock(cells_per_superblock),
                  m_superblock_size(m_cell_size * cells_per_superblock) {}

            size_t cellsExisting() const
            {
                return m_cells_per_superblock * m_superblocks.size();
            }
        };

        std::unique_ptr<Admin> m_admin;
        std::forward_list<void *> m_free_cells;
        size_t m_cells_allocated;

        /** @brief Validates if a pointer is a valid cell within this pool. */
        void checkCell(const void *p) const;

        // Calls checkCell, and further checks that the cell is not on
        // the free list:
        void checkAllocatedCell(const void *p) const;

        /** @brief Counts the number of free cells in the pool. */
        size_t cellsFree() const;

        // Return true iff c belongs to the heap at root:
        bool isFreeCell(const void *c) const;

        /** @brief Allocates a new superblock and returns a pointer to the first cell. */
        void seekMemory();

        CellPool(CellPool &) = delete;
        CellPool(CellPool &&) = delete;
        CellPool &operator=(CellPool &) = delete;
        CellPool &operator=(CellPool &&) = delete;
    };
} // namespace CXXR

#endif /* CELLPOOL_HPP */
