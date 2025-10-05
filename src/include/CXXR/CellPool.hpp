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

#include <memory>
#include <vector>
#include <cstdint>

// #define CXXR_USE_SKEW_HEAP

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
     *
     * If CXXR_USE_SKEW_HEAP is defined then CellPool always allocates the cell
     * with the minimum address from among the currently available cells.
     * This is achieved using a skew heap data structure (Sleator and Tarjan
     * 1986).  This data structure works most efficiently if cells are
     * as far as possible released in the reverse order of allocation.
     */
    class CellPool
    {
    public:
        /** @brief Constructor.
         *
         * Note that CellPool objects must be initialized by calling
         * initialize() before being used.
         */
        CellPool() : m_admin(nullptr), m_free_cells(nullptr)
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
            void *cell = pop();
            if (!cell)
            {
                cell = seekMemory();
                // check();
            }

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
            void *cell = pop();
            if (!cell)
            {
                return nullptr;
            }
            // check();

#if VALGRIND_LEVEL >= 2
            VALGRIND_MEMPOOL_ALLOC(this, cell, cellSize());
#endif
            // check();
            return cell;
        }

        /** @brief Deallocate a cell
         *
         * @param p Pointer to a block of memory previously allocated
         * from this pool, or a null pointer (in which case method
         * does nothing).
         */
        void deallocate(void *ptr)
        {
            if (!ptr)
                return;
#ifdef DEBUG_RELEASE_MEM
            checkAllocatedCell(ptr);
#endif
#if VALGRIND_LEVEL >= 2
            VALGRIND_MEMPOOL_FREE(this, ptr);
            VALGRIND_MAKE_MEM_UNDEFINED(ptr, sizeof(Cell));
#endif
            // check();
            Cell *cell = static_cast<Cell *>(ptr);
            cell->m_next = m_free_cells;
            m_free_cells = cell;
#ifdef CXXR_USE_SKEW_HEAP
            defragment();
#endif
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
        void initialize(uint16_t dbls_per_cell, uint16_t cells_per_superblock);

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
        size_t cellsAllocated() const
        {
            return m_admin->cellsAvailable() - cellsFree() - cellsPendingAllocation();
        }

        /**
         * @return The size in bytes of the superblocks from which
         *         cells are allocated.
         */
        size_t superblockSize() const { return m_admin->m_cell_size * m_admin->m_cells_per_superblock; }

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
        struct Cell
        {
            Cell *m_next;

            explicit Cell(Cell *next = nullptr) : m_next(next) {}
        };

        void *pop()
        {
            Cell *cell = nullptr;

            if (m_free_cells)
            {
                cell = m_free_cells;
                m_free_cells = m_free_cells->m_next;
            }

            return static_cast<void *>(cell);
        }

        // We put data fields that are used relatively rarely in a
        // separate data structure stored on the heap, so that an
        // array of CellPool objects, as used in MemoryBank, can be as
        // compact as possible.
        struct Admin
        {
            const size_t m_cell_size;
            const uint16_t m_cells_per_superblock;
            std::vector<std::unique_ptr<char[]>> m_superblocks;
            size_t m_cell_index;
            char *m_pool;

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
            Admin(uint16_t dbls_per_cell, uint16_t cells_per_superblock)
                : m_cell_size(dbls_per_cell * sizeof(double)),
                  m_cells_per_superblock(cells_per_superblock),
                  m_cell_index(cells_per_superblock), m_pool(nullptr) {}

            size_t cellsAvailable() const
            {
                return m_cells_per_superblock * m_superblocks.size();
            }
        };

        std::unique_ptr<Admin> m_admin;
        Cell *m_free_cells;

        /** @brief Validates if a pointer is a valid cell within this pool. */
        void checkCell(const void *p) const;

        // Calls checkCell, and further checks that the cell is not on
        // the free list:
        void checkAllocatedCell(const void *p) const;

        /** @brief Counts the number of free cells in the pool. */
        size_t cellsFree() const;

        /** @brief Counts the number pending cells in the current pool. */
        uint16_t cellsPendingAllocation() const { return (m_admin->m_cells_per_superblock - m_admin->m_cell_index); }

        // Helper method to check if a given block is on the m_free_cells
        bool isOnFreeCellList(const void *c) const;

        /** @brief Allocates a new superblock if needed and returns a pointer to the first available cell. */
        void *seekMemory();

        CellPool(const CellPool &) = delete;
        CellPool(CellPool &&) = delete;
        CellPool &operator=(const CellPool &) = delete;
        CellPool &operator=(CellPool &&) = delete;
    };
} // namespace CXXR

#endif // CELLPOOL_HPP
