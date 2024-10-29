/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

/** @file CellPool.cpp
 *
 * Implementation of class CellPool
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <CXXR/CellPool.hpp>

#ifdef HAVE_FEATURES_H
// For posix_memalign:
#define HAVE_POSIX_MEMALIGN
#endif

#ifdef HAVE_FEATURES_H
#include <features.h>
#endif

namespace CXXR
{
    CellPool::~CellPool()
    {
        if (!m_admin)
            return;
#if VALGRIND_LEVEL >= 2
        VALGRIND_DESTROY_MEMPOOL(this);
#endif
        for (auto &cell : m_admin->m_superblocks)
        {
#ifdef HAVE_POSIX_MEMALIGN
            free(cell);
#else
            ::operator delete(cell);
#endif
        }
    }

    void CellPool::check() const
    {
        unsigned int free_cells = 0;
        for (Cell *cell = m_free_cells; cell; cell = cell->m_next)
        {
            checkCell(cell);
            ++free_cells;
        }

        if (free_cells > m_admin->cellsExisting())
        {
            throw std::runtime_error("CellPool::check(): internal inconsistency");
        }
    }

    void CellPool::checkAllocatedCell(const void *p) const
    {
        checkCell(p);
        const Cell *cp = static_cast<const Cell *>(p);
        bool found = false;
        for (Cell *c = m_free_cells; !found && c; c = c->m_next)
            found = (c == cp);
        if (found)
        {
            throw std::runtime_error("CellPool::checkCell : designated block is (already) free.");
        }
    }

    void CellPool::checkCell(const void *p) const
    {
        if (!p)
            return;
        const char *pc = static_cast<const char *>(p);
        bool is_valid = false;

        for (const auto &cell : m_admin->m_superblocks)
        {
            ptrdiff_t offset = pc - static_cast<const char *>(cell);
            if (offset >= 0 && offset < static_cast<long>(m_admin->m_superblock_size) &&
                std::size_t(offset) % m_admin->m_cell_size == 0)
            {
                is_valid = true;
                if (std::size_t(offset) % m_admin->m_cell_size != 0)
                {
                    throw std::runtime_error("CellPool::checkCell : designated block is misaligned");
                }
                break; // break out from the loop
            }
        }

        if (!is_valid)
        {
            throw std::runtime_error("CellPool::checkCell : designated block doesn't belong to this CellPool");
        }
    }

    void CellPool::defragment()
    {
        std::vector<Cell *> free_cell_list(cellsFree());
        Cell *cell = m_free_cells;
        for (auto &entry : free_cell_list)
        {
            entry = cell;
            cell = cell->m_next;
        }
        // Sort by increasing address:
        std::sort(free_cell_list.begin(), free_cell_list.end());
        // Restring the pearls:
        {
            Cell *next = nullptr;
            for (auto it = free_cell_list.rbegin(); it != free_cell_list.rend(); ++it)
            {
                (*it)->m_next = next;
                next = *it;
            }
            m_free_cells = next;
        }
        // check();
    }

    void CellPool::initialize(size_t dbls_per_cell, size_t cells_per_superblock)
    {
        if (m_admin)
        {
            throw std::runtime_error("CellPool is already initialized.");
        }
        m_admin = std::make_unique<Admin>(dbls_per_cell, cells_per_superblock);
    }

    size_t CellPool::cellsFree() const
    {
        size_t ans = 0;
        for (const Cell *cell = m_free_cells; cell; cell = cell->m_next)
        {
            ++ans;
        }
        return ans;
    }

    CellPool::Cell *CellPool::seekMemory()
    {
#ifdef HAVE_POSIX_MEMALIGN
        void *memblock;
        // We assume the memory page size is some multiple of 4096 bytes:
        if (posix_memalign(&memblock, 4096, m_admin->m_superblock_size) != 0)
        {
            throw std::bad_alloc();
        }
        char *superblock = static_cast<char *>(memblock);
#else
        char *superblock = static_cast<char *>(::operator new(m_admin->m_superblock_size));
        if (!superblock)
        {
            throw std::bad_alloc();
        }
#endif
        m_admin->m_superblocks.push_back(superblock);

        ptrdiff_t offset = static_cast<ptrdiff_t>(m_admin->m_superblock_size - m_admin->m_cell_size);
        Cell *next = nullptr;
        while (offset >= 0)
        {
            next = new (superblock + offset) Cell(next);
#if VALGRIND_LEVEL >= 2
            VALGRIND_MAKE_MEM_NOACCESS(next + 1, m_admin->m_cell_size - sizeof(Cell));
#endif
            offset -= static_cast<ptrdiff_t>(m_admin->m_cell_size);
        }
        return next;
    }
} // namespace CXXR
