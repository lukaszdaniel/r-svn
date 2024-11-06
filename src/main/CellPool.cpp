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
#if VALGRIND_LEVEL >= 2
        VALGRIND_DESTROY_MEMPOOL(this);
#endif
    }

    void CellPool::check() const
    {
        if (cellsAllocated() + cellsFree() + cellsPendingAllocation() != m_admin->cellsAvailable())
        {
            std::cerr << "CellPool::check(): internal inconsistency\n";
            std::cerr << "cells allocated      = " << cellsAllocated() << "\n";
            std::cerr << "free cells           = " << cellsFree() << "\n";
            std::cerr << "cells per superblock = " << m_admin->m_cells_per_superblock << "\n";
            std::cerr << "superblocks size     = " << m_admin->m_superblocks.size() << "\n";
            throw std::runtime_error("CellPool::check(): internal inconsistency detected.");
        }

        for (const Cell *cell = m_free_cells; cell; cell = cell->m_next)
        {
            checkCell(cell);
        }
    }

    void CellPool::checkAllocatedCell(const void *p) const
    {
        checkCell(p);
        if (isFreeCell(p))
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

        auto superblock_size = static_cast<long>(m_admin->m_cell_size * m_admin->m_cells_per_superblock);
        for (const auto &cell : m_admin->m_superblocks)
        {
            ptrdiff_t offset = pc - static_cast<const char *>(cell.get());
            if (offset >= 0 && offset < superblock_size)
            {
                if (std::size_t(offset) % m_admin->m_cell_size != 0)
                {
                    throw std::runtime_error("CellPool::checkCell : designated block is misaligned");
                }
                is_valid = true;
                break; // break out from the loop
            }
        }

        if (!is_valid)
        {
            throw std::runtime_error("CellPool::checkCell : designated block doesn't belong to this CellPool");
        }
#ifdef CXXR_USE_SKEW_HEAP
        Cell *prev = nullptr;
        for (Cell *cell = m_free_cells; cell; cell = cell->m_next)
        {
            if (prev && prev > cell)
            {
                throw std::runtime_error("CellPool::checkCell : child with lower address than parent.");
            }
            prev = cell;
        }
#endif
    }

    void CellPool::defragment()
    {
#ifndef CXXR_USE_SKEW_HEAP
        std::vector<Cell *> free_cell_list(cellsFree());
        // Assemble vector of pointers to free cells:
        {
            Cell *cell = m_free_cells;
            for (auto &entry : free_cell_list)
            {
                entry = cell;
                cell = cell->m_next;
            }
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
#endif
    }

    void CellPool::initialize(uint16_t dbls_per_cell, uint16_t cells_per_superblock)
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

    bool CellPool::isFreeCell(const void *p) const
    {
        if (!m_free_cells || !p)
            return false;
        const Cell *cp = static_cast<const Cell *>(p);
        for (Cell *c = m_free_cells; c; c = c->m_next)
            if (c == cp)
                return true;
        return false;
    }

    void CellPool::seekMemory()
    {
        if (m_admin->m_cell_index == m_admin->m_cells_per_superblock)
        {
#ifdef HAVE_POSIX_MEMALIGN
            void *memblock;
            if (posix_memalign(&memblock, 4096, m_admin->m_cell_size * m_admin->m_cells_per_superblock) != 0)
            {
                throw std::bad_alloc();
            }
            char *superblock = static_cast<char *>(memblock);
            m_admin->m_superblocks.push_back(std::unique_ptr<char[]>(superblock));
#else
            m_admin->m_superblocks.emplace_back(new char[m_admin->m_cell_size * m_admin->m_cells_per_superblock]);
#endif
            m_admin->m_pool = m_admin->m_superblocks.back().get();
            m_admin->m_cell_index = 0;
        }
    }
} // namespace CXXR
