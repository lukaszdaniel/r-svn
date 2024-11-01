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

/** @file CellHeap.cpp
 *
 * Implementation of class CellHeap
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <iterator>
#include <cstdlib>
#include <iostream>
#include <CXXR/CellHeap.hpp>

#ifdef HAVE_FEATURES_H
// For posix_memalign:
#define HAVE_POSIX_MEMALIGN
#endif

#ifdef HAVE_FEATURES_H
#include <features.h>
#endif

namespace CXXR
{
    CellHeap::~CellHeap()
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

    void CellHeap::check() const
    {
        size_t free_cells = cellsFree();
        if (m_cells_allocated + free_cells != m_admin->cellsExisting())
        {
            std::cerr << "CellHeap::check(): internal inconsistency\n";
            std::cerr << "cells allocated      = " << m_cells_allocated << "\n";
            std::cerr << "free cells           = " << free_cells << "\n";
            std::cerr << "cells per superblock = " << m_admin->m_cells_per_superblock << "\n";
            std::cerr << "superblocks size     = " << m_admin->m_superblocks.size() << "\n";
            throw std::runtime_error("CellHeap::check(): internal inconsistency detected.");
        }

        for (const auto &cell : m_free_cells)
        {
            checkCell(cell);
        }
    }

    void CellHeap::checkAllocatedCell(const void *p) const
    {
        checkCell(p);
        if (isFreeCell(p))
        {
            throw std::runtime_error("CellHeap::checkCell : designated block is (already) free.");
        }
    }

    void CellHeap::checkCell(const void *p) const
    {
        if (!p)
            return;
        const char *pc = static_cast<const char *>(p);
        bool is_valid = false;

        for (const auto &cell : m_admin->m_superblocks)
        {
            ptrdiff_t offset = pc - static_cast<const char *>(cell);
            if (offset >= 0 && offset < static_cast<long>(m_admin->m_superblock_size))
            {
                if (std::size_t(offset) % m_admin->m_cell_size != 0)
                {
                    throw std::runtime_error("CellHeap::checkCell : designated block is misaligned");
                }
                is_valid = true;
                break; // break out from the loop
            }
        }

        if (!is_valid)
        {
            throw std::runtime_error("CellHeap::checkCell : designated block doesn't belong to this CellHeap");
        }

        for (auto it = m_free_cells.begin(); it != m_free_cells.end(); ++it)
        {
            if (*it == p)
            {
                bool prevGreater = (it != m_free_cells.begin() && *std::prev(it) > p);
                bool nextLesser = (std::next(it) != m_free_cells.end() && *std::next(it) < p);
                if (prevGreater || nextLesser)
                {
                    throw std::runtime_error("CellHeap::checkCell : child with lower address than parent.");
                }
            }
        }
    }

    void CellHeap::defragment()
    {
        m_free_cells.sort();
        // check();
    }

    void CellHeap::initialize(size_t dbls_per_cell, size_t cells_per_superblock)
    {
        if (m_admin)
        {
            throw std::runtime_error("CellHeap is already initialized.");
        }
        m_admin = std::make_unique<Admin>(dbls_per_cell, cells_per_superblock);
    }

    size_t CellHeap::cellsFree() const
    {
        return m_free_cells.size();
    }

    bool CellHeap::isFreeCell(const void *c) const
    {
        if (m_free_cells.empty() || !c)
            return false;
        std::list<void *>::const_iterator pos = std::find(m_free_cells.begin(), m_free_cells.end(), c);
        return (pos != m_free_cells.end());
    }

    void CellHeap::seekMemory()
    {
        if (!m_free_cells.empty())
            return;

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
        while (offset >= 0)
        {
            m_free_cells.emplace_front(new (superblock + offset) char(cellSize()));
            offset -= static_cast<ptrdiff_t>(m_admin->m_cell_size);
        }
    }
} // namespace CXXR
