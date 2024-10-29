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
        size_t free_cells = cellsFree(m_free_cells);
        if (m_cells_allocated + free_cells != m_admin->m_cells_per_superblock * m_admin->m_superblocks.size())
        {
            std::cerr << "CellHeap::check(): internal inconsistency\n";
            std::cerr << "cells allocated      = " << m_cells_allocated << "\n";
            std::cerr << "free cells           = " << free_cells << "\n";
            std::cerr << "cells per superblock = " << m_admin->m_cells_per_superblock << "\n";
            std::cerr << "superblocks size     = " << m_admin->m_superblocks.size() << "\n";
            throw std::runtime_error("CellHeap::check(): internal inconsistency detected.");
        }
    }

    void CellHeap::checkAllocatedCell(const void *p) const
    {
        checkCell(p);
        const Cell *cp = static_cast<const Cell *>(p);
        if (isFreeCell(m_free_cells, cp))
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
                is_valid = true;
                if (std::size_t(offset) % m_admin->m_cell_size != 0)
                {
                    throw std::runtime_error("CellHeap::checkCell : designated block is misaligned");
                }
                break; // break out from the loop
            }
        }

        if (!is_valid)
        {
            throw std::runtime_error("CellHeap::checkCell : designated block doesn't belong to this CellHeap");
        }

        const Cell *c = reinterpret_cast<const Cell *>(p);
        if ((c->m_l && c->m_l < c) || (c->m_r && c->m_r < c))
        {
            throw std::runtime_error("CellHeap::checkCell : child with lower address than parent.\n");
        }
    }

    void CellHeap::defragment()
    {
    }

    void CellHeap::initialize(size_t dbls_per_cell, size_t cells_per_superblock)
    {
        if (m_admin)
        {
            throw std::runtime_error("CellHeap is already initialized.");
        }
        m_admin = std::make_unique<Admin>(dbls_per_cell, cells_per_superblock);
    }

    size_t CellHeap::cellsFree(const Cell *root)
    {
        if (!root)
            return 0;
        return 1 + cellsFree(root->m_l) + cellsFree(root->m_r);
    }

    bool CellHeap::isFreeCell(const Cell *root, const Cell *c)
    {
        if (!root || !c)
            return false;
        return c == root || isFreeCell(root->m_l, c) || isFreeCell(root->m_r, c);
    }

    void CellHeap::meld_aux(Cell *host, Cell *guest)
    {
        std::swap(host->m_l, host->m_r);
        while (host->m_l)
        {
            // if (host >= host->m_l) abort();
            // if (host->m_r && host >= host->m_r) abort();
            // if (guest->m_l && guest >= guest->m_l) abort();
            // if (guest->m_r && guest >= guest->m_r) abort();
            if (guest < host->m_l)
                std::swap(host->m_l, guest);
            host = host->m_l;
            std::swap(host->m_l, host->m_r);
        }
        host->m_l = guest;
    }

    void CellHeap::seekMemory()
    {
        if (m_free_cells)
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
        Cell *next = nullptr;
        while (offset >= 0)
        {
            next = new (superblock + offset) Cell(next);
#if VALGRIND_LEVEL >= 2
            VALGRIND_MAKE_MEM_NOACCESS(next + 1, m_admin->m_cell_size - sizeof(Cell));
#endif
            offset -= static_cast<ptrdiff_t>(m_admin->m_cell_size);
        }
        m_free_cells = next;
    }
} // namespace CXXR
