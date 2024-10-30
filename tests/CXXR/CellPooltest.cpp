/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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

/** @file CellPooltest.cpp
 *
 * Test of class CXXR::CellPool
 */

#include <iostream>
#include <CXXR/CellPool.hpp>

namespace
{
    double *dptrs[16];

    CXXR::CellPool pool;
} // anonymous namespace

int main()
{
    pool.initialize(1, 5);
    for (int i = 0; i < 16; ++i)
        dptrs[i] = 0;
    pool.check();
    std::cout << "Cell size: " << pool.cellSize()
              << "\nSuperblock size: " << pool.superblockSize() << std::endl;
    for (int i = 0; i < 10; ++i)
    {
        std::cout << "Allocating dptrs[" << i << "]\n";
        dptrs[i] = static_cast<double *>(pool.allocate());
    }
    pool.check();
    std::cout << "Cells allocated: " << pool.cellsAllocated() << std::endl;
    for (int i = 3; i < 10; i += 2)
    {
        std::cout << "Deallocating dptrs[" << i << "]\n";
        pool.deallocate(dptrs[i]);
    }
    pool.check();
    std::cout << "Cells allocated: " << pool.cellsAllocated() << std::endl;
    for (int i = 11; i < 16; i += 2)
    {
        std::cout << "Allocating dptrs[" << i << "]\n";
        dptrs[i] = static_cast<double *>(pool.allocate());
    }
    pool.check();
    std::cout << "Cells allocated: " << pool.cellsAllocated() << std::endl;
    return 0;
}
