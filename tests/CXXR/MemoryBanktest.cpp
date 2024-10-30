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

/** @file MemoryBanktest.cpp
 *
 * Test of class CXXR::MemoryBank
 */

#ifndef R_MEMORY_PROFILING
#define R_MEMORY_PROFILING
#endif

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <CXXR/GCManager.hpp>
#include <CXXR/MemoryBank.hpp>

namespace
{
    void usage(const char *cmd)
    {
        std::cerr << "Usage: " << cmd << " num_init_allocs num_churns\n";
        exit(1);
    }

    struct Tr
    {
        unsigned int serial;
        size_t size;
        char *cptr;

        Tr(unsigned int sr, size_t sz, char *cp)
            : serial(sr), size(sz), cptr(cp)
        {
        }
    };

    std::vector<Tr> trs;

    // Crude congruential generator in range 0 to 1023; repeatability
    // on different platforms is more important than randomness!
    // Deliberately the first value returned is 0.
    size_t qrnd()
    {
        static size_t r = 0;
        size_t ans = r;
        r = (r * 633 + 633) & 0x3ff;
        return ans;
    }

    // Allocate a block of size bytes.
    void alloc(size_t bytes)
    {
        static int serial = 0;
        std::cout << "Allocating #" << serial << " with size "
                  << bytes << std::endl;
        char *cptr = reinterpret_cast<char *>(CXXR::MemoryBank::allocate(bytes));
        memset(cptr, 0, bytes);
        trs.push_back(Tr(serial++, bytes, cptr));
    }

    size_t cueGC(size_t bytes)
    {
        std::cout << "GC cued for " << bytes << "\n";
        return 0;
    }

    void monitor(size_t bytes)
    {
        std::cout << "Monitored allocation of " << bytes << " bytes\n";
    }
} // anonymous namespace

bool CXXR::GCManager::FORCE_GC()
{
    return false;
}

int main(int argc, char *argv[])
{
    if (argc != 3)
        usage(argv[0]);
    unsigned int num_init_allocs, num_churns;
    // Get number of initial allocations:
    {
        std::istringstream is(argv[1]);
        if (!(is >> num_init_allocs))
            usage(argv[0]);
    }
    // Get number of churn operations:
    {
        std::istringstream is(argv[2]);
        if (!(is >> num_churns))
            usage(argv[0]);
    }
    // Carry out initial allocations:
    {
        CXXR::MemoryBank::setMonitor(monitor, 100);
        for (unsigned int i = 0; i < num_init_allocs; ++i)
            alloc(qrnd());
        CXXR::MemoryBank::check();
        std::cout << "Blocks allocated: " << CXXR::MemoryBank::blocksAllocated()
                  << "\nBytes allocated: " << CXXR::MemoryBank::bytesAllocated() << std::endl;
    }
    // Carry out churns:
    {
        CXXR::MemoryBank::setMonitor(nullptr);
        CXXR::MemoryBank::setGCCuer(cueGC, 0);
        for (unsigned int i = 0; i < num_churns; ++i)
        {
            long rnd = qrnd();
            if (rnd & 2 || trs.empty())
                alloc(rnd);
            else
            {
                // Select element to deallocate:
                unsigned int k = int(double(rnd) * double(trs.size()) / 1024.0);
                std::cout << "Deallocating #" << trs[k].serial << std::endl;
                CXXR::MemoryBank::deallocate(trs[k].cptr, trs[k].size);
                std::swap(trs[k], trs.back());
                trs.pop_back();
            }
        }
        CXXR::MemoryBank::check();
        std::cout << "Blocks allocated: " << CXXR::MemoryBank::blocksAllocated()
                  << "\nBytes allocated: " << CXXR::MemoryBank::bytesAllocated() << std::endl;
    }
    // Clear up:
    {
        for (unsigned int k = 0; k < trs.size(); ++k)
        {
            std::cout << "Deallocating #" << trs[k].serial << std::endl;
            CXXR::MemoryBank::deallocate(trs[k].cptr, trs[k].size);
        }
    }
    return 0;
}
