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

/** @file Allocatortest.cpp
 *
 * Test of class CXXR::Allocator
 */

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <sstream>
#include <vector>
#include <CXXR/GCManager.hpp>
#include <CXXR/Allocator.hpp>
#include <CXXR/MemoryBank.hpp>

namespace
{
    void usage(const char *cmd)
    {
        std::cerr << "Usage: " << cmd << " num_init_allocs num_churns\n";
        exit(1);
    }

    std::list<int, CXXR::Allocator<int>> ilist;

    std::vector<std::list<int>::iterator, CXXR::Allocator<std::list<int>::iterator>> ilv;

    void alloc()
    {
        static int serial = 0;
        std::cout << "Allocating list item #" << serial << std::endl;
        ilv.push_back(ilist.insert(ilist.end(), serial++));
    }

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

    size_t cueGC(size_t bytes)
    {
        std::cout << "GC cued for " << bytes << "\n";
        return 0;
    }

    void monitor(size_t bytes)
    {
        std::cout << "Monitored allocation of " << bytes << " bytes\n";
    }
} // namespace

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
            alloc();
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
            if (rnd & 2 || ilv.empty())
                alloc();
            else
            {
                // Select element to deallocate:
                unsigned int k = int(double(rnd) * double(ilv.size()) / 1024.0);
                std::cout << "Deallocating list item #" << *ilv[k] << std::endl;
                ilist.erase(ilv[k]);
                std::swap(ilv[k], ilv.back());
                ilv.pop_back();
            }
        }
        CXXR::MemoryBank::check();
        std::cout << "Blocks allocated: " << CXXR::MemoryBank::blocksAllocated()
                  << "\nBytes allocated: " << CXXR::MemoryBank::bytesAllocated() << std::endl;
    }
    // Clear up:
    {
        for (unsigned int k = 0; k < ilv.size(); ++k)
        {
            std::cout << "Deallocating list item #" << *ilv[k] << std::endl;
            ilist.erase(ilv[k]);
        }
    }
    return 0;
}
