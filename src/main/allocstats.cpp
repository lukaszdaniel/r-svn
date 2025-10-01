/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include <CXXR/MemoryBank.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/IntVector.hpp>
#include <Rinternals.h>

using namespace CXXR;

#ifdef ALLOC_STATS
constexpr int nbins = 32;
namespace CXXR
{
    extern size_t alloc_counts[nbins];
    extern size_t free_counts[nbins];
} // namespace CXXR
#endif

// Returns a vector list with columns 'size', 'alloc', 'free' representing alloc and free stats.
SEXP allocstats(void)
{
#ifdef ALLOC_STATS

    // Copy frequency tables to avoid concurrent modifications affecting result.
    size_t allocs[nbins];
    size_t frees[nbins];
    std::copy(std::begin(alloc_counts), std::end(alloc_counts), std::begin(allocs));
    std::copy(std::begin(free_counts), std::end(free_counts), std::begin(frees));

    GCStackRoot<IntVector> size_column;
    size_column = IntVector::create(nbins);
    GCStackRoot<IntVector> alloc_column;
    alloc_column = IntVector::create(nbins);
    GCStackRoot<IntVector> free_column;
    free_column = IntVector::create(nbins);
    GCStackRoot<> ans;
    ans = Rf_allocVector(VECSXP, 3);

    for (int i = 0; i < nbins; ++i)
    {
        SET_INTEGER_ELT(size_column, i, (i + 1) * 8);
        SET_INTEGER_ELT(alloc_column, i, allocs[i]);
        SET_INTEGER_ELT(free_column, i, frees[i]);
    }
    SET_VECTOR_ELT(ans, 0, size_column);
    SET_VECTOR_ELT(ans, 1, alloc_column);
    SET_VECTOR_ELT(ans, 2, free_column);

    return ans;
#else
    return nullptr;
#endif // ALLOC_STATS
}
