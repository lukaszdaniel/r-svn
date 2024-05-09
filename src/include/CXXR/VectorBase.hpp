/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/** @file VectorBase.hpp
 * @brief Class CXXR::VectorBase and associated C interface.
 */

#ifndef VECTORBASE_HPP
#define VECTORBASE_HPP

#include <CXXR/RObject.hpp>

namespace CXXR
{
/* The generational collector uses a reduced version of RObject as a
   header in vector nodes.  The layout MUST be kept consistent with
   the RObject definition. The standard RObject takes up the size of 7 doubles
   and the reduced version takes 6 doubles on most 64-bit systems. On most
   32-bit systems, RObject takes 8 doubles and the reduced version 7 doubles. */
class VectorBase : public GCNode {
    public:
    using size_type = R_xlen_t;

    VectorBase(SEXPTYPE stype = NILSXP) : GCNode(stype)
    {
        vecsxp.m_length = 0;
        vecsxp.m_truelength = 0;
        // vecsxp.m_data = nullptr;
    }
    // ~VectorBase() {}

    /** @brief Number of elements in the vector.
     *
     * @return The number of elements in the vector.
     *
     * @note AltRep uses its own version of size().
     */
    size_type size() const
    {
        return vecsxp.m_length;
    }

    /** @brief Number of occupied elements in the vector.
     *
     * @return The number of occupied elements in the vector.
     */
    size_type truelength() const
    {
        return vecsxp.m_truelength;
    }

    struct vecsxp_struct vecsxp;
};
typedef class VectorBase *VECSEXP;
} // namespace CXXR

#endif /* VECTORBASE_HPP */
