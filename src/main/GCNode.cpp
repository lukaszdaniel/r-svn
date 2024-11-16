/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

/** @file GCNode.cpp
 *
 * Class GCNode and associated C-callable functions.
 */

#include <stdexcept>
#include <CXXR/GCNode.hpp>
#include <CXXR/MemoryBank.hpp>

namespace CXXR
{
    size_t GCNode::s_num_nodes = 0;
    // struct GCNode::R_GenHeap_t GCNode::s_R_GenHeap;

#if CXXR_FALSE
    HOT_FUNCTION void *GCNode::operator new(size_t bytes)
    {
        return memset(MemoryBank::allocate(bytes), 0, bytes);
    }

    void GCNode::operator delete(void *pointer, size_t bytes)
    {
        MemoryBank::deallocate(pointer, bytes, static_cast<GCNode *>(pointer)->sxpinfo.gccls);
    }
#endif

    void GCNode::cleanup()
    {
    }

    void GCNode::initialize()
    {
    }
} // namespace CXXR
