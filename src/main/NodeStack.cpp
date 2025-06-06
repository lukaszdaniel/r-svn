/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file NodeStack.cpp
 *
 * Implementation of class NodeStack.
 */

#include <CXXR/NodeStack.hpp>

namespace CXXR
{
    NodeStack::NodeStack(size_t initial_capacity)
    {
        CXXR::NodeStack::m_R_BCNodeStackBase = std::make_unique<R_bcstack_t[]>(initial_capacity);
        m_R_BCNodeStackTop = m_R_BCNodeStackBase.get();
        m_R_BCNodeStackEnd = m_R_BCNodeStackBase.get() + initial_capacity;
        m_R_BCProtTop = m_R_BCNodeStackTop;
    }
} // namespace CXXR
