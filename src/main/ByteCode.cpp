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

/** @file ByteCode.cpp
 *
 * @brief Class CXXR::ByteCode.
 */

#include <stdexcept>
#include <CXXR/ByteCode.hpp>

namespace CXXR
{
    GCRoot<> ByteCode::s_bc_body;
    void *ByteCode::s_BCpc = nullptr;
    R_bcFrame_type *ByteCode::s_BCFrame = nullptr;
    bool ByteCode::s_BCIntActive = false;
    bool ByteCode::s_bytecode_disabled = false; // R_disable_bytecode
    std::unique_ptr<NodeStack> ByteCode::s_nodestack;

    void ByteCode::initialize()
    {
        if (s_nodestack)
        {
            throw std::runtime_error("ByteCode nodestack is already initialized.");
        }

        s_nodestack = std::make_unique<NodeStack>(300000);
    }

    void ByteCode::initInterpreter()
    {
#ifdef THREADED_CODE
        bcEval_loop(nullptr);
#endif
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

