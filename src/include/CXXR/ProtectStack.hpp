/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file ProtectStack.hpp
 *
 * @brief Class CXXR::ProtectStack and associated C interface.
 *
 * CXXR::ProtectStack encapsulates the functionality of the CR pointer
 * protection stack.
 *
 * See the paragraph 'Caller Protects' in the description of class
 * CXXR::GCStackRoot for recommended coding policy.
 */

#ifndef PROTECTSTACK_HPP
#define PROTECTSTACK_HPP

#include <utility>
#include <vector>
#include <cstddef>
#include <CXXR/NodeStack.hpp>

namespace CXXR
{
    /** @brief Class implementing CR's 'pointer protection stack'.
     *
     * All members of this class are static.
     */
    class ProtectStack
    {
    public:
    };
} // namespace CXXR

namespace R
{
    /**
     * Check that the C pointer protection stack has the expected size,
     * and print a warning if not.
     *
     * @param op Operation being performed.
     *
     * @param save The expected size of the pointer protection stack.
     *
     * @todo A warning seems too mild a response in this eventuality.
     */
    void check_stack_balance(SEXP op, size_t save);

    std::pair<bool, unsigned int> Rf_isProtected(SEXP s);
} // namespace R

extern "C"
{
} // extern "C"

#endif // PROTECTSTACK_HPP
