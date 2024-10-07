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

/** @file PairList.cpp
 *
 * @brief Class PairList and associated C interface.
 */

#include <CXXR/PairList.hpp>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &allocListptr = Rf_allocList;
        const auto &allocSExpptr = Rf_allocSExp;
        const auto &consptr = Rf_cons;
        const auto &BINDING_IS_LOCKEDptr = R::BINDING_IS_LOCKED;
        const auto &IS_ACTIVE_BINDINGptr = R::IS_ACTIVE_BINDING;
        const auto &LOCK_BINDINGptr = R::LOCK_BINDING;
        const auto &SET_ACTIVE_BINDING_BITptr = R::SET_ACTIVE_BINDING_BIT;
        const auto &UNLOCK_BINDINGptr = R::UNLOCK_BINDING;
    } // namespace ForceNonInline
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

