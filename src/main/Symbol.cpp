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
 *  This program is free software; you can redistribute it and/or modify
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

/** @file Symbol.cpp
 *
 * @brief Implementation of class Symbol and associated C
 * interface.
 */

#include <cassert>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <R_ext/Error.h>
#include <Rinternals.h>

using namespace R;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DDVALptr = DDVAL;
        const auto &installptr = Rf_install;
        const auto &isSymbolptr = Rf_isSymbol;
        const auto &PRINTNAMEptr = PRINTNAME;
        const auto &SYMVALUEptr = SYMVALUE;
        const auto &SET_PRINTNAMEptr = R::SET_PRINTNAME;
        const auto &SET_SYMVALUEptr = R::SET_SYMVALUE;
        const auto &SET_DDVALptr = R::SET_DDVAL;
    } // namespace ForceNonInline

    SEXP *Symbol::s_symbol_table = nullptr;

    // Symbol::s_special_symbol_names is in names.cpp

    SEXP Symbol::unboundValue()
    {
        return R_UnboundValue;
    }

    SEXP Symbol::obtainS3Signature(const char *className,
                                   const char *methodName)
    {
        assert(className != nullptr);
        assert(methodName != nullptr);

        std::string signature = std::string(className) + "." + std::string(methodName);
        constexpr int maxLength = 512;
        if (signature.length() >= maxLength)
            Rf_error(_("signature is too long in '%s'"), signature.c_str());
        return obtain(signature);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****