/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2025 The R Core Team
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

#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include "grid.h"

using namespace CXXR;

bool isMask(SEXP mask) {
    return Rf_inherits(mask, "GridMask");
}

SEXP resolveMask(SEXP mask, pGEDevDesc dd)
{
    GCStackRoot<> resolveFn, R_fcall;
    resolveFn = findFun(install("resolveMask"), R_gridEvalEnv);
    R_fcall = lang2(resolveFn, mask);
    SEXP result = eval(R_fcall, R_gridEvalEnv);

    return result;
}

