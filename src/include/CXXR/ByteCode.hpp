/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file ByteCode.hpp
 * @brief Class CXXR::ByteCode.
 */

#ifndef BYTECODE_HPP
#define BYTECODE_HPP

#include <CXXR/RObject.hpp>

namespace CXXR
{
} // namespace CXXR

namespace R
{
    bool isByteCode(SEXP x);
    void R_initialize_bcode(void);
    SEXP R_bcEncode(SEXP);
    SEXP R_bcDecode(SEXP);
    void R_registerBC(SEXP, SEXP);
    bool R_checkConstants(Rboolean);
    bool R_BCVersionOK(SEXP);
} // namespace R

extern "C"
{
    SEXP R_PromiseExpr(SEXP);
    SEXP R_ClosureExpr(SEXP);
    SEXP R_BytecodeExpr(SEXP e);
} // extern "C"

#endif /* BYTECODE_HPP */
