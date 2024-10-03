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
#include <CXXR/GCRoot.hpp>

namespace CXXR
{
    /* saved bcEval() state for implementing recursion using goto */
    using R_bcFrame_type = struct R_bcFrame;

    /** @brief ByteCode interpreter.
     */
    class ByteCode : public RObject
    {
    public:
        /** @brief Is an RObject a ByteCode?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a ByteCode.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == BCODESXP;
        }

        static ptrdiff_t codeDistane(SEXP body, void *bcpc);

        static void *s_BCpc; /* current byte code instruction */
#define R_BCpc CXXR::ByteCode::s_BCpc
        static GCRoot<> s_bc_body; /* current byte code object */
#define R_BCbody CXXR::ByteCode::s_bc_body
        static R_bcFrame_type *s_BCFrame; /* bcEval() frame */
#define R_BCFrame CXXR::ByteCode::s_BCFrame
        static bool s_BCIntActive; /* bcEval called more recently than eval */
#define R_BCIntActive CXXR::ByteCode::s_BCIntActive
    };
} // namespace CXXR

namespace R
{
    bool (isByteCode)(SEXP x);
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
