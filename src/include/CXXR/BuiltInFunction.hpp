/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2007-2014  Andrew R. Runnalls.
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

/** @file BuiltInFunction.hpp
 *
 * @brief Class CXXR::BuiltInFunction and associated C interface functions.
 */

#ifndef BUILTINFUNCTION_HPP
#define BUILTINFUNCTION_HPP

#include <CXXR/FunctionBase.hpp>

namespace CXXR
{
    /** @brief R function implemented within the interpreter.
     *
     * A BuiltInFunction object represents an R function that is
     * implemented within the interpreter by a function in C++ or C.
     * These objects are of two kinds, according to whether the
     * arguments passed to BuiltInFunction::apply() are evaluated
     * before being passed on to the encapsulated C/C++ function (CR's
     * BUILTINSXP), or are passed on unevaluated (SPECIALSXP).
     */
    class BuiltInFunction : public FunctionBase
    {
    public:
        BuiltInFunction(SEXPTYPE stype) : FunctionBase(stype)
        {
        }

        /** @brief Is an RObject a BuiltInFunction?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a BuiltInFunction.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == BUILTINSXP || st == SPECIALSXP;
        }

    private:
        // Declared private to ensure that BuiltInFunction objects are
        // allocated only using 'new':
        ~BuiltInFunction() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        BuiltInFunction(const BuiltInFunction &);
        BuiltInFunction &operator=(const BuiltInFunction &);
    };
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
} // extern "C"

#endif // BUILTINFUNCTION_HPP
