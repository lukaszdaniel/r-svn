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

#include <vector>
#include <CXXR/FunctionBase.hpp>

namespace CXXR
{
    /** @brief The type of the do_xxxx functions.
     *
     * These are the built-in R functions.
     */
    using CCODE = RObject *(*)(RObject *, RObject *, RObject *, RObject *);

    /* Information for Deparsing Expressions */
    enum PPkind
    {
        PP_INVALID = 0,
        PP_ASSIGN = 1,
        PP_ASSIGN2 = 2,
        PP_BINARY = 3,
        PP_BINARY2 = 4,
        PP_BREAK = 5,
        PP_CURLY = 6,
        PP_FOR = 7,
        PP_FUNCALL = 8,
        PP_FUNCTION = 9,
        PP_IF = 10,
        PP_NEXT = 11,
        PP_PAREN = 12,
        PP_RETURN = 13,
        PP_SUBASS = 14,
        PP_SUBSET = 15,
        PP_WHILE = 16,
        PP_UNARY = 17,
        PP_DOLLAR = 18,
        PP_FOREIGN = 19,
        PP_REPEAT = 20
    };

    enum PPprec
    {
        PREC_FN = 0,
        PREC_EQ = 1,
        PREC_LEFT = 2,
        PREC_RIGHT = 3,
        PREC_TILDE = 4,
        PREC_OR = 5,
        PREC_AND = 6,
        PREC_NOT = 7,
        PREC_COMPARE = 8,
        PREC_SUM = 9,
        PREC_PROD = 10,
        PREC_PERCENT = 11,
        PREC_COLON = 12,
        PREC_SIGN = 13,
        PREC_POWER = 14,
        PREC_SUBSET = 15,
        PREC_DOLLAR = 16,
        PREC_NS = 17
    };

    struct PPinfo
    {
        PPkind kind;             /* deparse kind */
        PPprec precedence;       /* operator precedence */
        unsigned int rightassoc; /* right associative? */
    };

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

        // The type of internal dispatch (if any) that the function does.
        enum class DispatchType
        {
            NONE,
            INTERNAL,
            GROUP_MATH,
            GROUP_OPS,
            GROUP_COMPLEX,
            GROUP_SUMMARY
        };

        /* The type definitions for the table of built-in functions. */
        /* This table can be found in ../main/names.c */
        struct FUNTAB
        {
            const char *name; /* print name */
            CCODE cfun;       /* c-code address */
            int code;         /* offset within c-code */
            int eval;         /* evaluate args? */
            int arity;        /* function arity */
            PPinfo gram;      /* pretty-print info */
            const char *first_arg_name;
            enum DispatchType dispatch_type;
            FUNTAB(const char *name, CCODE cfun, int variant,
                   int flags, int arity, PPinfo ppinfo,
                   const char *first_arg_name = nullptr,
                   enum DispatchType dispatch = DispatchType::NONE)
                : name(name), cfun(cfun), code(variant), eval(flags), arity(arity), gram(ppinfo),
                  first_arg_name(first_arg_name), dispatch_type(dispatch) {}
        };

        static std::vector<FUNTAB> s_R_FunTab;
#define R_FunTab CXXR::BuiltInFunction::s_R_FunTab

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
    /** @brief Get offset of a CXXR::BuiltInFunction.
     *
     * @param x Pointer to a CXXR::BuiltInFunction.
     *
     * @return The offset of this function within the function table.
     */
    int (PRIMOFFSET)(SEXP x);

    void (SET_PRIMOFFSET)(SEXP x, int v);
} // namespace R

extern "C"
{
} // extern "C"

#endif // BUILTINFUNCTION_HPP
