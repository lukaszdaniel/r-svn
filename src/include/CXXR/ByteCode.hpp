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

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif

#include <memory>
#include <CXXR/GCRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/NodeStack.hpp>

namespace CXXR
{
    /* saved bcEval() state for implementing recursion using goto */
    using R_bcFrame_type = struct R_bcFrame;
    struct bcEval_locals;

    /** @brief ByteCode interpreter.
     */
    class ByteCode : public RObject
    {
    public:
        static ByteCode *create(SEXP code = R_NilValue, SEXP constants = R_NilValue)
        {
            return new ByteCode(code, constants);
        }

        // Interim accessor functions.  Try to get rid of these:

        /** @brief Not for general use.
         */
        RObject *code() const
        {
            return u.bytecode.m_code;
        }

        /** @brief Not for general use.
         */
        RObject *constants() const
        {
            return u.bytecode.m_constants;
        }

        /** @brief Not for general use.
         */
        RObject *expression() const
        {
            return u.bytecode.m_expression;
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "bytecode";
        }

        // Normally this implements evaluate() by evaluating bcode in
        // the environment env.  However, if called with a null
        // pointer for bcode, it initialises the opcode despatch
        // table(s).
        static SEXP interpret(SEXP body, SEXP rho, bool useCache);
#define bcEval CXXR::ByteCode::interpret

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

        /** @brief Initialize the class.
         *
         * This function should be called before any other use is made
         * of the ByteCode class.
         */
        static void initInterpreter();
        static SEXP bcEval_loop(struct bcEval_locals *);

        static bool ByteCodeDisabled()
        {
            return s_bytecode_disabled;
        }

        static bool ByteCodeEnabled()
        {
            return !s_bytecode_disabled;
        }

        static void disableByteCode(bool val)
        {
            s_bytecode_disabled = val;
        }

        static ptrdiff_t codeDistane(SEXP body, void *bcpc);

        // Virtual functions of RObject:
        const char *typeName() const override;

        static size_t nodeStackSize()
        {
            return s_nodestack->size();
        }

        // Initialize static data (called by InitMemory()):
        static void initialize();


        static std::unique_ptr<NodeStack> s_nodestack;

        static void *s_BCpc; /* current byte code instruction */
#define R_BCpc CXXR::ByteCode::s_BCpc
        static GCRoot<> s_bc_body; /* current byte code object */
#define R_BCbody CXXR::ByteCode::s_bc_body
        static R_bcFrame_type *s_BCFrame; /* bcEval() frame */
#define R_BCFrame CXXR::ByteCode::s_BCFrame
        static bool s_BCIntActive; /* bcEval called more recently than eval */
#define R_BCIntActive CXXR::ByteCode::s_BCIntActive

    private:
        static bool s_bytecode_disabled;

        /**
         * @param code Non-null pointer to the 'bytecode' (actually a
         *          set of integers).
         *
         * @param constants Non-null pointer to the associated
         *          constants (FIXME: improve this documentation.)
         */
        explicit ByteCode(SEXP code, SEXP constants)
            : RObject(BCODESXP)
        {
            u.bytecode.m_code = code;
            u.bytecode.m_constants = constants;
            u.bytecode.m_expression = R_NilValue;
        }

        // Declared private to ensure that ByteCode objects are
        // allocated only using 'new':
        ~ByteCode() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        ByteCode(const ByteCode &);
        ByteCode &operator=(const ByteCode &);
    };
} // namespace CXXR

namespace R
{
    bool (isByteCode)(SEXP x);
    void R_initialize_bcode(void);
    SEXP R_bcEncode(SEXP);
    SEXP R_bcDecode(SEXP);
    void R_registerBC(SEXP, SEXP);
    bool R_checkConstants(bool);
    bool R_BCVersionOK(SEXP);
} // namespace R

extern "C"
{
    SEXP R_PromiseExpr(SEXP);
    SEXP R_ClosureExpr(SEXP);
    SEXP R_BytecodeExpr(SEXP e);
} // extern "C"

#endif /* BYTECODE_HPP */
