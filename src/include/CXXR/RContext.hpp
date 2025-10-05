/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2022  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file RContext.hpp
 *
 * @brief Class CXXR::Evaluator::RContext.
 *
 * @note The name RContext.hpp is used rather than
 * Context.hpp because otherwise the corresponding file Context.cpp
 * would differ only in case from the filename of context.cpp derived
 * from CR's context.cpp.
 */

#ifndef RCONTEXT_HPP
#define RCONTEXT_HPP

#ifndef __cplusplus
#error RContext.hpp can only be included in C++ files
#endif

#include <string_view>
#include <CXXR/RTypes.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/NodeStack.hpp>
#include <R_ext/Visibility.h>
#include <R_ext/Error.h> // for NORET
#include <TryCatch.h>

namespace CXXR
{

    using R_bcFrame_type = struct R_bcFrame;

#define RCNTXT CXXR::Evaluator::RContext

    /** @brief Housekeeping information on R call stack.
     *
     * Evaluator::RContext objects must be declared on the processor
     * stack (i.e. as C++ automatic variables).
     *
     * This class (together with its derived classes) performs two
     * functions:
     * <ol>
     *
     * <li>The primary function is to maintain an 'Ariadne's thread',
     * recording information about the stack of R function calls
     * currently active.  This is to support error reporting,
     * traceback, and the 'sys' R functions.  As in CR, calls to
     * 'special' BuiltInFunction objects (SPECIALSXP) are not
     * recorded; however, unlike CR, calls to other BuiltInFunction
     * objects (BUILTINSXP) are always recorded.</li>
     *
     * <li>Derived classes may also carry out a secondary function,
     * namely to save and restore information about the evaluation
     * state.  Certain aspects of the state of the current Evaluator
     * are saved by the derived class's constructor; the state is then
     * restored by the class's destructor.  Since Context objects are
     * allocated on the processor stack, this means that the
     * evaluation state will automatically be restored both under the
     * normal flow of control and when the stack is unwound during the
     * propagation of a C++ exception.  (Beware however that some of
     * the save/restore functionality that CR's RCNTXT offers is
     * handled separately in rho via classes such as Browser; this
     * trend of moving save/restore functionality out of classes
     * derived from Evaluator::RContext is likely to continue.
     * Moreover, in cases where save/restore functions continue to be
     * effected by classes inheriting from Evaluator::RContext, this is
     * in some cases achieved by incorporating within a Context object
     * an object with more specific save/restore role, such as a
     * ProtectStack::Scope object.</li>
     *
     * </ol>
     */
    class Evaluator::RContext
    {
    public:
        /** @brief The Various Context Types.
         *
         * In general the type is a bitwise OR of the values below.
         * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
         * Only functions should have the third bit turned on;
         * this allows us to move up the context stack easily
         * with either RETURN's or GENERIC's or RESTART's.
         * If you add a new context type for functions make sure
         *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
         *
         * TOP   0 0 0 0 0 0 0 0 = 0
         * NEX   1 0 0 0 0 0 0 0 = 1
         * BRE   0 1 0 0 0 0 0 0 = 2
         * LOO   1 1 0 0 0 0 0 0 = 3
         * FUN   0 0 1 0 0 0 0 0 = 4
         * CCO   0 0 0 1 0 0 0 0 = 8
         * BRO   0 0 0 0 1 0 0 0 = 16
         * RET   0 0 1 1 0 0 0 0 = 12
         * GEN   0 0 1 0 1 0 0 0 = 20
         * RES   0 0 0 0 0 0 1 0 = 32
         * BUI   0 0 0 0 0 0 0 1 = 64
         */
        enum Type
        {
            TOPLEVEL = 0,
            NEXT = 1,
            BREAK = 2,
            LOOP = 3, /* break OR next target */
            FUNCTION = 4,
            CCODE = 8,
            RETURN = 12,
            BROWSER = 16,
            GENERIC = 20,
            RESTART = 32,
            BUILTIN = 64, /* used in profiling */
            UNWIND = 128
        };
#define CTXT_TOPLEVEL RCNTXT::Type::TOPLEVEL
#define CTXT_NEXT RCNTXT::Type::NEXT
#define CTXT_BREAK RCNTXT::Type::BREAK
#define CTXT_LOOP RCNTXT::Type::LOOP
#define CTXT_FUNCTION RCNTXT::Type::FUNCTION
#define CTXT_CCODE RCNTXT::Type::CCODE
#define CTXT_RETURN RCNTXT::Type::RETURN
#define CTXT_BROWSER RCNTXT::Type::BROWSER
#define CTXT_GENERIC RCNTXT::Type::GENERIC
#define CTXT_RESTART RCNTXT::Type::RESTART
#define CTXT_BUILTIN RCNTXT::Type::BUILTIN
#define CTXT_UNWIND RCNTXT::Type::UNWIND

        RContext();
        RContext(Type flags, SEXP syscall, SEXP env, SEXP sysp, SEXP promargs, SEXP callfun);
        ~RContext();
        RContext *nextcontext; /* The next context up the chain */
        Type callflag;         /* The context "type" */
        size_t m_cstacktop;    /* Top of the pointer protection stack */
        int m_evaldepth;       /* evaluation depth at inception */
        GCRoot<> promargs;     /* Promises supplied to closure */
        GCRoot<> callfun;      /* The closure called */
        GCRoot<> sysparent;    /* environment the closure was called from */
        GCRoot<> call;         /* The call that effected this context*/
        GCRoot<> cloenv;       /* The environment */
        GCRoot<> conexit;      /* Interpreted "on.exit" code */
        size_t m_vmax;         /* top of R_alloc stack */
        bool m_intsusp;        /* interrupts are suspended */
        bool m_bcintactive;    /* Evaluator::bcActive() value */
        GCRoot<> bcbody;       /* R_BCbody value */
        void *bcpc;            /* R_BCpc value */
        ptrdiff_t relpc;       /* pc offset when begincontext is called */
        GCRoot<> handlerstack; /* condition handler stack */
        GCRoot<> restartstack; /* stack of available restarts */
        R_bcstack_t *nodestack;
        R_bcstack_t *bcprottop;
        R_bcFrame_type *bcframe;
        GCRoot<> srcref;         /* The source line in effect */
        bool browserfinish;       /* should browser finish this context without
                                    stopping */
        R_bcstack_t returnValue; /* only set during on.exit calls */
        int jumpmask;            /* associated LONGJMP argument */
        bool m_restart;          /* TRUE if restart bit is set */

        static void maybeRunOnExit(RContext *cptr, bool intermediate_jump = false);
        void runOnExit(bool intermediate_jump = false);

        constexpr std::string_view namedType() const
        {
            switch (callflag)
            {
            case CTXT_TOPLEVEL:
                return "TOPLEVEL";
            case CTXT_NEXT:
                return "NEXT";
            case CTXT_BREAK:
                return "BREAK";
            case CTXT_LOOP:
                return "LOOP";
            case CTXT_FUNCTION:
                return "FUNCTION";
            case CTXT_CCODE:
                return "CCODE";
            case CTXT_RETURN:
                return "RETURN";
            case CTXT_BROWSER:
                return "BROWSER";
            case CTXT_GENERIC:
                return "GENERIC";
            case CTXT_RESTART:
                return "RESTART";
            case CTXT_BUILTIN:
                return "BUILTIN";
            case CTXT_UNWIND:
                return "UNWIND";
            default:
                return "UNKNOWN";
            }
        }

        /** @brief The innermost Context.
         *
         * @return Pointer to the innermost Context belonging to the
         * current Evaluator.
         */
        static RContext *innermost();

        /** @brief Search outwards for a Function Context.
         *
         * This function works outwards from the Evaluator::RContext \a
         * start until it finds a Function Context (possibly \a start
         * itself), and returns a pointer to that Function Context.
         *
         * @param start The Evaluator::RContext from which the search
         * is to start.
         *
         * @return Pointer to the innermost Function Context found, or
         * a null pointer if no such context was found.
         */
        static RContext *innermostFrom(Evaluator::RContext *start = Evaluator::RContext::innermost());

        /** @brief Next Context out.
         *
         * @return pointer to the Context object most narrowly
         * enclosing this Context, or a null pointer if this is the
         * outermost Context of the current Evaluator.
         */
        RContext *nextOut() const
        {
            return nextcontext; /*m_next_out;*/
        }

        static RContext *s_exit_context; /* The active context for on.exit processing */
#define R_GlobalContext CXXR::Evaluator::RContext::innermost()
#define R_ExitContext CXXR::Evaluator::RContext::s_exit_context
    private:
        RContext(RContext &) = delete;
        RContext &operator=(const RContext &) = delete;
    };

    bool isTopLevelContext(RCNTXT *cptr);

    /* The toplevel context */
    RCNTXT *CXXR_R_ToplevelContext();

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void printContexts();
} // namespace CXXR

namespace R
{
    #define IS_RESTART_BIT_SET(cntxt) ((cntxt)->m_restart)
    #define SET_RESTART_BIT_ON(cntxt) ((cntxt)->m_restart = true)
    #define SET_RESTART_BIT_OFF(cntxt) ((cntxt)->m_restart = false)

    SEXP R_findBCInterpreterSrcref(RCNTXT *);
    void begincontext(RCNTXT *, RCNTXT::Type, SEXP, SEXP, SEXP, SEXP, SEXP);
    SEXP dynamicfindVar(SEXP, RCNTXT *);
    void endcontext(RCNTXT *);
    int framedepth(RCNTXT *);
    void R_InsertRestartHandlers(RCNTXT *, const char *);
    NORET void R_JumpToContext(RCNTXT *, int, SEXP);
    SEXP R_syscall(int, RCNTXT *);
    int R_sysparent(int, RCNTXT *);
    SEXP R_sysframe(int, RCNTXT *);
    SEXP R_sysfunction(int, RCNTXT *);
    RCNTXT *R_findExecContext(RCNTXT *, SEXP);
    RCNTXT *R_findParentContext(RCNTXT *, int);

    void R_run_onexits(RCNTXT *);
    NORET void R_jumpctxt(RCNTXT *, int, SEXP);
    SEXP getLexicalCall(SEXP);
} // namespace R

#endif // RCONTEXT_HPP
