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

#include <CXXR/RTypes.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/NodeStack.hpp>
#include <R_ext/Visibility.h>
#include <R_ext/Error.h> // for NORET
#include <TryCatch.h>

namespace R
{
    typedef struct R_bcFrame R_bcFrame_type;
} // namespace R

namespace CXXR {

#define RCNTXT CXXR::RContext

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
    class RContext {
    public:
        RContext();
        RContext(int flags, SEXP syscall, SEXP env, SEXP sysp, SEXP promargs, SEXP callfun);
        ~RContext();
        RContext *nextcontext;	/* The next context up the chain */
        int callflag;       /* The context "type" */
        JMP_BUF cjmpbuf;    /* C stack and register information */
        size_t cstacktop;   /* Top of the pointer protection stack */
        int evaldepth;	    /* evaluation depth at inception */
        SEXP promargs;      /* Promises supplied to closure */
        SEXP callfun;       /* The closure called */
        SEXP sysparent;     /* environment the closure was called from */
        SEXP call;          /* The call that effected this context*/
        SEXP cloenv;        /* The environment */
        SEXP conexit;       /* Interpreted "on.exit" code */
        void (*cend)(void *); /* C "on.exit" thunk */
        void *cenddata;	    /* data for C "on.exit" thunk */
        void *vmax;         /* top of R_alloc stack */
        bool intsusp;       /* interrupts are suspended */
        bool gcenabled;	    /* R_GCEnabled value */
        bool bcintactive;   /* Evaluator::bcActive() value */
        SEXP bcbody;        /* R_BCbody value */
        void *bcpc;         /* R_BCpc value */
        ptrdiff_t relpc;    /* pc offset when begincontext is called */
        SEXP handlerstack;  /* condition handler stack */
        SEXP restartstack;  /* stack of available restarts */
        R_bcstack_t *nodestack;
        R_bcstack_t *bcprottop;
        R::R_bcFrame_type *bcframe;
        SEXP srcref;	    /* The source line in effect */
        int browserfinish;  /* should browser finish this context without
                               stopping */
        R_bcstack_t returnValue;   /* only set during on.exit calls */
        int jumpmask;       /* associated LONGJMP argument */

    private:
        RContext(RContext &) = delete;
        RContext &operator=(const RContext &) = delete;
    };

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
    enum {
        CTXT_TOPLEVEL = 0,
        CTXT_NEXT = 1,
        CTXT_BREAK = 2,
        CTXT_LOOP = 3,	/* break OR next target */
        CTXT_FUNCTION = 4,
        CTXT_CCODE = 8,
        CTXT_RETURN = 12,
        CTXT_BROWSER = 16,
        CTXT_GENERIC = 20,
        CTXT_RESTART = 32,
        CTXT_BUILTIN = 64, /* used in profiling */
        CTXT_UNWIND = 128
    };
} // namespace CXXR

namespace R
{
#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)

    void FinalizeSrcRefStateOnError(void *dummy);
    extern SEXP R_findBCInterpreterSrcref(RCNTXT *);
    void begincontext(RCNTXT *, int, SEXP, SEXP, SEXP, SEXP, SEXP);
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

#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif

    extern0 RCNTXT *getLexicalContext(SEXP);
    extern0 SEXP getLexicalCall(SEXP);

    extern0 RCNTXT R_Toplevel;	      /* Storage for the toplevel context */
    extern0 RCNTXT *R_ToplevelContext;  /* The toplevel context */
    LibExtern RCNTXT *R_GlobalContext;    /* The global context */
    extern0 RCNTXT *R_SessionContext;   /* The session toplevel context */
    extern0 RCNTXT *R_ExitContext INI_as(nullptr);     /* The active context for on.exit processing */

#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as
} // namespace R

#endif // RCONTEXT_HPP
