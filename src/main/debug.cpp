/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2025   The R Core Team.
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

/** @file debug.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/Evaluator.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>

using namespace R;
using namespace CXXR;

attribute_hidden SEXP do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

    checkArity(op,args);
#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	GCStackRoot<> s;					\
	s = installTrChar(STRING_ELT(CAR(args), 0));		\
	SETCAR(args, findFun(s, rho));				\
    }
    find_char_fun

    if (!Rf_isFunction(CAR(args)))
	error("%s", _("argument must be a function"));
    switch(PRIMVAL(op)) {
    case 0: // debug()
	SET_RDEBUG(CAR(args), 1);
	break;
    case 1: // undebug()
	if (RDEBUG(CAR(args)) != 1)
	    warning("%s", _("argument is not being debugged"));
	SET_RDEBUG(CAR(args), 0);
	break;
    case 2: // isdebugged()
	ans = ScalarLogical(RDEBUG(CAR(args)));
	break;
    case 3: // debugonce()
	SET_RSTEP(CAR(args), 1);
	break;
    }
    return ans;
}

/* primitives .primTrace() and .primUntrace() */
attribute_hidden SEXP do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    find_char_fun

    if (!Rf_isFunction(CAR(args)))
	    errorcall(call, _("'%s' argument must be a function"), "what");

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace & debug state */

static bool tracing_state = TRUE, debugging_state = TRUE;
#define GET_TRACE_STATE tracing_state
#define GET_DEBUG_STATE debugging_state
#define SET_TRACE_STATE(value) tracing_state = value
#define SET_DEBUG_STATE(value) debugging_state = value

attribute_hidden SEXP do_traceOnOff(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP onOff = CAR(args);
    bool trace = (PRIMVAL(op) == 0),
	prev = trace ? GET_TRACE_STATE : GET_DEBUG_STATE;

    if (length(onOff) > 0) {
	bool _new = asLogical(onOff);
	if (_new == TRUE || _new == FALSE)
	    if(trace) SET_TRACE_STATE(_new);
	    else      SET_DEBUG_STATE(_new);
	else
	    error(_("'%s' argument must be TRUE or FALSE"),
		  trace ? "tracingState" : "debuggingState");
    }
    return ScalarLogical(prev);
}

// GUIs, packages, etc can query:
attribute_hidden /* would need to be in an installed header if not hidden */
bool R::R_current_debug_state(void) { return GET_DEBUG_STATE; }
attribute_hidden /* would need to be in an installed header if not hidden */
bool R::R_current_trace_state(void) { return GET_TRACE_STATE; }


/* memory tracing */
/* report when a traced object is duplicated */

#ifdef R_MEMORY_PROFILING

attribute_hidden SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP object;
    char buffer[21];

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
    if (Rf_isFunction(object))
	errorcall(call, "%s", _("argument must not be a function"));

    if (object == R_NilValue)
	errorcall(call, "%s", _("cannot trace NULL"));

    if (TYPEOF(object) == ENVSXP || TYPEOF(object) == PROMSXP)
	errorcall(call, "%s",
		  _("'tracemem' is not useful for promise and environment objects"));
    if (TYPEOF(object) == EXTPTRSXP || TYPEOF(object) == WEAKREFSXP)
	errorcall(call, "%s",
		  _("'tracemem' is not useful for weak reference or external pointer objects"));

    SET_RTRACE(object, 1);
    snprintf(buffer, 21, "<%p>", (void *) object);
    return mkString(buffer);
}

attribute_hidden SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP object;

    checkArity(op, args);
    check1arg(args, call, "x");

    object=CAR(args);
    if (Rf_isFunction(object))
	errorcall(call, "%s", _("argument must not be a function"));

    if (RTRACE(object))
	SET_RTRACE(object, 0);
    return R_NilValue;
}

#else

NORET attribute_hidden SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    errorcall(call, "%s", _("R was not compiled with support for memory profiling"));
}

NORET attribute_hidden SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    errorcall(call, "%s", _("R was not compiled with support for memory profiling"));
}

#endif /* R_MEMORY_PROFILING */

#ifndef R_MEMORY_PROFILING
void R::memtrace_report(void* old, void *_new) {
    return;
}
#else
static void memtrace_stack_dump(void)
{
    for (RCNTXT *cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    Rprintf("%s ",
		    TYPEOF(fun) == SYMSXP ? EncodeChar(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    Rprintf("\n");
}

void R::memtrace_report(void * old, void * _new)
{
    static_cast<RObject *>(_new)->maybeTraceMemory(static_cast<RObject *>(old));
}

void RObject::traceMemory(const RObject *src1, const RObject *src2,
                          const RObject *src3)
{
    setMemoryTracing(true);
    if (!R_current_trace_state())
        return;
    Rprintf("tracemem[");
    bool needs_comma = false;
    if (src1->memoryTraced())
    {
        Rprintf("%p", (void *)src1);
        needs_comma = true;
    }
    if (src2 && src2->memoryTraced())
    {
        if (needs_comma)
            Rprintf(", ");
        Rprintf("%p", (void *)src2);
        needs_comma = true;
    }
    if (src3 && src3->memoryTraced())
    {
        if (needs_comma)
            Rprintf(", ");
        Rprintf("%p", (void *)src3);
    }
    Rprintf(" -> %p]: ", (void *)this);
    memtrace_stack_dump();
}

#endif /* R_MEMORY_PROFILING */

attribute_hidden SEXP do_retracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans;
    GCStackRoot<> argList;
    char buffer[21];
    static SEXP do_retracemem_formals = NULL;
    bool visible; 

    if (do_retracemem_formals == NULL)
	do_retracemem_formals = allocFormalsList2(install("x"),
						  R_PreviousSymbol);

    argList =  matchArgs_NR(do_retracemem_formals, args, call);
    if (CAR(argList) == R_MissingArg) SETCAR(argList, R_NilValue);
    if (CADR(argList) == R_MissingArg) SETCAR(CDR(argList), R_NilValue);

    object = CAR(argList);
    if (Rf_isFunction(object))
	errorcall(call, "%s", _("argument must not be a function"));

    previous = CADR(argList);
    if (!isNull(previous) && (!isString(previous) || LENGTH(previous) != 1))
	    errorcall(call, _("invalid '%s' argument"), "previous");

    if (RTRACE(object)) {
	snprintf(buffer, 21, "<%p>", (void *) object);
	visible = TRUE;
	ans = mkString(buffer);
    } else {
	visible = FALSE;
	ans = R_NilValue;
    }

    if (previous != R_NilValue){
	SET_RTRACE(object, 1);
	if (R_current_trace_state()) {
	    /* FIXME: previous will have <0x....> whereas other values are
	       without the < > */
	    Rprintf("tracemem[%s -> %p]: ",
		    translateChar(STRING_ELT(previous, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    Evaluator::enableResultPrinting(visible);
    return ans;
#else
    Evaluator::enableResultPrinting(false); /* for consistency with other case */
    return R_NilValue;
#endif
}
