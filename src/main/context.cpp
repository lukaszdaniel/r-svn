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
 *
 *
 *  Contexts:
 *
 *  A linked-list of execution contexts is kept so that control-flow
 *  constructs like "next", "break" and "return" will work.  It is also
 *  used for error returns to top-level.
 *
 *	context[k] -> context[k-1] -> ... -> context[0]
 *	^				     ^
 *	R_GlobalContext			     R_ToplevelContext
 *
 *  Contexts are allocated on the stack as the evaluator invokes itself
 *  recursively.  The memory is reclaimed naturally on return through
 *  the recursions (the R_GlobalContext pointer needs adjustment).
 *
 *  A context contains the following information (and more):
 *
 *	nextcontext	the next level context
 *	cjmpbuf		longjump information for non-local return
 *	cstacktop	the current level of the pointer protection stack
 *	callflag	the context "type"
 *	call		the call (name of function, or expression to
 *			get the function) that effected this
 *			context if a closure, otherwise often NULL.
 *	callfun		the function, if this was a closure.
 *	cloenv		for closures, the environment of the closure.
 *	sysparent	the environment the closure was called from
 *	conexit		code for on.exit calls, to be executed in cloenv
 *			at exit from the closure (normal or abnormal).
 *	vmax		the current setting of the R_alloc stack
 *	srcref		the srcref at the time of the call
 *
 *  Context types can be one of:
 *
 *	CTXT_TOPLEVEL	The toplevel context
 *	CTXT_BREAK	target for "break"
 *	CTXT_NEXT	target for "next"
 *	CTXT_LOOP	target for either "break" or "next"
 *	CTXT_RETURN	target for "return" (i.e. a closure)
 *	CTXT_BROWSER	target for "return" to exit from browser
 *	CTXT_CCODE	other functions that need clean up if an error occurs
 *	CTXT_RESTART	a function call to restart was made inside the
 *			closure.
 *
 *	Code (such as the sys.xxx) that looks for CTXT_RETURN must also
 *	look for a CTXT_RESTART and CTXT_GENERIC.
 *	The mechanism used by restart is to change
 *	the context type; error/errorcall then looks for a RESTART and does
 *	a long jump there if it finds one.
 *
 *  A context is created with a call to
 *
 *	void begincontext(RCNTXT *cptr, int flags,
 *			  SEXP syscall, SEXP env, SEXP
 *			  sysp, SEXP promargs, SEXP callfun)
 *
 *  which sets up the context pointed to by cptr in the appropriate way.
 *  When the context goes "out-of-scope" a call to
 *
 *	void endcontext(RCNTXT *cptr)
 *
 *  restores the previous context (i.e. it adjusts the R_GlobalContext
 *  pointer).
 *
 *  The non-local jump to a given context takes place in a call to
 *
 *	void findcontext(int mask, SEXP env, SEXP val)
 *
 *  This causes "val" to be stuffed into a globally accessible place and
 *  then a search to take place back through the context list for an
 *  appropriate context.  The kind of context sort is determined by the
 *  value of "mask".  The value of mask should be the logical OR of all
 *  the context types desired.
 *
 *  The value of "mask" is returned as the value of the setjump call at
 *  the level longjumped to.  This is used to distinguish between break
 *  and next actions.
 *
 *  Contexts can be used as a wrapper around functions that create windows
 *  or open files. These can then be shut/closed gracefully if an error
 *  occurs.
 */

/** @file context.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <Localization.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Browser.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/StackChecker.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/CommandTerminated.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/RawVector.hpp>
#include <Defn.h>
#include <Internal.h>

using namespace R;
using namespace CXXR;

namespace CXXR
{
    RCNTXT *RCNTXT::s_exit_context = nullptr;

    bool isTopLevelContext(RCNTXT *cptr)
    {
        return (!cptr || (cptr->nextcontext == nullptr));
    }

    RCNTXT *CXXR_R_ToplevelContext()
    {
        RCNTXT *cptr = R_GlobalContext;
        while (cptr && !isTopLevelContext(cptr))
        {
            cptr = cptr->nextcontext;
        }
        return cptr;
    }

    RCNTXT *RCNTXT::innermost()
    {
        return Evaluator::current()->innermostContext();
    }

    RCNTXT *RCNTXT::innermostFrom(Evaluator::RContext *start)
    {
        while (start && start->callflag != CTXT_FUNCTION)
            start = start->nextOut();
        return start;
    }

    void printContexts()
    {
        Evaluator *evl = Evaluator::current();
        unsigned int count = 0;
        while (evl)
        {
            std::cerr << "Evaluator " << count++ << ":\n  ";
            RCNTXT *cptr = evl->innermostContext();
            std::cerr << "Contexts: ";
            while (cptr)
            {
                std::cerr << cptr->namedType();
                std::cerr << " [res: " << std::boolalpha << cptr->m_restart;
                std::cerr << ", fin: " << cptr->browserfinish << "]";
                std::cerr << " -> ";
                cptr = cptr->nextcontext;
            }
            std::cerr << "o\n";
            evl = evl->next();
        }
    }
} // namespace CXXR

/* R_run_onexits - runs the R's onexit code for all contexts from
   R_GlobalContext down to but not including the argument context.
   This routine does not stop at a CTXT_TOPLEVEL--the code that
   determines the argument is responsible for making sure
   CTXT_TOPLEVEL's are not crossed unless appropriate. */

attribute_hidden void R::R_run_onexits(RCNTXT *cptr)
{
    for (RCNTXT *c = R_GlobalContext; c != cptr; c = c->nextcontext) {
	// a user embedding R incorrectly triggered this (PR#15420)
	if (c == NULL)
	    error("%s", _("bad target context--should NEVER happen if R was called correctly"));
	RCNTXT::maybeRunOnExit(c, true);
    }
}

namespace CXXR
{
    void RCNTXT::runOnExit(bool intermediate_jump)
    {
        GCStackRoot<> s(this->conexit);
        bool savevis = Evaluator::resultPrinted();
        RCNTXT *savecontext = R_ExitContext;
        R_ExitContext = this;
        this->conexit = R_NilValue; /* prevent recursion */
        if (intermediate_jump) {
        /* we are in intermediate jump, so returnValue is undefined */
            this->returnValue = SEXP_TO_STACKVAL(NULL);
        }
        SEXP cptr_retval =
            this->returnValue.tag == 0 ? this->returnValue.u.sxpval : NULL;
        if (cptr_retval) // why is this needed???
            INCREMENT_LINKS(cptr_retval);
        if (intermediate_jump) {
            /* Since these are run before any jumps rather than after
               jumping to the context where the exit handler was set
               we need to make sure there is enough room on the
               evaluation stack in case the jump is from handling a
               stack overflow. To be safe it is good to also call
               R_CheckStack. LT */
            StackChecker::extraDepth(true);
            R_CheckStack();
        }
        for (; s != R_NilValue; s = CDR(s.get())) {
            this->conexit = CDR(s.get());
            Evaluator::evaluate(CAR(s), cloenv);
        }
        if (cptr_retval) // why is this needed???
            DECREMENT_LINKS(cptr_retval);
        R_ExitContext = savecontext;
        Evaluator::enableResultPrinting(savevis);
    }

    void RCNTXT::maybeRunOnExit(RCNTXT *cptr, bool intermediate_jump)
    {
        R_HandlerStack = R_UnwindHandlerStack(cptr->handlerstack);
        R_RestartStack = cptr->restartstack;
        if (cptr && cptr->cloenv != R_NilValue && cptr->conexit != R_NilValue)
        {
            cptr->runOnExit(intermediate_jump);
        }
        if (R_ExitContext == cptr)
            R_ExitContext = NULL; /* Not necessary?  Better safe than sorry. */
    }
} // namespace CXXR

/* R_jumpctxt - jump to the named context */

NORET attribute_hidden void R::R_jumpctxt(RCNTXT *cptr, int mask, SEXP val)
{
    StackChecker::restoreCStackLimit();

    throw JMPException(cptr, mask, R_FixupExitingHandlerResult(val));
}

RCNTXT::RContext()
{
    nextcontext = nullptr;
    callflag = CTXT_TOPLEVEL;
    m_cstacktop = 0;
    m_evaldepth = 0;
    promargs = R_NilValue;
    callfun = R_NilValue;
    sysparent = R_BaseEnv;
    call = R_NilValue;
    cloenv = R_BaseEnv;
    conexit = R_NilValue;
    m_vmax = 0;
    m_intsusp = FALSE;
    m_bcintactive = Evaluator::bcActive();
    bcbody = R_BCbody;
    bcpc = R_BCpc;
    relpc = 0;
    handlerstack = R_HandlerStack;
    restartstack = R_RestartStack;
    nodestack = R_BCNodeStackTop;
    bcprottop = R_BCProtTop;
    bcframe = nullptr;
    srcref = R_Srcref;
    browserfinish = 0;
    returnValue = SEXP_TO_STACKVAL(nullptr);
    jumpmask = 0;
    m_restart = false;
}

/* begincontext - begin an execution context */
RCNTXT::RContext(Type flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun)
{
    begincontext(this, flags, syscall, env, sysp, promargs, callfun);
}

/* begincontext and endcontext are used in dataentry.c and modules */
void R::begincontext(RCNTXT *cptr, RCNTXT::Type flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun)
{
    cptr->m_cstacktop = R_PPStackTop;
    cptr->relpc = R_BCRelPC(R_BCbody, R_BCpc);
    cptr->bcpc = R_BCpc;
    cptr->bcbody = R_BCbody;
    cptr->bcframe = R_BCFrame;
    cptr->m_bcintactive = Evaluator::bcActive();
    cptr->m_evaldepth = StackChecker::depth();
    cptr->callflag = flags;
    cptr->call = syscall;
    cptr->cloenv = env;
    cptr->sysparent = sysp;
    cptr->conexit = R_NilValue;
    cptr->promargs = promargs;
    cptr->callfun = callfun;
    cptr->m_vmax = RAllocStack::size();
    cptr->m_intsusp = Evaluator::interruptsSuspended();
    cptr->handlerstack = R_HandlerStack;
    cptr->restartstack = R_RestartStack;
    cptr->nodestack = R_BCNodeStackTop;
    cptr->bcprottop = R_BCProtTop;
    cptr->srcref = R_Srcref;
    cptr->browserfinish = R_GlobalContext ? R_GlobalContext->browserfinish : 0;
    cptr->returnValue = SEXP_TO_STACKVAL(NULL);
    cptr->jumpmask = 0;
    cptr->m_restart = false;

    cptr->nextcontext = R_GlobalContext;
    Evaluator::current()->m_innermost_context = cptr;
}


/* endcontext - end an execution context */

void R::endcontext(RCNTXT *cptr)
{
}

RCNTXT::~RContext()
{
    try
    {
        RContext::maybeRunOnExit(this);
    }
    catch (...)
    {
        // Don't allow exceptions to escape.
    }

    ProtectStack::restoreSize(this->m_cstacktop);
    R_BCpc = this->bcpc;
    R_BCbody = this->bcbody;
    R_BCFrame = this->bcframe;
    Evaluator::enableBCActive(this->m_bcintactive);
    StackChecker::setDepth(this->m_evaldepth);
    RAllocStack::restoreSize(this->m_vmax);
    Evaluator::setInterruptsSuspended(this->m_intsusp);
    R_HandlerStack = this->handlerstack;
    R_RestartStack = this->restartstack;
    R_BCNodeStackTop = this->nodestack;
    R_BCProtReset(this->bcprottop);
    R_Srcref = this->srcref;

    Evaluator::current()->m_innermost_context = this->nextcontext;
}

/* findcontext - find the correct context */

NORET attribute_hidden void R::findcontext(int mask, SEXP env, SEXP val)
{
    if (mask & CTXT_LOOP) {		/* break/next */
	for (RCNTXT *cptr = R_GlobalContext;
	     cptr && !isTopLevelContext(cptr);
	     cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_LOOP && cptr->cloenv == env )
		R_jumpctxt(cptr, mask, val);
	error("%s", _("no loop for break/next, jumping to top level"));
    }
    else {				/* return; or browser */
	for (RCNTXT *cptr = R_GlobalContext;
	     cptr && !isTopLevelContext(cptr);
	     cptr = cptr->nextcontext)
	    if ((cptr->callflag & mask) && cptr->cloenv == env)
		R_jumpctxt(cptr, mask, val);
	error("%s", _("no function to return from, jumping to top level"));
    }
}

NORET attribute_hidden void R::R_JumpToContext(RCNTXT *target, int mask, SEXP val)
{
    for (RCNTXT *cptr = R_GlobalContext;
	 cptr && !isTopLevelContext(cptr);
	 cptr = cptr->nextcontext) {
	if (cptr == target)
	    R_jumpctxt(cptr, mask, val);
	if (cptr == R_ExitContext)
	    R_ExitContext = NULL;
    }
    error("%s", _("target context is not on the stack"));
}


/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that cloenv. */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

attribute_hidden SEXP R::R_sysframe(int n, RCNTXT *cptr)
{
    if (n == 0)
	return R_GlobalEnv;

    if (n == NA_INTEGER) error("%s", _("NA argument is invalid"));

    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = -n;

    if (n < 0)
	error("%s", _("not that many frames on the stack"));

    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0) {  /* we need to detach the enclosing env */
		return cptr->cloenv;
	    }
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL)
	return R_GlobalEnv;
    else
	error("%s", _("not that many frames on the stack"));
    return R_NilValue;	   /* just for -Wall */
}


/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the cloenv pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->sysparent */
/* but then we wouldn't be compatible with S. */

attribute_hidden int R::R_sysparent(int n, RCNTXT *cptr)
{
    int j;
    SEXP s;
    if (n <= 0)
    {
        RCNTXT *topctxt = CXXR_R_ToplevelContext();
        errorcall(topctxt ? topctxt->call.get() : R_NilValue, "%s",
		  _("only positive values of 'n' are allowed"));
    }
    while (cptr->nextcontext != NULL && n > 1) {
	if (cptr->callflag & CTXT_FUNCTION )
	    n--;
	cptr = cptr->nextcontext;
    }
    /* make sure we're looking at a return context */
    while (cptr->nextcontext != NULL && !(cptr->callflag & CTXT_FUNCTION) )
	cptr = cptr->nextcontext;
    if (!cptr)
        return 0;
    // Foll. 3 lines probably soon redundant in CXXR:
    s = cptr->sysparent;
    if (s == R_GlobalEnv)
	return 0;
    j = 0;
    while (cptr != NULL ) {
	if (cptr->callflag & CTXT_FUNCTION) {
	    j++;
	    if ( cptr->cloenv == s )
		n=j;
	}
	cptr = cptr->nextcontext;
    }
    n = j - n + 1;
    if (n < 0)
	n = 0;
    return n;
}

attribute_hidden int R::framedepth(RCNTXT *cptr)
{
    int nframe = 0;
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION )
	    nframe++;
	cptr = cptr->nextcontext;
    }
    return nframe;
}

static SEXP getCallWithSrcref(RCNTXT *cptr)
{
    GCStackRoot<> result;

    result = shallow_duplicate(cptr->call);
    if (cptr->srcref && !isNull(cptr->srcref)) {
	SEXP sref;
	if (cptr->srcref == R_InBCInterpreter)
	    /* FIXME: this is expensive, it might be worth changing sys.call */
	    /* to return srcrefs only on request (add `with.source` option) */
	    sref = R_findBCInterpreterSrcref(cptr);
	else
	    sref = cptr->srcref;
	setAttrib(result, R_SrcrefSymbol, duplicate(sref));
    }

    return result;
}

attribute_hidden SEXP R::R_syscall(int n, RCNTXT *cptr)
{
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0)
	error("%s", _("not that many frames on the stack"));
    while (cptr && !isTopLevelContext(cptr)) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0)
		return getCallWithSrcref(cptr);
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr && isTopLevelContext(cptr))
	return getCallWithSrcref(cptr);
    error("%s", _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

attribute_hidden SEXP R::R_sysfunction(int n, RCNTXT *cptr)
{
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0)
	error("%s", _("not that many frames on the stack"));
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0)
		return duplicate(cptr->callfun);  /***** do we need to DUP? */
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL)
	return duplicate(cptr->callfun);  /***** do we need to DUP? */
    error("%s", _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

/* count how many contexts of the specified type are present on the stack */
/* browser contexts are a bit special because they are transient and for  */
/* any closure context with the debug bit set one will be created; so we  */
/* need to count those as well                                            */
attribute_hidden int R::countContexts(unsigned int /*Evaluator::RContext::Type*/ctxttype, bool browser) {
    int n = 0;
    RCNTXT *cptr = R_GlobalContext;
    while (cptr && !isTopLevelContext(cptr)) {
	if (cptr->callflag == ctxttype)
	    n++;
	else if (browser) {
	   if (cptr->callflag & CTXT_FUNCTION && ENV_RDEBUG(cptr->cloenv))
	      n++;
	}
	cptr = cptr->nextcontext;
    }
    return n;
}


/* functions to support looking up information about the browser */
/* contexts that are in the evaluation stack */

attribute_hidden SEXP do_sysbrowser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval = R_NilValue;
    RCNTXT *prevcptr = NULL;
    int n;

    checkArity(op, args);
    n = asInteger(CAR(args));
    if (n < 1) error("%s", _("number of contexts must be positive"));

    if (Browser::numberActive() == 0)
	Rf_error("%s", _("no browser context to query"));

    switch (PRIMVAL(op)) {
    case 1: /* text */
    case 2: /* condition */
    {
	if (n > int(Browser::numberActive())) {
	   n = Browser::numberActive();
	   // error("%s", _("not that many calls to browser are active"));
	}

        Browser *browser = Browser::fromOutermost(Browser::numberActive() - n);
        return PRIMVAL(op) == 1 ? browser->text() : browser->condition();
    }
	break;
    case 3: /* turn on debugging n levels up */
    {
	Browser *browser = Browser::fromOutermost(Browser::numberActive() - 1);
	Evaluator::RContext *cptr = browser->context(); // Evaluator::RContext::innermostFrom(browser->context());
	while ( (cptr && !isTopLevelContext(cptr)) && n > 0 ) {
	    if (cptr->callflag & CTXT_FUNCTION)
		  n--;
	    prevcptr = cptr;
	    cptr = cptr->nextcontext;
	}
	if (!cptr || !(cptr->callflag & CTXT_FUNCTION) )
	    error("%s", _("not that many functions on the call stack"));
	if ( prevcptr && prevcptr->srcref == R_InBCInterpreter ) {
	    if ( TYPEOF(cptr->callfun) == CLOSXP &&
		    TYPEOF(BODY(cptr->callfun.get())) == BCODESXP )
		warning("%s", _("debug flag in compiled function has no effect"));
	    else
		warning("%s", _("debug will apply when function leaves compiled code"));
	}
	SET_ENV_RDEBUG(cptr->cloenv, 1);
    }
	break;
    }
    return rval;
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in, so the */
/* indexing is adjusted to handle this. */

/* Return first `CTXT_FUNCTION` context whose execution
   env matches `rho` */
namespace {
RCNTXT *getLexicalContext(SEXP rho)
{
    RCNTXT *cptr = R_GlobalContext;

    while (cptr && !isTopLevelContext(cptr)) {
	if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }

    return cptr;
}
} // anonymous namespace

/* Return call of the first `CTXT_FUNCTION` context whose
   execution env matches `rho` */
attribute_hidden SEXP R::getLexicalCall(SEXP rho)
{
    RCNTXT *cptr = getLexicalContext(rho);
    if (cptr)
	return cptr->call;
    else
	return R_NilValue;
}

attribute_hidden SEXP do_sys(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, n  = -1, nframe;
    SEXP rval;

    checkArity(op, args);

    /* first find the context that sys.xxx needs to be evaluated in */
    SEXP t = R_GlobalContext->sysparent;
    RCNTXT *cptr = getLexicalContext(t);

    if (length(args) == 1) n = asInteger(CAR(args));

    switch (PRIMVAL(op)) {
    case 1: /* parent */
	if (n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "n");
	i = nframe = framedepth(cptr);
	/* This is a pretty awful kludge, but the alternative would be
	   a major redesign of everything... -pd */
	while (n-- > 0)
	    i = R_sysparent(nframe - i + 1, cptr);
	return ScalarInteger(i);
    case 2: /* call */
	if (n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_syscall(n, cptr);
    case 3: /* frame */
	if (n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_sysframe(n, cptr);
    case 4: /* sys.nframe */
	return ScalarInteger(framedepth(cptr));
    case 5: /* sys.calls */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t=rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_syscall(i, cptr));
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t = rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_sysframe(i, cptr));
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
	{
	    SEXP conexit = cptr ? cptr->conexit.get() : R_NilValue;
	    if (conexit == R_NilValue)
		return R_NilValue;
	    else if (CDR(conexit) == R_NilValue)
		return CAR(conexit);
	    else
		return LCONS(R_BraceSymbol, conexit);
	}
    case 8: /* sys.parents */
	nframe = framedepth(cptr);
	rval = IntVector::create(nframe);
	for(i = 0; i < nframe; i++)
	    INTEGER(rval)[i] = R_sysparent(nframe - i, cptr);
	return rval;
    case 9: /* sys.function */
	if (n == NA_INTEGER)
	    error(_("invalid '%s' value"), "which");
	return (R_sysfunction(n, cptr));
    default:
	error(_("internal error in '%s'"), "do_sys");
	return R_NilValue;/* just for -Wall */
    }
}

attribute_hidden SEXP do_parentframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    int n = asInteger(CAR(args));
    if (n == NA_INTEGER || n < 1 )
	error(_("invalid '%s' value"), "n");

    RCNTXT *cptr = R_findParentContext(R_GlobalContext, n);

    if (cptr)
	return cptr->sysparent;
    else
	return R_GlobalEnv;
}

/* R_findExecContext - Find a context frame older than `cptr` that has
   `envir` as execution environment (the `cloenv` field). */
attribute_hidden
RCNTXT *R::R_findExecContext(RCNTXT *cptr, SEXP envir)
{
    while (cptr->nextcontext != NULL) {
	if ((cptr->callflag & CTXT_FUNCTION) != 0 && cptr->cloenv == envir)
	    return cptr;
	cptr = cptr->nextcontext;
    }
    return NULL;
}

/* R_findParentContext - Find a context frame older than `cptr` whose
   execution environment (`cloenv` field) is the same as cptr's
   calling environment (`sysparent` field). In other words, find the
   frame where `cptr->syscall` was (seemingly) called. This algorithm
   powers `parent.frame()`. */
attribute_hidden
RCNTXT *R::R_findParentContext(RCNTXT *cptr, int n)
{
    while ((cptr = R_findExecContext(cptr, cptr->sysparent)) != NULL) {
	if (n == 1)
	    return cptr;
	n--;
    }
    return NULL;
}

/* R_ToplevelExec - call fun(data) within a top level context to
   insure that this function cannot be left by a LONGJMP.  R errors in
   the call to fun will result in a jump to top level. The return
   value is TRUE if fun returns normally, FALSE if it results in a
   jump to top level. */

Rboolean R_ToplevelExec(void (*fun)(void *), void *data)
{
    SEXP topExp, oldHStack, oldRStack;
    bool oldvis;
    Rboolean result;


    PROTECT(topExp = R_CurrentExpr);
    PROTECT(oldHStack = R_HandlerStack);
    PROTECT(oldRStack = R_RestartStack);
    oldvis = Evaluator::resultPrinted();
    R_HandlerStack = R_NilValue;
    R_RestartStack = R_NilValue;
    try
    {
        Evaluator evalr;
        RCNTXT toplevel(CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
        fun(data);
        result = TRUE;
    }
    catch (CommandTerminated)
    {
        result = FALSE;
    }

    R_CurrentExpr = topExp;
    R_HandlerStack = oldHStack;
    R_RestartStack = oldRStack;
    Evaluator::enableResultPrinting(oldvis);
    UNPROTECT(3);

    return result;
}

/* Return the current environment. */
/* The _current environment_ is taken to be the top closure call
   environment on the context stack, or .GlobalEnv if there is none.
   An alternative would be the environment in which a .Call or similar
   expression is evaluated. This is currently not recorded; doing so
   would incur some overhead that does not seem warranted.
 */
SEXP R_GetCurrentEnv(void) {
    RCNTXT *cptr = R_GlobalContext;
    while (cptr->nextcontext != NULL) {
	if ((cptr->callflag & CTXT_FUNCTION) != 0)
	    return cptr->cloenv;
	else cptr = cptr->nextcontext;
    }
    return R_GlobalEnv;
}


/*
  This is a simple interface for evaluating R expressions
  from C with a guarantee that one will return to the
  point in the code from which the call was made (if it does
  return at all).
  This uses R_TopleveExec to do this.  It is important
  in applications that embed R or wish to make general
  callbacks to R with error handling.

  It is currently hidden with a data structure definition
  and C routine visible only here. The R_tryEval() is the
  only visible aspect. This can be lifted into the header
  files if necessary. (DTL)

  R_tryEval is in Rinternals.h (so public), but not in the API.
 */
typedef struct {
    SEXP expression;
    SEXP val;
    SEXP env;
} ProtectedEvalData;

static void protectedEval(void *d)
{
    ProtectedEvalData *data = (ProtectedEvalData *)d;
    SEXP env = R_GlobalEnv;
    if (data->env) {
	env = data->env;
    }
    data->val = eval(data->expression, env);
    R_PreserveObject(data->val);
}

SEXP R_tryEval(SEXP e, SEXP env, int *ErrorOccurred)
{
    ProtectedEvalData data;

    data.expression = e;
    data.val = NULL;
    data.env = env;

    bool ok = R_ToplevelExec(protectedEval, &data);
    if (ErrorOccurred) {
	*ErrorOccurred = (ok == FALSE);
    }
    if (ok == FALSE)
	data.val = NULL;
    else
	R_ReleaseObject(data.val);

    return (data.val);
}

/* Temporary hack to suppress error message printing around a
   R_tryEval call for use in methods_list_dispatch.c; should be
   replaced once we have a way of establishing error handlers from C
   code (probably would want a calling handler if we want to allow
   user-defined calling handlers to enter a debugger, for
   example). LT */
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred)
{
    bool oldshow = R_ShowErrorMessages;
    R_ShowErrorMessages = FALSE;
    SEXP val = R_tryEval(e, env, ErrorOccurred);
    R_ShowErrorMessages = oldshow;
    return val;
}

SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata)
{
    GCStackRoot<> result;

    try {
    result = fun(data);
    cleanfun(cleandata);
    } catch (CommandTerminated &ignored_error)
    {
    }

    return result;
}


/* Unwind-protect mechanism to support C++ stack unwinding. */

typedef struct {
    int jumpmask;
    RCNTXT *jumptarget;
} unwind_cont_t;

SEXP R_MakeUnwindCont(void)
{
    return CONS(R_NilValue, RawVector::create(sizeof(unwind_cont_t)));
}

#define RAWDATA(x) ((void *) RAW0(x))

NORET void R_ContinueUnwind(SEXP cont)
{
    SEXP retval = CAR(cont);
    unwind_cont_t *u = (unwind_cont_t *) RAWDATA(CDR(cont));
    R_jumpctxt(u->jumptarget, u->jumpmask, retval);
}

SEXP R_UnwindProtect(SEXP (*fun)(void *data), void *data,
		     void (*cleanfun)(void *data, Rboolean jump),
		     void *cleandata, SEXP cont)
{
    SEXP result = R_NilValue;
    Rboolean jump = FALSE;

    /* Allow simple usage with a NULL continuation token. This _could_
       result in a failure in allocation or exceeding the PROTECT
       stack limit before calling fun(), so fun() and cleanfun should
       be written accordingly. */
    if (cont == NULL) {
	PROTECT(cont = R_MakeUnwindCont());
	result = R_UnwindProtect(fun, data, cleanfun, cleandata, cont);
	UNPROTECT(1);
	return result;
    }

    {
    RCNTXT thiscontext(CTXT_UNWIND, R_NilValue, R_GlobalEnv,
		 R_BaseEnv, R_NilValue, R_NilValue);
    try
    {
        result = fun(data);
        SETCAR(cont, result);
        jump = FALSE;
    }
    catch (JMPException &e)
    {
        // We want to intercept the jump to unwind
        // so that R_ContinueUnwind() can be called
        // to continue the unwind process.
        // This is a bit of a hack to comply with CR's approach,
        // but it allows us to handle the unwind in a controlled manner.
        // Normally, we would have used below code:
        // if (e.context() != &thiscontext)
        //     throw;
        if (e.context() != &thiscontext)
            jump = TRUE;
        SETCAR(cont, e.value());
        unwind_cont_t *u = (unwind_cont_t *) RAWDATA(CDR(cont));
        u->jumpmask = e.mask();
        u->jumptarget = e.context();
    }

    endcontext(&thiscontext);
    }
    cleanfun(cleandata, jump);

    if (jump)
	R_ContinueUnwind(cont);	

    return result;
}
