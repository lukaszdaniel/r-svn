/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025	The R Core Team.
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
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

/** @file eval.cpp
 *
 * General evaluation of expressions, including implementation of R flow
 * control constructs, and R profiling.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define USE_BASE_R_SUPPORT // for R_gc_running(), WrongArgCount()

#include <string>
#include <cmath>
#include <cerrno>
#include <array>
#include <Localization.h>
#include <Rdynpriv.h>
#include <CXXR/GCRoot.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/StackChecker.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/String.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <Parse.h>
#include <R_ext/Print.h>
#include "arithmetic.h"

using namespace R;
using namespace CXXR;

/* BC_PROFILING needs to be enabled at build time. It is not enabled
   by default as enabling it disables the more efficient threaded code
   implementation of the byte code interpreter. */
#ifdef BC_PROFILING
static bool s_bc_profiling = false;
#endif

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <csignal>
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>		/* for open */
# endif
# ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>		/* for write */
# endif
#endif /* not Win32 */

#if !defined(Win32) && defined(HAVE_PTHREAD)
// <csignal> is needed for pthread_kill on most platforms (and by POSIX
//  but apparently not FreeBSD): it is included above.
# include <pthread.h>
# ifdef HAVE_SCHED_H
#   include <sched.h>
# endif
static pthread_t R_profiled_thread;
#endif

#ifdef Win32
static FILE *R_ProfileOutfile = NULL;
#else
static int R_ProfileOutfile = -1;
#endif

static bool R_Mem_Profiling = false;
static bool R_GC_Profiling = false;                     /* indicates GC profiling */
static int R_Line_Profiling = 0;                   /* indicates line profiling, and also counts the filenames seen (+1) */
static char **R_Srcfiles;			   /* an array of pointers into the filename buffer */
static size_t R_Srcfile_bufcount;                  /* how big is the array above? */
static SEXP R_Srcfiles_buffer = NULL;              /* a big RAWSXP to use as a buffer for filenames and pointers to them */
static int R_Profiling_Error;		           /* record errors here */
static bool R_Filter_Callframes = false;	      	   /* whether to record only the trailing branch of call trees */

typedef enum { RPE_CPU, RPE_ELAPSED } rpe_type;    /* profiling event, CPU time or elapsed time */
static rpe_type R_Profiling_Event;

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;
#else
# ifdef HAVE_PTHREAD
typedef struct {
    pthread_t thread;
    pthread_mutex_t terminate_mu;
    pthread_cond_t terminate_cv;
    int should_terminate;
    int interval_us;
} R_profile_thread_info_t;
static R_profile_thread_info_t R_Profile_Thread_Info;
# endif
#endif

/* Careful here!  These functions are called asynchronously, maybe in the
   middle of GC, so don't do any allocations. They get called in a signal
   handler on Unix, so they are only allowed to call library functions
   that are async-signal-safe. They get called while the main R thread
   is suspended on Windows, and hence they cannot call into any C runtime
   function which may possibly include synchronization.

   Note that snprintf() is not safe on Unix nor on Windows. On Windows 10
   it has been seen to deadlock when the main thread has been suspended
   in a locale-specific operation. */

/* This does a linear search through the previously recorded filenames.  If
   this one is new, we try to add it.  FIXME:  if there are eventually
   too many files for an efficient linear search, do hashing. */

static int getFilenum(const char* filename) {
    int fnum;

    for (fnum = 0; fnum < R_Line_Profiling-1
		   && !streql(filename, R_Srcfiles[fnum]); fnum++);

    if (fnum == R_Line_Profiling-1) {
	size_t len = strlen(filename);
	if ((size_t) fnum >= R_Srcfile_bufcount) { /* too many files */
	    R_Profiling_Error = 1;
	    return 0;
	}
	if (R_Srcfiles[fnum] - (char*)RAW(R_Srcfiles_buffer) + len + 1 >
	    (size_t) length(R_Srcfiles_buffer)) {

	    /* out of space in the buffer */
	    R_Profiling_Error = 2;
	    return 0;
	}
	strcpy(R_Srcfiles[fnum], filename);
	R_Srcfiles[fnum+1] = R_Srcfiles[fnum] + len + 1;
	*(R_Srcfiles[fnum+1]) = '\0';
	R_Line_Profiling++;
    }

    return fnum + 1;
}

#define PROFBUFSIZ 10500

/* It would also be better to flush the buffer when it gets full,
   even if the line isn't complete. But this isn't possible if we rely
   on writing all line profiling files first. In addition, while on Unix
   we could use write() (not fprintf) to flush, it is not guaranteed we
   could do this on Windows with the main thread suspended. 

   With this size hitting the limit is fairly unlikely, but if we do then
   the output file will miss some entries. Maybe writing an overflow marker
   of some sort would be better.  LT, TK */

/* The pb_* functions write to profiling buffer, advancing the "ptr" and
   maintaining "left". If the write wouldn't fit leaving one more byte
   available for the terminator, "left" is set to zero. They do not
   terminate the string. */

typedef struct {
    char *ptr;
    size_t left;
} profbuf;

/* If a string fits with terminator to the buffer, add it, excluding
   the terminator. If it doesn't fit, set left to 0. */
static void pb_str(profbuf *pb, const char *str)
{
    size_t len = strlen(str);

    if (len < pb->left) {
	for (size_t i = 0; i < len; i++)
	    pb->ptr[i] = str[i];
	pb->ptr += len;
	pb->left -= len;
    } else
	pb->left = 0;
}

static void pb_uint(profbuf *pb, uint64_t num)
{
    char digits[20]; /* 64-bit unsigned integers */
    int i, j;

    for (i = 0;;) {
	digits[i++] = num % 10 + '0';
	num /= 10;
	if (num == 0)
	    break;
    }
    if ((size_t) i < pb->left) {
	j = 0;
	for (i--; i >= 0;)
	    pb->ptr[j++] = digits[i--];
	pb->ptr += j;
	pb->left -= j;
    } else
	pb->left = 0; 
}

static void pb_int(profbuf *pb, int64_t num)
{
    char digits[19]; /* 64-bit signed integers */
    int i, j, negative;

    if (num < 0) {
	negative = 1;
	num *= -1;
    } else
	negative = 0;
    for (i = 0;;) {
	digits[i++] = num % 10 + '0';
	num /= 10;
	if (num == 0)
	    break;
    }
    if ((size_t) (negative + i) < pb->left) {
        if (negative) {
	    pb->ptr[0] = '-';
	    pb->ptr++;
	    pb->left--;
	}
	j = 0;
	for (i--; i >= 0;)
	    pb->ptr[j++] = digits[i--];
	pb->ptr += j;
	pb->left -= j;
    } else
	pb->left = 0;
}

/* IEEE doubles */
#define PB_MAX_DBL_DIGITS 309

/* Careful: this is very simplistic printing of the integer parts of doubles
   (like %0.f) used only (in a special case) for stack trace in profiling data.
   Not suitable for re-use. */
static void pb_dbl(profbuf *pb, double num)
{
    char digits[PB_MAX_DBL_DIGITS]; 
    int i, j, negative;

    if (!R_FINITE(num)) {
	if (ISNA(num))
	    pb_str(pb, "NA");
	else if (ISNAN(num))
	    pb_str(pb,  "NaN");
	else if (num > 0)
	    pb_str(pb, "Inf");
	else
	    pb_str(pb, "-Inf");
	return;
    }
    if (num < 0) {
	negative = 1;
	num *= -1.0;
    } else
	negative = 0;
    for (i = 0;;) {
	digits[i++] = (char) ((int) fmod(num, 10.0) + '0');
	num /= 10.0;
	if (num < 1)
	    break;
	if (i >= PB_MAX_DBL_DIGITS)
	    /* This cannot happen with IEEE double */
	    return;
    }
    if ((size_t) (negative + i) < pb->left) {
	if (negative) {
	    pb->ptr[0] = '-';
	    pb->ptr++;
	    pb->left--;
	}
	j = 0;
	for (i--; i >= 0;)
	    pb->ptr[j++] = digits[i--];
	pb->ptr += j;
	pb->left -= j;
    } else
	pb->left = 0;
}

static void lineprof(profbuf* pb, SEXP srcref)
{
    if (srcref && !isNull(srcref)) {
	int fnum, line = asInteger(srcref);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	const char *filename;

	if (!srcfile || TYPEOF(srcfile) != ENVSXP) return;
	srcfile = R_findVar(install("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP || !length(srcfile)) return;
	filename = CHAR(STRING_ELT(srcfile, 0));

	if ((fnum = getFilenum(filename))) {
	    pb_int(pb, fnum); /* %d */
	    pb_str(pb, "#");
	    pb_int(pb, line); /* %d */
	    pb_str(pb, " " );
	}
    }
}


#if defined(__APPLE__)
#include <mach/mach_init.h>
#include <mach/mach_port.h>
static mach_port_t R_profiled_thread_id;
#undef FALSE
#undef TRUE
#endif

static RCNTXT *findProfContext(RCNTXT *cptr)
{
    if (cptr && !R_Filter_Callframes)
	return cptr->nextcontext;

    if (isTopLevelContext(cptr))
	return NULL;

    /* Find parent context, same algorithm as in `parent.frame()`. */
    RCNTXT *parent = R_findParentContext(cptr, 1);

    /* If we're in a frame called by `eval()`, find the evaluation
       environment higher up the stack, if any. */
    if (parent && parent->callfun == INTERNAL(R_EvalSymbol))
	parent = R_findExecContext(parent->nextcontext, cptr->sysparent);

    if (parent)
	return parent;

    /* Base case, this interrupts the iteration over context frames */
    if (isTopLevelContext(cptr->nextcontext))
	return NULL;

    /* There is no parent frame and we haven't reached the top level
       context. Find the very first context on the stack which should
       always be included in the profiles. */
    while (cptr && !isTopLevelContext(cptr->nextcontext))
	cptr = cptr->nextcontext;
    return cptr;
}

/* Write string to the profile file.
   On Unix, pf_* functions are called from a signal handler, hence avoid
   calling fprintf. */
static ssize_t pf_str(const char *s)
{
#ifdef Win32
    return fprintf(R_ProfileOutfile, "%s", s);
#else
    size_t wbyte = 0;
    size_t nbyte = strlen(s);
    for (;;) {
	ssize_t w = write(R_ProfileOutfile, s + wbyte, nbyte - wbyte);
	if (w == -1) {
	    if (errno == EINTR)
		continue;
	    else
		return -1;
	}
	wbyte += w;
	if (wbyte == nbyte || w == 0)
	    return wbyte;
    }
#endif
}

static void pf_int(int num)
{
#ifdef Win32
    fprintf(R_ProfileOutfile, "%d", num);
#else
    char buf[32];
    profbuf nb;
    nb.ptr = buf;
    nb.left = sizeof(buf);
    pb_int(&nb, num);
    nb.ptr[0] = '\0';
    pf_str(buf);
#endif
}

static void doprof(int sig)  /* sig is ignored in Windows */
{
    char buf[PROFBUFSIZ];
    size_t bigv, smallv, nodes;
    int prevnum = R_Line_Profiling;
    int old_errno = errno;

    profbuf pb;
    pb.ptr = buf;
    pb.left = PROFBUFSIZ;

#ifdef Win32
    SuspendThread(MainThread);
#elif defined(__APPLE__)
    if (R_Profiling_Event == RPE_CPU) {
	/* Using Mach thread API to detect whether we are on the main thread,
	   because pthread_self() sometimes crashes R due to a page fault when
	   the signal handler runs just after the new thread is created, but
	   before pthread initialization has been finished. */
	mach_port_t id = mach_thread_self();
	mach_port_deallocate(mach_task_self(), id);
	if (id != R_profiled_thread_id) {
	    pthread_kill(R_profiled_thread, sig);
	    errno = old_errno;
	    return;
	}
    }
#elif defined(HAVE_PTHREAD)
    if (R_Profiling_Event == RPE_CPU) {
	if (!pthread_equal(pthread_self(), R_profiled_thread)) {
	    pthread_kill(R_profiled_thread, sig);
	    errno = old_errno;
	    return;
	}
    }
#endif /* Win32 */

    if (R_Mem_Profiling) {
	get_current_mem(&smallv, &bigv, &nodes);
	pb_str(&pb, ":");
	pb_uint(&pb, (uint64_t) smallv);
	pb_str(&pb, ":");
	pb_uint(&pb, (uint64_t) bigv);
	pb_str(&pb, ":");
	pb_uint(&pb, (uint64_t) nodes);
	pb_str(&pb, ":");
	pb_uint(&pb, (uint64_t) get_duplicate_counter());
	pb_str(&pb, ":");
	reset_duplicate_counter();
    }

    if (R_GC_Profiling && R_gc_running())
	pb_str(&pb, "\"<GC>\" ");

    if (R_Line_Profiling)
	lineprof(&pb, R_getCurrentSrcref());

    for (RCNTXT *cptr = R_GlobalContext;
	 cptr != NULL;
	 cptr = findProfContext(cptr)) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {

	    SEXP fun = CAR(cptr->call);
	    pb_str(&pb, "\"");

	    if (TYPEOF(fun) == SYMSXP) {
		pb_str(&pb, CHAR(PRINTNAME(fun)));

	    } else if ((CAR(fun) == R_DoubleColonSymbol ||
			CAR(fun) == R_TripleColonSymbol ||
			CAR(fun) == R_DollarSymbol) &&
		       TYPEOF(CADR(fun)) == SYMSXP &&
		       TYPEOF(CADDR(fun)) == SYMSXP) {
		/* Function accessed via ::, :::, or $. Both args must be
		   symbols. It is possible to use strings with these
		   functions, as in "base"::"list", but that's a very rare
		   case so we won't bother handling it. */
		pb_str(&pb, CHAR(PRINTNAME(CADR(fun))));
		pb_str(&pb, CHAR(PRINTNAME(CAR(fun))));
		pb_str(&pb, CHAR(PRINTNAME(CADDR(fun))));
	    } else if (CAR(fun) == R_Bracket2Symbol &&
		       TYPEOF(CADR(fun)) == SYMSXP &&
		       ((TYPEOF(CADDR(fun)) == SYMSXP ||
			 TYPEOF(CADDR(fun)) == STRSXP ||
			 TYPEOF(CADDR(fun)) == INTSXP ||
			 TYPEOF(CADDR(fun)) == REALSXP) &&
			length(CADDR(fun)) > 0)) {
		/* Function accessed via [[. The first arg must be a symbol
		   and the second can be a symbol, string, integer, or
		   real. */
		SEXP arg1 = CADR(fun);
		SEXP arg2 = CADDR(fun);

		pb_str(&pb, CHAR(PRINTNAME(arg1)));
		pb_str(&pb, "[[");

		if (TYPEOF(arg2) == SYMSXP) {
		    pb_str(&pb, CHAR(PRINTNAME(arg2)));
		} else if (TYPEOF(arg2) == STRSXP) {
		    pb_str(&pb, "\"");
		    pb_str(&pb, CHAR(STRING_ELT(arg2, 0)));
		    pb_str(&pb, "\"");
		} else if (TYPEOF(arg2) == INTSXP) {
		    pb_int(&pb, INTEGER(arg2)[0]);
		} else if (TYPEOF(arg2) == REALSXP) {
		    pb_dbl(&pb, REAL(arg2)[0]); /* %0.f */
		}

		pb_str(&pb, "]]");

	    } else {
		pb_str(&pb, "<Anonymous>");
	    }

	    pb_str(&pb, "\" ");
	    if (R_Line_Profiling) {
		if (cptr->srcref == R_InBCInterpreter)
		    lineprof(&pb, R_findBCInterpreterSrcref(cptr));
		else
		    lineprof(&pb, cptr->srcref);
	    }
	}
    }

    if (pb.left)
	pb.ptr[0] = '\0';
    else {
	/* overflow */
	buf[0] = '\0';
	R_Profiling_Error = 3;
    }

#ifdef Win32
    /* resume before calling pf_* functions to avoid deadlock */
    ResumeThread(MainThread);
#endif

    for (int i = prevnum; i < R_Line_Profiling; i++) {
	pf_str("#File ");
	pf_int(i); /* %d */
	pf_str(": ");
	pf_str(R_Srcfiles[i-1]);
	pf_str("\n"); 
    }

    if (strlen(buf)) {
	pf_str(buf);
	pf_str("\n");
    }

#ifndef Win32
    signal(SIGPROF, doprof);
#endif /* not Win32 */
    errno = old_errno;
}

#ifdef Win32
/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait); /* milliseconds */

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof(0);
    }
}
#else /* not Win32 */
/* Profiling thread main function */
static void *ProfileThread(void *pinfo)
{
#ifdef HAVE_PTHREAD
    R_profile_thread_info_t *nfo = (R_profile_thread_info_t *) pinfo;

    pthread_mutex_lock(&nfo->terminate_mu);
    while(!nfo->should_terminate) {
	struct timespec until;
	double duntil_s = currentTime() + nfo->interval_us / 1e6;

	until.tv_sec = (time_t) duntil_s;
	until.tv_nsec = (long) (1e9 * (duntil_s - until.tv_sec));

	for (;;) {
	    int res = pthread_cond_timedwait(&nfo->terminate_cv,
					     &nfo->terminate_mu, &until);
	    if (nfo->should_terminate)
		break;
	    if (res == ETIMEDOUT) {
		pthread_kill(R_profiled_thread, SIGPROF);
		break;
	    }
	}
    }
    pthread_mutex_unlock(&nfo->terminate_mu);
#endif
    return NULL;
}
static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
    if (R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
#else /* not Win32 */
    if (R_Profiling_Event == RPE_CPU) {
	struct itimerval itv;

	itv.it_interval.tv_sec = 0;
	itv.it_interval.tv_usec = 0;
	itv.it_value.tv_sec = 0;
	itv.it_value.tv_usec = 0;
	setitimer(ITIMER_PROF, &itv, NULL);
    }
    if (R_Profiling_Event == RPE_ELAPSED) {
	R_profile_thread_info_t *nfo = &R_Profile_Thread_Info;
	pthread_mutex_lock(&nfo->terminate_mu);
	nfo->should_terminate = 1;
	pthread_cond_signal(&nfo->terminate_cv);
	pthread_mutex_unlock(&nfo->terminate_mu);
	pthread_join(nfo->thread, NULL);
	pthread_cond_destroy(&nfo->terminate_cv);
	pthread_mutex_destroy(&nfo->terminate_mu);
    }
    signal(SIGPROF, doprof_null);
    if (R_ProfileOutfile >= 0) close(R_ProfileOutfile);
    R_ProfileOutfile = -1;
#endif /* not Win32 */
    Evaluator::enableProfiling(false);
    if (R_Srcfiles_buffer) {
	R_ReleaseObject(R_Srcfiles_buffer);
	R_Srcfiles_buffer = NULL;
    }
    if (R_Profiling_Error) {
	if (R_Profiling_Error == 3)
	    /* It is hard to imagine this could happen in practice, but
	       if needed, it could be configurable like numfiles/bufsize. */
	    warning("%s", _("samples too large for I/O buffer skipped by Rprof"));
	else
	    warning(_("source files skipped by Rprof; please increase '%s'"),
		      R_Profiling_Error == 1 ? "numfiles" : "bufsize");
    }
}

static void R_InitProfiling(SEXP filename, bool append, double dinterval,
			    bool mem_profiling, bool gc_profiling,
			    bool line_profiling, bool filter_callframes,
			    int numfiles, int bufsize, rpe_type event)
{
#ifndef Win32
    CXXR::RAllocStack::Scope rscope;

    if (R_ProfileOutfile >= 0) R_EndProfiling();
    if (filename != NA_STRING && filename) {
	const char *fn = R_ExpandFileName(translateCharFP(filename));
	int flags = O_CREAT | O_WRONLY;
	if (append)
	    flags |= O_APPEND;
	else
	    flags |= O_TRUNC;
	int mode = S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH;
	R_ProfileOutfile = open(fn, flags, mode);
	if (R_ProfileOutfile < 0)
	    error(_("Rprof: cannot open profile file '%s'"), fn);
    }
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();

    if (R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == NULL)
	error(_("Rprof: cannot open profile file '%s'"),
	      translateChar(filename));
#endif
    int interval;

    interval = (int)(1e6 * dinterval + 0.5);
    if (mem_profiling)
	pf_str("memory profiling: ");
    if (gc_profiling)
	pf_str("GC profiling: ");
    if (line_profiling)
	pf_str("line profiling: ");
    pf_str("sample.interval=");
    pf_int(interval); /* %d */
    pf_str("\n");

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

    R_Profiling_Error = 0;
    R_Line_Profiling = line_profiling;
    R_GC_Profiling = gc_profiling;
    R_Filter_Callframes = filter_callframes;

    if (line_profiling) {
	/* Allocate a big RAW vector to use as a buffer.  The first len1 bytes are an array of pointers
	   to strings; the actual strings are stored in the second len2 bytes. */
	R_Srcfile_bufcount = numfiles;
	size_t len1 = R_Srcfile_bufcount*sizeof(char *), len2 = bufsize;
	R_PreserveObject( R_Srcfiles_buffer = RawVector::create(len1 + len2) );
 //	memset(RAW(R_Srcfiles_buffer), 0, len1+len2);
	R_Srcfiles = reinterpret_cast<char **>(RAW(R_Srcfiles_buffer));
	R_Srcfiles[0] = reinterpret_cast<char *>(RAW(R_Srcfiles_buffer)) + len1;
	*(R_Srcfiles[0]) = '\0';
    }

    R_Profiling_Event = event;

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if (!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       ((int) _beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide(_("unable to create profiling thread"));
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */

# ifdef HAVE_PTHREAD
    R_profiled_thread = pthread_self();
# else
    error(_("profiling requires 'pthread' support"));
# endif

# if defined(__APPLE__)
    if (R_Profiling_Event == RPE_CPU) {
	/* see comment in doprof for why R_profiled_thread is not enough */
	R_profiled_thread_id = mach_thread_self();
	mach_port_deallocate(mach_task_self(), R_profiled_thread_id);
    }
# endif

    signal(SIGPROF, doprof);

    if (R_Profiling_Event == RPE_ELAPSED) {
# ifdef HAVE_PTHREAD
	R_profile_thread_info_t *nfo = &R_Profile_Thread_Info;

	pthread_mutex_init(&nfo->terminate_mu, NULL);
	pthread_cond_init(&nfo->terminate_cv, NULL);
	nfo->should_terminate = 0;
	nfo->interval_us = interval;
	sigset_t all, old_set;
	sigfillset(&all);
	pthread_sigmask(SIG_BLOCK, &all, &old_set);
	if (pthread_create(&nfo->thread, NULL, ProfileThread,
	    nfo))
	    R_Suicide(_("unable to create profiling thread"));
	pthread_sigmask(SIG_SETMASK, &old_set, NULL);

#  ifdef HAVE_SCHED_H
	/* attempt to set FIFO scheduling with maximum priority
	   at least on Linux it requires special permissions */
	struct sched_param p;
	p.sched_priority = sched_get_priority_max(SCHED_FIFO);
	int res = -1;
	if (p.sched_priority >= 0)
	    res = pthread_setschedparam(nfo->thread, SCHED_FIFO, &p);
	if (res) {
	    /* attempt to set maximum priority at least with
	       the current scheduling policy */
	    int policy;
	    if (!pthread_getschedparam(nfo->thread, &policy, &p)) {
		p.sched_priority = sched_get_priority_max(policy);
		if (p.sched_priority >= 0)
		    pthread_setschedparam(nfo->thread, policy, &p);
	    }
	}
#  endif
# endif
    } else if (R_Profiling_Event == RPE_CPU) {
	/* The macOS implementation requires normalization here:

	   setitimer is obsolescent (POSIX >= 2008), replaced by
	   timer_create / timer_settime, but the supported clocks are
	   implementation-dependent.

	   Recent Linux has CLOCK_PROCESS_CPUTIME_ID
	   Solaris has CLOCK_PROF, in -lrt.
	   FreeBSD only supports CLOCK_{REALTIME,MONOTONIC}
	   Seems not to be supported at all on macOS.
	*/ 
	struct itimerval itv;
	itv.it_interval.tv_sec = interval / 1000000;
	itv.it_interval.tv_usec =
	    (suseconds_t)(interval - itv.it_interval.tv_sec * 1000000);
	itv.it_value.tv_sec = interval / 1000000;
	itv.it_value.tv_usec =
	    (suseconds_t)(interval - itv.it_value.tv_sec * 1000000);
	if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	    R_Suicide(_("setting profile timer failed"));
    }
#endif /* not Win32 */
    Evaluator::enableProfiling(true);
}

SEXP do_Rprof(SEXP args)
{
    SEXP filename;
    int append_mode, mem_profiling, gc_profiling, line_profiling,
	filter_callframes;
    double dinterval;
    int numfiles, bufsize;
    const char *event_arg;
    rpe_type event;

#ifdef BC_PROFILING
    if (s_bc_profiling) {
	warning("%s", _("cannot use R profiling while byte code profiling"));
	return R_NilValue;
    }
#endif
    if (!isString(filename = CAR(args)) || (LENGTH(filename)) != 1)
	error(_("invalid '%s' argument"), "filename");
					      args = CDR(args);
    append_mode = asLogical(CAR(args));       args = CDR(args);
    dinterval = asReal(CAR(args));            args = CDR(args);
    mem_profiling = asLogical(CAR(args));     args = CDR(args);
    gc_profiling = asLogical(CAR(args));      args = CDR(args);
    line_profiling = asLogical(CAR(args));    args = CDR(args);
    filter_callframes = asLogical(CAR(args));  args = CDR(args);
    numfiles = asInteger(CAR(args));	      args = CDR(args);
    if (numfiles < 0)
	error(_("invalid '%s' argument"), "numfiles");
    bufsize = asInteger(CAR(args));           args = CDR(args);
    if (bufsize < 0)
	error(_("invalid '%s' argument"), "bufsize");
    if (!isString(CAR(args)) || length(CAR(args)) != 1
        || STRING_ELT(CAR(args), 0) == NA_STRING)
	error(_("invalid '%s' argument"), "event");
    event_arg = translateChar(STRING_ELT(CAR(args), 0));
#ifdef Win32
    if (streql(event_arg, "elapsed") || streql(event_arg, "default"))
	event = RPE_ELAPSED;
    else if (streql(event_arg, "cpu"))
	error(_("event type '%s' not supported on this platform"), event_arg);
    else
	error(_("invalid '%s' argument"), "event");
#else
    if (streql(event_arg, "cpu") || streql(event_arg, "default"))
	event = RPE_CPU;
    else if (streql(event_arg, "elapsed"))
	event = RPE_ELAPSED;
    else
	error(_("invalid '%s' argument"), "event");
#endif

#if defined(linux) || defined(__linux__)
    if (dinterval < 0.01) {
	dinterval = 0.01;
	warning(_("interval too short for this platform, using '%f'"), dinterval);
    }
#else
    if (dinterval < 0.001) {
	dinterval = 0.001;
	warning(_("interval too short, using '%f'"), dinterval);
    }
#endif

    filename = STRING_ELT(filename, 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling,
			gc_profiling, line_profiling, filter_callframes,
			numfiles, bufsize, event);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
SEXP do_Rprof(SEXP args)
{
    error("%s", _("R profiling is not available on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

attribute_hidden void R::check_stack_balance(SEXP op, size_t save)
{
    if (save == R_PPStackTop) return;
    REprintf(_("Warning: stack imbalance in '%s', %td then %td\n"),
	     PRIMNAME(op), save, R_PPStackTop);
}


#define CXXR_POP(n) R_BCNodeStackTop -= (n)

/*
 * Protecting the Stack During Possibly Mutating Operations
 *
 * Values below R_BCProtTop should be protected during a mutating
 * operation by incrementing their link counts. Actual incrementing is
 * deferred until a call to INCLNK_stack_commit, which should happen
 * before a mutation that might affect stack values. (applydefine() in
 * the AST interpreter, STARTASSIGN/STARTASSIGN2 and INCLNK/INCLNKSTK
 * in the byte code interpreter. Deferring until needed avoids the
 * cost of incrementing and decrementing for code written in a
 * functional style.
 */

static R_INLINE void INCLNK_stack(R_bcstack_t *top)
{
    R_BCProtTop = top;
}

static R_INLINE void INCLNK_stack_commit(void)
{
    if (R_BCProtCommitted < R_BCProtTop) {
	R_bcstack_t *base = R_BCProtCommitted;
	R_bcstack_t *top = R_BCProtTop;
	for (R_bcstack_t *p = base; p < top; p++) {
	    if (p->tag == RAWMEM_TAG || p->tag == CACHESZ_TAG)
		p += p->u.ival;
	    else if (p->tag == 0)
		INCREMENT_LINKS(p->u.sxpval);
	}
	R_BCProtCommitted = R_BCProtTop;
    }
}

static R_INLINE void DECLNK_stack(R_bcstack_t *base)
{
    if (base < R_BCProtCommitted) {
	R_bcstack_t *top = R_BCProtCommitted;
	for (R_bcstack_t *p = base; p < top; p++) {
	    if (p->tag == RAWMEM_TAG || p->tag == CACHESZ_TAG)
		p += p->u.ival;
	    else if (p->tag == 0)
		DECREMENT_LINKS(p->u.sxpval);
	}
	R_BCProtCommitted = base;
    }
    R_BCProtTop = base;
}

attribute_hidden void R::R_BCProtReset(R_bcstack_t *ptop)
{
    DECLNK_stack(ptop);
}

#define INCREMENT_BCSTACK_LINKS() do {			\
	if (R_BCNodeStackTop > R_BCProtTop)		\
	    INCLNK_stack(R_BCNodeStackTop);		\
    } while (0)

#define DECREMENT_BCSTACK_LINKS(oldptop) do {		\
	if (R_BCProtTop > (oldptop))			\
	    DECLNK_stack(oldptop);			\
    } while (0)

#define INCREMENT_EVAL_DEPTH() do {		\
	StackChecker::s_depth++;				\
	if (StackChecker::s_depth > StackChecker::s_depth_threshold)	\
	    handle_eval_depth_overflow();	\
    } while (0)

static void handle_eval_depth_overflow(void)
{
    /* This bump of R_Expressions doesn't really work in many cases
       since jumps (e.g. from explicit return() calls or in UseMethod
       dispatch) reset this. Something more sophisticated might work,
       but also increase the risk of a C stack overflow. LT */
    StackChecker::extraDepth(true);

    /* the condition is pre-allocated and protected with R_PreserveObject */
    SEXP cond = R_getExpressionStackOverflowError();

    /* We need to pass a NULL call here to circumvent attempts to
       deparse the call in the error-handler */
    R_signalErrorCondition(cond, R_NilValue);
}

namespace
{
    SEXP ByteCode_evaluate(SEXP e, SEXP rho)
    {
        return ByteCode::interpret(e, rho, true);
    }

    SEXP Symbol_evaluate(SEXP e, SEXP rho)
    {
        SEXP tmp = R_NilValue;

        if (e == R_DotsSymbol)
            error("%s", _("'...' used in an incorrect context"));
        if (DDVAL(e))
            tmp = ddfindVar(e, rho);
        else
            tmp = R_findVar(e, rho);
        if (tmp == Symbol::unboundValue())
            errorcall_cpy(getLexicalCall(rho),
                _("object '%s' not found"),
                EncodeChar(PRINTNAME(e)));
        else if (tmp == R_MissingArg) {
            /* the error signaled here for a missing ..d matches the one
               signaled in getvar() for byte compiled code, but ...elt()
               signals a slightly different error (see PR18661) */
            R_MissingArgError(e, getLexicalCall(rho), "evalError");
        }
        else if (Promise::isA(tmp)) {
            return SEXP_downcast<Promise *>(tmp)->force();
        }
        else ENSURE_NAMED(tmp); /* needed for .Last.value - LT */

        return tmp;
    }

    SEXP Promise_evaluate(SEXP expr, SEXP rho)
    {
        GCStackRoot<> e(expr);
        if (expr && !PROMISE_IS_EVALUATED(expr))
        {
            if (PRSEEN(e) == UNDER_EVALUATION)
                errorcall(R_GlobalContext->call, "%s",
                    _("promise already under evaluation: recursive default argument reference or earlier problems?"));
            else if (PRSEEN(e) == INTERRUPTED)
            {
                /* set PRSEEN to 1 to avoid infinite recursion */
                SET_PRSEEN(e, UNDER_EVALUATION);
                warningcall(R_GlobalContext->call, "%s",
                    _("restarting interrupted promise evaluation"));
            }

            /* Mark the promise as under evaluation and push it on a stack
               that can be used to unmark pending promises if a jump out
               of the evaluation occurs. */
            SET_PRSEEN(e, UNDER_EVALUATION);
            try
            {
                SEXP val = Evaluator::evaluate(PRCODE(e.get()), PRENV(e.get()));
                SET_PRVALUE(e, val);
                ENSURE_NAMEDMAX(val);
            }
            catch (...)
            {
                /* The value INTERRUPTED installed in PRSEEN allows
                   to signal a warning when asked to evaluate a promise
                   whose evaluation has been interrupted by a jump. */
                SET_PRSEEN(e, INTERRUPTED);
                throw;
            }
            /* Unmark the promise and set its value field.
               Also set the environment to R_NilValue to allow GC to
               reclaim the promise environment; this is also useful for
               fancy games with delayedAssign() */
            SET_PRSEEN(e, DEFAULT);
            SET_PRENV(e, R_NilValue);
        }
        return PRVALUE(e.get());
        /* This does _not_ change the value of NAMED on the value tmp,
           in contrast to the handling of promises bound to symbols in
           the SYMSXP case above.  The reason is that one (typically
           the only) place promises appear in source code is as
           wrappers for the RHS value in replacement function calls for
           complex assignment expression created in applydefine().  If
           the RHS value is freshly created it will have NAMED = 0 and
           we want it to stay that way or a BUILTIN or SPECIAL
           replacement function might have to duplicate the value
           before inserting it to avoid creating cycles.  (Closure
           replacement functions will get the value via the SYMSXP case
           from evaluating their 'value' argument so the value will
           end up getting duplicated if NAMED > 1.) LT */
    }

    SEXP Expression_evaluate(SEXP expr, SEXP rho)
    {
        GCStackRoot<> tmp(R_NilValue);
        GCStackRoot<> op, e(expr);
        if (TYPEOF(CAR(e)) == SYMSXP) {
            /* This will throw an error if the function is not found */
            SEXP ecall = e;

            /* This picks the correct/better error expression for
               replacement calls running in the AST interpreter. */
            if (R_GlobalContext != NULL &&
                (R_GlobalContext->callflag == CTXT_CCODE))
                ecall = R_GlobalContext->call;
            op = findFun3(CAR(e), rho, ecall);
        }
        else
            op = Evaluator::evaluate(CAR(e), rho);

        if (RTRACE(op) && R_current_trace_state()) {
            Rprintf("trace: ");
            PrintValue(e);
        }
        if (TYPEOF(op) == SPECIALSXP) {
            size_t save = R_PPStackTop;
            int flag = PRIMPRINT(op.get());
            CXXR::RAllocStack::Scope rscope;
            Evaluator::enableResultPrinting(flag != 1);
            tmp = PRIMFUN(op.get()) (e, op, CDR(e.get()), rho);
#ifdef CHECK_VISIBILITY
            if (flag < 2 && Evaluator::resultPrinted() == flag) {
                const char *nm = PRIMNAME(op);
                if (!streql(nm, "for")
                    && !streql(nm, "repeat") && !streql(nm, "while")
                    && !streql(nm, "[[<-") && !streql(nm, "on.exit"))
                    printf("vis: special %s\n", nm);
            }
#endif
            if (flag < 2) Evaluator::enableResultPrinting(flag != 1);

            check_stack_balance(op, save);
        }
        else if (TYPEOF(op) == BUILTINSXP) {
            size_t save = R_PPStackTop;
            int flag = PRIMPRINT(op.get());
            CXXR::RAllocStack::Scope rscope;
            tmp = evalList(CDR(e.get()), rho, e, 0);
            if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
            /* We used to insert a context only if profiling,
               but helps for tracebacks on .C etc. */
            if (Evaluator::profiling() || (PPINFO(op.get()).kind == PP_FOREIGN)) {
                SEXP oldref = R_Srcref;
                RCNTXT cntxt(CTXT_BUILTIN, e, R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
                R_Srcref = NULL;
                tmp = PRIMFUN(op.get()) (e, op, tmp, rho);
                R_Srcref = oldref;
                endcontext(&cntxt);
            }
            else {
                tmp = PRIMFUN(op.get()) (e, op, tmp, rho);
            }
#ifdef CHECK_VISIBILITY
            if (flag < 2 && Evaluator::resultPrinted() == flag) {
                const char *nm = PRIMNAME(op);
                printf("vis: builtin %s\n", nm);
            }
#endif
            if (flag < 2) Evaluator::enableResultPrinting(flag != 1);

            check_stack_balance(op, save);
        }
        else if (TYPEOF(op) == CLOSXP) {
            GCStackRoot<> pargs;
            pargs = promiseArgs(CDR(e.get()), rho);
            tmp = applyClosure(e, op, pargs, rho, R_NilValue, true);
        }
        else
            error("%s", _("attempt to apply non-function"));

        return tmp;
    }

    SEXP DottedArgs_evaluate(SEXP e, SEXP rho)
    {
        error("%s", _("'...' used in an incorrect context"));
        return R_NilValue;
    }
} // anonymous namespace

namespace CXXR
{
    RObject *Promise::force()
    {
        return Promise_evaluate(this, R_NilValue);
    }
} // namespace CXXR

/* Return value of "e" evaluated in "rho". */

/* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
SEXP Evaluator::evaluate(SEXP e, SEXP rho)
{
    enableResultPrinting(true);

    /* this is needed even for self-evaluating objects or something like
       'while (TRUE) NULL' will not be interruptable */
    maybeCheckForUserInterrupts();

    /* handle self-evaluating objects with minimal overhead */
    if (isSelfEvaluated(e))
    {
    /* Make sure constants in expressions are NAMED before being
       used as values.  Setting NAMED to NAMEDMAX makes sure weird calls
       to replacement functions won't modify constants in
       expressions.  */
        ENSURE_NAMEDMAX(e);
        return e;
    }

    bool bcintactivesave = Evaluator::bcActive();
    Evaluator::enableBCActive(false);

    if (TYPEOF(e) != PROMSXP) // non-environment check only matters for non-Promise objects
    {
        if (!rho)
            error("'rho' cannot be C NULL: detected in C-level eval");
        if (!isEnvironment(rho))
            error("'rho' must be an environment not %s: detected in C-level eval",
                R_typeToChar(rho));
    }

    /* Save the current srcref context. */

    SEXP srcrefsave = R_Srcref;

    /* The use of IncrementStackDepthScope below is necessary because of the
       possibility of non-local returns from evaluation.  Without this
       an "expression too complex error" is quite likely. */

    IncrementStackDepthScope scope;

    SEXP tmp = R_NilValue;		/* -Wall */
#ifdef Win32
    /* This is an inlined version of Rwin_fpset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
# if (defined(__i386) || defined(__x86_64))
    __asm__("fninit");
# elif defined(__aarch64__)
    __asm__ volatile("msr fpcr, %0" : : "r"(0LL));
# else
    _fpreset();
# endif
#endif

    switch (TYPEOF(e)) {
    case BCODESXP:
    {
        tmp = ByteCode_evaluate(e, rho);
    }
    break;
    case SYMSXP:
    {
        tmp = Symbol_evaluate(e, rho);
    }
    break;
    case PROMSXP:
    {
        tmp = SEXP_downcast<Promise *>(e)->force();
    }
    break;
    case LANGSXP:
    {
        tmp = Expression_evaluate(e, rho);
    }
    break;
    case DOTSXP:
    {
        tmp = DottedArgs_evaluate(e, rho);
    }
    break;
    default:
        UNIMPLEMENTED_TYPE("eval", e);
    }

    R_Srcref = srcrefsave;
    Evaluator::enableBCActive(bcintactivesave);
    return tmp;
}

SEXP Rf_eval(SEXP e, SEXP rho)
{
	return Evaluator::evaluate(e, rho);
}

attribute_hidden
void R::SrcrefPrompt(const char *prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != R_NilValue) {
	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = R_findVar(install("filename"), srcfile);
	    if (isString(filename) && length(filename)) {
		Rprintf(_("%s at %s#%d: "), prefix,
			CHAR(STRING_ELT(filename, 0)),
			asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

/* JIT support */
typedef unsigned long R_exprhash_t;

namespace R {
R_exprhash_t hash(unsigned char *str, size_t n, R_exprhash_t hash)
{
    // djb2 from http://www.cse.yorku.ca/~oz/hash.html
    // (modified for n-byte lengths)

    for (size_t i = 0; i < n; i++)
        hash = ((hash << 5) + hash) + str[i]; /* hash * 33 + c */

    return hash;
}
} // namespace R

#define HASH(x, h) R::hash((unsigned char *) &x, sizeof(x), h)

static R_exprhash_t hashexpr1(SEXP e, R_exprhash_t h)
{
#define SKIP_NONSCALAR 	if (len != 1) break /* non-scalars hashed by address */
    int len = length(e);
    SEXPTYPE type = TYPEOF(e);
    h = HASH(type, h);
    h = HASH(len, h);

    switch(type) {
    case LANGSXP:
    case LISTSXP:
	/**** safer to only follow while CDR is LANGSXP/LISTSXP */
	for (; e != R_NilValue; e = CDR(e))
	    h = hashexpr1(CAR(e), h);
	return h;
    case LGLSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    int lval = LOGICAL(e)[i];
	    h = HASH(lval, h);
	}
	return h;
    case INTSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    int ival = INTEGER(e)[i];
	    h = HASH(ival, h);
	}
	return h;
    case REALSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    double dval = REAL(e)[i];
	    h = HASH(dval, h);
	}
	return h;
    case STRSXP:
	SKIP_NONSCALAR;
	for (int i = 0; i < len; i++) {
	    SEXP cval = STRING_ELT(e, i);
	    h = R::hash((unsigned char *) CHAR(cval), LENGTH(cval), h);
	}
	return h;
    default:
	break;
    }

    return HASH(e, h);
#undef SKIP_NONSCALAR
}

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind);
static R_exprhash_t hashsrcref(SEXP e, R_exprhash_t h)
{
    if (TYPEOF(e) == INTSXP && LENGTH(e) >= 6) {
	for (int i = 0; i < 6; i++) {
	    int ival = INTEGER(e)[i];
	    h = HASH(ival, h);
	}
	/* FIXME: update this when deep-comparison of srcref is available */
	SEXP srcfile = getAttrib(e, R_SrcfileSymbol);
	h = HASH(srcfile, h);
    }
    return h;
}
#undef HASH

static R_exprhash_t hashexpr(SEXP e)
{
    return hashexpr1(e, 5381);
}

static R_exprhash_t hashfun(SEXP f)
{
    R_exprhash_t h = hashexpr(BODY(f));
    if (getAttrib(BODY(f), R_SrcrefSymbol) == R_NilValue)
	h = hashsrcref(getAttrib(f, R_SrcrefSymbol), h);
    return h;
}

static void loadCompilerNamespace(void)
{
    GCStackRoot<> arg;
    arg = mkString("compiler");
    R_FindNamespace(arg);
}

static void checkCompilerOptions(int jitEnabled)
{
    bool old_visible = Evaluator::resultPrinted();
    SEXP packsym, funsym;
    GCStackRoot<> call, fcall, arg;

    packsym = install("compiler");
    funsym = install("checkCompilerOptions");

    arg = ScalarInteger(jitEnabled);
    fcall = lang3(R_TripleColonSymbol, packsym, funsym);
    call = lang2(fcall, arg);
    Evaluator::evaluate(call, Environment::base());
    Evaluator::enableResultPrinting(old_visible);
}

static SEXP R_IfSymbol = NULL;
static SEXP R_ForSymbol = NULL;
static SEXP R_WhileSymbol = NULL;
static SEXP R_RepeatSymbol = NULL;

#define JIT_CACHE_SIZE 1024
static SEXP JIT_cache = NULL;
static R_exprhash_t JIT_cache_hashes[JIT_CACHE_SIZE];

/**** allow MIN_JIT_SCORE, or both, to be changed by environment variables? */
static int MIN_JIT_SCORE = 50;
#define LOOP_JIT_SCORE MIN_JIT_SCORE

static struct { unsigned long count, envcount, bdcount; } jit_info = {0, 0, 0};

attribute_hidden void R::R_init_jit_enabled(void)
{
    /* Need to force the lazy loading promise to avoid recursive
       promise evaluation when JIT is enabled. Might be better to do
       this in baseloader.R. */
    Evaluator::evaluate(Symbol::obtain(".ArgsEnv"), Environment::base());

    int val = 3; /* turn JIT on by default */
    char *enable = getenv("R_ENABLE_JIT");
    if (enable != NULL)
	val = atoi(enable);
    if (val) {
	loadCompilerNamespace();
	checkCompilerOptions(val);
    }
    R_jit_enabled = val;

    if (R_compile_pkgs <= 0) {
	char *compile = getenv("_R_COMPILE_PKGS_");
	if (compile != NULL) {
	    int val = atoi(compile);
	    if (val > 0)
		R_compile_pkgs = TRUE;
	    else
		R_compile_pkgs = FALSE;
	}
    }

    if (ByteCode::ByteCodeEnabled()) {
	char *disable = getenv("R_DISABLE_BYTECODE");
	if (disable != NULL) {
	    int val = atoi(disable);
	    ByteCode::disableByteCode(val > 0);
	}
    }

    /* -1 ... duplicate constants on LDCONST and PUSHCONSTARG, no checking
        0 ... no checking (no duplication for >= 0) [DEFAULT]
	1 ... check at error, session exit and reclamation
	2 ... check also at full GC
	3 ... check also at partial GC
	4 ... check also at .Call
	5 ... (very) verbose report on modified constants
    */
    if (R_check_constants <= 1) {
	char *check = getenv("R_CHECK_CONSTANTS");
	if (check != NULL)
	    R_check_constants = atoi(check);
    }

    /* initialize JIT variables */
    R_IfSymbol = install("if");
    R_ForSymbol = install("for");
    R_WhileSymbol = install("while");
    R_RepeatSymbol = install("repeat");

    R_PreserveObject(JIT_cache = ListVector::create(JIT_CACHE_SIZE));
}

static int JIT_score(SEXP e)
{
    if (TYPEOF(e) == LANGSXP) {
	SEXP fun = CAR(e);
	if (fun == R_IfSymbol) {
	    int cons = JIT_score(CADR(e));
	    int alt =  JIT_score(CADDR(e));
	    return cons > alt ? cons : alt;
	}
	else if (fun == R_ForSymbol ||
		 fun == R_WhileSymbol ||
		 fun == R_RepeatSymbol)
	    return LOOP_JIT_SCORE;
	else {
	    int score = 1;
	    for (SEXP args = CDR(e); args != R_NilValue; args = CDR(args))
		score += JIT_score(CAR(args));
	    return score;
	}
    }
    else return 1;
}

#define STRATEGY_NO_SMALL 0
#define STRATEGY_TOP_SMALL_MAYBE 1
#define STRATEGY_ALL_SMALL_MAYBE 2
#define STRATEGY_NO_SCORE 3
#define STRATEGY_NO_CACHE 4
/* max strategy index is hardcoded in R_CheckJIT */

/*
  NO_CACHE
      functions are compiled 1st time seen
        code is never cached

  NO_SCORE
      functions are compiled 1st time seen
        code is cached
	in case of conflict function may be marked NOJIT

  ALL_SMALL_MAYBE
      functions with small score are compiled 2nd time seen
      function with high score are compiled
          1st time seen if top-level, 2nd time seen otherwise

  TOP_SMALL_MAYBE
      functions with small score compiled
          2nd time seen if top-level, never otherwise
      functions with high score compiled
          1st time seen if top-level, 2nd time seen otherwise
*/

static int jit_strategy = -1;

static R_INLINE bool R_CheckJIT(SEXP fun)
{
    /* to help with testing */
    if (jit_strategy < 0) {
	int dflt = R_jit_enabled == 1 ?
	    STRATEGY_NO_SMALL : STRATEGY_TOP_SMALL_MAYBE;
	int val = dflt;
	char *valstr = getenv("R_JIT_STRATEGY");
	if (valstr != NULL)
	    val = atoi(valstr);
	if (val < 0 || val > 4)
	    jit_strategy = dflt;
	else
	    jit_strategy = val;

	valstr = getenv("R_MIN_JIT_SCORE");
	if (valstr != NULL)
	    MIN_JIT_SCORE = atoi(valstr);
    }

    SEXP body = BODY(fun);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP &&
	ByteCode::ByteCodeEnabled() && !NOJIT(fun)) {

	if (MAYBEJIT(fun)) {
	    /* function marked as MAYBEJIT the first time now seen
	       twice, so go ahead and compile */
	    UNSET_MAYBEJIT(fun);
	    return true;
	}

	if (jit_strategy == STRATEGY_NO_SCORE ||
	    jit_strategy == STRATEGY_NO_CACHE)
	    return true;

	int score = JIT_score(body);
	if (jit_strategy == STRATEGY_ALL_SMALL_MAYBE)
	    if (score < MIN_JIT_SCORE) { SET_MAYBEJIT(fun); return false; }

	if (CLOENV(fun) == R_GlobalEnv) {
	    /* top level functions are only compiled if score is high enough */
	    if (score < MIN_JIT_SCORE) {
		if (jit_strategy == STRATEGY_TOP_SMALL_MAYBE)
		    SET_MAYBEJIT(fun);
		else
		    SET_NOJIT(fun);
		return false;
	    }
	    else return true;
	}
	else {
	    /* only compile non-top-level function if score is high
	       enough and seen twice */
	    if (score < MIN_JIT_SCORE) {
		SET_NOJIT(fun);
		return false;
	    }
	    else {
		SET_MAYBEJIT(fun);
		return false;
	    }
	}
    }
    return false;
}

#ifdef DEBUG_JIT
# define PRINT_JIT_INFO							\
    REprintf("JIT cache hits: %lu; env: %lu; body %lu\n",		\
	     jit_info.count, jit_info.envcount, jit_info.bdcount)
#else
# define PRINT_JIT_INFO	do { } while(0)
#endif


/* FIXME: this should not depend on internals from envir.c but does for now. */
/* copied from envir.c for now */
#define IS_USER_DATABASE(rho)  (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"))
#define IS_STANDARD_UNHASHED_FRAME(e) (!IS_USER_DATABASE(e) && HASHTAB(e) == R_NilValue)
#define IS_STANDARD_HASHED_FRAME(e) (!IS_USER_DATABASE(e) && HASHTAB(e) != R_NilValue)

/* This makes a snapshot of the local variables in cmpenv and creates
   a new environment with the same top level environment and bindings
   with value R_NilValue for the local variables. This guards against
   the cmpenv changing after being entered in the cache, and also
   allows large values that might be bound to local variables in
   cmpenv to be reclaimed (also, some package tests, e.g. in shiny, test
   when things get reclaimed). Standard local frames are processed directly,
   hashed frames are processed via lsInternal3, which involves extra
   allocations, but should be used rarely. If a local environment is
   of unsupported type, topenv is returned as a valid conservative
   answer.

   Since we compute the local variables at compile
   time we should record them in the byte code object and use the
   recorded value. */
static R_INLINE void cmpenv_enter_frame(SEXP frame, SEXP newenv)
{
    for (; frame != R_NilValue; frame = CDR(frame))
	defineVar(TAG(frame), R_NilValue, newenv);
}

static R_INLINE SEXP make_cached_cmpenv(SEXP fun)
{
    SEXP frmls = FORMALS(fun);
    SEXP cmpenv = CLOENV(fun);
    SEXP top = topenv(R_NilValue, cmpenv);
    if (cmpenv == top && frmls == R_NilValue)
	return cmpenv;
    else {
	GCStackRoot<> newenv;
	newenv = NewEnvironment(R_NilValue, R_NilValue, top);
	for (; frmls != R_NilValue; frmls = CDR(frmls))
	    defineVar(TAG(frmls), R_NilValue, newenv);
	for (SEXP env = cmpenv; env != top; env = ENCLOS(env)) {
	    if (IS_STANDARD_UNHASHED_FRAME(env))
		cmpenv_enter_frame(FRAME(env), newenv);
	    else if (IS_STANDARD_HASHED_FRAME(env)) {
		SEXP h = HASHTAB(env);
		int n = length(h);
		for (int i = 0; i < n; i++)
		    cmpenv_enter_frame(VECTOR_ELT(h, i), newenv);
	    } else {
		return top;
	    }
		/* topenv is a safe conservative answer; if a closure
		   defines anything, its environment will not match, and
		   it will never be compiled */
		/* FIXME: would it be safe to simply ignore elements of
		   these environments? */
	}
	return newenv;
    }
}

// JITEntry
/* Cache entries are CONS cells with the body in CAR, the environment
   in CDR, and the Srcref in the TAG. */
static R_INLINE void set_jit_cache_entry(R_exprhash_t hash, SEXP val)
{
    int hashidx = hash % JIT_CACHE_SIZE;

    PROTECT(val);
    SEXP entry = CXXR_cons(BODY(val), make_cached_cmpenv(val), getAttrib(val, R_SrcrefSymbol));
    SET_VECTOR_ELT(JIT_cache, hashidx, entry);
    UNPROTECT(1); /* val */

    JIT_cache_hashes[hashidx] = hash;
}

static R_INLINE SEXP jit_cache_code(SEXP entry)
{
    return CAR(entry);
}

static R_INLINE SEXP jit_cache_env(SEXP entry)
{
    return CDR(entry);
}

static R_INLINE SEXP jit_cache_srcref(SEXP entry)
{
    return TAG(entry);
}

/* forward declaration */
static SEXP bytecodeExpr(SEXP);

static R_INLINE SEXP jit_cache_expr(SEXP entry)
{
    return bytecodeExpr(jit_cache_code(entry));
}

static R_INLINE SEXP get_jit_cache_entry(R_exprhash_t hash)
{
    int hashidx = hash % JIT_CACHE_SIZE;
    if (JIT_cache_hashes[hashidx] == hash) {
	SEXP entry = VECTOR_ELT(JIT_cache, hashidx);
	if (TYPEOF(jit_cache_code(entry)) == BCODESXP)
	    return entry;
	else
	    /* function has been de-compiled; clear the cache entry */
	    SET_VECTOR_ELT(JIT_cache, hashidx, R_NilValue);
    }
    return R_NilValue;
}

static R_INLINE bool jit_expr_match(SEXP expr, SEXP body)
{
    /*** is 16 right here??? does this need to be faster??? */
    return R_compute_identical(expr, body, 16);
}

static R_INLINE SEXP cmpenv_topenv(SEXP cmpenv)
{
    return topenv(R_NilValue, cmpenv);
}

static R_INLINE bool cmpenv_exists_local(SEXP sym, SEXP cmpenv, SEXP top)
{
    if (cmpenv != top)
	for (SEXP frame = FRAME(cmpenv);
	     frame != R_NilValue;
	     frame = CDR(frame))
	    if (TAG(frame) == sym)
		return true;
    return false;
}

static R_INLINE bool jit_env_match(SEXP cmpenv, SEXP fun)
{
    /* Can code compiled for environment cmpenv be used as compiled
       code for environment env?  These tests rely on the assumption
       that compilation is only affected by what variables are bound,
       not their values. So as long as both cmpenv and env have the
       same top level environment and all local bindings present in
       the formals and environment of fun are also present in cmpenv
       the code for cmpenv can be reused, though it might be less
       efficient if a binding in cmpenv prevents an optimization that
       would be possible in env. */

    SEXP env = CLOENV(fun);
    SEXP top = topenv(R_NilValue, env);

    if (top == cmpenv_topenv(cmpenv)) {
	for (SEXP frmls = FORMALS(fun); frmls != R_NilValue; frmls = CDR(frmls))
	    if (!cmpenv_exists_local(TAG(frmls), cmpenv, top))
		return false;
	for (; env != top; env = ENCLOS(env)) {
	    if (IS_STANDARD_UNHASHED_FRAME(env)) {
		/* To keep things simple, for a match this code
		   requires that the local frames be standard unhashed
		   frames. */
		for (SEXP frame = FRAME(env);
		     frame != R_NilValue;
		     frame = CDR(frame))
		    if (!cmpenv_exists_local(TAG(frame), cmpenv, top))
			return false;
	    }
	    else return false;
	}
	return true;
    }
    else return false;
}

static R_INLINE bool jit_srcref_match(SEXP cmpsrcref, SEXP srcref)
{
    return R_compute_identical(cmpsrcref, srcref, 0);
}

attribute_hidden SEXP R::R_cmpfun1(SEXP fun)
{
    bool old_visible = Evaluator::resultPrinted();
    SEXP packsym, funsym;
    GCStackRoot<> call, fcall, val;

    packsym = install("compiler");
    funsym = install("tryCmpfun");

    fcall = lang3(R_TripleColonSymbol, packsym, funsym);
    call = lang2(fcall, fun);
    val = eval(call, R_BaseEnv);
    if (TYPEOF(BODY(val.get())) != BCODESXP)
	/* Compilation may have failed because R allocator could not malloc
	   memory to extend the R heap, so we run GC to release some pages.
	   This problem has been observed while byte-compiling packages on
	   installation: serialization uses malloc to allocate buffers and
	   fails when the compiler makes R allocator exhaust malloc memory.
	   A more general solution might be to run the GC conditionally inside
	   error handling. */
	R_gc();

    Evaluator::enableResultPrinting(old_visible);
    return val;
}

/* fun is modified in-place when compiled */
static void R_cmpfun(SEXP fun)
{
    R_exprhash_t hash = 0;
    if (jit_strategy != STRATEGY_NO_CACHE) {
	hash = hashfun(fun);
	SEXP entry = get_jit_cache_entry(hash);

	if (entry != R_NilValue) {
	    jit_info.count++;
	    if (jit_env_match(jit_cache_env(entry), fun)) {
		jit_info.envcount++;
		if (jit_expr_match(jit_cache_expr(entry), BODY(fun))) {
		    jit_info.bdcount++;
		    /* if function body has a srcref, all srcrefs compiled
		       in that function only depend on the body srcref;
		       but, otherwise the srcrefs compiled in are taken
		       from the function (op) */
		    if (getAttrib(BODY(fun), R_SrcrefSymbol) != R_NilValue ||
			jit_srcref_match(jit_cache_srcref(entry),
					 getAttrib(fun, R_SrcrefSymbol))) {
			PRINT_JIT_INFO;
			SET_BODY(fun, jit_cache_code(entry));
			/**** reset the cache here?*/
			return;
		    }
		}
		/* The functions probably differ only in source references
		   (for functions with bodies that have no source references
		   we know for sure, for other functions we speculate).
		   Therefore, we allow re-compilation and re-caching. This
		   situation may be caused e.g. by re-sourcing the same source
		   file or re-pasting the same definitions for a function in
		   interactive R session. Note srcref information includes
		   environments (srcfile), which are now compared by address,
		   so it may be we actually have logically identical source
		   references, anyway. */
		/* FIXME: revisit this when deep comparison of environments
			  (and srcrefs) is available */
	    } else {
		SET_NOJIT(fun);
		/**** also mark the cache entry as NOJIT, or as need to see
		      many times? */
		return;
	    }
	}
	PRINT_JIT_INFO;
    }

    SEXP val = R_cmpfun1(fun);

    if (TYPEOF(BODY(val)) != BCODESXP)
	SET_NOJIT(fun);
    else {
	if (jit_strategy != STRATEGY_NO_CACHE)
	    set_jit_cache_entry(hash, val); /* val is protected by callee */
	SET_BODY(fun, BODY(val));
    }
}

static SEXP R_compileExpr(SEXP expr, SEXP rho)
{
    bool old_visible = Evaluator::resultPrinted();
    SEXP packsym, funsym, quotesym;
    GCStackRoot<> qexpr, call, fcall;

    packsym = install("compiler");
    funsym = install("tryCompile");
    quotesym = install("quote");

    fcall = lang3(R_TripleColonSymbol, packsym, funsym);
    qexpr = lang2(quotesym, expr);
    /* compile(e, env, options, srcref) */
    call = lang5(fcall, qexpr, rho, R_NilValue, R_getCurrentSrcref());
    SEXP val = Evaluator::evaluate(call, Environment::base());
    Evaluator::enableResultPrinting(old_visible);
    return val;
}

static bool R_compileAndExecute(SEXP call, SEXP rho)
{
    int old_enabled = R_jit_enabled;
    GCStackRoot<> code;
    bool ans = false;

    R_jit_enabled = 0;
    PROTECT(call);
    PROTECT(rho);
    code = R_compileExpr(call, rho);
    R_jit_enabled = old_enabled;

    if (TYPEOF(code) == BCODESXP) {
	bcEval(code, rho, true);
	ans = true;
    }

    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_enablejit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_jit_enabled;
    checkArity(op, args);
    int new_ = asInteger(CAR(args));
    if (new_ >= 0) {
	if (new_ > 0)
	    loadCompilerNamespace();
	checkCompilerOptions(new_);
	R_jit_enabled = new_;
    }
    /* negative 'new_' just returns 'old' */
    return ScalarInteger(old);
}

attribute_hidden SEXP do_compilepkgs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_compile_pkgs, new_;
    checkArity(op, args);
    new_ = asLogical(CAR(args));
    if (new_ != NA_LOGICAL && new_)
	loadCompilerNamespace();
    R_compile_pkgs = new_;
    return ScalarLogical(old);
}

/* this function gets the srcref attribute from a statement block,
   and confirms it's in the expected format */

static R_INLINE SEXP getBlockSrcrefs(SEXP call)
{
    SEXP srcrefs = getAttrib(call, R_SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) return srcrefs;
    return R_NilValue;
}

/* this function extracts one srcref, and confirms the format */
/* It assumes srcrefs has already been validated to be a VECSXP or NULL */

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind)
{
    SEXP result;
    if (!isNull(srcrefs)
	&& length(srcrefs) > ind
	&& !isNull(result = VECTOR_ELT(srcrefs, ind))
	&& TYPEOF(result) == INTSXP
	&& length(result) >= 6)
	return result;
    else
	return R_NilValue;
}

#ifdef ADJUST_ENVIR_REFCNTS
static R_INLINE bool R_isReplaceSymbol(SEXP fun)
{
    /* fun is a replacement function name if it contains '<-'
       anywhere. For internally dispatched replacement functions this
       may occur in the middle; in other cases it will be at the
       end. */
    return (TYPEOF(fun) == SYMSXP && strstr(CHAR(PRINTNAME(fun)), "<-"));
}
#endif

void R::PrintCall(SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines,
        blines = asInteger(GetOption1(install("deparse.max.lines")));
    if (blines != NA_INTEGER && blines > 0)
	R_BrowseLines = blines;

    R_PrintData pars;
    PrintInit(&pars, rho);
    PrintValueRec(call, &pars);

    R_BrowseLines = old_bl;
}

#ifdef ADJUST_ENVIR_REFCNTS
/* After executing a closure call the environment created for the call
   may no longer be reachable. If this is the case, then its bindings
   can be cleared to reduce the reference counts on the binding
   values.

   The environment will no longer be reachable if it is not being
   returned as the value of the closure and has no references. It will
   also no longer be reachable be the case if all references to it are
   internal cycles through its bindings. A full check for internal
   cycles would be too expensive, but the two most important cases can
   be checked at reasonable cost:

   - a promise with no other references, most likely from an
     unevaluated argument default expression;

   - a closure with no further references and not returned as the
     value, most likely a local helper function.

   The promises created for a closure call can also be cleared one the
   call is complete and the promises are no longer reachable. This
   drops reference counts on the values and the environments.
*/

static int countCycleRefs(SEXP rho, SEXP val)
{
    /* check for simple cycles */
    int crefs = 0;
    for (SEXP b = FRAME(rho);
	 b != R_NilValue && REFCNT(b) == 1;
	 b = CDR(b)) {
	if (BNDCELL_TAG(b)) continue;
	SEXP v = CAR(b);
	if (val != v) {
	    switch(TYPEOF(v)) {
	    case PROMSXP:
		if (REFCNT(v) == 1 && PRENV(v) == rho)
		    crefs++;
		break;
	    case CLOSXP:
		if (REFCNT(v) == 1 && CLOENV(v) == rho)
		    crefs++;
		break;
	    case ENVSXP: /* is this worth bothering with? */
		if (v == rho)
		    crefs++;
		break;
	    default:
		break;
	    }
	}
    }
    return crefs;
}

static R_INLINE void clearPromise(SEXP p)
{
#ifdef IMMEDIATE_PROMISE_VALUES
    /* If PROMISE_TAG(p) is not zero then the promise is evaluated and
       the environment is already R_NilValue. Setting the value is not
       necessary, and clearing the code is probably not worth it. */
    if (PROMISE_TAG(p))	return;
#endif
    SET_PRVALUE(p, R_UnboundValue);
    SET_PRENV(p, R_NilValue);
    SET_PRCODE(p, R_NilValue); /* for calls with literal values */
}

static R_INLINE void cleanupEnvDots(SEXP d)
{
    for (; d != R_NilValue && REFCNT(d) == 1; d = CDR(d)) {
	SEXP v = CAR(d);
	if (REFCNT(v) == 1 && TYPEOF(v) == PROMSXP)
	    clearPromise(v);
	SETCAR(d, R_NilValue);
    }
}

static R_INLINE void cleanupEnvVector(SEXP v)
{
    /* This is mainly for handling results of list(...) stored as a
       local variable. It would be cheaper to just use
       DECREMENT_REFCNT. It might also make sense to max out at len =
       10 or so, and to avoid ALTREP objects. */

    /* FIXME: Disabled for now since a BUILTIN that saves its (NR)
       list can cause problems. .External.graphics does this for
       recording. Probably the best option is to not have the args go
       down as NR. Most of these are fixed now, but this still seems
       to wake things up, so hold off for now. */
    return;

    // avoid ODS compiler warning.
#ifdef FALSE
    R_xlen_t len = XLENGTH(v);
    for (R_xlen_t i = 0; i < len; i++)
	SET_VECTOR_ELT(v, i, R_NilValue);
#endif
}

static R_INLINE void R_CleanupEnvir(SEXP rho, SEXP val)
{
    if (val != rho) {
	/* release the bindings and promises in rho if rho is no
	   longer accessible from R */
	int refs = REFCNT(rho);
	if (refs > 0)
	    refs -= countCycleRefs(rho, val);
	if (refs == 0) {
	    for (SEXP b = FRAME(rho);
		 b != R_NilValue && REFCNT(b) == 1;
		 b = CDR(b)) {
		if (BNDCELL_TAG(b)) continue;
		SEXP v = CAR(b);
		if (REFCNT(v) == 1 && v != val) {
		    switch(TYPEOF(v)) {
		    case PROMSXP:
			clearPromise(v);
			break;
		    case DOTSXP:
			cleanupEnvDots(v);
			break;
		    case VECSXP: /* mainly for list(...) */
			cleanupEnvVector(v);
			break;
		    default:
			break;
		    }
		}
		SETCAR(b, R_NilValue);
	    }
	    SET_ENCLOS(rho, Environment::empty());
	}
    }
}

/* this needs more work -- PUSHCALLARG_RC needed in more places */
//#define NO_CALL_FRAME_ARGS_NR

static void unpromiseArgs(SEXP pargs)
{
    /* This assumes pargs will no longer be referenced. We could
       double check the refcounts on pargs as a sanity check. */
    for (; pargs != R_NilValue; pargs = CDR(pargs)) {
	SEXP v = CAR(pargs);
	if (TYPEOF(v) == PROMSXP && REFCNT(v) == 1)
	    clearPromise(v);
	SETCAR(pargs, R_NilValue);
    }
}
#endif

#define SUPPORT_TAILCALL
#ifdef SUPPORT_TAILCALL
static SEXP R_exec_token = NULL; /* initialized in R_initEvalSymbols below */

static R_INLINE bool is_exec_continuation(SEXP val)
{
    return (TYPEOF(val) == VECSXP && XLENGTH(val) == 4 &&
	    VECTOR_ELT(val, 0) == R_exec_token);
}

static SEXP applyClosure_core(SEXP, SEXP, SEXP, SEXP, SEXP, bool);

static R_INLINE SEXP handle_exec_continuation(SEXP val)
{
    while (is_exec_continuation(val)) {
	SEXP call = PROTECT(VECTOR_ELT(val, 1));
	SEXP rho = PROTECT(VECTOR_ELT(val, 2));
	SET_VECTOR_ELT(val, 2, R_NilValue); // to drop REFCNT
	SEXP op = PROTECT(VECTOR_ELT(val, 3));

	if (TYPEOF(op) == CLOSXP) {
	    SEXP arglist = PROTECT(promiseArgs(CDR(call), rho));
	    SEXP suppliedvars = R_NilValue;
	    val = applyClosure_core(call, op, arglist, rho, suppliedvars, true);
# ifdef ADJUST_ENVIR_REFCNTS
	    R_CleanupEnvir(rho, val);
# endif
	    UNPROTECT(1); /* arglist */
	}
	else {
	    /* Ideally this should handle BUILTINSXP/SPECIALSXP calls
	       in the standard way as in eval() or bceval(). For now,
	       just build a new call and eval. */
	    SEXP expr = PROTECT(LCONS(op, CDR(call)));
	    val = eval(expr, rho);
	    UNPROTECT(1); /* expr */
	}
	UNPROTECT(3); /* call, rho, op */
    }
    return val;
}
#endif

/* Note: GCC will not inline execClosure because it calls setjmp */
static R_INLINE SEXP R_execClosure(SEXP call, SEXP newrho, SEXP sysparent,
                                   SEXP rho, SEXP arglist, SEXP op);

static SEXP make_applyClosure_env(SEXP call, SEXP op, SEXP arglist, SEXP rho,
				  SEXP suppliedvars)
{
    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call,
		  "%s", _("'rho' cannot be C NULL: detected in C-level applyClosure"));
    if (!isEnvironment(rho))
	errorcall(call, _("'rho' must be an environment not %s: detected in C-level applyClosure"),
		  R_typeToChar(rho));

    SEXP formals = FORMALS(op);
    SEXP savedrho = CLOENV(op);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  matchArgs_RC is used since the
	result becomes part of the environment frame and so needs
	reference couting enabled. */

    SEXP actuals = matchArgs_RC(formals, arglist, call);
    GCStackRoot<> newrho;
    newrho = NewEnvironment(formals, actuals, savedrho);

    /*  Use the default code for unbound formals.  FIXME: It looks like
	this code should preceed the building of the environment so that
	this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    SEXP f = formals;
    SEXP a = actuals;
    while (f != R_NilValue) {
	if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
	addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
	SET_NO_SPECIAL_SYMBOLS(newrho);

    return newrho;
}

/* Apply SEXP op of type CLOSXP to actuals */
static SEXP applyClosure_core(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			      SEXP suppliedvars, bool unpromise)
{
    GCStackRoot<> newrho;
    newrho = make_applyClosure_env(call, op, arglist, rho, suppliedvars);

    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    SEXP val = R_execClosure(call, newrho,
			     (R_GlobalContext && R_GlobalContext->callflag == CTXT_GENERIC) ?
			     R_GlobalContext->sysparent.get() : rho,
			     rho, arglist, op);
#ifdef ADJUST_ENVIR_REFCNTS
    bool is_getter_call =
	(CADR(call) == R_TmpvalSymbol && !R_isReplaceSymbol(CAR(call)));
    R_CleanupEnvir(newrho, val);
    if (is_getter_call && MAYBE_REFERENCED(val))
    	val = shallow_duplicate(val);
    if (unpromise)
	unpromiseArgs(arglist);
#endif

    return val;
}

attribute_hidden
SEXP R::applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho,
		  SEXP suppliedvars, bool unpromise)
{
    SEXP val = applyClosure_core(call, op, arglist, rho,
				 suppliedvars, unpromise);
#ifdef SUPPORT_TAILCALL
    val = handle_exec_continuation(val);
#endif
    return val;
}

static SEXP STACKVAL_TO_SEXP(R_bcstack_t);

static R_INLINE SEXP R_execClosure(SEXP call, SEXP newrho, SEXP sysparent,
                                   SEXP rho, SEXP arglist, SEXP op)
{
    SEXP retValue = R_NilValue;

    if (R_CheckJIT(op)) {
	int old_enabled = R_jit_enabled;
	R_jit_enabled = 0;
	R_cmpfun(op);
	R_jit_enabled = old_enabled;
    }
    SEXP body = BODY(op);

    {
    RCNTXT cntxt(CTXT_RETURN, call, newrho, sysparent, arglist, op);

    /* Get the srcref record from the closure object. The old srcref was
       saved in cntxt. */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* Debugging */
    bool dbg = ((RDEBUG(op) && R_current_debug_state()) || RSTEP(op)
         || (ENV_RDEBUG(rho) && R_BrowserLastCommand == 's'));
    Closure::DebugScope debugscope(op, call, rho, dbg);

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */
    bool redo = false;
    bool evaluate = true;
    do
    {
        redo = false;
        try
        {
            /* make it available to on.exit and implicitly protect */
            if (evaluate)
            {
                evaluate = false;
                cntxt.returnValue = SEXP_TO_STACKVAL(eval(body, newrho));
            }
            R_Srcref = cntxt.srcref;
            retValue = STACKVAL_TO_SEXP(cntxt.returnValue);
            RCNTXT::maybeRunOnExit(&cntxt); // so that endcontext doesn't run it
            endcontext(&cntxt);
        }
        catch (JMPException &e)
        {
            if (e.context() != &cntxt)
                throw;
            redo = true;
            cntxt.returnValue = SEXP_TO_STACKVAL(e.value());
        }
    } while (redo);
    }

    return retValue;
}

void Closure::DebugScope::startDebugging() const
{
    Evaluator::RContext *cntxt = R_GlobalContext;
    SEXP body = BODY(m_closure);
    SEXP newrho = cntxt->cloenv;

    SET_RSTEP(m_closure, 0);
    SET_ENV_RDEBUG(newrho, 1);
    cntxt->browserfinish = false; // Don't want to inherit the "f"
    // switch to interpreted version when debugging compiled code
    if (ByteCode::isA(body))
        body = bytecodeExpr(body);
    Rprintf("debugging in: ");
    PrintCall(m_call, m_env);
    SrcrefPrompt("debug", R_Srcref);
    Rf_PrintValue(body);
    do_browser(m_call, m_closure, R_NilValue, newrho);
}

void Closure::DebugScope::endDebugging() const
{
    try
    {
        Rprintf("exiting from: ");
        PrintCall(m_call, m_env);
    }
    // Don't allow exceptions to escape destructor:
    catch (...)
    {
    }
}

SEXP R_forceAndCall(SEXP expr, int n, SEXP rho)
{
    GCStackRoot<> fun, e(expr);
    GCStackRoot<> tmp(R_NilValue);
    if (TYPEOF(CAR(e)) == SYMSXP)
	/* This will throw an error if the function is not found */
	fun = findFun(CAR(e), rho);
    else
	fun = eval(CAR(e), rho);

    if (TYPEOF(fun) == SPECIALSXP) {
	int flag = PRIMPRINT(fun.get());
	Evaluator::enableResultPrinting(flag != 1);
	tmp = PRIMFUN(fun.get()) (e, fun, CDR(e.get()), rho);
	if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
    }
    else if (TYPEOF(fun) == BUILTINSXP) {
	int flag = PRIMPRINT(fun.get());
	tmp = evalList(CDR(e.get()), rho, e, 0);
	if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
	/* We used to insert a context only if profiling,
	   but helps for tracebacks on .C etc. */
	if (Evaluator::profiling() || (PPINFO(fun.get()).kind == PP_FOREIGN)) {
	    SEXP oldref = R_Srcref;
	    RCNTXT cntxt(CTXT_BUILTIN, e,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    tmp = PRIMFUN(fun.get()) (e, fun, tmp, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    tmp = PRIMFUN(fun.get()) (e, fun, tmp, rho);
	}
	if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
    }
    else if (TYPEOF(fun) == CLOSXP) {
	tmp = promiseArgs(CDR(e.get()), rho);
	int i = 0;
	for (SEXP a = tmp; i < n && a != R_NilValue; a = CDR(a), i++) {
	    SEXP p = CAR(a);
	    if (TYPEOF(p) == PROMSXP)
		eval(p, rho);
	    else if (p == R_MissingArg)
		errorcall(e, _("argument %d is empty"), i + 1);
	    else error("%s", _("something weird happened"));
	}
	SEXP pargs = tmp;
	tmp = applyClosure(e, fun, pargs, rho, R_NilValue, true);
    }
    else {
	error("%s", _("attempt to apply non-function"));
    }

    return tmp;
}

attribute_hidden SEXP do_forceAndCall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n = asInteger(eval(CADR(call), rho));
    GCStackRoot<> e;
    e = CDDR(call);

    /* this would not be needed if CDDR(call) was a LANGSXP */
    e = LCONS(CAR(e), CDR(e.get()));
    SEXP val = R_forceAndCall(e, n, rho);

    return val;
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R::R_execMethod(SEXP op, SEXP rho)
{
    SEXP next, val;
    GCStackRoot<> newrho;

    /* create a new environment frame enclosed by the lexical
       environment of the method */
    newrho = NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /* copy the bindings for the formal environment from the top frame
       of the internal environment of the generic call to the new
       frame.  need to make sure missingness information is preserved
       and the environments for any default expression promises are
       set to the new environment.  should move this to envir.c where
       it can be done more efficiently. */
    for (next = FORMALS(op); next != R_NilValue; next = CDR(next)) {
	SEXP symbol =  TAG(next);
	R_varloc_t loc = R_findVarLocInFrame(rho,symbol);
	if (R_VARLOC_IS_NULL(loc))
	    error(_("could not find symbol \"%s\" in environment of the generic function"),
		  CHAR(PRINTNAME(symbol)));
	bool missing = R_GetVarLocMISSING(loc);
	val = R_GetVarLocValue(loc);
	SET_FRAME(newrho, CONS(val, FRAME(newrho.get())));
	SET_TAG(FRAME(newrho.get()), symbol);
	if (missing) {
	    SET_MISSING(FRAME(newrho.get()), missing);
	    if (TYPEOF(val) == PROMSXP && PRENV(val) == rho) {
		SEXP deflt;
		SET_PRENV(val, newrho);
		/* find the symbol in the method, copy its expression
		 * to the promise */
		for (deflt = FORMALS(op); deflt != R_NilValue; deflt = CDR(deflt)) {
		    if (TAG(deflt) == symbol)
			break;
		}
		if (deflt == R_NilValue)
		    error(_("symbol \"%s\" not in environment of method"),
			  CHAR(PRINTNAME(symbol)));
		SET_PRCODE(val, CAR(deflt));
	    }
	}
#ifdef SWITCH_TO_REFCNT
	/* re-promise to get reference counts for references from rho
	   and newrho right. */
	if (TYPEOF(val) == PROMSXP)
	    SETCAR(FRAME(newrho.get()), mkPROMISE(val, rho));
#endif
    }

    /* copy the bindings of the special dispatch variables in the top
       frame of the generic call to the new frame */
    defineVar(R_dot_defined, R_findVarInFrame(rho, R_dot_defined), newrho);
    defineVar(R_dot_Method, R_findVarInFrame(rho, R_dot_Method), newrho);
    defineVar(R_dot_target, R_findVarInFrame(rho, R_dot_target), newrho);

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    defineVar(R_dot_Generic, R_findVar(R_dot_Generic, rho), newrho);
    defineVar(R_dot_Methods, R_findVar(R_dot_Methods, rho), newrho);

    /* Find the calling context.  Should be R_GlobalContext unless
       profiling has inserted a CTXT_BUILTIN frame. */
    RCNTXT *cptr = R_GlobalContext;
    if (cptr->callflag & CTXT_BUILTIN)
	cptr = cptr->nextcontext;

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    SEXP callerenv = cptr->sysparent, /* or rho? */

    /* get the rest of the stuff we need from the current context,
       execute the method, and return the result */
    call    = cptr->call,
    arglist = cptr->promargs;
    val = R_execClosure(call, newrho, callerenv, callerenv, arglist, op);
#ifdef ADJUST_ENVIR_REFCNTS
    R_CleanupEnvir(newrho, val);
#endif

#ifdef SUPPORT_TAILCALL
    if (is_exec_continuation(val))
	error("'Exec' and 'Tailcall' are not supported in methods yet");
#endif
    return val;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho, R_varloc_t *ploc)
{
    GCStackRoot<> vl;

    if ((vl = R_findVarInFrame(rho, symbol)) != R_UnboundValue) {
	vl = eval(symbol, rho);	/* for promises */
	if (MAYBE_SHARED(vl)) {
	    /* Using R_shallow_duplicate_attr may defer duplicating
	       data until it it is needed. If the data are duplicated,
	       then the wrapper can be discarded at the end of the
	       assignment process in try_assign_unwrap(). */
	    vl = R_shallow_duplicate_attr(vl);
	    defineVar(symbol, vl, rho);
	    INCREMENT_NAMED(vl);
	}
	*ploc = R_findVarLocInFrame(rho, symbol);
	return vl;
    }

    vl = eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));

    vl = shallow_duplicate(vl);
    defineVar(symbol, vl, rho);
    *ploc = R_findVarLocInFrame(rho, symbol);
    INCREMENT_NAMED(vl);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP R_valueSym = NULL; /* initialized in R_initEvalSymbols below */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    ptmp = tmp = allocLang(length(args)+3);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != R_NilValue) {
	SETCAR(ptmp, CAR(args));
	SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, R_valueSym);
    MARK_ASSIGNMENT_CALL(tmp);
    return tmp;
}



static R_INLINE bool asLogicalNoNA2(SEXP s, SEXP call)
{
    int cond = NA_LOGICAL; // cannot be Rboolean

    /* handle most common special case directly */
    if (IS_SCALAR(s, LGLSXP)) {
	cond = SCALAR_LVAL(s);
	if (cond != NA_LOGICAL)
	    return cond;
    }
    else if (IS_SCALAR(s, INTSXP)) {
	int val = SCALAR_IVAL(s);
	if (val != NA_INTEGER)
	    return val != 0;
    }

    int len = length(s);
    if (len > 1)
	errorcall(call, "%s", _("the condition has length > 1"));
    if (len > 0) {
	/* inline common cases for efficiency */
	switch(TYPEOF(s)) {
	case LGLSXP:
	    cond = LOGICAL(s)[0];
	    break;
	case INTSXP:
	    cond = INTEGER(s)[0]; /* relies on NA_INTEGER == NA_LOGICAL */
	    break;
	default:
	    cond = asLogical(s);
	}
    }

    if (cond == NA_LOGICAL) {
	char *msg = len ? (isLogical(s) ?
			   _("missing value where TRUE/FALSE needed") :
			   _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	errorcall(call, "%s", msg);
    }
    return cond;
}


#define BodyHasBraces(body) \
    (isLanguage(body) && CAR(body) == R_BraceSymbol)

/* Allocate space for the loop variable value the first time through
   (when v == R_NilValue) and when the value may have been assigned to
   another variable. This should be safe and avoid allocation in many
   cases. */
#define ALLOC_LOOP_VAR(v, val_type) do {			\
	if (v == R_NilValue || MAYBE_SHARED(v) ||		\
	    ATTRIB(v) != R_NilValue || (v) != CAR(cell)) {	\
	    v = allocVector(val_type, 1);			\
	    INCREMENT_NAMED(v);					\
	}							\
    } while(0)

attribute_hidden SEXP do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> Cond;
    SEXP Stmt = R_NilValue;
    bool vis = false;

    Cond = eval(CAR(args), rho);
    if (asLogicalNoNA2(Cond, call))
	Stmt = CADR(args);
    else {
	if (length(args) > 2)
	    Stmt = CADDR(args);
	else
	    vis = true;
    }
    if (!vis && ENV_RDEBUG(rho) && !BodyHasBraces(Stmt) && !R_GlobalContext->browserfinish) {
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(Stmt);
	do_browser(call, op, R_NilValue, rho);
    }
    if (vis) {
	Evaluator::enableResultPrinting(false); /* case of no 'else' so return invisible NULL */
	return Stmt;
    }
    return (eval(Stmt, rho));
}

#define IS_USER_DATABASE(rho)					\
    (OBJECT((rho)) && inherits((rho), "UserDefinedDatabase"))

static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace || IS_USER_DATABASE(rho))
	return R_NilValue;
    else {
	R_varloc_t loc = R_findVarLocInFrame(rho, symbol);
	return (!R_VARLOC_IS_NULL(loc) && !IS_ACTIVE_BINDING(loc.cell)) ?
	    loc.cell : R_NilValue;
    }
}

static R_INLINE bool SET_BINDING_VALUE(SEXP loc, SEXP value)
{
    /* This depends on the current implementation of bindings */
    if (loc == R_NilValue)
        return false;
    if (BINDING_IS_LOCKED(loc))
        return false;
    if (IS_ACTIVE_BINDING(loc))
        return false;

    if (BNDCELL_TAG(loc) || CAR(loc) != value)
    {
        SET_BNDCELL(loc, value);
        if (MISSING(loc))
            SET_MISSING(loc, 0);
    }
    return true;
}

attribute_hidden SEXP do_for(SEXP call, SEXP op, SEXP args_, SEXP rho_)
{
    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and might change between
       the setjmp and longjmp calls. Theoretically this does not
       include n and bgn, but gcc -O2 -Wclobbered warns about these so
       to be safe we declare them volatile as well. */
    R_xlen_t n;
    GCStackRoot<> v;
    GCStackRoot<> val;
    GCStackRoot<> cell;
    GCStackRoot<> rho(rho_);
    GCStackRoot<> args(args_);

    checkArity(op, args);
    SEXP sym = CAR(args);
    val = CADR(args.get());
    SEXP body = CADDR(args.get());

    if (!isSymbol(sym)) errorcall(call, "%s", _("non-symbol loop variable"));

    bool dbg = ENV_RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && ByteCode::ByteCodeEnabled()
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    val = eval(val, rho);

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if (inherits(val, "factor")) {
	SEXP tmp = asCharacterFactor(val);
	val = tmp;
    }

    if (isList(val) || isNull(val))
	n = length(val);
    else
	n = XLENGTH(val);

    SEXPTYPE val_type = TYPEOF(val);

    defineVar(sym, R_NilValue, rho);
    cell = GET_BINDING_CELL(sym, rho);
    bool bgn = BodyHasBraces(body);

    /* bump up links count of sequence to avoid modification by loop code */
    INCREMENT_LINKS(val);

    v = R_NilValue;

    {
    RCNTXT cntxt(CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue, R_NilValue);

    for (R_xlen_t i = 0; i < n; i++) {

	switch (val_type) {

	case EXPRSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(XVECTOR_ELT(val, i));
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    defineVar(sym, XVECTOR_ELT(val, i), rho);
	    break;
	case VECSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(VECTOR_ELT(val, i));
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    defineVar(sym, VECTOR_ELT(val, i), rho);
	    break;

	case LISTSXP:
	    /* make sure loop variable is not modified via other vars */
	    ENSURE_NAMEDMAX(CAR(val));
	    defineVar(sym, CAR(val), rho);
	    val = CDR(val.get());
	    break;

	default:

	    switch (val_type) {
	    case LGLSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_SCALAR_LVAL(v, LOGICAL_ELT(val, i));
		break;
	    case INTSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_SCALAR_IVAL(v, INTEGER_ELT(val, i));
		break;
	    case REALSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_SCALAR_DVAL(v, REAL_ELT(val, i));
		break;
	    case CPLXSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_SCALAR_CVAL(v, COMPLEX_ELT(val, i));
		break;
	    case STRSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_STRING_ELT(v, 0, STRING_ELT(val, i));
		break;
	    case RAWSXP:
		ALLOC_LOOP_VAR(v, val_type);
		SET_SCALAR_BVAL(v, RAW(val)[i]);
		break;
	    default:
		errorcall(call, "%s", _("invalid for() loop sequence"));
	    }
	    if (CAR(cell) == R_UnboundValue || !SET_BINDING_VALUE(cell, v))
		defineVar(sym, v, rho);
	}
	if (!bgn && ENV_RDEBUG(rho) && !R_GlobalContext->browserfinish) {
	    SrcrefPrompt("debug", R_Srcref);
	    PrintValue(body);
	    do_browser(call, op, R_NilValue, rho);
	}
    try
    {
        Evaluator::evaluate(body, rho);
    }
    catch (JMPException &e)
    {
        if (e.context() != &cntxt)
            throw;
        if (e.mask() == CTXT_BREAK)
            break;
        // Otherwise assume it's CTXT_NEXT
    }
    }

    endcontext(&cntxt);
    }
    DECREMENT_LINKS(val);
    SET_ENV_RDEBUG(rho, dbg);
    return R_NilValue;
}


attribute_hidden SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    bool dbg = ENV_RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && ByteCode::ByteCodeEnabled()
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    SEXP body = CADR(args);
    bool bgn = BodyHasBraces(body);

    {
    RCNTXT cntxt(CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue, R_NilValue);

    while (true)
    {
        try
        {
            GCStackRoot<> cond;
            cond = eval(CAR(args), rho);
            bool condl = asLogicalNoNA2(cond, call);
            if (!condl)
                break;
            if (ENV_RDEBUG(rho) && !bgn && !R_GlobalContext->browserfinish)
            {
                SrcrefPrompt("debug", R_Srcref);
                PrintValue(body);
                do_browser(call, op, R_NilValue, rho);
            }
            Evaluator::evaluate(body, rho);
            if (ENV_RDEBUG(rho) && !R_GlobalContext->browserfinish)
            {
                SrcrefPrompt("debug", R_Srcref);
                Rprintf("(while) ");
                PrintValue(CAR(args));
                do_browser(call, op, R_NilValue, rho);
            }
        }
        catch (JMPException &e)
        {
            if (e.context() != &cntxt)
                throw;
            if (e.mask() == CTXT_BREAK)
                break;
        }
    }

    endcontext(&cntxt);
    }
    SET_ENV_RDEBUG(rho, dbg);
    return R_NilValue;
}


attribute_hidden SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    bool dbg = ENV_RDEBUG(rho);
    if (R_jit_enabled > 2 && !dbg && ByteCode::ByteCodeEnabled()
	    && rho == R_GlobalEnv
	    && isUnmodifiedSpecSym(CAR(call), rho)
	    && R_compileAndExecute(call, rho))
	return R_NilValue;

    SEXP body = CAR(args);
    {
    RCNTXT cntxt(CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue, R_NilValue);

    while (true)
    {
        try
        {
            Evaluator::evaluate(body, rho);
        }
        catch (JMPException &e)
        {
            if (e.context() != &cntxt)
                throw;
            if (e.mask() == CTXT_BREAK)
                break;
        }
    }
    endcontext(&cntxt);
    }
    SET_ENV_RDEBUG(rho, dbg);
    return R_NilValue;
}


NORET attribute_hidden SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    findcontext(PRIMVAL(op), rho, R_NilValue);
}


attribute_hidden SEXP do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
}

attribute_hidden SEXP do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
	GCStackRoot<> srcrefs(getBlockSrcrefs(call));
	int i = 1;
	while (args != R_NilValue) {
	    R_Srcref = getSrcref(srcrefs, i++);
	    if (ENV_RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	    s = eval(CAR(args), rho);
	    args = CDR(args);
	}
	R_Srcref = R_NilValue;
    }
    return s;
}


NORET attribute_hidden SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP v;

    if (args == R_NilValue) /* zero arguments provided */
	v = R_NilValue;
    else if (CDR(args) == R_NilValue) /* one argument */
	v = eval(CAR(args), rho);
    else {
	v = R_NilValue; /* to avoid compiler warnings */
	errorcall(call, "%s", _("multi-argument returns are not permitted"));
    }

    findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, v);
}

static bool checkTailPosition(SEXP call, SEXP code, SEXP rho)
{
    /* Could allow for switch() as well.

       Ideally this should check that functions are from base;
       pretty safe bet for '{' and 'if'.

       Constructed code containing the call in multiple places could
       produce false positives.

       All this would be best done at compile time. */
    if (call == code)
	return true;
    else if (TYPEOF(code) == LANGSXP) {
	if (CAR(code) == R_BraceSymbol) {
	    while (CDR(code) != R_NilValue)
		code = CDR(code);
	    return checkTailPosition(call, CAR(code), rho);
	}
	else if (CAR(code) == R_IfSymbol)
	    return checkTailPosition(call, CADDR(code), rho) ||
		checkTailPosition(call, CADDDR(code), rho);
	else return false;
    }
    else return false;
}

attribute_hidden SEXP do_tailcall(SEXP call, SEXP op, SEXP args_, SEXP rho)
{
#ifdef SUPPORT_TAILCALL
    SEXP expr, env;
    GCStackRoot<> args(args_);

    if (PRIMVAL(op) == 0) { // exec
	static SEXP formals = NULL;
	if (formals == NULL)
	    formals = allocFormalsList2(install("expr"), install("envir"));

	args = matchArgs_NR(formals, args, call);
	args = evalListKeepMissing(args, rho);
	expr = CAR(args);
        if (expr == R_MissingArg)
	    R_MissingArgError(install("expr"), getLexicalCall(rho), "tailcallError");
	if (TYPEOF(expr) == EXPRSXP && XLENGTH(expr) == 1)
	    expr = VECTOR_ELT(expr, 0);
	if (TYPEOF(expr) != LANGSXP)
	    error("%s", _("\"expr\" must be a call expression"));
	env = CADR(args.get());
	if (env == R_MissingArg)
	    env = rho;
    }
    else { // tailcall
	/* could do argument matching here */
	if (args == R_NilValue || CAR(args) == R_MissingArg)
	    R_MissingArgError(install("FUN"), getLexicalCall(rho), "tailcallRecError");
	expr = LCONS(CAR(args), CDR(args.get()));
	env = rho;
    }

    PROTECT(expr);
    PROTECT(env);

    /* A jump should only be used if there are no on.exit expressions
       and the call is in tail position. Determining tail position
       accurately in the AST interprester would be expensive, so this
       is an approximation for now. The compiler could do a better
       job, and eventually we may want to only jump from a compiled
       call.  The JIT could be taught to always compile functions
       containing Tailcall/Exec calls. */
    bool jump_OK =
	(R_GlobalContext->conexit == R_NilValue &&
	 R_GlobalContext->callflag & CTXT_FUNCTION &&
	 R_GlobalContext->cloenv == rho &&
	 TYPEOF(R_GlobalContext->callfun) == CLOSXP &&
	 checkTailPosition(call, BODY_EXPR(R_GlobalContext->callfun), rho));

    if (jump_OK) {
	/* computing the function before the jump allows the idiom
	   Tailcall(sys.function(), ...) to be used */
	SEXP fun = CAR(expr);
	if (TYPEOF(fun) == STRSXP && XLENGTH(fun) == 1)
	    fun = installTrChar(STRING_ELT(fun, 0));
	if (TYPEOF(fun) == SYMSXP)
	    /* might need to adjust the call here as in eval() */
	    fun = findFun3(fun, env, call);
	else
	    fun = eval(fun, env);

	/* allocating a vector result could be avoided by passing expr,
	   env, and fun in some globals or on the byte code stack */
	PROTECT(fun);
	SEXP val = ListVector::create(4);
	UNPROTECT(1); /* fun */
	SET_VECTOR_ELT(val, 0, R_exec_token);
	SET_VECTOR_ELT(val, 1, expr);
	SET_VECTOR_ELT(val, 2, env);
	SET_VECTOR_ELT(val, 3, fun);

	R_jumpctxt(R_GlobalContext, CTXT_FUNCTION, val);
    }
    else {
	/**** maybe have an optional diagnostic about why no tail call? */
	SEXP val = eval(expr, rho);
	UNPROTECT(2); /* expr, rho */
	return val;
    }
#else
    error("%s", _("recompile eval.c with -DSUPPORT_TAILCALL to enable Exec and Tailcall"));
#endif
}

/* Declared with a variable number of args in names.c */
attribute_hidden SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, srcref;

    if (Promise::isA(op)) {
	op = SEXP_downcast<Promise *>(op)->force();
    }
    if (length(args) < 2) WrongArgCount("function");
    CheckFormals(CAR(args), "function");
    rval = mkCLOSXP(CAR(args), CADR(args), rho);
    srcref = CADDR(args);
    if (!isNull(srcref)) setAttrib(rval, R_SrcrefSymbol, srcref);
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the expression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (eval(x$a[3])	eval(x$a)  eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static SEXP evalseq(SEXP expr_, SEXP rho, int forcelocal,  R_varloc_t tmploc,
		    R_varloc_t *ploc)
{
    GCStackRoot<> val, nval, nexpr;
    GCStackRoot<> expr(expr_);
    if (isNull(expr))
	error("%s", _("invalid (NULL) left side of assignment"));
    if (isSymbol(expr)) { /* now we are down to the target symbol */
	if (forcelocal) {
	    nval = EnsureLocal(expr, rho, ploc);
	}
	else {
	    nval = eval(expr, ENCLOS(rho));
	    *ploc = R_findVarLoc(expr, ENCLOS(rho));
	}
	int maybe_in_assign = ploc->cell ?
	    ASSIGNMENT_PENDING(ploc->cell) : false;
	if (ploc->cell)
	    SET_ASSIGNMENT_PENDING(ploc->cell, true);
	if (maybe_in_assign || MAYBE_SHARED(nval))
	    nval = shallow_duplicate(nval);
	return CONS_NR(nval, expr);
    }
    else if (isLanguage(expr)) {
	val = evalseq(CADR(expr.get()), rho, forcelocal, tmploc, ploc);
	R_SetVarLocValue(tmploc, CAR(val));
	nexpr = LCONS(R_GetVarLocSymbol(tmploc), CDDR(expr.get()));
	nexpr = LCONS(CAR(expr), nexpr);
	nval = eval(nexpr, rho);
	/* duplicate nval if it might be shared _or_ if the container,
	   CAR(val), has become possibly shared by going through a
	   closure.  This is taken to indicate that the corresponding
	   replacement function might be a closure and will need to
	   see an unmodified LHS value. This heuristic fails if the
	   accessor function called here is not a closure but the
	   replacement function is. */
	if (MAYBE_REFERENCED(nval) &&
	    (MAYBE_SHARED(nval) || MAYBE_SHARED(CAR(val))))
	    nval = shallow_duplicate(nval);

	return CONS_NR(nval, val);
    }
    else error("%s", _("target of assignment expands to non-language object"));
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

#define NUM_ASYM 4
static constexpr std::array<const char *, NUM_ASYM> asym = {":=", "<-", "<<-", "="};
static std::array<SEXP, NUM_ASYM> asymSymbol;

static SEXP R_ReplaceFunsTable = NULL;
static SEXP R_SubsetSym = NULL;
static SEXP R_SubassignSym = NULL;
static SEXP R_Subset2Sym = NULL;
static SEXP R_Subassign2Sym = NULL;
static SEXP R_DollarGetsSymbol = NULL;
static SEXP R_AssignSym = NULL;

attribute_hidden void R::R_initEvalSymbols(void)
{
    for (unsigned int i = 0; i < asymSymbol.size(); i++)
	asymSymbol[i] = install(asym[i]);

    R_ReplaceFunsTable = R_NewHashedEnv(R_EmptyEnv, 1099);
    R_PreserveObject(R_ReplaceFunsTable);

    R_SubsetSym = install("[");
    R_SubassignSym = install("[<-");
    R_Subset2Sym = install("[[");
    R_Subassign2Sym = install("[[<-");
    R_DollarGetsSymbol = install("$<-");
    R_valueSym = install("value");
    R_AssignSym = install("<-");

#ifdef SUPPORT_TAILCALL
    R_exec_token = CONS(install(".__EXEC__."), R_NilValue);
    R_PreserveObject(R_exec_token);
#endif
}

static R_INLINE SEXP lookupAssignFcnSymbol(SEXP fun)
{
    return R_findVarInFrame(R_ReplaceFunsTable, fun);
}

static void enterAssignFcnSymbol(SEXP fun, SEXP val)
{
    defineVar(fun, val, R_ReplaceFunsTable);
}

/* This macro stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
#define SET_TEMPVARLOC_FROM_CAR(loc, lhs) do { \
	SEXP __lhs__ = (lhs); \
	SEXP __v__ = CAR(__lhs__); \
	if (MAYBE_SHARED(__v__)) { \
	    __v__ = shallow_duplicate(__v__); \
	    ENSURE_NAMED(__v__); \
	    SETCAR(__lhs__, __v__); \
	} \
	R_SetVarLocValue(loc, __v__); \
    } while(0)

/* This macro makes sure the RHS NAMED value is 0 or NAMEDMAX. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__)) \
	    ENSURE_NAMEDMAX(__rhs__); \
    } while (0)

#define ASSIGNBUFSIZ 32
static SEXP installAssignFcnSymbol(SEXP fun)
{
    char buf[ASSIGNBUFSIZ];

    /* install the symbol */
    if (strlen(CHAR(PRINTNAME(fun))) + 3 > ASSIGNBUFSIZ)
	error(_("overlong name in '%s'"), EncodeChar(PRINTNAME(fun)));
    snprintf(buf, ASSIGNBUFSIZ, "%s<-", CHAR(PRINTNAME(fun)));
    SEXP val = install(buf);

    enterAssignFcnSymbol(fun, val);
    return val;
}

static R_INLINE SEXP getAssignFcnSymbol(SEXP fun)
{
    /* handle [<-, [[<-, and $<- efficiently */
    if (fun == R_SubsetSym)
	return R_SubassignSym;
    else if (fun == R_Subset2Sym)
	return R_Subassign2Sym;
    else if (fun == R_DollarSymbol)
	return R_DollarGetsSymbol;

    /* look up in the replacement functions table */
    SEXP val = lookupAssignFcnSymbol(fun);
    if (val != R_UnboundValue)
	return val;

    /* instal symbol, entern in table,  and return */
    return installAssignFcnSymbol(fun);
}

static R_INLINE SEXP mkRHSPROMISE(SEXP expr, SEXP rhs)
{
    return R_mkEVPROMISE_NR(expr, rhs);
}

static SEXP GET_BINDING_CELL(SEXP, SEXP);
static SEXP BINDING_VALUE(SEXP);

static R_INLINE SEXP try_assign_unwrap(SEXP value, SEXP sym, SEXP rho, SEXP cell)
{
    /* If EnsureLocal() has introduced a wrapper for the LHS object in
       a complex assignment and the data has been duplicated, then it
       may be possible to remove the wrapper before assigning the
       final value to a its symbol. */
    if (value && !MAYBE_REFERENCED(value))
	/* Typical case for NAMED; can also happen for REFCNT. */
	return R_tryUnwrap(value);
#ifdef SWITCH_TO_REFCNT
    else {
	/* Typical case for REFCNT; might not be safe to unwrap for NAMED. */
	if (value && !MAYBE_SHARED(value)) {
	    if (cell == NULL)  /* for AST; byte code has the binding */
		cell = GET_BINDING_CELL(sym, rho);
	    /* Ruling out active bindings may not be necessary at this
	       point, but just to be safe ... */
	    if (cell && !IS_ACTIVE_BINDING(cell) &&
		value == BINDING_VALUE(cell))
		return R_tryUnwrap(value);
	}
    }
#endif
    return value;
}

static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> expr, lhs, rhs, saverhs, tmp, afun, rhsprom;
    R_varloc_t tmploc;

    expr = CAR(args);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    R_bcstack_t *old_bcprot_top = R_BCProtTop;
    INCREMENT_BCSTACK_LINKS();
    INCLNK_stack_commit();

    saverhs = rhs = eval(CADR(args), rho);
#ifdef SWITCH_TO_REFCNT
    int refrhs = MAYBE_REFERENCED(saverhs);
    if (refrhs) INCREMENT_REFCNT(saverhs);
#endif

    /*  FIXME: We need to ensure that this works for hashed
	environments.  This code only works for unhashed ones.  the
	syntax error here is a deliberate marker so I don't forget that
	this needs to be done.  The code used in "missing" will help
	here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we include the special case here?  */

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  We need to protect
	the location in case the biding is removed from its
	environment by user code or an assignment within the
	assignment arguments */

    /*  There are two issues with the approach here:

	    A complex assignment within a complex assignment, like
	    f(x, y[] <- 1) <- 3, can cause the value temporary
	    variable for the outer assignment to be overwritten and
	    then removed by the inner one.  This could be addressed by
	    using multiple temporaries or using a promise for this
	    variable as is done for the RHS.  Printing of the
	    replacement function call in error messages might then need
	    to be adjusted.

	    With assignments of the form f(g(x, z), y) <- w the value
	    of 'z' will be computed twice, once for a call to g(x, z)
	    and once for the call to the replacement function g<-.  It
	    might be possible to address this by using promises.
	    Using more temporaries would not work as it would mess up
	    replacement functions that use substitute and/or
	    nonstandard evaluation (and there are packages that do
	    that -- igraph is one).

	    LT */

    FIXUP_RHS_NAMED(rhs);

    if (rho == R_BaseNamespace)
	errorcall(call, "%s", _("cannot do complex assignments in base namespace"));
    if (rho == R_BaseEnv)
	errorcall(call, "%s", _("cannot do complex assignments in base environment"));

    tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol);
    GCStackRoot<> lcell(tmploc.cell);
    GCStackRoot<> oldTmpval;
    if (tmploc.cell == NULL) {
	defineVar(R_TmpvalSymbol, R_NilValue, rho);
	tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol);
	DECREMENT_REFCNT(CDR(tmploc.cell));
	DISABLE_REFCNT(tmploc.cell);
    }
    else {
	oldTmpval = R_GetVarLocValue(tmploc);
	if (BINDING_IS_LOCKED(tmploc.cell))
	    error("%s", _("existing `*tmp*` binding is locked"));
	if (IS_ACTIVE_BINDING(tmploc.cell))
	    error("%s", _("existing `*tmp*` binding is an active binding"));
    }

    /* Now set up a context to remove it when we are done, even in the
     * case of an error.  This all helps error() provide a better call.
     */
    {
    RCNTXT cntxt(CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);

    try {
    /*  Do a partial evaluation down through the LHS. */
    R_varloc_t lhsloc;
    lhs = evalseq(CADR(expr.get()), rho,
		  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc, &lhsloc);
    if (lhsloc.cell == NULL)
	lhsloc.cell = R_NilValue;
    GCStackRoot<> lcell2(lhsloc.cell);

    rhsprom = mkRHSPROMISE(CADR(args), rhs);

    while (isLanguage(CADR(expr.get()))) {
	if (TYPEOF(CAR(expr)) == SYMSXP)
	    tmp = getAssignFcnSymbol(CAR(expr));
	else {
	    /* check for and handle assignments of the form
	       foo::bar(x) <- y or foo:::bar(x) <- y */
	    tmp = R_NilValue; /* avoid uninitialized variable warnings */
	    if (TYPEOF(CAR(expr)) == LANGSXP &&
		(CAR(CAR(expr)) == R_DoubleColonSymbol ||
		 CAR(CAR(expr)) == R_TripleColonSymbol) &&
		length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
		tmp = getAssignFcnSymbol(CADDR(CAR(expr)));
		tmp = lang3(CAAR(expr), CADR(CAR(expr)), tmp);
	    }
	    else
		error("%s", _("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr.get()), rhsprom);
	rhs = eval(rhs, rho);
	SET_PRVALUE(rhsprom, rhs);
	SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	lhs = CDR(lhs.get());
	expr = CADR(expr.get());
    }

    if (TYPEOF(CAR(expr)) == SYMSXP)
	afun = getAssignFcnSymbol(CAR(expr));
    else {
	/* check for and handle assignments of the form
	   foo::bar(x) <- y or foo:::bar(x) <- y */
	afun = R_NilValue; /* avoid uninitialized variable warnings */
	if (TYPEOF(CAR(expr)) == LANGSXP &&
	    (CAR(CAR(expr)) == R_DoubleColonSymbol ||
	     CAR(CAR(expr)) == R_TripleColonSymbol) &&
	    length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
	    afun = getAssignFcnSymbol(CADDR(CAR(expr)));
	    afun = lang3(CAAR(expr), CADR(CAR(expr)), afun);
	}
	else
	    error("%s", _("invalid function in complex assignment"));
    }
    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
    SEXP lhsSym = CDR(lhs.get());

    expr = replaceCall(afun, R_TmpvalSymbol, CDDR(expr.get()), rhsprom);
    GCStackRoot<> value;
    value = eval(expr, rho);

    SET_ASSIGNMENT_PENDING(lhsloc.cell, false);
    if (PRIMVAL(op) == 2)                       /* <<- */
	setVar(lhsSym, value, ENCLOS(rho));
    else {                                      /* <-, = */
	if (ALTREP(value)) {
	    value = try_assign_unwrap(value, lhsSym, rho, NULL);
	}
	defineVar(lhsSym, value, rho);
    }
    INCREMENT_NAMED(value);
    Evaluator::enableResultPrinting(false);
    }
    catch (JMPException &e) {
        unbindVar(R_TmpvalSymbol, rho);
        if (e.context() != &cntxt)
            throw;
    }
    endcontext(&cntxt); /* which does not run the remove */
    }
    if (oldTmpval == NULL)
	unbindVar(R_TmpvalSymbol, rho);
    else
	R_SetVarLocValue(tmploc, oldTmpval);
#ifdef OLD_RHS_NAMED
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = NAMEDMAX */
    ENSURE_NAMEDMAX(saverhs);
#else
    INCREMENT_NAMED(saverhs);
#endif
#ifdef SWITCH_TO_REFCNT
    if (refrhs) DECREMENT_REFCNT(saverhs);
#endif

    DECREMENT_BCSTACK_LINKS(old_bcprot_top);

    return saverhs;
}

/*  Assignment in its various forms  */

attribute_hidden SEXP do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lhs, rhs;

    if (args == R_NilValue ||
	CDR(args) == R_NilValue ||
	CDDR(args) != R_NilValue)
	WrongArgCount(asym[PRIMVAL(op)]);

    lhs = CAR(args);

    switch (TYPEOF(lhs)) {
    case STRSXP:
	lhs = installTrChar(STRING_ELT(lhs, 0));
	/* fall through */
    case SYMSXP:
	rhs = eval(CADR(args), rho);
	INCREMENT_NAMED(rhs);
	if (PRIMVAL(op) == 2)                       /* <<- */
	    setVar(lhs, rhs, ENCLOS(rho));
	else                                        /* <-, = */
	    defineVar(lhs, rhs, rho);
	Evaluator::enableResultPrinting(false);
	return rhs;
    case LANGSXP:
	Evaluator::enableResultPrinting(false);
	return applydefine(call, op, args, rho);
    default:
	errorcall(call, "%s", _("invalid (do_set) left-hand side to assignment"));
    }

    return R_NilValue;/*NOTREACHED*/
}


/* Evaluate each expression in "el" in the environment "rho".  This is
   a naturally recursive algorithm, but we use the iterative form below
   because it is does not cause growth of the pointer protection stack,
   and because it is a little more efficient.
*/

#define COPY_TAG(to, from) do { \
  SEXP __tag__ = (TAG)(from); \
  if (__tag__ != R_NilValue) SET_TAG(to, __tag__); \
} while (0)

/* Used in eval and applyMethod (object.c) for builtin primitives,
   do_internal (names.c) for builtin .Internals
   and in evalArgs.

   'n' is the number of arguments already evaluated and hence not
   passed to evalArgs and hence to here.
 */
attribute_hidden SEXP R::evalList(SEXP el, SEXP rho, SEXP call, int n)
{
    SEXP tail, ev, val;
    GCStackRoot<> head, h;
    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {
	n++;

	if (CAR(el) == R_DotsSymbol) {
	    /* If we have a ... symbol, we look to see what it is bound to.
	     * If its binding is Null (i.e. zero length)
	     *	we just ignore it and return the cdr with all its expressions evaluated;
	     * if it is bound to a ... list of promises,
	     *	we force all the promises and then splice
	     *	the list of resulting values into the return value.
	     * Anything else bound to a ... symbol is an error
	     */
	    h = R_findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    val = eval(CAR(h), rho);
		    INCREMENT_LINKS(val);
		    ev = CONS_NR(val, R_NilValue);
		    if (head == R_NilValue) {
			head = ev;
		    }
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h.get());
		}
	    }
	    else if (h != R_MissingArg)
		error("%s", _("'...' used in an incorrect context"));
	} else if (CAR(el) == R_MissingArg) {
	    /* It was an empty element: most likely get here from evalArgs
	       which may have been called on part of the args. */
	    errorcall(call, _("argument %d is empty"), n);
#ifdef CHECK_IS_MISSING_IN_evalList
	    /* Radford Neal drops this R_isMissing check in pqR in
	       03-zap-isMissing (but it seems to creep in again later
	       with helper thread stuff?)  as it takes quite a bit of
	       time (essentially the equivalent of evaluating the
	       symbol, but maybe not as efficiently as eval) and only
	       serves to change the error message, not always for the
	       better. Also, the byte code interpreter does not do
	       this, so dropping this makes compiled and interpreted
	       code more consistent. */
	} else if (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)) {
	    /* It was missing */
	    errorcall_cpy(call,
	                  _("'%s' is missing"),
	                  EncodeChar(PRINTNAME(CAR(el))));
#endif
	} else {
	    val = eval(CAR(el), rho);
	    INCREMENT_LINKS(val);
	    ev = CONS_NR(val, R_NilValue);
	    if (head == R_NilValue)
		head = ev;
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    for (el = head; el != R_NilValue; el = CDR(el))
	DECREMENT_LINKS(CAR(el));

    return head;

} /* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */

/* used in evalArgs, arithmetic.c, seq.c */
attribute_hidden SEXP R::evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP tail, ev, val;
    GCStackRoot<> head, h;
    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = R_findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			val = R_MissingArg;
		    else
			val = eval(CAR(h), rho);
		    INCREMENT_LINKS(val);
		    ev = CONS_NR(val, R_NilValue);
		    if (head == R_NilValue) {
			head = ev;
		    } else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h.get());
		}
	    }
	    else if (h != R_MissingArg)
		error("%s", _("'...' used in an incorrect context"));
	}
	else {
	    if (CAR(el) == R_MissingArg ||
		(isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)))
		val = R_MissingArg;
	    else
		val = eval(CAR(el), rho);
	    INCREMENT_LINKS(val);
	    ev = CONS_NR(val, R_NilValue);
	    if (head==R_NilValue)
		head = ev;
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    for (el = head; el != R_NilValue; el = CDR(el))
	DECREMENT_LINKS(CAR(el));

    return head;
}


/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

attribute_hidden SEXP R::promiseArgs(SEXP el, SEXP rho)
{
    SEXP tail;
    GCStackRoot<> ans, h;
    ans = tail = CONS(R_NilValue, R_NilValue);

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* double promises are needed to make sure a function argument
	   passed via ... is marked as referenced in the caller and
	   the callee */

	if (CAR(el) == R_DotsSymbol) {
	    h = R_findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
		      SETCDR(tail, CONS(CAR(h), R_NilValue));
                    else
		      SETCDR(tail, CONS(mkPROMISE(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    COPY_TAG(tail, h);
		    h = CDR(h.get());
		}
	    }
	    else if (h != R_MissingArg)
		error("%s", _("'...' used in an incorrect context"));
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	else {
	    SETCDR(tail, CONS(mkPROMISE(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	el = CDR(el);
    }
    ans = CDR(ans.get());
    DECREMENT_REFCNT(ans);
    return ans;
}


/* Check that each formal is a symbol */

/* used in coerce.c */
attribute_hidden void R::CheckFormals(SEXP ls, const char *name)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		error(_("invalid formal argument list for \"%s\""), name);
	return;
    }
    error(_("invalid formal argument list for \"%s\""), name);
}


static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int len = 0;
    CXXR::RAllocStack::Scope rscope;

    PROTECT(x);
    PROTECT(xnames = getAttrib(x, R_NamesSymbol)); /* isn't this protected via x? */
    bool named = (xnames != R_NilValue);
    if (named)
	for (R_len_t i = 0; i < length(x); i++)
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') len++;

    if (len) {
	PROTECT(xnew = allocList(len));
	xptr = xnew;
	for (R_len_t i = 0; i < length(x); i++) {
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') {
		SETCAR(xptr, VECTOR_ELT(x, i));
		SET_TAG(xptr, installTrChar(STRING_ELT(xnames, i)));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = allocList(0);
    UNPROTECT(2);

    return xnew;
}

/* "eval": Evaluate the first argument
   in the environment specified by the second argument. */

attribute_hidden SEXP do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x;
    GCStackRoot<> tmp;
    GCStackRoot<> expr;
    GCStackRoot<> env;

    int frame;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    if (isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if (!isEnvironment(encl) &&
		!isEnvironment((encl = simple_as_environment(encl, true)))) {
	error(_("invalid '%s' argument of type '%s'"),
	      "enclos", R_typeToChar(encl));
    }
    if (IS_S4_OBJECT(env) && (TYPEOF(env) == OBJSXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* so eval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = NewEnvironment(R_NilValue, duplicate(CADR(args)), encl);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(CADR(args));
	for (SEXP xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	    ENSURE_NAMEDMAX(CAR(xptr));
	env = NewEnvironment(R_NilValue, x, encl);
	break;
    case INTSXP:
    case REALSXP:
	if (length(env) != 1)
	    error("%s", _("numeric 'envir' arg not of length one"));
	frame = asInteger(env);
	if (frame == NA_INTEGER)
	    error(_("invalid '%s' argument of type '%s'"),
		  "envir", R_typeToChar(env));
	env = R_sysframe(frame, R_GlobalContext);
	break;
    default:
	error(_("invalid '%s' argument of type '%s'"),
	      "envir", R_typeToChar(env));
    }

    /* isLanguage include NILSXP, and that does not need to be
       evaluated
    if (isLanguage(expr) || isSymbol(expr) || isByteCode(expr)) { */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP || isByteCode(expr))
    {
    RCNTXT cntxt(CTXT_RETURN, R_GlobalContext->call,
	             env, rho, args, op);
    try
    {
        expr = eval(expr, env);
    }
    catch (JMPException &e)
    {
        if (e.context() != &cntxt)
            throw;
        expr = e.value();
    }
    endcontext(&cntxt);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	SEXP srcrefs = getBlockSrcrefs(expr);
	tmp = R_NilValue;
    {
    RCNTXT cntxt(CTXT_RETURN, R_GlobalContext->call,
	             env, rho, args, op);
    try
    {
        int n = LENGTH(expr);
        for (int i = 0; i < n; i++)
        {
            R_Srcref = getSrcref(srcrefs, i);
            tmp = eval(XVECTOR_ELT(expr, i), env);
        }
    }
    catch (JMPException &e)
    {
        if (e.context() != &cntxt)
            throw;
        tmp = e.value();
    }
    endcontext(&cntxt);
    }
	expr = tmp;
    }
    else if (Promise::isA(expr)) {
	expr = Evaluator::evaluate(expr, rho);
    } /* else expr is returned unchanged */
    return expr;
}

/* This is a special .Internal */
attribute_hidden SEXP do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    checkArity(op, args);
    x = CAR(args);
    x = eval(x, rho);
    PROTECT(x);
    PROTECT(ret = ListVector::create(2));
    PROTECT(nm = StringVector::create(2));
    SET_STRING_ELT(nm, 0, mkChar("value"));
    SET_STRING_ELT(nm, 1, mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, ScalarLogical(Evaluator::resultPrinted()));
    setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
attribute_hidden SEXP do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> s;
    SEXP ans;
    RCNTXT *cptr = R_GlobalContext;
    /* get the args supplied */
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr != NULL) {
	args = cptr->promargs;
    }
    /* get the env recall was called from */
    s = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == s)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error("%s", _("'Recall' called from outside a closure"));

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->callfun != R_NilValue)
	s = cptr->callfun;
    else if (TYPEOF(CAR(cptr->call)) == SYMSXP)
	s = findFun(CAR(cptr->call), cptr->sysparent);
    else
	s = eval(CAR(cptr->call), cptr->sysparent);
    if (TYPEOF(s) != CLOSXP)
	error("%s", _("'Recall' called from outside a closure"));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_NilValue, true);

    return ans;
}


static SEXP evalArgs(SEXP el, SEXP rho, int dropmissing, SEXP call, int n)
{
    if (dropmissing) return evalList(el, rho, call, n);
    else return evalListKeepMissing(el, rho);
}

/* A version of DispatchOrEval that checks for possible S4 methods for
 * any argument, not just the first.  Used in the code for `c()` in do_c()
 * and previously used in the code for `[` in do_subset.
 * Differs in that all arguments are evaluated
 * immediately, rather than after the call to R_possible_dispatch.
 */
attribute_hidden
bool R::DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		      SEXP rho, SEXP *ans, bool dropmissing, bool argsevald)
{
    std::pair<bool, RObject *> result = R::DispatchAnyOrEval(call, op, generic, args, rho, dropmissing, argsevald);

    *ans = result.second;
    return result.first;
}

attribute_hidden
std::pair<bool, RObject *> R::DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		      SEXP rho, bool dropmissing, bool argsevald)
{
    if (R_has_methods(op)) {
	GCStackRoot<> argValue;

	/* Rboolean hasS4 = FALSE; */
	if (!argsevald) {
	    argValue = evalArgs(args, rho, dropmissing, call, 0);
	    argsevald = TRUE;
	}
	else argValue = args;
	for (SEXP el = argValue; el != R_NilValue; el = CDR(el)) {
	    if (IS_S4_OBJECT(CAR(el))) {
		auto value = R_possible_dispatch(call, op, argValue, rho, true);
		if (value.first) {
		    return value;
		}
		else break;
	    }
	}
	 /* else, use the regular DispatchOrEval, but now with evaluated args */
	return DispatchOrEval(call, op, generic, argValue, rho, dropmissing, argsevald);
    }
    return DispatchOrEval(call, op, generic, args, rho, dropmissing, argsevald);
}


/* DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns them so that
 * the generic built-in C code can continue.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 */
attribute_hidden
bool R::DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, SEXP *ans, bool dropmissing, bool argsevald)
{
    std::pair<bool, RObject *> result = R::DispatchOrEval(call, op, generic, args, rho, dropmissing, argsevald);

    *ans = result.second;
    return result.first;
}

attribute_hidden
std::pair<bool, RObject *>
R::DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, bool dropmissing, bool argsevald)
{
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    GCStackRoot<> x(R_NilValue);
    bool dots = false;

    if (argsevald)
	{ x = CAR(args); }
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = R_findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error("%s", _("value in '...' is not a promise"));
#endif
		    dots = true;
		    x = eval(CAR(h), rho);
		    break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    error("%s", _("'...' used in an incorrect context"));
	    }
	    else {
		dots = false;
		x = eval(CAR(args), rho);
		break;
	    }
	}
    }
	/* try to dispatch on the object */
    if (isObject(x)) {
	char *pt;
	/* Try for formal method. */
	if (IS_S4_OBJECT(x) && R_has_methods(op)) {
	    GCStackRoot<> argValue;
	    /* create a promise to pass down to applyClosure  */
	    if (!argsevald) {
		argValue = promiseArgs(args, rho);
		IF_PROMSXP_SET_PRVALUE(CAR(argValue), x);
	    } else argValue = args;
	    /* This means S4 dispatch */
	    auto value = R_possible_dispatch(call, op, argValue, rho, true);
	    if (value.first) {
		return value;
	    }
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    argValue = evalArgs(argValue, rho, dropmissing,
						call, 0);
		else {
		    argValue = CONS_NR(x, evalArgs(CDR(argValue.get()), rho,
							   dropmissing, call, 1));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		args = argValue;
		argsevald = 1;
	    }
	}
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = Rf_strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || !streql(pt,".default")) {
	    GCStackRoot<> pargs;
	    pargs = promiseArgs(args, rho);
	    /* The context set up here is needed because of the way
	       usemethod() is written.  DispatchGroup() repeats some
	       internal usemethod() code and avoids the need for a
	       context; perhaps the usemethod() code should be
	       refactored so the contexts around the usemethod() calls
	       in this file can be removed.

	       Using rho for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other usemethod() uses below a
	       new environment rho1 is created and used.  LT */
	    GCStackRoot<> rho1;
	    rho1 = NewEnvironment(R_NilValue, R_NilValue, rho);
	    IF_PROMSXP_SET_PRVALUE(CAR(pargs), x);
	    std::pair<bool, RObject *> dispatched{false, R_NilValue};
	    {
	    RCNTXT cntxt(CTXT_RETURN, call, rho1, rho, pargs, op);
	    dispatched = usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv);
	    endcontext(&cntxt);
	    }
	    if (dispatched.first)
	    {
#ifdef ADJUST_ENVIR_REFCNTS
		R_CleanupEnvir(rho1, dispatched.second);
		unpromiseArgs(pargs);
#endif
		return dispatched;
	    }
#ifdef ADJUST_ENVIR_REFCNTS
	    R_CleanupEnvir(rho1, R_NilValue);
	    unpromiseArgs(pargs);
#endif
	}
    }
    if (!argsevald) {
	if (dots) {
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    SEXP ans = evalArgs(args, rho, dropmissing, call, 0);
	    return std::make_pair(false, ans);
	} else {
	    INCREMENT_LINKS(x);
	    GCStackRoot<> ans;
	    ans = CONS_NR(x, evalArgs(CDR(args), rho, dropmissing, call, 1));
	    DECREMENT_LINKS(x);
	    SET_TAG(ans, CreateTag(TAG(args)));
	    return std::make_pair(false, ans);
	}
    }
    return std::make_pair(false, args);
}

static R_INLINE void updateObjFromS4Slot(SEXP objSlot, const char *className) {
    SEXP obj = CAR(objSlot);

    if (IS_S4_OBJECT(obj) && isBasicClass(className)) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
	if (NAMED(obj)) ENSURE_NAMEDMAX(obj);
	obj = R_getS4DataSlot(obj, OBJSXP); /* the .S3Class obj. or NULL*/
	if (obj != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(objSlot, obj);
    }
}

/* gr needs to be protected on return from this function */
static void findmethod(SEXP Class, const char *group, const char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       SEXP objSlot, SEXP rho)
{
    int len, whichclass;
    CXXR::RAllocStack::Scope rscope;

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	*meth = installS3Signature(generic, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (FunctionBase::isA(*sxp)) {
	    *gr = R_BlankScalarString;
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
	*meth = installS3Signature(group, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (FunctionBase::isA(*sxp)) {
	    *gr = mkString(group);
	    if (whichclass > 0) updateObjFromS4Slot(objSlot, ss);
	    break;
	}
    }
    *which = whichclass;
}

static SEXP classForGroupDispatch(SEXP obj) {

    return IS_S4_OBJECT(obj) ? R_data_class2(obj)
	    : getAttrib(obj, R_ClassSymbol);
}

static bool R_chooseOpsMethod(SEXP x, SEXP y, SEXP mx, SEXP my,
				  SEXP call, bool rev, SEXP rho) {
    static SEXP expr = NULL;
    static SEXP xSym = NULL;
    static SEXP ySym = NULL;
    static SEXP mxSym = NULL;
    static SEXP mySym = NULL;
    static SEXP clSym = NULL;
    static SEXP revSym = NULL;
    if (expr == NULL) {
	xSym = install("x");
	ySym = install("y");
	mxSym = install("mx");
	mySym = install("my");
	clSym = install("cl");
	revSym = install("rev");
	expr = R_ParseString("base::chooseOpsMethod(x, y, mx, my, cl, rev)");
	R_PreserveObject(expr);
    }
    
    GCStackRoot<> newrho;
    newrho = R_NewEnv(rho, FALSE, 0);
    defineVar(xSym, x, newrho); INCREMENT_NAMED(x);
    defineVar(ySym, y, newrho); INCREMENT_NAMED(y);
    defineVar(mxSym, mx, newrho); INCREMENT_NAMED(mx);
    defineVar(mySym, my, newrho); INCREMENT_NAMED(my);
    defineVar(clSym, call, newrho); INCREMENT_NAMED(call);
    defineVar(revSym, ScalarLogical(rev), newrho);

    SEXP ans = eval(expr, newrho);
#ifdef ADJUST_ENVIR_REFCNTS
    R_CleanupEnvir(newrho, R_NilValue);
#endif

    return ans == R_NilValue ? false : asRbool(ans, call);
}

attribute_hidden
bool R::DispatchGroup(const char *group, SEXP call, SEXP op, SEXP args, SEXP rho,
		  SEXP *ans)
{
    std::pair<bool, RObject *> result = R::DispatchGroup(group, call, op, args, rho);

    if (result.first)
        *ans = result.second;
    return result.first;
}

attribute_hidden
std::pair<bool, RObject *> R::DispatchGroup(const char *group, SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (args != R_NilValue && !isObject(CAR(args)) &&
	(CDR(args) == R_NilValue || !isObject(CADR(args))))
	return std::pair<bool, RObject *>(false, nullptr);

    bool isOps = (streql(group, "Ops") || streql(group, "matrixOps"));

    /* try for formal method */
    if (length(args) == 1 && !IS_S4_OBJECT(CAR(args))) {
	// no S4
    } else if (length(args) == 2 && !IS_S4_OBJECT(CAR(args)) && !IS_S4_OBJECT(CADR(args))) {
	// no S4
    } else { // try to use S4 :
	/* Remove argument names to ensure positional matching */
	if (isOps)
	    for (SEXP s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);

	if (R_has_methods(op)) {
	   auto dispatched = R_possible_dispatch(call, op, args, rho, false);
	   if (dispatched.first) {
	       return dispatched;
	   }
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if (isSymbol(CAR(call))) {
	const char *cstr = strchr(CHAR(PRINTNAME(CAR(call))), '.');
	if (cstr && streql(cstr + 1, "default"))
	    return std::pair<bool, RObject *>(false, nullptr);
    }

    int nargs = isOps ? length(args) : 1;

    if (nargs == 1 && !isObject(CAR(args)))
	return std::pair<bool, RObject *>(false, nullptr);

    const char *generic = PRIMNAME(op);
    SEXP lclass = PROTECT(classForGroupDispatch(CAR(args))), rclass;
    if (nargs == 2)
	rclass = classForGroupDispatch(CADR(args));
    else
	rclass = R_NilValue;
    PROTECT(rclass);

    SEXP lmeth = R_NilValue, lsxp = R_NilValue, lgr = R_NilValue,
	 rmeth = R_NilValue, rsxp = R_NilValue, rgr = R_NilValue;
    int lwhich, rwhich;
    findmethod(lclass, group, generic,
	       &lsxp, &lgr, &lmeth, &lwhich, args, rho);
    PROTECT(lgr);

    if (nargs == 2)
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth,
		   &rwhich, CDR(args), rho);
    else
	rwhich = 0;
    PROTECT(rgr);

    if (!FunctionBase::isA(lsxp) && !FunctionBase::isA(rsxp)) {
	UNPROTECT(4);
	return std::pair<bool, RObject *>(false, nullptr); /* no generic or group method so use default */
    }

    if (lsxp != rsxp) {
	if (FunctionBase::isA(lsxp) && FunctionBase::isA(rsxp)) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if (streql(rname, "Ops.difftime") &&
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")))
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") &&
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;

	    /* Strict comparison, the docs requires methods to be "the same":
	      16 to take environments into account
	     1+2 for bitwise comparison of numbers
	       4 for the same order of attributes
	         bytecode ignored (can change at runtime)
	         srcref ignored (as per default)
	    */
	    else if (!R_compute_identical(lsxp, rsxp, 16 + 1 + 2 + 4)) {
		SEXP x = CAR(args), y = CADR(args);
		if (R_chooseOpsMethod(x, y, lsxp, rsxp, call, false, rho)) {
		    rsxp = R_NilValue;
		}
		else if (R_chooseOpsMethod(y, x, rsxp, lsxp, call, true, rho)) {
		    lsxp = R_NilValue;
		}
		else {
		    warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			    lname, rname, generic);
		    UNPROTECT(4);
		    return std::pair<bool, RObject *>(false, nullptr);
		}
	    }
	}
	/* if the right hand side is the one */
	if (!FunctionBase::isA(lsxp)) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
	}
    }

    /* we either have a group method or a class method */

    CXXR::RAllocStack::Scope rscope;
    SEXP s = args;
    const char *dispatchClassName = translateChar(STRING_ELT(lclass, lwhich));

    SEXP t, m = PROTECT(StringVector::create(nargs));
    for (int i = 0 ; i < nargs ; i++) {
	t = classForGroupDispatch(CAR(s));
	if (isString(t) && (stringPositionTr(t, dispatchClassName) >= 0))
	    SET_STRING_ELT(m, i, PRINTNAME(lmeth));
	else
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
    }

    SEXP newvars = PROTECT(createS3Vars(
	PROTECT(mkString(generic)),
	lgr,
	PROTECT(stringSuffix(lclass, lwhich)),
	m,
	rho,
	R_BaseEnv
    ));

    PROTECT(t = LCONS(lmeth, CDR(call)));

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */

    PROTECT(s = promiseArgs(CDR(call), rho));
    if (length(s) != length(args))
	error("%s", _("dispatch error in group dispatch"));
    for (m = s ; m != R_NilValue ; m = CDR(m), args = CDR(args) ) {
	IF_PROMSXP_SET_PRVALUE(CAR(m), CAR(args));
	/* ensure positional matching for operators */
	if (isOps) SET_TAG(m, R_NilValue);
    }

    SEXP ans = applyClosure(t, lsxp, s, rho, newvars, true);
    UNPROTECT(10);
    return std::make_pair(true, ans);
}

/* start of bytecode section */
static int R_bcVersion = 12;
static int R_bcMinVersion = 9;

static SEXP R_AddSym = NULL;
static SEXP R_SubSym = NULL;
static SEXP R_MulSym = NULL;
static SEXP R_DivSym = NULL;
static SEXP R_ExptSym = NULL;
static SEXP R_SqrtSym = NULL;
static SEXP R_ExpSym = NULL;
static SEXP R_EqSym = NULL;
static SEXP R_NeSym = NULL;
static SEXP R_LtSym = NULL;
static SEXP R_LeSym = NULL;
static SEXP R_GeSym = NULL;
static SEXP R_GtSym = NULL;
static SEXP R_AndSym = NULL;
static SEXP R_OrSym = NULL;
static SEXP R_NotSym = NULL;
static SEXP R_CSym = NULL;
static SEXP R_LogSym = NULL;
static SEXP R_DotInternalSym = NULL;
static SEXP R_DotExternalSym = NULL;
static SEXP R_DotExternal2Sym = NULL;
static SEXP R_DotExternalgraphicsSym = NULL;
static SEXP R_DotCallSym = NULL;
static SEXP R_DotCallgraphicsSym = NULL;
static SEXP R_DotFortranSym = NULL;
static SEXP R_DotCSym = NULL;

/* R_ConstantsRegistry allows runtime detection of modification of compiler
   constants. It is a linked list of weak references. Each weak reference
   refers to a byte-code object (BCODESXPs) as key and to a deep copy of the
   object's constants as value. The head of the list has a nil payload
   instead of a weak reference, stays in the list forever, and is a GC root.*/
static SEXP R_ConstantsRegistry = NULL;

attribute_hidden
void R::R_initialize_bcode(void)
{
  R_AddSym = install("+");
  R_SubSym = install("-");
  R_MulSym = install("*");
  R_DivSym = install("/");
  R_ExptSym = install("^");
  R_SqrtSym = install("sqrt");
  R_ExpSym = install("exp");
  R_EqSym = install("==");
  R_NeSym = install("!=");
  R_LtSym = install("<");
  R_LeSym = install("<=");
  R_GeSym = install(">=");
  R_GtSym = install(">");
  R_AndSym = install("&");
  R_OrSym = install("|");
  R_NotSym = install("!");
  R_CSym = install("c");
  R_LogSym = install("log");
  R_DotInternalSym = install(".Internal");
  R_DotExternalSym = install(".External");
  R_DotExternal2Sym = install(".External2");
  R_DotExternalgraphicsSym = install(".External.graphics");
  R_DotCallSym = install(".Call");
  R_DotCallgraphicsSym = install(".Call.graphics");
  R_DotFortranSym = install(".Fortran");
  R_DotCSym = install(".C");

  ByteCode::initInterpreter();

  /* the first constants record always stays in place for protection */
  R_ConstantsRegistry = ListVector::create(2);
  R_PreserveObject(R_ConstantsRegistry);
  SET_VECTOR_ELT(R_ConstantsRegistry, 0, R_NilValue);
  SET_VECTOR_ELT(R_ConstantsRegistry, 1, R_NilValue);

  R_BCProtCommitted = R_BCNodeStackBase;
}

enum {
  BCMISMATCH_OP,
  RETURN_OP,
  GOTO_OP,
  BRIFNOT_OP,
  POP_OP,
  DUP_OP,
  PRINTVALUE_OP,
  STARTLOOPCNTXT_OP,
  ENDLOOPCNTXT_OP,
  DOLOOPNEXT_OP,
  DOLOOPBREAK_OP,
  STARTFOR_OP,
  STEPFOR_OP,
  ENDFOR_OP,
  SETLOOPVAL_OP,
  INVISIBLE_OP,
  LDCONST_OP,
  LDNULL_OP,
  LDTRUE_OP,
  LDFALSE_OP,
  GETVAR_OP,
  DDVAL_OP,
  SETVAR_OP,
  GETFUN_OP,
  GETGLOBFUN_OP,
  GETSYMFUN_OP,
  GETBUILTIN_OP,
  GETINTLBUILTIN_OP,
  CHECKFUN_OP,
  MAKEPROM_OP,
  DOMISSING_OP,
  SETTAG_OP,
  DODOTS_OP,
  PUSHARG_OP,
  PUSHCONSTARG_OP,
  PUSHNULLARG_OP,
  PUSHTRUEARG_OP,
  PUSHFALSEARG_OP,
  CALL_OP,
  CALLBUILTIN_OP,
  CALLSPECIAL_OP,
  MAKECLOSURE_OP,
  UMINUS_OP,
  UPLUS_OP,
  ADD_OP,
  SUB_OP,
  MUL_OP,
  DIV_OP,
  EXPT_OP,
  SQRT_OP,
  EXP_OP,
  EQ_OP,
  NE_OP,
  LT_OP,
  LE_OP,
  GE_OP,
  GT_OP,
  AND_OP,
  OR_OP,
  NOT_OP,
  DOTSERR_OP,
  STARTASSIGN_OP,
  ENDASSIGN_OP,
  STARTSUBSET_OP,
  DFLTSUBSET_OP,
  STARTSUBASSIGN_OP,
  DFLTSUBASSIGN_OP,
  STARTC_OP,
  DFLTC_OP,
  STARTSUBSET2_OP,
  DFLTSUBSET2_OP,
  STARTSUBASSIGN2_OP,
  DFLTSUBASSIGN2_OP,
  DOLLAR_OP,
  DOLLARGETS_OP,
  ISNULL_OP,
  ISLOGICAL_OP,
  ISINTEGER_OP,
  ISDOUBLE_OP,
  ISCOMPLEX_OP,
  ISCHARACTER_OP,
  ISSYMBOL_OP,
  ISOBJECT_OP,
  ISNUMERIC_OP,
  VECSUBSET_OP,
  MATSUBSET_OP,
  VECSUBASSIGN_OP,
  MATSUBASSIGN_OP,
  AND1ST_OP,
  AND2ND_OP,
  OR1ST_OP,
  OR2ND_OP,
  GETVAR_MISSOK_OP,
  DDVAL_MISSOK_OP,
  VISIBLE_OP,
  SETVAR2_OP,
  STARTASSIGN2_OP,
  ENDASSIGN2_OP,
  SETTER_CALL_OP,
  GETTER_CALL_OP,
  SWAP_OP,
  DUP2ND_OP,
  SWITCH_OP,
  RETURNJMP_OP,
  STARTSUBSET_N_OP,
  STARTSUBASSIGN_N_OP,
  VECSUBSET2_OP,
  MATSUBSET2_OP,
  VECSUBASSIGN2_OP,
  MATSUBASSIGN2_OP,
  STARTSUBSET2_N_OP,
  STARTSUBASSIGN2_N_OP,
  SUBSET_N_OP,
  SUBSET2_N_OP,
  SUBASSIGN_N_OP,
  SUBASSIGN2_N_OP,
  LOG_OP,
  LOGBASE_OP,
  MATH1_OP,
  DOTCALL_OP,
  COLON_OP,
  SEQALONG_OP,
  SEQLEN_OP,
  BASEGUARD_OP,
  INCLNK_OP,
  DECLNK_OP,
  DECLNK_N_OP,
  INCLNKSTK_OP,
  DECLNKSTK_OP,
  OPCOUNT
};

static SEXP seq_int(int n1, int n2)
{
#define USE_ALTREP_COMPACT_INTRANGE
#ifdef USE_ALTREP_COMPACT_INTRANGE
    return R_compact_intrange(n1, n2);
#else
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = IntVector::create(n);
    int *data = INTEGER(ans);
    if (n1 <= n2)
	for (int i = 0; i < n; i++)
	    data[i] = n1 + i;
    else
	for (int i = 0; i < n; i++)
	    data[i] = n1 - i;
    return ans;
#endif
}

#define COMPACT_INTSEQ
#ifdef COMPACT_INTSEQ
# define INTSEQSXP 9999
#endif
/* tag for boxed stack entries to be ignored by stack protection */
#define NLNKSXP 9996

#define GETSTACK_FLAGS(n) (R_BCNodeStackTop[n].flags)
#define SETSTACK_FLAGS(n, v) (R_BCNodeStackTop[n].flags = (v))

static R_INLINE SEXP GETSTACK_PTR_TAG(R_bcstack_t *s)
{
    /* no error checking since only called with tag != 0 */
    SEXP value;
    switch (s->tag) {
    case REALSXP:
	value = ScalarReal(s->u.dval);
	break;
    case INTSXP:
	value = ScalarInteger(s->u.ival);
	break;
    case LGLSXP:
	value = ScalarLogical(s->u.lval);
	break;
#ifdef COMPACT_INTSEQ
    case INTSEQSXP:
	{
	    int *seqinfo = INTEGER(s->u.sxpval);
	    value = seq_int(seqinfo[0], seqinfo[1]);
	}
	break;
#endif
    default: /* not reached */
	value = NULL;
    }
    s->tag = NILSXP;
    s->u.sxpval = value;
    return value;
}

#define GETSTACK_PTR(s) ((s)->tag ? GETSTACK_PTR_TAG(s) : (s)->u.sxpval)

#define GETSTACK_SXPVAL_PTR(s) ((s)->u.sxpval)

#define GETSTACK_IVAL_PTR(s) ((s)->u.ival)

#define SETSTACK_NLNK_PTR(s, v) do {			\
	R_bcstack_t *__s__ = (s);			\
	SEXP __v__ = (v);				\
	__s__->tag = NLNKSXP;				\
	__s__->u.sxpval = __v__;			\
    } while (0)
#define SETSTACK_NLNK(i, v) SETSTACK_NLNK_PTR(R_BCNodeStackTop + (i), v)

#ifdef TESTING_WRITE_BARRIER
# define CHECK_SET_BELOW_PROT(s)					\
    if ((s) < R_BCProtTop) error("%s", _("changing stack value below R_BCProt pointer"))
#else
# define CHECK_SET_BELOW_PROT(s) do { } while (0)
#endif

#define SETSTACK_PTR(s, v) do { \
    CHECK_SET_BELOW_PROT(s); \
    SEXP __v__ = (v); \
    (s)->tag = NILSXP; \
    (s)->u.sxpval = __v__; \
} while (0)

#define SETSTACK_REAL_PTR(s, v) do { \
    double __v__ = (v); \
    (s)->tag = REALSXP; \
    (s)->u.dval = __v__; \
} while (0)

#define SETSTACK_INTEGER_PTR(s, v) do { \
    int __v__ = (v); \
    (s)->tag = INTSXP; \
    (s)->u.ival = __v__; \
} while (0)

#define SETSTACK_LOGICAL_PTR(s, v) do {		\
	int __v__ = (v);			\
	(s)->tag = LGLSXP;			\
	if (__v__ == NA_LOGICAL)		\
	    (s)->u.lval = NA_LOGICAL;		\
	else					\
	    (s)->u.lval = __v__;	\
    } while (0)

#define IS_STACKVAL_BOXED(idx)	(R_BCNodeStackTop[idx].tag == NILSXP)

#ifdef COMPACT_INTSEQ
#define SETSTACK_INTSEQ(idx, rn1, rn2) do {	\
	SEXP info = IntVector::create(2);	\
	INTEGER(info)[0] = (int) rn1;		\
	INTEGER(info)[1] = (int) rn2;		\
	R_BCNodeStackTop[idx] = R_bcstack_t(INTSEQSXP, info); \
    } while (0)
#else
#define SETSTACK_INTSEQ(idx, rn1, rn2) \
    SETSTACK(idx, seq_int((int) rn1, (int) rn2))
#endif

#define GETSTACK_SXPVAL(i) GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (i))

#define GETSTACK(i) GETSTACK_PTR(R_BCNodeStackTop + (i))

#define SETSTACK(i, v) SETSTACK_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_REAL(i, v) SETSTACK_REAL_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_INTEGER(i, v) SETSTACK_INTEGER_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_LOGICAL(i, v) SETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (i), v)

static R_INLINE SEXP STACKVAL_TO_SEXP(R_bcstack_t x) {
    return GETSTACK_PTR(&x);
}

/* bcStackScalar() checks whether the object in the specified stack
   location is an immediate scalar or a boxed simple real, integer, or
   logical scalar (i.e. length one and no attributes).  For immediate
   values the stack pointer is returned; for others the supplied stack
   structure pointer is returned after filling its fields
   appropriately. */
static R_INLINE R_bcstack_t *bcStackScalar(R_bcstack_t *s, R_bcstack_t *v)
{
    switch (s->tag) {
    case REALSXP:
    case INTSXP:
    case LGLSXP: return s;
    }

    SEXP x = GETSTACK_SXPVAL_PTR(s);
    if (IS_SIMPLE_SCALAR(x, REALSXP)) {
	v->tag = REALSXP;
	v->u.dval = SCALAR_DVAL(x);
	return v;
    }
    else if (IS_SIMPLE_SCALAR(x, INTSXP)) {
	v->tag = INTSXP;
	v->u.ival = SCALAR_IVAL(x);
	return v;
    }
    else if (IS_SIMPLE_SCALAR(x, LGLSXP)) {
	v->tag = LGLSXP;
	v->u.lval = SCALAR_LVAL(x);
	return v;
    }
    else {
	v->tag = NILSXP;
	v->u.sxpval = NULL;
	return v;
    }
}

#define INTEGER_TO_LOGICAL(x) \
    ((x) == NA_INTEGER ? NA_LOGICAL : (x) ? TRUE : FALSE)
#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

static R_INLINE R_bcstack_t *bcStackScalarReal(R_bcstack_t *s, R_bcstack_t *v)
{
    v = bcStackScalar(s, v);
    if (v->tag == INTSXP) {
	v->tag = REALSXP;
	v->u.dval = INTEGER_TO_REAL(v->u.ival);
    }
    return v;
}

#define DO_FAST_RELOP2(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_LOGICAL(-2, ((a) op (b)) ? TRUE : FALSE);	\
    CXXR_POP(1); \
    Evaluator::enableResultPrinting(true); \
    NEXT(); \
} while (0)

#define INCLNK_STACK_PTR(s) do {		\
	if ((s)->tag == 0)			\
	    INCREMENT_LINKS((s)->u.sxpval);	\
    } while (0)

#define DECLNK_STACK_PTR(s) do {		\
	if ((s)->tag == 0)			\
	    DECREMENT_LINKS((s)->u.sxpval);	\
    } while (0)

#define FastRelop2(op,opval,opsym) do {					\
	R_bcstack_t vvx, vvy;						\
	R_bcstack_t *vx = bcStackScalar(R_BCNodeStackTop - 2, &vvx);	\
	R_bcstack_t *vy = bcStackScalar(R_BCNodeStackTop - 1, &vvy);	\
	if (vx->tag == REALSXP && !ISNAN(vx->u.dval)) {			\
	    if (vy->tag == REALSXP && !ISNAN(vy->u.dval))		\
		DO_FAST_RELOP2(op, vx->u.dval, vy->u.dval);		\
	    else if (vy->tag == INTSXP && vy->u.ival != NA_INTEGER)	\
		DO_FAST_RELOP2(op, vx->u.dval, vy->u.ival);		\
	}								\
	else if (vx->tag == INTSXP && vx->u.ival != NA_INTEGER) {	\
	    if (vy->tag == REALSXP && !ISNAN(vy->u.dval))		\
		DO_FAST_RELOP2(op, vx->u.ival, vy->u.dval);		\
	    else if (vy->tag == INTSXP && vy->u.ival != NA_INTEGER) {	\
		DO_FAST_RELOP2(op, vx->u.ival, vy->u.ival);		\
	    }								\
	}								\
	Relop2(opval, opsym);						\
    } while (0)

/* not actually optimized yet; ignore op, opval for now */
#define FastLogic2(op, opval, opsym) do {		\
	Builtin2(do_logic, opsym, rho);		\
    } while (0)

static R_INLINE SEXP getPrimitive(SEXP symbol, SEXPTYPE type)
{
    SEXP value = SYMVALUE(symbol);
    if (Promise::isA(value)) {
	value = SEXP_downcast<Promise *>(value)->force();
    }
    if (TYPEOF(value) != type) {
	/* probably means a package redefined the base function so
	   try to get the real thing from the internal table of
	   primitives */
	value = R_Primitive(CHAR(PRINTNAME(symbol)));
	if (TYPEOF(value) != type)
	    /* if that doesn't work we signal an error */
	    error(_("\"%s\" is not a %s function"),
		  CHAR(PRINTNAME(symbol)),
		  type == BUILTINSXP ? "BUILTIN" : "SPECIAL");
    }
    return value;
}

static SEXP cmp_relop(SEXP call, RELOP_TYPE opval, SEXP opsym, SEXP x, SEXP y,
		      SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	GCStackRoot<> args;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	auto dgroup = DispatchGroup("Ops", call, op, args, rho);
	if (dgroup.first) {
	    return dgroup.second;
	}
    }
    return do_relop_dflt(call, op, x, y);
}

static SEXP cmp_arith1(SEXP call, SEXP opsym, SEXP x, SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x)) {
	GCStackRoot<> args;
	args = CONS_NR(x, R_NilValue);
	auto dgroup = DispatchGroup("Ops", call, op, args, rho);
	if (dgroup.first) {
	    return dgroup.second;
	}
    }
    return R_unary(call, op, x);
}

static SEXP cmp_arith2(SEXP call, ARITHOP_TYPE opval, SEXP opsym, SEXP x, SEXP y,
		       SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	GCStackRoot<> args;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	auto dgroup = DispatchGroup("Ops", call, op, args, rho);
	if (dgroup.first) {
	    return dgroup.second;
	}
    }
    return R_binary(call, op, x, y);
}

#define Builtin1(do_fun,which,rho) do {					\
	SEXP call = GETCONST(constants, GETOP());			\
	SETSTACK(-1, CONS_NR(GETSTACK(-1), R_NilValue));		\
	SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP),	\
			    GETSTACK(-1), rho));			\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
    } while(0)

#define Builtin2(do_fun,which,rho) do {					\
	SEXP stack1 = GETSTACK(-1);					\
	SEXP stack2 = GETSTACK(-2);					\
	SEXP call = GETCONST(constants, GETOP());			\
	SEXP tmp = CONS_NR(stack1, R_NilValue);				\
	SETSTACK(-2, CONS_NR(stack2, tmp));				\
	CXXR_POP(1);							\
	SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP),	\
			    GETSTACK(-1), rho));			\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
    } while(0)

#define NewBuiltin2(do_fun,opval,opsym,rho) do {	\
  SEXP call = GETCONST(constants, GETOP()); \
  SEXP x = GETSTACK(-2); \
  SEXP y = GETSTACK(-1); \
  SETSTACK(-2, do_fun(call, opval, opsym, x, y,rho));	\
  CXXR_POP(1); \
  Evaluator::enableResultPrinting(true); \
  NEXT(); \
} while(0)

#define Arith1(opsym) do {		\
  SEXP call = GETCONST(constants, GETOP()); \
  SEXP x = GETSTACK(-1); \
  SETSTACK(-1, cmp_arith1(call, opsym, x, rho)); \
  Evaluator::enableResultPrinting(true); \
  NEXT(); \
} while(0)


#define Arith2(opval,opsym) NewBuiltin2(cmp_arith2,opval,opsym,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

#define R_MSG_NA	_("NaNs produced")
#define FastMath1(fun, sym) do {					\
	R_bcstack_t vvx;						\
	R_bcstack_t *vx = bcStackScalar(R_BCNodeStackTop - 1, &vvx);	\
	if (vx->tag == REALSXP) {					\
	    double dval = fun(vx->u.dval);				\
	    if (ISNAN(dval)) {						\
		SEXP call = GETCONST(constants, GETOP());		\
		if (ISNAN(vx->u.dval)) dval = vx->u.dval;		\
		else warningcall(call, "%s", R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    SETSTACK_REAL(-1, dval);					\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	else if (vx->tag == INTSXP && vx->u.ival != NA_INTEGER) {	\
	    double dval = fun((double) vx->u.ival);			\
	    if (ISNAN(dval)) {						\
		SEXP call = GETCONST(constants, GETOP());		\
		warningcall(call, "%s", R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    SETSTACK_REAL(-1, dval);					\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	Builtin1(do_math1,sym,rho);					\
    } while (0)

#define DO_FAST_BINOP(fun,a,b) do {		\
	SKIP_OP();				\
	SETSTACK_REAL(-2, fun(a, b));		\
	CXXR_POP(1);				\
	Evaluator::enableResultPrinting(true);	\
	NEXT();					\
    } while (0)

#define DO_FAST_BINOP_INT(fun, a, b) do {		\
	double dval = fun((double) (a), (double) (b));	\
	if (dval <= INT_MAX && dval >= INT_MIN + 1) {	\
	    SKIP_OP();					\
	    SETSTACK_INTEGER(-2, (int) dval);		\
	    CXXR_POP(1);				\
	    Evaluator::enableResultPrinting(true);	\
	    NEXT();					\
	}						\
    } while(0)

#define FastUnary(op, opsym) do {					\
	R_bcstack_t vvx;						\
	R_bcstack_t *vx = bcStackScalar(R_BCNodeStackTop - 1, &vvx);	\
	if (vx->tag == REALSXP) {					\
	    SKIP_OP();							\
	    SETSTACK_REAL(-1, op vx->u.dval);				\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	else if (vx->tag == INTSXP && vx->u.ival != NA_INTEGER) {	\
	    SKIP_OP();							\
	    SETSTACK_INTEGER(-1, op vx->u.ival);			\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	Arith1(opsym);							\
    } while (0)

#define FastBinary(op,opval,opsym) do {					\
	{								\
	    R_bcstack_t *sx = R_BCNodeStackTop - 2;			\
	    R_bcstack_t *sy = R_BCNodeStackTop - 1;			\
	    if (sx->tag == REALSXP && sy->tag == REALSXP)		\
		DO_FAST_BINOP(op, sx->u.dval, sy->u.dval);		\
	}								\
	R_bcstack_t vvx, vvy;						\
	R_bcstack_t *vx = bcStackScalar(R_BCNodeStackTop - 2, &vvx);	\
	R_bcstack_t *vy = bcStackScalar(R_BCNodeStackTop - 1, &vvy);	\
	if (vx->tag == REALSXP) {					\
	    if (vy->tag == REALSXP)					\
		DO_FAST_BINOP(op, vx->u.dval, vy->u.dval);		\
	    else if (vy->tag == INTSXP && vy->u.ival != NA_INTEGER)	\
		DO_FAST_BINOP(op, vx->u.dval, vy->u.ival);		\
	}								\
	else if (vx->tag == INTSXP && vx->u.ival != NA_INTEGER) {	\
	    int ix = vx->u.ival;					\
	    if (vy->tag == REALSXP)					\
		DO_FAST_BINOP(op, ix, vy->u.dval);			\
	    else if (vy->tag == INTSXP && vy->u.ival != NA_INTEGER) {	\
		int iy = vy->u.ival;					\
		if (opval == DIVOP || opval == POWOP)			\
		    DO_FAST_BINOP(op, (double) ix, (double) iy);	\
		else							\
		    DO_FAST_BINOP_INT(op, ix, iy);			\
	    }								\
	}								\
	Arith2(opval, opsym);						\
    } while (0)

#define R_ADD(x, y) ((x) + (y))
#define R_SUB(x, y) ((x) - (y))
#define R_MUL(x, y) ((x) * (y))
#define R_DIV(x, y) ((x) / (y))

#define DO_LOG() do {							\
	R_bcstack_t vvx;						\
	R_bcstack_t *vx = bcStackScalarReal(R_BCNodeStackTop - 1, &vvx); \
	if (vx->tag == REALSXP) {					\
	    double dval = R_log(vx->u.dval);				\
	    if (ISNAN(dval)) {						\
		SEXP call = GETCONST(constants, GETOP());		\
		if (ISNAN(vx->u.dval)) dval = vx->u.dval;		\
		else warningcall(call, "%s", R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    SETSTACK_REAL(-1, dval);					\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	SEXP call = GETCONST(constants, GETOP());			\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
 } while (0)

#define DO_LOGBASE() do {						\
	R_bcstack_t vvx, vvy;						\
	R_bcstack_t *vx = bcStackScalarReal(R_BCNodeStackTop - 2, &vvx); \
	R_bcstack_t *vy = bcStackScalarReal(R_BCNodeStackTop - 1, &vvy); \
	if (vx->tag == REALSXP && vy->tag == REALSXP) {			\
	    double dval = logbase(vx->u.dval, vy->u.dval);		\
	    if (ISNAN(dval)) {						\
		SEXP call = GETCONST(constants, GETOP());		\
		if (ISNAN(vx->u.dval)) dval = vx->u.dval;		\
		else if (ISNAN(vy->u.dval)) dval = vy->u.dval;		\
		else warningcall(call, "%s", R_MSG_NA);			\
	    }								\
	    else SKIP_OP();						\
	    CXXR_POP(1);						\
	    SETSTACK_REAL(-1, dval);					\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	SEXP call = GETCONST(constants, GETOP());			\
	SEXP tmp = GETSTACK(-2);					\
	SEXP args = CONS_NR(tmp, CONS_NR(GETSTACK(-1), R_NilValue));	\
	CXXR_POP(1);							\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(R_LogSym, SPECIALSXP);			\
	SETSTACK(-1, do_log_builtin(call, op, args, rho));		\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
    } while (0)

#include <Rmath.h>
/* Keep the order consistent with the order in the byte code compiler! */
static struct { const char *name; SEXP sym; double (*fun)(double); }
    math1funs[] = {
	{"floor", NULL, floor},
	{"ceiling", NULL, ceil},
	{"sign", NULL, sign},

	{"expm1", NULL, expm1},
	{"log1p", NULL, log1p},

	{"cos", NULL, cos},
	{"sin", NULL, sin},
	{"tan", NULL, tan},
	{"acos", NULL, acos},
	{"asin", NULL, asin},
	{"atan", NULL, atan},

	{"cosh", NULL, cosh},
	{"sinh", NULL, sinh},
	{"tanh", NULL, tanh},
	{"acosh", NULL, acosh},
	{"asinh", NULL, asinh},
	{"atanh", NULL, atanh},

	{"lgamma", NULL, lgammafn},
	{"gamma", NULL, gammafn},
	{"digamma", NULL, digamma},
	{"trigamma", NULL, trigamma},

	{"cospi", NULL, cospi},
	{"sinpi", NULL, sinpi},
	{"tanpi", NULL, Rtanpi}
    };

static R_INLINE double (*getMath1Fun(int i, SEXP call))(double) {
    if (math1funs[i].sym == NULL)
	math1funs[i].sym = install(math1funs[i].name);
    if (CAR(call) != math1funs[i].sym)
	error("%s", _("math1 compiler/interpreter mismatch"));
    return math1funs[i].fun;
}

#define DO_MATH1() do {							\
	SEXP call = GETCONST(constants, GETOP());			\
	double (*fun)(double) = getMath1Fun(GETOP(), call);		\
	R_bcstack_t vvx;						\
	R_bcstack_t *vx = bcStackScalarReal(R_BCNodeStackTop - 1, &vvx); \
	if (vx->tag == REALSXP) {					\
	    double dval = fun(vx->u.dval);				\
            if (ISNAN(dval)) {						\
		if (ISNAN(vx->u.dval)) dval = vx->u.dval;		\
		else warningcall(call, "%s", R_MSG_NA);			\
	    }								\
	    SETSTACK_REAL(-1, dval);					\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	SEXP args = CONS_NR(GETSTACK(-1), R_NilValue);			\
	SEXP sym = CAR(call);						\
	SETSTACK(-1, args); /* to protect */				\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, do_math1(call, op, args, rho));			\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
    } while (0)

#define DOTCALL_MAX 16
#define DO_DOTCALL() do {						\
	SEXP call = GETCONST(constants, GETOP());			\
	int nargs = GETOP();						\
	DL_FUNC ofun = R_dotCallFn(GETSTACK(- nargs - 1), call, nargs);	\
	if (ofun && nargs <= DOTCALL_MAX) {				\
	    SEXP cargs[DOTCALL_MAX];					\
	    for (int i = 0; i < nargs; i++)				\
		cargs[i] = GETSTACK(i - nargs);				\
	    void *vmax = vmaxget();					\
	    SEXP val = R_doDotCall(ofun, nargs, cargs, call);		\
	    vmaxset(vmax);						\
	    CXXR_POP(nargs);						\
	    SETSTACK(-1, val);						\
	    Evaluator::enableResultPrinting(true);			\
	    NEXT();							\
	}								\
	SEXP args = R_NilValue;						\
	BCNPUSH(args); /* allocate space for protecting args */		\
	while (nargs-- >= 0) {						\
	    args = CONS_NR(GETSTACK(-2), args);				\
	    SETSTACK(-2, args); /* to protect */			\
	    BCNPOP_IGNORE_VALUE();					\
	}								\
	SEXP sym = CAR(call);						\
	SEXP op = getPrimitive(sym, BUILTINSXP);			\
	SETSTACK(-1, do_dotcall(call, op, args, rho));			\
	Evaluator::enableResultPrinting(true);				\
	NEXT();								\
    } while (0)

#define DO_COLON() do {							\
	R_bcstack_t vvx, vvy;						\
	R_bcstack_t *vx = bcStackScalarReal(R_BCNodeStackTop - 2, &vvx); \
	R_bcstack_t *vy = bcStackScalarReal(R_BCNodeStackTop - 1, &vvy); \
	if (vx->tag == REALSXP && vy->tag == REALSXP) {			\
	    double rn1 = vx->u.dval;					\
	    double rn2 = vy->u.dval;					\
	    if (R_FINITE(rn1) && R_FINITE(rn2) &&			\
		INT_MIN <= rn1 && INT_MAX >= rn1 &&			\
		INT_MIN <= rn2 && INT_MAX >= rn2 &&			\
		rn1 == (int) rn1 && rn2 == (int) rn2) {			\
		SKIP_OP(); /* skip 'call' index */			\
		CXXR_POP(1);						\
		SETSTACK_INTSEQ(-1, rn1, rn2);				\
		Evaluator::enableResultPrinting(true);			\
		NEXT();							\
	    }								\
	}								\
	Builtin2(do_colon, R_ColonSymbol, rho);				\
    } while (0)

#define DO_SEQ_ALONG() do {					\
	SEXP x = GETSTACK(-1);					\
	if (!OBJECT(x)) {					\
	    R_xlen_t len = xlength(x);				\
	    if (len >= 1 && len <= INT_MAX) {			\
		SKIP_OP(); /* skip 'call' index */		\
		SETSTACK_INTSEQ(-1, 1, len);			\
		Evaluator::enableResultPrinting(true);		\
		NEXT();						\
	    }							\
	}							\
	Builtin1(do_seq_along, install("seq_along"), rho);	\
    } while (0)

#define DO_SEQ_LEN() do {						\
	R_bcstack_t vvx;						\
	R_bcstack_t *vx = bcStackScalarReal(R_BCNodeStackTop - 1, &vvx); \
	if (vx->tag == REALSXP) {					\
	    double rlen = vx->u.dval;					\
	    if (1 <= rlen && INT_MAX >= rlen &&				\
		rlen == (int) rlen) {					\
		SKIP_OP(); /* skip 'call' index */			\
		SETSTACK_INTSEQ(-1, 1, rlen);				\
		Evaluator::enableResultPrinting(true);			\
		NEXT();							\
	    }								\
	}								\
	Builtin1(do_seq_len, install("seq_len"), rho);			\
    } while (0)

static R_INLINE SEXP getForLoopSeq(int offset, bool *iscompact)
{
#ifdef COMPACT_INTSEQ
    R_bcstack_t *s = R_BCNodeStackTop + offset;
    if (s->tag == INTSEQSXP) {
	*iscompact = true;
	return s->u.sxpval;
    }
#endif
    *iscompact = false;
    return GETSTACK(offset);
}

#define BCNPUSH(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  SETSTACK(0, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_NODE(v) do { \
	R_BCNodeStackTop[0] = R_BCNodeStackTop[-1];	\
	R_BCNodeStackTop++;				\
} while (0)

#define BCNPUSH_NLNK(v) do {			\
	BCNPUSH(R_NilValue);			\
	SETSTACK_NLNK(-1, v);			\
    } while (0)

#define BCNPUSH_REAL(v) do { \
  double __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = R_bcstack_t(REALSXP, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_INTEGER(v) do { \
  int __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = R_bcstack_t(INTSXP, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_LOGICAL(v) do { \
  int __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = R_bcstack_t(LGLSXP, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_CACHE(v) do { \
  int __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = R_bcstack_t(CACHESZ_TAG, __value__); \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_STACKVAL(v) do {				\
	R_bcstack_t __value__ = (v);				\
	R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1;		\
	if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow();	\
	__ntop__[-1] = __value__;				\
	R_BCNodeStackTop = __ntop__;				\
    } while (0)

#define BCNDUP() do {						\
	R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1;		\
	if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow();	\
	__ntop__[-1] = __ntop__[-2];				\
	R_BCNodeStackTop = __ntop__;				\
    } while(0)

#define BCNDUP2ND() do {					\
	R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1;		\
	if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow();	\
	__ntop__[-1] = __ntop__[-3];				\
	R_BCNodeStackTop = __ntop__;				\
    } while(0)

#define BCNDUP3RD() do {					\
	R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1;		\
	if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow();	\
	__ntop__[-1] = __ntop__[-4];				\
	R_BCNodeStackTop = __ntop__;				\
    } while(0)

#define BCNPOP() (CXXR_POP(1), GETSTACK(0))
#define BCNPOP_IGNORE_VALUE() CXXR_POP(1)

#define BCNSTACKCHECK(n)  do {						\
	if (R_BCNodeStackTop + (n) > R_BCNodeStackEnd) nodeStackOverflow(); \
    } while (0)

/* use a struct to force use of correct accessors */
struct R_bcconsts_t
{
    SEXP const *p;
    R_bcconsts_t(SEXP const *data = nullptr) : p(data)
    {
    }
};

#define BCCONSTS(e) \
    R_bcconsts_t(((SEXP const *) DATAPTR_RO(BCODE_CONSTS(e))))
#define BCCONSTS_LEN(e) XLENGTH(BCODE_CONSTS(e))
#define GETCONST(x, i) ((x).p)[i]

NORET static void nodeStackOverflow(void)
{
    /* condition is pre-allocated and protected with R_PreserveObject */
    GCStackRoot<> cond;
    cond = R_getNodeStackOverflowError();
    R_signalErrorCondition(cond, R_CurrentExpression);
}

#define NELEMS_FOR_SIZE(size) \
    ((int) (((size) + sizeof(R_bcstack_t) - 1) / sizeof(R_bcstack_t)))

/* Allocate contiguous space on the node stack */
static R_INLINE void* BCNALLOC(size_t size)
{
    int nelems = NELEMS_FOR_SIZE(size);
    BCNSTACKCHECK(nelems + 1);
    R_BCNodeStackTop->tag = RAWMEM_TAG;
    R_BCNodeStackTop->u.ival = nelems;
    R_BCNodeStackTop++;
    void *ans = R_BCNodeStackTop;
    R_BCNodeStackTop += nelems;
    return ans;
}

static SEXP bytecodeExpr(SEXP e)
{
    if (isByteCode(e)) {
	if (LENGTH(BCODE_CONSTS(e)) > 0)
	    return VECTOR_ELT(BCODE_CONSTS(e), 0);
	else return R_NilValue;
    }
    else return e;
}

SEXP R_BytecodeExpr(SEXP e)
{
    return bytecodeExpr(e);
}

SEXP R_PromiseExpr(SEXP p)
{
    return bytecodeExpr(PRCODE(p));
}

SEXP R_ClosureExpr(SEXP p)
{
    return bytecodeExpr(BODY(p));
}

#ifdef THREADED_CODE
/* This is based on: Ian Piumarta and Fabio Riccardi (1998),
   Optimizing Direct Threaded Code by Selective Inlining, in
   Proceedings of the 1998 ACM SIGPLAN Conference on Programming
   Language Design and Implementation (PLDI). */

typedef union { void *v; int i; } BCODE;

/* Declare opinfo volatile to prevent gcc 6 from making a local copy
   in bcEval stack frames and thus increasing stack usage
   dramatically */
volatile
static struct { void *addr; int argc; const char *instname; } opinfo[OPCOUNT];

#define OP(name,n) \
  case name##_OP: opinfo[name##_OP].addr = (__extension__ &&op_##name); \
    opinfo[name##_OP].argc = (n); \
    opinfo[name##_OP].instname = #name; \
    goto loop; \
    op_##name

#define BEGIN_MACHINE NEXT(); init: { which = 0; loop: switch(which++)
#define LASTOP } return R_NilValue
#define INITIALIZE_MACHINE()			\
    do {					\
	static bool s_loop_initialized = false;	\
	if (!s_loop_initialized) {		\
	    s_loop_initialized = true;		\
	    goto init;				\
	}					\
    } while (0)

#define NEXT() (__extension__ ({R_BCpc = currentpc = pc; goto *(*pc++).v;}))
#define GETOP() (*pc++).i
#define SKIP_OP() (pc++)

#define BCCODE(e) BCODE_PTR(BCODE_CODE(e))
#else
typedef int BCODE;

#define OP(name,argc) case name##_OP

#ifdef BC_PROFILING
#define BEGIN_MACHINE  loop: R_BCpc = currentpc = pc; s_current_opcode = *pc; switch(*pc++)
#else
#define BEGIN_MACHINE  loop: R_BCpc = currentpc = pc; switch(*pc++)
#endif
#define LASTOP  default: error("%s", _("bad opcode"))
#define INITIALIZE_MACHINE()

#define NEXT() goto loop
#define GETOP() *pc++
#define SKIP_OP() (pc++)

#define BCCODE(e) INTEGER(BCODE_CODE(e))
#endif

/**** is there a way to avoid the locked check here? */
/**** always boxing on lock is one option */
#define BNDCELL_TAG_WR(v) (BINDING_IS_LOCKED(v) ? NILSXP : BNDCELL_TAG(v))

#define BNDCELL_WRITABLE(v)						\
    (v != R_NilValue &&	 !BINDING_IS_LOCKED(v) && !IS_ACTIVE_BINDING(v))
#define BNDCELL_UNBOUND(v) (BNDCELL_TAG(v) == NILSXP && CAR0(v) == R_UnboundValue)

static R_INLINE void NEW_BNDCELL_DVAL(SEXP cell, double dval)
{
    INIT_BNDCELL(cell, REALSXP);
    SET_BNDCELL_DVAL(cell, dval);
}

static R_INLINE void NEW_BNDCELL_IVAL(SEXP cell, int ival)
{
    INIT_BNDCELL(cell, INTSXP);
    SET_BNDCELL_IVAL(cell, ival);
}

static R_INLINE void NEW_BNDCELL_LVAL(SEXP cell, int lval)
{
    INIT_BNDCELL(cell, LGLSXP);
    SET_BNDCELL_LVAL(cell, lval);
}

static R_INLINE SEXP BINDING_VALUE(SEXP loc)
{
    if (BNDCELL_TAG(loc)) {
	R_expand_binding_value(loc);
	return CAR0(loc);
    }
    else if (loc != R_NilValue && !IS_ACTIVE_BINDING(loc))
	return CAR0(loc);
    else
	return R_UnboundValue;
}

#define BINDING_SYMBOL(loc) TAG(loc)

/* Defining USE_BINDING_CACHE enables a cache for GETVAR, SETVAR, and
   others to more efficiently locate bindings in the top frame of the
   current environment.  The index into of the symbol in the constant
   table is used as the cache index.  Two options can be used to chose
   among implementation strategies:

       If CACHE_ON_STACK is defined the cache is allocated on the
       byte code stack. Otherwise it is allocated on the heap as a
       VECSXP.  The stack-based approach is more efficient, but runs
       the risk of running out of stack space.

       If CACHE_MAX is defined, then a cache of at most that size is
       used. The value must be a power of 2 so a modulus computation x
       % CACHE_MAX can be done as x & (CACHE_MAX - 1). More than 90%
       of the closures in base have constant pools with fewer than 128
       entries when compiled, to that is a good value to use. But
       increasing to 256 handles some benchmark scripts a bit better.

   On average about 1/3 of constant pool entries are symbols, so this
   approach wastes some space.  This could be avoided by grouping the
   symbols at the beginning of the constant pool and recording the
   number.

   Bindings recorded may become invalid if user code removes a
   variable.  The code in envir.c has been modified to insert
   R_UnboundValue as the value of a binding when it is removed, and
   code using cached bindings checks for this.

   It would be nice if we could also cache bindings for variables
   found in enclosing environments. These would become invalid if a
   new variable is defined in an intervening frame. Some mechanism for
   invalidating the cache would be needed. This is certainly possible,
   but finding an efficient mechanism does not seem to be easy.   LT */

/* Both mechanisms implemented here make use of the stack to hold
   cache information.  This is not a problem except for "safe" for()
   loops using the STARTLOOPCNTXT instruction to run the body in a
   separate bcEval call.  Since this approach expects loop setup
   information to be passed on the stack from the outer bcEval call to
   an inner one the inner one cannot put things on the stack. For now,
   bcEval takes an additional argument that disables the cache in
   calls via STARTLOOPCNTXT for all "safe" loops. It would be better
   to deal with this in some other way, for example by having a
   specific STARTFORLOOPCNTXT instruction that deals with transferring
   the information in some other way. For now disabling the cache is
   an expedient solution. LT */

#define USE_BINDING_CACHE
# ifdef USE_BINDING_CACHE
/* CACHE_MAX must be a power of 2 for modulus using & CACHE_MASK to work*/
# define CACHE_MAX 256
# ifdef CACHE_MAX
#  define CACHE_MASK (CACHE_MAX - 1)
#  define CACHEIDX(i) ((i) & CACHE_MASK)
# else
#  define CACHEIDX(i) (i)
# endif

# define CACHE_ON_STACK
# ifdef CACHE_ON_STACK
typedef R_bcstack_t * R_binding_cache_t;
#  define VCACHE(i) GETSTACK_SXPVAL_PTR(vcache + (i))
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VCACHE(CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) VCACHE(sidx)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) VCACHE(CACHEIDX(sidx)) = (cell); } while (0)
# else
typedef SEXP R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) SET_VECTOR_ELT(vcache, CACHEIDX(sidx), cell); } while (0)
# endif
#else
typedef void *R_binding_cache_t;
# define GET_CACHED_BINDING_CELL(vcache, sidx) R_NilValue
# define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) R_NilValue

# define SET_CACHED_BINDING(vcache, sidx, cell)
#endif

static R_INLINE SEXP GET_BINDING_CELL_CACHE(SEXP symbol, SEXP rho,
					    R_binding_cache_t vcache, int idx)
{
    SEXP cell = GET_CACHED_BINDING_CELL(vcache, idx);
    /* The value returned by GET_CACHED_BINDING_CELL is either a
       binding cell or R_NilValue.  TAG(R_NilValue) is R_NilValue, and
       that will not equal symbol. So a separate test for cell !=
       R_NilValue is not needed. */
    if (TAG(cell) == symbol && !BNDCELL_UNBOUND(cell))
	return cell;
    else {
	SEXP ncell = GET_BINDING_CELL(symbol, rho);
	if (ncell != R_NilValue)
	    SET_CACHED_BINDING(vcache, idx, ncell);
	else if (cell != R_NilValue && BNDCELL_UNBOUND(cell))
	    SET_CACHED_BINDING(vcache, idx, R_NilValue);
	return ncell;
    }
}

NORET static void UNBOUND_VARIABLE_ERROR(SEXP symbol, SEXP rho)
{
    errorcall_cpy(getLexicalCall(rho),
		  _("object '%s' not found"),
		  EncodeChar(PRINTNAME(symbol)));
}

static R_INLINE SEXP FIND_VAR_NO_CACHE(SEXP symbol, SEXP rho, SEXP cell)
{
    R_varloc_t loc =  R_findVarLoc(symbol, rho);
    if (loc.cell && IS_ACTIVE_BINDING(loc.cell)) {
	SEXP value = R_GetVarLocValue(loc);
	return value;
    }
    else return R_GetVarLocValue(loc);
}

/* findVar variant that handles dd vars and cached bindings */
static R_INLINE SEXP findVarEX(SEXP symbol, SEXP rho, bool dd,
			       R_binding_cache_t vcache, int sidx)
{
    if (dd)
	return ddfindVar(symbol, rho);
    else if (vcache != NULL) {
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	SEXP value = BINDING_VALUE(cell);
	if (value == R_UnboundValue)
	    return FIND_VAR_NO_CACHE(symbol, rho, cell);
	else
	    return value;
    }
    else
	return R_findVar(symbol, rho);
}

#ifdef IMMEDIATE_PROMISE_VALUES
# define SET_PROMISE_DVAL SET_BNDCELL_DVAL
# define SET_PROMISE_IVAL SET_BNDCELL_IVAL
# define SET_PROMISE_LVAL SET_BNDCELL_LVAL
#endif
#define PROMISE_DVAL BNDCELL_DVAL
#define PROMISE_IVAL BNDCELL_IVAL
#define PROMISE_LVAL BNDCELL_LVAL

static R_INLINE SEXP getvar(SEXP symbol, SEXP rho,
			    bool dd, bool keepmiss,
			    R_binding_cache_t vcache, int sidx)
{
    SEXP value = findVarEX(symbol, rho, dd, vcache, sidx);

    if (value == R_UnboundValue)
	UNBOUND_VARIABLE_ERROR(symbol, rho);
    else if (value == R_MissingArg) {
	if (!keepmiss) R_MissingArgError(symbol, getLexicalCall(rho), "getvarError");
	return R_MissingArg;
    }
    else if (TYPEOF(value) == PROMSXP) {
	if (PROMISE_IS_EVALUATED(value))
	    return PRVALUE(value);
	else {
	    /**** R_isMissing is inefficient */
	    if (keepmiss) {
		PROTECT(value);
		bool miss = R_isMissing(symbol, rho);
		UNPROTECT(1);
		if (miss)
		    return R_MissingArg;
	    }
	    return SEXP_downcast<Promise *>(value)->force();
	}
    }
    else {
	ENSURE_NAMED(value); /* needed for .Last.value - LT */
	return value;
    }
}

#ifdef IMMEDIATE_PROMISE_VALUES
# define SET_PROMISE_VALUE_FROM_STACKVAL(prom, ubval)  do {		\
	SEXP value;							\
	SET_PROMISE_TAG(prom, (SEXPTYPE) ubval.tag);				\
	switch ((ubval).tag) {						\
	case REALSXP: SET_PROMISE_DVAL(prom, (ubval).u.dval); break;	\
	case INTSXP: SET_PROMISE_IVAL(prom, (ubval).u.ival); break;	\
	case LGLSXP: SET_PROMISE_LVAL(prom, (ubval).u.ival); break;	\
	default:							\
	    value = STACKVAL_TO_SEXP(ubval);				\
	    SET_PRVALUE(prom, value);					\
	    ENSURE_NAMEDMAX(value);					\
	}								\
    } while(0)
#else
# define SET_PROMISE_VALUE_FROM_STACKVAL(prom, ubval)  do {		\
	SEXP value = STACKVAL_TO_SEXP(ubval);				\
	SET_PRVALUE(prom, value);					\
	ENSURE_NAMEDMAX(value);						\
    } while(0)
#endif

#define INLINE_GETVAR
#ifdef INLINE_GETVAR
/* Try to handle the most common case as efficiently as possible.  If
   smallcache is true then a modulus operation on the index is not
   needed, nor is a check that a non-null value corresponds to the
   requested symbol. The symbol from the constant pool is also usually
   not needed. Active bindings will have functions as their values.
   Skipping SYMSXP values rules out R_MissingArg and R_UnboundValue as
   these are implemented as symbols.  It also rules other symbols, but
   as those are rare they are handled by the getvar() call. */
#define DO_GETVAR(dd,keepmiss) do { \
    int sidx = GETOP(); \
    Evaluator::enableResultPrinting(true);	 			\
    if (!dd && smallcache) {						\
	SEXP cell = GET_SMALLCACHE_BINDING_CELL(vcache, sidx);		\
	if (cell == R_NilValue) {					\
	    /* make sure local variable cells are cached */		\
	    SEXP symbol = GETCONST(constants, sidx);			\
	    cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);	\
	}								\
	/* handle immediate binings */					\
	switch (BNDCELL_TAG(cell)) {					\
	case REALSXP: BCNPUSH_REAL(BNDCELL_DVAL(cell)); NEXT();		\
	case INTSXP: BCNPUSH_INTEGER(BNDCELL_IVAL(cell)); NEXT();	\
	case LGLSXP: BCNPUSH_LOGICAL(BNDCELL_LVAL(cell)); NEXT();	\
	default:                                                      	\
	    break; 							\
	}								\
	SEXP value = CAR(cell);						\
	SEXPTYPE type = TYPEOF(value);					\
	/* extract value of forced promises */				\
	if (type == PROMSXP) {						\
	    if (PROMISE_IS_EVALUATED(value)) {				\
		switch (PROMISE_TAG(value)) {				\
		case REALSXP: BCNPUSH_REAL(PROMISE_DVAL(value)); NEXT(); \
		case INTSXP: BCNPUSH_INTEGER(PROMISE_IVAL(value)); NEXT(); \
		case LGLSXP: BCNPUSH_LOGICAL(PROMISE_LVAL(value)); NEXT(); \
		default:                                                \
	    	    break; 						\
		}							\
		value = PRVALUE(value);					\
		type = TYPEOF(value);					\
	    }								\
	}								\
	/* try fast handling of some types; for these the */		\
	/* cell won't be R_NilValue or an active binding */		\
	switch(type) {							\
	case REALSXP:							\
	case INTSXP:							\
	case LGLSXP:							\
	case CPLXSXP:							\
	case STRSXP:							\
	case VECSXP:							\
	case RAWSXP:							\
	    BCNPUSH(value);						\
	    NEXT();							\
	case SYMSXP:							\
	case PROMSXP:							\
	    break;							\
	default:							\
	    if (cell != R_NilValue && !IS_ACTIVE_BINDING(cell)) {	\
		BCNPUSH(value);						\
		NEXT();							\
	    }								\
	}								\
    }									\
    SEXP symbol = GETCONST(constants, sidx);				\
    SEXP value = findVarEX(symbol, rho, dd, vcache, sidx);		\
    if (!keepmiss && TYPEOF(value) == PROMSXP &&			\
	(PRSEEN(value) == DEFAULT) && !PROMISE_IS_EVALUATED(value) &&	\
	TYPEOF(PRCODE(value)) == BCODESXP) {				\
	START_BCFRAME_PROM(value);					\
	NEXT();								\
	/* return cleanup is in DO_GETVAR_FORCE_PROMISE_RETURN */	\
    }									\
    BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));		\
    NEXT();								\
} while (0)
#else
#define DO_GETVAR(dd,keepmiss) do { \
  int sidx = GETOP(); \
  SEXP symbol = GETCONST(constants, sidx); \
  Evaluator::enableResultPrinting(true); \
  BCNPUSH(getvar(symbol, rho, dd, keepmiss, vcache, sidx));	\
  NEXT(); \
} while (0)
#endif

/* call frame accessors */
#define CALL_FRAME_FUN() GETSTACK(-3)
#define CALL_FRAME_ARGS() GETSTACK(-2)
#define CALL_FRAME_FTYPE() TYPEOF(CALL_FRAME_FUN())
#define CALL_FRAME_SIZE() (3)

static R_INLINE SEXP BUILTIN_CALL_FRAME_ARGS(void)
{
    SEXP args = CALL_FRAME_ARGS();
    for (SEXP a = args; a  != R_NilValue; a = CDR(a))
	DECREMENT_LINKS(CAR(a));
    return args;
}

static R_INLINE SEXP CLOSURE_CALL_FRAME_ARGS(void)
{
    SEXP args = CALL_FRAME_ARGS();
    /* it would be better not to build this arglist with CONS_NR in
       the first place */
#ifndef NO_CALL_FRAME_ARGS_NR
    for (SEXP a = args; a  != R_NilValue; a = CDR(a)) {
	DECREMENT_LINKS(CAR(a));
	if (!REFCNT_ENABLED(a)) {
	    ENABLE_REFCNT(a);
	    INCREMENT_REFCNT(CAR(a));
	    INCREMENT_REFCNT(CDR(a));
	}
    }
#endif
    return args;
}

#define GETSTACK_BELOW_CALL_FRAME(n) GETSTACK((n) - CALL_FRAME_SIZE())
#define SETSTACK_BELOW_CALL_FRAME(n, v) SETSTACK((n) - CALL_FRAME_SIZE(), v)

/* create room for accumulating the arguments. */
#define INIT_CALL_FRAME_ARGS() do { \
	BCNSTACKCHECK(2);	  \
	BCNPUSH(R_NilValue);	  \
	BCNPUSH(R_NilValue);	  \
    } while (0)

/* push the function and create room for accumulating the arguments. */
#define INIT_CALL_FRAME(fun) do { \
	BCNPUSH(fun);		\
	INIT_CALL_FRAME_ARGS();	\
    } while (0)

/* remove the call frame from the stack and push the return value */
#define POP_CALL_FRAME(value) POP_CALL_FRAME_PLUS(0, value)

#define POP_CALL_FRAME_PLUS(n, value) do {	\
	CXXR_POP(2 + (n));			\
	SETSTACK(-1, value);			\
    } while (0)

/* push an argument to existing call frame */
/* a call frame always uses boxed stack values, so GETSTACK will not allocate */
#define PUSHCALLARG_EX(v, RC) do {					\
	SEXP __cell__ =							\
	    (RC) ? CONS(v, R_NilValue) : CONS_NR(v, R_NilValue);	\
	if (GETSTACK(-2) == R_NilValue) SETSTACK(-2, __cell__);		\
	else SETCDR(GETSTACK(-1), __cell__);				\
	SETSTACK(-1, __cell__);						\
	if (RC) INCREMENT_NAMED(CAR(__cell__));				\
	else INCREMENT_LINKS(CAR(__cell__));				\
    } while (0)
#define PUSHCALLARG(v) PUSHCALLARG_EX(v, FALSE)
#ifdef NO_CALL_FRAME_ARGS_NR
#define PUSHCALLARG_RC(v) PUSHCALLARG_EX(v, TRUE)
#else
#define PUSHCALLARG_RC PUSHCALLARG
#endif

/* place a tag on the most recently pushed call argument */
#define SETCALLARG_TAG(t) do {			\
	SEXP __tag__ = (t);			\
	if (__tag__ != R_NilValue) {		\
	    SEXP __cell__ = GETSTACK(-1);	\
	    if (__cell__ != R_NilValue)		   \
		SET_TAG(__cell__, CreateTag(__tag__));	\
	}						\
    } while (0)

/* same, but tag is known to be a symbol */
#define SETCALLARG_TAG_SYMBOL(t) do {			\
	SEXP __cell__ = GETSTACK(-1);			\
	if (__cell__ != R_NilValue)			\
	    SET_TAG(__cell__, t);			\
    } while (0)

static bool tryDispatch(const char *generic, SEXP call, SEXP x, SEXP rho, SEXP *pv)
{
  GCStackRoot<> pargs, rho1;
  bool dispatched = false;
  SEXP op = SYMVALUE(install(generic)); /**** avoid this */

  pargs = promiseArgs(CDR(call), rho);
  IF_PROMSXP_SET_PRVALUE(CAR(pargs), x);

  /**** Minimal hack to try to handle the S4 case.  If we do the check
	and do not dispatch then some arguments beyond the first might
	have been evaluated; these will then be evaluated again by the
	compiled argument code. */
  if (IS_S4_OBJECT(x) && R_has_methods(op)) {
    auto val = R_possible_dispatch(call, op, pargs, rho, true);
    if (val.first) {
      *pv = val.second;
      return true;
    }
  }

  /* See comment at first usemethod() call in this file. LT */
  rho1 = NewEnvironment(R_NilValue, R_NilValue, rho);
  {
  RCNTXT cntxt(CTXT_RETURN, call, rho1, rho, pargs, op);
  if (usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv, pv))
    dispatched = true;
  endcontext(&cntxt);
  }

#ifdef ADJUST_ENVIR_REFCNTS
  R_CleanupEnvir(rho1, dispatched ? *pv : R_NilValue);
  unpromiseArgs(pargs);
#else
  if (!dispatched) DECREMENT_REFCNT(x);
#endif
  return dispatched;
}

static bool tryAssignDispatch(const char *generic, SEXP call, SEXP lhs, SEXP rhs,
			     SEXP rho, SEXP *pv)
{
    bool result;
    SEXP last, prom;
    GCStackRoot<> ncall;

    ncall = duplicate(call);
    last = ncall;
    while (CDR(last) != R_NilValue)
	last = CDR(last);
    prom = mkRHSPROMISE(CAR(last), rhs);
    SETCAR(last, prom);
    result = tryDispatch(generic, ncall, lhs, rho, pv);

    return result;
}

#define DO_STARTDISPATCH(generic) do { \
  SEXP call = GETCONST(constants, GETOP()); \
  int label = GETOP(); \
  SEXP value = GETSTACK(-1); \
  if (isObject(value) && tryDispatch(generic, call, value, rho, &value)) {\
    SETSTACK(-1, value);						\
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(value); \
    SETCALLARG_TAG(tag);   \
  } \
  NEXT(); \
} while (0)

#define DO_DFLTDISPATCH(fun, symbol) do { \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = BUILTIN_CALL_FRAME_ARGS(); \
  SEXP value = fun(call, symbol, args, rho); \
  POP_CALL_FRAME_PLUS(2, value); \
  Evaluator::enableResultPrinting(true); \
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = GETCONST(constants, GETOP()); \
  int label = GETOP(); \
  SEXP lhs = GETSTACK(-2); \
  SEXP rhs = GETSTACK(-1); \
  MARK_ASSIGNMENT_CALL(call); \
  if (MAYBE_SHARED(lhs)) { \
    lhs = shallow_duplicate(lhs); \
    SETSTACK(-2, lhs); \
    ENSURE_NAMED(lhs); \
  } \
  SEXP value = NULL; \
  if (isObject(lhs) && \
      tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
    CXXR_POP(1);	\
    SETSTACK(-1, value); \
    BC_CHECK_SIGINT(); \
    pc = codebase + label; \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    BCNPUSH(call); \
    INIT_CALL_FRAME(R_NilValue); \
    PUSHCALLARG(lhs); \
    SETCALLARG_TAG(tag);   \
  } \
  NEXT(); \
} while (0)

#define DO_DFLT_ASSIGN_DISPATCH(fun, symbol) do { \
  SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-2); \
  SEXP call = GETSTACK_BELOW_CALL_FRAME(-1); \
  SEXP args = BUILTIN_CALL_FRAME_ARGS(); \
  MARK_ASSIGNMENT_CALL(call); \
  PUSHCALLARG(rhs); \
  SEXP value = fun(call, symbol, args, rho); \
  POP_CALL_FRAME_PLUS(3, value); \
  NEXT(); \
} while (0)

#define DO_STARTDISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    SEXP value = GETSTACK(-1); \
    if (isObject(value)) { \
	SEXP call = GETCONST(constants, callidx); \
	if (tryDispatch(generic, call, value, rho, &value)) { \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    int label = GETOP(); \
	    pc = codebase + label; \
	    NEXT(); \
	} \
    } \
    SKIP_OP(); \
    NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH_N(generic) do { \
    int callidx = GETOP(); \
    int label = GETOP(); \
    SEXP lhs = GETSTACK(-2); \
    if (isObject(lhs)) { \
	SEXP call = GETCONST(constants, callidx); \
	MARK_ASSIGNMENT_CALL(call); \
	SEXP rhs = GETSTACK(-1); \
	if (MAYBE_SHARED(lhs)) { \
	    lhs = shallow_duplicate(lhs); \
	    SETSTACK(-2, lhs); \
	    ENSURE_NAMED(lhs); \
	} \
	SEXP value = NULL; \
	if (tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
	    CXXR_POP(1); \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = codebase + label; \
	    NEXT(); \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_ISTEST(fun) do { \
  SETSTACK(-1, fun(GETSTACK(-1)) ? R_TrueValue : R_FalseValue);	\
  Evaluator::enableResultPrinting(true); \
  NEXT(); \
} while(0)
#define DO_ISTYPE(type) do { \
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? R_TrueValue : R_FalseValue); \
  Evaluator::enableResultPrinting(true); \
  NEXT(); \
} while (0)
#define isNumericOnly(x) (isNumeric(x) && !isLogical(x))

#ifdef BC_PROFILING
#define NO_CURRENT_OPCODE -1
static int s_current_opcode = NO_CURRENT_OPCODE;
static int opcode_counts[OPCOUNT];
#endif

static void bc_check_sigint(void)
{
    R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
    /* finalizers are run here since this should only be called at
       points where running arbitrary code should be safe */
    R_RunPendingFinalizers();
#endif
}

#define BC_COUNT_DELTA 1023
#define BC_CHECK_SIGINT() do {			\
	if (++s_evalcount > BC_COUNT_DELTA) {	\
	    bc_check_sigint();			\
	    s_evalcount = 0;			\
	}					\
    } while (0)

/* use loop index for faster check */
#define BC_LOOP_COUNT_MASK 1023
#define BC_CHECK_SIGINT_LOOP(i) do {		\
	if ((i & BC_LOOP_COUNT_MASK) == 0) {	\
	    bc_check_sigint();			\
	    s_evalcount = 0;			\
	}					\
    } while (0)

static void loopWithContext(SEXP code, SEXP rho)
{
    RCNTXT cntxt(CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue, R_NilValue);

    while (true)
    {
        try
        {
            ByteCode::interpret(code, rho, false);
            break;
        }
        catch (JMPException &e)
        {
            if (e.context() != &cntxt)
                throw;
            if (e.mask() == CTXT_BREAK)
                break;
        }
    }

    endcontext(&cntxt);
}

static R_INLINE R_xlen_t bcStackIndex(R_bcstack_t *s)
{
    switch(s->tag) {
    case INTSXP:
	if (s->u.ival != NA_INTEGER)
	    return s->u.ival;
	else return -1;
    case REALSXP:
	{
	    double val = s->u.dval;
	    if (!ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
		return (R_xlen_t) s->u.dval;
	    else return -1;
	}
    case LGLSXP: return -1;
    default: break;
    }

    SEXP idx = GETSTACK_SXPVAL_PTR(s);
    if (IS_SCALAR(idx, INTSXP)) {
	int ival = SCALAR_IVAL(idx);
	if (ival != NA_INTEGER)
	    return ival;
	else return -1;
    }
    else if (IS_SCALAR(idx, REALSXP)) {
	double val = SCALAR_DVAL(idx);
	if (!ISNAN(val) && val <= R_XLEN_T_MAX && val > 0)
	    return (R_xlen_t) val;
	else return -1;
    }
    else return -1;
}

static R_INLINE SEXP mkVector1(SEXP s)
{
    return ListVector::createScalar(s);
}

#define DO_FAST_VECELT(sv, vec,  i, subset2) do {		\
	switch (TYPEOF(vec)) {					\
	case REALSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SETSTACK_REAL_PTR(sv, REAL_ELT(vec, i));		\
	    return;						\
	}							\
	case INTSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SETSTACK_INTEGER_PTR(sv, INTEGER_ELT(vec, i));	\
	    return;						\
	}							\
	case LGLSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SETSTACK_LOGICAL_PTR(sv, LOGICAL_ELT(vec, i));	\
	    return;						\
	}							\
	case CPLXSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SETSTACK_PTR(sv, ScalarComplex(COMPLEX_ELT(vec, i)));	\
	    return;						\
	}							\
	case RAWSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SETSTACK_PTR(sv, ScalarRaw(RAW(vec)[i]));		\
	    return;						\
	}							\
	case VECSXP:						\
	{							\
	    if (i < 0 || XLENGTH(vec) <= i) break;		\
	    SEXP elt = VECTOR_ELT(vec, i);			\
	    RAISE_NAMED(elt, NAMED(vec));			\
	    if (subset2)					\
		SETSTACK_PTR(sv, elt);				\
	    else						\
		SETSTACK_PTR(sv, mkVector1(elt));		\
	    return;						\
	}							\
	default:                                                \
	    break;                                              \
	}							\
    } while (0)

#define FAST_VECELT_OK(vec) \
    (ATTRIB(vec) == R_NilValue ||		\
     (TAG(ATTRIB(vec)) == R_DimSymbol &&	\
      CDR(ATTRIB(vec)) == R_NilValue))

static R_INLINE void VECSUBSET_PTR(SEXP vec, R_bcstack_t *si,
				   R_bcstack_t *sv, SEXP rho,
				   R_bcconsts_t consts, int callidx,
				   bool subset2)
{
    R_xlen_t i = bcStackIndex(si) - 1;
    if ((subset2 || FAST_VECELT_OK(vec)))
	DO_FAST_VECELT(sv, vec, i, subset2);

    /* fall through to the standard default handler */
    SEXP idx, args, value;
    idx = GETSTACK_PTR(si);
    args = CONS_NR(idx, R_NilValue);
    args = CONS_NR(vec, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define	DFVE_NEXT() do {	\
	Evaluator::enableResultPrinting(true);	\
	CXXR_POP(1);		\
	NEXT();			\
    } while (0)

#define DO_VECSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - 2;				\
	R_bcstack_t *si = R_BCNodeStackTop - 1;				\
	SEXP vec = GETSTACK_PTR(sx);					\
	if (si->tag == INTSXP && (sub2 || FAST_VECELT_OK(vec))) {	\
	    R_xlen_t i = si->u.ival;					\
	    switch (TYPEOF(vec)) {					\
		case REALSXP:						\
		    if (i <= 0 || XLENGTH(vec) < i) break;		\
		    SETSTACK_REAL_PTR(sx, REAL_ELT(vec, i - 1));	\
		    DFVE_NEXT();					\
		case INTSXP:						\
		    if (i <= 0 || XLENGTH(vec) < i) break;		\
		    SETSTACK_INTEGER_PTR(sx, INTEGER_ELT(vec, i - 1));	\
		    DFVE_NEXT();					\
		case LGLSXP:						\
		    if (i <= 0 || XLENGTH(vec) < i) break;		\
		    SETSTACK_LOGICAL_PTR(sx, LOGICAL_ELT(vec, i - 1));	\
		    DFVE_NEXT();					\
		default:                                                \
		    break;                                              \
	    }								\
	}								\
	VECSUBSET_PTR(vec, si, sx, rho, constants, callidx, sub2);	\
	DFVE_NEXT();							\
    } while(0)

static R_INLINE SEXP getMatrixDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
	return dim;
    else return R_NilValue;
}

static R_INLINE SEXP getArrayDim(SEXP mat)
{
    SEXP attr = ATTRIB(mat);
    /* look for the common case of 'dim' as the only attribute first */
    SEXP dim = TAG(attr) == R_DimSymbol ? CAR(attr) :
	getAttrib(mat, R_DimSymbol);
    if (TYPEOF(dim) == INTSXP && LENGTH(dim) > 0)
	return dim;
    else return R_NilValue;
}

static R_INLINE R_xlen_t colMajorStackIndex(SEXP dim, int rank, R_bcstack_t *si)
{
    if (rank != LENGTH(dim))
    return -1;

    int *idim = INTEGER(dim);

    R_xlen_t mul = idim[0];
    R_xlen_t idx = bcStackIndex(si);

    if (idx < 1 || idx > idim[0])
	return -1;

    R_xlen_t k = idx - 1;
    for (int i = 1; i < rank; i++) {
	idx = bcStackIndex(si + i);
	if (idx < 1 || idx > idim[i])
	    return -1;
	k = k + mul * (idx - 1);
	mul = mul * idim[i];
    }
    return k;
}

static R_INLINE void MATSUBSET_PTR(R_bcstack_t *sx,
				   R_bcstack_t *si, R_bcstack_t *sj,
				   R_bcstack_t *sv, SEXP rho,
				   R_bcconsts_t consts, int callidx,
				   bool subset2)
{
    SEXP idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

    if (subset2 || FAST_VECELT_OK(mat)) {
	SEXP dim = getMatrixDim(mat);
	if (dim != R_NilValue) {
	    R_xlen_t i = bcStackIndex(si);
	    R_xlen_t j = bcStackIndex(sj);
	    R_xlen_t nrow = INTEGER(dim)[0];
	    R_xlen_t ncol = INTEGER(dim)[1];
	    if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		R_xlen_t k = i - 1 + nrow * (j - 1);
		DO_FAST_VECELT(sv, mat, k, subset2);
	    }
	}
    }

    /* fall through to the standard default handler */
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(jdx, R_NilValue);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_MATSUBSET(rho, sub2) do {					\
	int callidx = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - 3;				\
	MATSUBSET_PTR(sx, R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
		      sx, rho, constants, callidx, sub2);		\
	CXXR_POP(2);							\
	Evaluator::enableResultPrinting(true);				\
    } while (0)

static R_INLINE SEXP addStackArgsList(int n, R_bcstack_t *start, SEXP val)
{
    R_bcstack_t *p = start + n - 1;
    BCNPUSH(val); /* to protect */
    for (int i = 0; i < n; i++, p--) {
	val = CONS_NR(GETSTACK_PTR(p), val);
	SETSTACK(-1, val); /* to protect */
    }
    BCNPOP_IGNORE_VALUE();
    return val;
}

static R_INLINE SEXP getStackArgsList(int n, R_bcstack_t *start)
{
    return addStackArgsList(n, start, R_NilValue);
}

static R_INLINE void SUBSET_N_PTR(R_bcstack_t *sx, int rank,
				  R_bcstack_t *si, R_bcstack_t *sv,
				  SEXP rho, R_bcconsts_t consts, int callidx,
				  bool subset2)
{
    SEXP args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (subset2 || FAST_VECELT_OK(x)) {
	SEXP dim = getArrayDim(x);
	if (dim != R_NilValue) {
	    R_xlen_t k = colMajorStackIndex(dim, rank, si);
	    if (k >= 0)
		DO_FAST_VECELT(sv, x, k, subset2);
	}
    }

    /* fall through to the standard default handler */
    PROTECT(args = CONS_NR(x, getStackArgsList(rank, si)));
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    if (subset2)
	value = do_subset2_dflt(call, R_Subset2Sym, args, rho);
    else
	value = do_subset_dflt(call, R_SubsetSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, value);
}

#define DO_SUBSET_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - rank - 1;			\
	SUBSET_N_PTR(sx, rank, R_BCNodeStackTop - rank, sx, rho,	\
		     constants, callidx, sub2);				\
	CXXR_POP(rank);							\
	Evaluator::enableResultPrinting(true);				\
    } while (0)

static R_INLINE bool setElementFromScalar(SEXP vec, R_xlen_t i,
					      R_bcstack_t *srhs)
{
    if (i < 0) return false;

    R_bcstack_t vv;
    R_bcstack_t *v = bcStackScalar(srhs, &vv);

    if (TYPEOF(vec) == REALSXP) {
	if (XLENGTH(vec) <= i) return false;
	switch(v->tag) {
	case REALSXP: REAL(vec)[i] = v->u.dval; return true;
	case INTSXP: REAL(vec)[i] = INTEGER_TO_REAL(v->u.ival); return true;
	case LGLSXP: REAL(vec)[i] = LOGICAL_TO_REAL(v->u.lval); return true;
	}
    }
    else if (v->tag == TYPEOF(vec)) {
	switch(v->tag) {
	case INTSXP:
	    if (XLENGTH(vec) <= i) return false;
	    INTEGER(vec)[i] = v->u.ival;
	    return true;
	case LGLSXP:
	    if (XLENGTH(vec) <= i) return false;
	    LOGICAL(vec)[i] = INTEGER_TO_LOGICAL(v->u.lval);
	    return true;
	}
    }
    return false;
}

#define DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2) do {		\
	if (setElementFromScalar(vec, i, srhs)) {			\
	    SETSTACK_PTR(sv, vec);					\
	    SETTER_CLEAR_NAMED(vec);					\
	    return;							\
	}								\
	else if (subassign2 && TYPEOF(vec) == VECSXP &&			\
		 i < XLENGTH(vec)) {					\
	    SEXP rhs = GETSTACK_PTR(srhs);				\
	    if (rhs != R_NilValue) {					\
		if (MAYBE_REFERENCED(rhs) && VECTOR_ELT(vec, i) != rhs)	\
		    rhs = R_FixupRHS(vec, rhs);				\
		SET_VECTOR_ELT(vec, i, rhs);				\
		SETTER_CLEAR_NAMED(vec);				\
		SETSTACK_PTR(sv, vec);					\
		return;							\
	    }								\
	}								\
    } while (0)

static R_INLINE void VECSUBASSIGN_PTR(SEXP vec, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sv,
				      SEXP rho, R_bcconsts_t consts,
				      int callidx, bool subassign2)
{
    SEXP idx, args, value;

    R_xlen_t i = bcStackIndex(si) - 1;
    if (i >= 0)
	DO_FAST_SETVECELT(sv, srhs, vec,  i, subset2);

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS_NR(idx, args);
    args = CONS_NR(vec, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    MARK_ASSIGNMENT_CALL(call);
    if (subassign2)
	vec = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	vec = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, vec);
}

#define DFVA_NEXT(sx, vec) do {		\
	SETSTACK_PTR(sx, vec);		\
	SETTER_CLEAR_NAMED(vec);	\
	CXXR_POP(2);			\
	NEXT();				\
    } while (0)

#define DO_VECSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - 3;				\
	R_bcstack_t *srhs = R_BCNodeStackTop - 2;			\
	R_bcstack_t *si = R_BCNodeStackTop - 1;				\
	SEXP vec = GETSTACK_PTR(sx);					\
	if (MAYBE_SHARED(vec)) {					\
	    vec = shallow_duplicate(vec);				\
	    SETSTACK_PTR(sx, vec);					\
	}								\
	if (srhs->tag && si->tag == INTSXP &&				\
	    srhs->tag == TYPEOF(vec)) {					\
	    R_xlen_t i = si->u.ival;					\
	    /* i >= 0 rules out NA_INTEGER */				\
	    if (i > 0 && i <= XLENGTH(vec)) {				\
		switch (TYPEOF(vec)) {					\
		case REALSXP:						\
		    REAL(vec)[i - 1] = srhs->u.dval;			\
		    DFVA_NEXT(sx, vec);					\
		case INTSXP:						\
		    INTEGER(vec)[i - 1] = srhs->u.ival;			\
		    DFVA_NEXT(sx, vec);					\
		case LGLSXP:						\
		    LOGICAL(vec)[i - 1] = srhs->u.lval;			\
		    DFVA_NEXT(sx, vec);					\
		default:                                                \
		    break;                                              \
		}							\
	    }								\
	}								\
	VECSUBASSIGN_PTR(vec, srhs, si, sx, rho, constants, callidx, sub2); \
	CXXR_POP(2);							\
	NEXT();								\
    } while (0)

static R_INLINE void MATSUBASSIGN_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sj,
				      R_bcstack_t *sv,
				      SEXP rho, R_bcconsts_t consts,
				      int callidx, bool subassign2)
{
    SEXP dim, idx, jdx, args, value;
    SEXP mat = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(mat)) {
	mat = shallow_duplicate(mat);
	SETSTACK_PTR(sx, mat);
    }

    dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
	R_xlen_t i = bcStackIndex(si);
	R_xlen_t j = bcStackIndex(sj);
	R_xlen_t nrow = INTEGER(dim)[0];
	R_xlen_t ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    R_xlen_t k = i - 1 + nrow * (j - 1);
	    DO_FAST_SETVECELT(sv, srhs, mat,  k, subset2);
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    jdx = GETSTACK_PTR(sj);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    args = CONS_NR(jdx, args);
    args = CONS_NR(idx, args);
    args = CONS_NR(mat, args);
    PROTECT(args);
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    MARK_ASSIGNMENT_CALL(call);
    if (subassign2)
	mat = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	mat = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, mat);
}

#define DO_MATSUBASSIGN(rho, sub2) do {					\
	int callidx = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - 4;				\
	MATSUBASSIGN_PTR(sx, R_BCNodeStackTop - 3,			\
			 R_BCNodeStackTop - 2, R_BCNodeStackTop - 1,	\
			 sx, rho, constants, callidx, sub2);		\
	CXXR_POP(3);							\
    } while (0)

static R_INLINE void SUBASSIGN_N_PTR(R_bcstack_t *sx, int rank,
				     R_bcstack_t *srhs,
				     R_bcstack_t *si, R_bcstack_t *sv,
				     SEXP rho, R_bcconsts_t consts,
				     int callidx, bool subassign2)
{
    SEXP dim, args, value;
    SEXP x = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(x)) {
	x = shallow_duplicate(x);
	SETSTACK_PTR(sx, x);
    }

    dim = getArrayDim(x);

    if (dim != R_NilValue) {
	R_xlen_t k = colMajorStackIndex(dim, rank, si);
	if (k >= 0)
	    DO_FAST_SETVECELT(sv, srhs, x,  k, subset2);
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    args = CONS_NR(value, R_NilValue);
    SET_TAG(args, R_valueSym);
    PROTECT(args = CONS_NR(x, addStackArgsList(rank, si, args)));
    SEXP call = callidx < 0 ? R_NilValue : GETCONST(consts, callidx);
    MARK_ASSIGNMENT_CALL(call);
    if (subassign2)
	x = do_subassign2_dflt(call, R_Subassign2Sym, args, rho);
    else
	x = do_subassign_dflt(call, R_SubassignSym, args, rho);
    UNPROTECT(1);
    SETSTACK_PTR(sv, x);
}

#define DO_SUBASSIGN_N(rho, sub2) do {					\
	int callidx = GETOP();						\
	int rank = GETOP();						\
	R_bcstack_t *sx = R_BCNodeStackTop - rank - 2;			\
	SUBASSIGN_N_PTR(sx, rank, R_BCNodeStackTop - rank - 1,		\
			R_BCNodeStackTop - rank, sx, rho,		\
			constants, callidx, sub2);			\
	CXXR_POP((rank + 1));						\
    } while (0)

/* rho is only needed for _R_CHECK_LENGTH_1_LOGIC2_ */
#define FIXUP_SCALAR_LOGICAL(rho, callidx, arg, op, warn_level) do {	\
	if (R_BCNodeStackTop[-1].tag == LGLSXP) break;			\
	SEXP val = GETSTACK(-1);					\
	if (IS_SIMPLE_SCALAR(val, LGLSXP))				\
	    SETSTACK(-1, ScalarLogical(SCALAR_LVAL(val)));		\
	else {								\
	    if (!isNumber(val))						\
		errorcall(GETCONST(constants, callidx),			\
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    SETSTACK(-1, ScalarLogical(asLogical2(			\
					   val, /*checking*/ 1,		\
					   GETCONST(constants, callidx) \
					   )));				\
	}								\
    } while(0)

static void signalMissingArgError(SEXP args, SEXP call)
{
    SEXP a, c;
    int n, k;
    for (a = args, n = 1; a != R_NilValue; a = CDR(a), n++)
	if (CAR(a) == R_MissingArg) {
	    /* check for an empty argument in the call -- start from
	       the beginning in case of ... arguments */
	    if (call != R_NilValue) {
		for (k = 1, c = CDR(call); c != R_NilValue; c = CDR(c), k++)
		    if (CAR(c) == R_MissingArg)
			errorcall(call, _("argument %d is empty"), k);
	    }
	    /* An error from evaluating a symbol will already have
	       been signaled.  The interpreter, in evalList, does
	       _not_ signal an error for a call expression that
	       produces an R_MissingArg value; for example

		   c(alist(a=)$a)

	       does not signal an error. If we decide we do want an
	       error in this case we can modify evalList for the
	       interpreter and here use the code below. */
#ifdef NO_COMPUTED_MISSINGS
	    /* otherwise signal a 'missing argument' error */
	    errorcall(call, _("argument %d is missing"), n);
#endif
	}
}

static R_INLINE void checkForMissings(SEXP args, SEXP call)
{
    bool found = false;
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
	if (CAR(a) == R_MissingArg) {
	    found = true;
	    break;
	}
    if (found)
	signalMissingArgError(args, call);
}

typedef struct {
    R_xlen_t idx, len;
    unsigned int type;
    /* Include the symbol in the loopinfo structure in case the
       binding cell is R_NilValue, e.g. for an active binding. Even if
       we eventually allow symbols to be garbage collected, the loop
       symbol is GC protected during the loop evaluation by its
       reference from the current byte code object. */
    SEXP symbol;
} R_loopinfo_t;

#define FOR_LOOP_STATE_SIZE 5
#define GET_FOR_LOOP_INFO() ((R_loopinfo_t *) RAW0(GETSTACK_SXPVAL(-2)))
#define GET_FOR_LOOP_BINDING() GETSTACK_SXPVAL(-3)
#define GET_FOR_LOOP_SEQ() GETSTACK_SXPVAL(-4)
#define SET_FOR_LOOP_SEQ(v) SETSTACK(-4, v);
#define SET_FOR_LOOP_BCPROT_OFFSET(v) SETSTACK_INTEGER(-5, (int) (v))
#define GET_FOR_LOOP_BCPROT_OFFSET() GETSTACK_IVAL_PTR(R_BCNodeStackTop - 5)
#define INSERT_FOR_LOOP_BCPROT_OFFSET() do {				\
	/* insert space for the BCProt offset below the sequence */	\
	BCNSTACKCHECK(1);						\
	BCNPUSH_NODE(R_BCNodeStackTop[-1]);				\
	SETSTACK_INTEGER(-2, 0);					\
    } while (0)

#define GET_VEC_LOOP_VALUE(var) do {			\
	(var) = GETSTACK_SXPVAL(-1);			\
	if (BNDCELL_TAG(cell) ||			\
	    (var) != CAR(cell) || MAYBE_SHARED(var) ||	\
	    ATTRIB(var) != R_NilValue) {		\
	    (var) = allocVector(TYPEOF(seq), 1);	\
	    SETSTACK_NLNK(-1, var);			\
	    INCREMENT_NAMED(var);			\
	}						\
    } while (0)

/* This uses use loopinfo->symbol in case cell is R_NilValue, e.g. for
   an active binding. */
#define SET_FOR_LOOP_VAR(value, cell, loopinfo, rho) do {	\
	if (BNDCELL_UNBOUND(cell) ||				\
	    !SET_BINDING_VALUE(cell, value))			\
	    defineVar(loopinfo->symbol, value, rho);		\
    } while (0)

/* Check whether a call is to a base function; if not use AST interpreter */
/***** need a faster guard check */
static R_INLINE SEXP SymbolValue(SEXP sym)
{
    if (IS_ACTIVE_BINDING(sym))
	return Evaluator::evaluate(sym, Environment::base());
    else {
	SEXP value = SYMVALUE(sym);
	if (Promise::isA(value)) {
	    if (PROMISE_IS_EVALUATED(value))
		value = PRVALUE(value);
	    else
		value = Evaluator::evaluate(sym, Environment::base());
	}
	return value;
    }
}

#define DO_BASEGUARD() do {				\
	SEXP expr = GETCONST(constants, GETOP());	\
	int label = GETOP();				\
	SEXP sym = CAR(expr);				\
	if (findFun(sym, rho) != SymbolValue(sym)) {	\
	    BCNPUSH(eval(expr, rho));			\
	    pc = codebase + label;			\
	}						\
    } while (0)

/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].m_eval % 100 )/10 == 0)

/* rho only needed for _R_CHECK_LENGTH_1_CONDITION_=package:name */
static R_INLINE bool GETSTACK_LOGICAL_NO_NA_PTR(R_bcstack_t *s, int callidx,
						    R_bcconsts_t constants,
						    SEXP rho)
{
    if (s->tag == LGLSXP && s->u.lval != NA_LOGICAL)
	return s->u.lval;

    SEXP value = GETSTACK_PTR(s);
    if (IS_SCALAR(value, LGLSXP)) {
	int lval = SCALAR_LVAL(value);
	if (lval != NA_LOGICAL)
	    return lval;
    }
    SEXP call = GETCONST(constants, callidx);
    PROTECT(value);
    bool ans = asLogicalNoNA2(value, call);
    UNPROTECT(1);
    return ans;
}

#define GETSTACK_LOGICAL(n) GETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (n))
static R_INLINE int GETSTACK_LOGICAL_PTR(R_bcstack_t *s)
{
    if (s->tag == LGLSXP) return (Rboolean) (s->u.lval);
    SEXP value = GETSTACK_PTR(s);
    return SCALAR_LVAL(value);
}

/* Find locations table in the constant pool */
static SEXP findLocTable(SEXP constants, const char *tclass)
{
    /* location tables are at the end of the constant pool */
    for (int i = LENGTH(constants) - 1; i >= 0 ; i--) {
	SEXP s = VECTOR_ELT(constants, i);
	/* could use exact check instead of inherits */
	if (TYPEOF(s) == INTSXP && inherits(s, tclass))
	    return s;
    }
    return R_NilValue;
}

/* Get a constant pool entry through locations table element */
static SEXP getLocTableElt(ptrdiff_t relpc, SEXP table, SEXP constants)
{
    if (table == R_NilValue || relpc >= LENGTH(table) || relpc < 0)
	return R_NilValue;

    int cidx = INTEGER(table)[relpc];
    if (cidx < 0 || cidx >= LENGTH(constants))
	return R_NilValue;
    return VECTOR_ELT(constants, cidx);
}

ptrdiff_t ByteCode::codeDistane(SEXP body, void *bcpc)
{
    BCODE *codebase = BCCODE(body);
    BCODE *target = static_cast<BCODE *>(bcpc);
    return std::distance(codebase, target);
}

attribute_hidden ptrdiff_t R::R_BCRelPC(SEXP body, void *current_pc)
{
    /* used to capture the pc offset from its codebase at the time a
       context is created */
    if (body && current_pc)
	return ByteCode::codeDistane(body, current_pc);
    else
	return -1;
}

/* Return the srcref/expression for the current instruction/operand
   being executed by the byte-code interpreter, or the one that was
   current when the supplied context was created. */
static SEXP R_findBCInterpreterLocation(RCNTXT *cptr, const char *iname)
{
    SEXP body = cptr ? cptr->bcbody.get() : R_BCbody.get();
    if (body == NULL)
	/* This has happened, but it is not clear how. */
	/* (R_Srcref == R_InBCInterpreter && R_BCbody == NULL) */
	return R_NilValue;
    SEXP constants = BCODE_CONSTS(body);
    SEXP ltable = findLocTable(constants, iname);
    if (ltable == R_NilValue)
	/* location table not available */
	return R_NilValue;

    /* use relpc stored in the context if available */
    if (cptr && cptr->relpc > 0)
	return getLocTableElt(cptr->relpc, ltable, constants);

    ptrdiff_t relpc = ByteCode::codeDistane(body, cptr ? cptr->bcpc : R_BCpc);

    return getLocTableElt(relpc, ltable, constants);
}

attribute_hidden SEXP R::R_findBCInterpreterSrcref(RCNTXT *cptr)
{
    return R_findBCInterpreterLocation(cptr, "srcrefsIndex");
}

static SEXP R_findBCInterpreterExpression(void)
{
    return R_findBCInterpreterLocation(NULL, "expressionsIndex");
}

attribute_hidden SEXP R::R_getCurrentSrcref(void)
{
    if (R_Srcref != R_InBCInterpreter)
	return R_Srcref;
    else
	return R_findBCInterpreterSrcref(NULL);
}

static bool maybeClosureWrapper(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return false;

    SEXP sym = CAR(expr);

    if (!(sym == R_DotInternalSym || sym == R_DotExternalSym ||
	sym == R_DotExternal2Sym || sym == R_DotExternalgraphicsSym ||
	sym == R_DotCallSym || sym == R_DotFortranSym ||
	sym == R_DotCSym || sym == R_DotCallgraphicsSym))

	return false;

    return CDR(expr) != R_NilValue && CADR(expr) != R_NilValue;
}

static bool maybeAssignmentCall(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return false;

    if (TYPEOF(CAR(expr)) != SYMSXP)
	return false;
    const char *name = CHAR(PRINTNAME(CAR(expr)));
    size_t slen = strlen(name);
    return slen > 2 && name[slen-2] == '<' && name[slen-1] == '-';
}

/* Check if the given expression is a call to a name that is also
   a builtin or special (does not search the environment!). */
static bool maybePrimitiveCall(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP)
	return false;

    if (TYPEOF(CAR(expr)) == SYMSXP) {
	SEXP value = SYMVALUE(CAR(expr));
	if (TYPEOF(value) == PROMSXP)
	    value = PRVALUE(value);
	return Rf_isPrimitive(value);
    }
    return false;
}

/* Inflate a (single-level) compiler-flattened assignment call.
   For example,
           `[<-`(x, c(-1, 1), value = 2)
   becomes
            x[c(-1,1)] <- 2 */
static SEXP inflateAssignmentCall(SEXP expr) {
    if (CDR(expr) == R_NilValue || CDDR(expr) == R_NilValue)
	return expr; /* need at least two arguments */

    SEXP assignForm = CAR(expr);
    if (TYPEOF(assignForm) != SYMSXP)
	return expr;
    const char *name = CHAR(PRINTNAME(assignForm));
    size_t slen = strlen(name);
    if (slen <= 2 || name[slen - 2] != '<' || name[slen - 1] != '-')
	return expr;

    /* not using strncpy as that produces warnings with gcc about bound
       depending on the length of the source argument */
    std::string nonAssignName(name); /* "names" for "names<-" */
    nonAssignName = nonAssignName.erase(nonAssignName.size() - 2);
    SEXP nonAssignForm = Symbol::obtain(nonAssignName);

    int nargs = length(expr) - 2;
    SEXP lhs = Rf_allocLang(nargs + 1);
    SETCAR(lhs, nonAssignForm);

    SEXP porig = CDR(expr);
    SEXP pnew = CDR(lhs);

    /* copy args except the last - the "value" */
    while(CDR(porig) != R_NilValue) {
	SETCAR(pnew, CAR(porig));
	ENSURE_NAMEDMAX(CAR(porig));
	porig = CDR(porig);
	pnew = CDR(pnew);
    }
    SEXP rhs = CAR(porig);
    ENSURE_NAMEDMAX(rhs);
    if (TAG(porig) != R_valueSym)
	return expr;
    return lang3(R_AssignSym, lhs, rhs);
}

/* Get the current expression being evaluated by the byte-code interpreter. */
attribute_hidden SEXP R::R_getBCInterpreterExpression(void)
{
    SEXP exp = R_findBCInterpreterExpression();
    if (Promise::isA(exp)) {
	exp = SEXP_downcast<Promise *>(exp)->force();
    }

    /* This tries to mimick the behavior of the AST interpreter to a
       reasonable level, based on relatively consistent expressions
       provided by the compiler in the constant pool. The AST
       interpreter behavior is rather inconsistent and should be fixed
       at some point. When this happens, the code below will have to
       be revisited, but the compiler code should mostly stay the
       same.

       Currently this code attempts to bypass implementation of
       closure wrappers for internals and other foreign functions
       called via a directive, hide away primitives, but show
       assignment calls. This code ignores less usual problematic
       situations such as overriding of builtins or inlining of the
       wrappers by the compiler. Simple assignment calls are inflated
       (back) into the usual form like x[1] <- y. Expressions made of
       a single symbol are hidden away (note these are e.g. for
       missing function arguments). */

    if (maybeAssignmentCall(exp)) {
	exp = inflateAssignmentCall(exp);
    } else if (TYPEOF(exp) == SYMSXP || maybeClosureWrapper(exp)
	|| maybePrimitiveCall(exp)) {

	RCNTXT *c = R_GlobalContext;
        while(c && !isTopLevelContext(c)) {
	    if (c->callflag & CTXT_FUNCTION) {
		exp = c->call;
		break;
	    }
	    c = c->nextcontext;
	}
    }
    return exp;
}

static SEXP markSpecialArgs(SEXP args)
{
    for (SEXP arg = args; arg != R_NilValue; arg = CDR(arg))
        MARK_NOT_MUTABLE(CAR(arg));
    return args;
}

attribute_hidden bool R::R_BCVersionOK(SEXP s)
{
    if (TYPEOF(s) != BCODESXP)
	return false;

    BCODE *pc = BCCODE(s);
    int version = GETOP();

    return (version >= R_bcMinVersion && version <= R_bcVersion);
}

struct bcEval_globals {
    R_bcstack_t *oldntop;
    bool oldbcintactive;
    SEXP oldbcbody;
    void *oldbcpc;
    R_bcFrame_type *oldbcframe;
    SEXP oldsrcref;
#ifdef BC_PROFILING
    int old_current_opcode;
#endif
    R_bcstack_t *old_bcprot_top;
    R_bcstack_t *old_bcprot_committed; // **** not sure this is really needed
    int oldevdepth;
};

static R_INLINE void save_bcEval_globals(struct bcEval_globals *g)
{
    g->oldntop = R_BCNodeStackTop;
    g->oldbcintactive = Evaluator::bcActive();
    g->oldbcbody = R_BCbody;
    g->oldbcpc = R_BCpc;
    g->oldbcframe = R_BCFrame;
    g->oldsrcref = R_Srcref;
#ifdef BC_PROFILING
    g->old_current_opcode = s_current_opcode;
#endif
    g->old_bcprot_top = R_BCProtTop;
    g->old_bcprot_committed = R_BCProtCommitted;
    g->oldevdepth = StackChecker::depth();
    INCREMENT_BCSTACK_LINKS();
}

static R_INLINE void restore_bcEval_globals(struct bcEval_globals *g)
{
    R_BCNodeStackTop = R_BCProtTop;
    DECREMENT_BCSTACK_LINKS(g->old_bcprot_top);
    StackChecker::setDepth(g->oldevdepth);
    R_BCProtCommitted = g->old_bcprot_committed;
    R_BCNodeStackTop = g->oldntop;
    Evaluator::enableBCActive(g->oldbcintactive);
    R_BCbody = g->oldbcbody;
    R_BCpc = g->oldbcpc;
    R_BCFrame = g->oldbcframe;
    R_Srcref = g->oldsrcref;
#ifdef BC_PROFILING
    s_current_opcode = g->old_current_opcode;
#endif
}

struct CXXR::bcEval_locals {
    // bcEval args:
    SEXP body;
    SEXP rho;
    // local variables
    R_binding_cache_t vcache;
    bool smallcache;
    bool m_useCache;
    BCODE *pc;
};

#define SAVE_BCEVAL_LOCALS(loc) do {		\
	(loc)->body = body;			\
	(loc)->rho = rho;			\
	(loc)->vcache = vcache;			\
	(loc)->m_useCache = useCache;		\
	(loc)->smallcache = smallcache;		\
	(loc)->pc = pc;				\
    } while (0)

#define RESTORE_BCEVAL_LOCALS(loc) do {		\
	body = (loc)->body;			\
	codebase = BCCODE(body);		\
	constants = BCCONSTS(body);		\
	rho = (loc)->rho;			\
	useCache = (loc)->m_useCache;		\
	vcache = (loc)->vcache;			\
	smallcache = (loc)->smallcache;		\
	pc = (loc)->pc;				\
    } while (0)

struct CXXR::R_bcFrame {
    struct bcEval_globals globals;
    struct bcEval_locals locals;
    struct { SEXP promise; } promvars;
};

#define BCFRAME_LOCALS() (&(R_BCFrame->locals))
#define BCFRAME_GLOBALS() (&(R_BCFrame->globals))
#define BCFRAME_PROMISE() (R_BCFrame->promvars.promise)
#define SET_BCFRAME_PROMISE(val) (R_BCFrame->promvars.promise = (val))

/* Allocate activation frame for inline calls on the node stack */
static R_INLINE R_bcFrame_type *PUSH_BCFRAME()
{
    R_bcstack_t *oldtop = R_BCNodeStackTop;
    R_bcFrame_type *rec = (R_bcFrame_type *) BCNALLOC(sizeof(R_bcFrame_type));
    save_bcEval_globals(&(rec->globals));
    /* modify saved stack top to the value before pushing the frame */
    rec->globals.oldntop = oldtop; // must come after save_bcEval_globals!!
    return rec;
}

static R_INLINE R_bcstack_t POP_BCFRAME()
{
    R_bcstack_t val = R_BCNodeStackTop[-1];
    restore_bcEval_globals(BCFRAME_GLOBALS());
    return val;
}

struct vcache_info
{
    R_binding_cache_t vcache;
    bool smallcache;
    vcache_info(R_binding_cache_t vc, bool val) : vcache(vc), smallcache(val)
    {
    }
};

static R_INLINE struct vcache_info setup_vcache(SEXP body, bool useCache)
{
    R_binding_cache_t vcache = NULL;
    bool smallcache = true;

#ifdef USE_BINDING_CACHE
    if (useCache) {
    R_xlen_t n = BCCONSTS_LEN(body);
# ifdef CACHE_MAX
    if (n > CACHE_MAX) {
	n = CACHE_MAX;
	smallcache = false;
    }
# endif
# ifdef CACHE_ON_STACK
    /* initialize binding cache on the stack */
    BCNSTACKCHECK(n + 1);
    BCNPUSH_CACHE(n);
    vcache = R_BCNodeStackTop;
    while (n > 0) {
	BCNPUSH_NLNK(R_NilValue);
	n--;
    }
# else
    /* allocate binding cache and protect on stack */
    vcache = ListVector::create(n);
    BCNPUSH(vcache);
# endif
    }
    else smallcache = false;
#endif
    R_BCProtTop = R_BCNodeStackTop;

    return vcache_info(vcache, smallcache);
}

static R_INLINE struct bcEval_locals bcode_setup_locals(SEXP body, SEXP rho, bool useCache)
{
    struct bcEval_locals loc;
    loc.body = body;
    loc.rho = rho;
    loc.pc = BCCODE(body) + 1; /* pop off version */
    loc.m_useCache = useCache;
    struct vcache_info vcinfo = setup_vcache(body, useCache);
    loc.vcache = vcinfo.vcache;
    loc.smallcache = vcinfo.smallcache;
    R_BCbody = body; //**** move this somewhere else?
    return loc;
}

static R_INLINE struct bcEval_locals setup_bcframe_prom(SEXP prom, bool useCache)
{
    PROTECT(prom);
    SET_PRSEEN(prom, UNDER_EVALUATION);
    R_BCFrame = PUSH_BCFRAME();
    INCREMENT_EVAL_DEPTH();
    SET_BCFRAME_PROMISE(prom);
    Evaluator::enableResultPrinting(true);
    return bcode_setup_locals(PRCODE(prom), PRENV(prom), useCache);
}

#define START_BCFRAME_PROM(prom) do {				\
	struct bcEval_locals locals =				\
	    setup_bcframe_prom(prom, useCache);			\
	SAVE_BCEVAL_LOCALS(BCFRAME_LOCALS());			\
	RESTORE_BCEVAL_LOCALS(&locals);				\
    } while (0)

static R_INLINE void finish_force_promise(void)
{
    SEXP prom = BCFRAME_PROMISE();
    R_bcstack_t ubval = POP_BCFRAME();
    BCNPUSH_STACKVAL(ubval); /* push early to protect */
    SET_PROMISE_VALUE_FROM_STACKVAL(prom, ubval);
    SET_PRSEEN(prom, DEFAULT);
    SET_PRENV(prom, R_NilValue);
    UNPROTECT(1); /* prom */
}

#define DO_GETVAR_FORCE_PROMISE_RETURN() do {			\
	RESTORE_BCEVAL_LOCALS(BCFRAME_LOCALS());		\
	finish_force_promise();					\
	NEXT();							\
    } while (0)

SEXP ByteCode::interpret(SEXP body, SEXP rho, bool useCache)
{
  /* check version and allow bytecode to be disabled for testing */
  if (ByteCode::ByteCodeDisabled() || !R_BCVersionOK(body))
      return eval(bytecodeExpr(body), rho);

  struct bcEval_globals globals;
  save_bcEval_globals(&globals);

  R_Srcref = R_InBCInterpreter;
  Evaluator::enableBCActive(true);

  R_BCFrame = NULL;

  struct bcEval_locals locals = bcode_setup_locals(body, rho, useCache);
  SEXP value = bcEval_loop(&locals);
  restore_bcEval_globals(&globals);
  return value;
}

SEXP ByteCode::bcEval_loop(struct bcEval_locals *ploc)
{
  struct bcEval_locals locals;
  SEXP body, rho;
  R_bcconsts_t constants;
  BCODE *pc, *codebase;
  R_binding_cache_t vcache;
  bool smallcache;
  bool useCache;
  BCODE *currentpc = NULL;
  void *oldbcpc = NULL;
#ifndef BC_PROFILING
  int which = 0;
#endif

  INITIALIZE_MACHINE();

  locals = *ploc;

  RESTORE_BCEVAL_LOCALS(&locals);

  currentpc = NULL;
  oldbcpc = R_BCpc;

  static int s_evalcount = 0;
  BC_CHECK_SIGINT();

  BEGIN_MACHINE {
    OP(BCMISMATCH, 0): error("%s", _("byte code version mismatch"));
    OP(RETURN, 0):
      if (R_BCFrame == NULL) {
	  R_BCpc = oldbcpc;
	  SEXP retvalue = GETSTACK(-1);
	  return retvalue;
      }
      else
	  DO_GETVAR_FORCE_PROMISE_RETURN();
    OP(GOTO, 1):
      {
	int label = GETOP();
	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(BRIFNOT, 2):
      {
	int callidx = GETOP();
	int label = GETOP();
	bool cond = GETSTACK_LOGICAL_NO_NA_PTR(R_BCNodeStackTop - 1,
						   callidx, constants, rho);
	BCNPOP_IGNORE_VALUE();
	if (!cond) {
	    BC_CHECK_SIGINT(); /**** only on back branch?*/
	    pc = codebase + label;
	}
	NEXT();
      }
    OP(POP, 0): BCNPOP_IGNORE_VALUE(); NEXT();
    OP(DUP, 0): BCNDUP(); NEXT();
    OP(PRINTVALUE, 0): PrintValue(BCNPOP()); NEXT();
    OP(STARTLOOPCNTXT, 2):
      {
	SKIP_OP(); // skip dummy operand - needed to keep the same number (2) of arguments needed by STARTLOOPCNTXT
	SEXP code = GETCONST(constants, GETOP());
	loopWithContext(code, rho);
	NEXT();
      }
    OP(ENDLOOPCNTXT, 1):
      {
	SKIP_OP(); // skip dummy operand - needed to keep the same number (1) of arguments needed by ENDLOOPCNTXT
	return R_NilValue;
      }
    OP(DOLOOPNEXT, 0):
      {
	findcontext(CTXT_NEXT, rho, R_NilValue);
      }
    OP(DOLOOPBREAK, 0):
      {
	findcontext(CTXT_BREAK, rho, R_NilValue);
      }
    OP(STARTFOR, 3):
      {
	bool iscompact = false;
	SEXP seq = getForLoopSeq(-1, &iscompact);
	int callidx = GETOP();
	SEXP symbol = GETCONST(constants, GETOP());
	int label = GETOP();

	INSERT_FOR_LOOP_BCPROT_OFFSET();

	/* if we are iterating over a factor, coerce to character first */
	if (inherits(seq, "factor")) {
	    seq = asCharacterFactor(seq);
	    SETSTACK(-1, seq);
	}

	defineVar(symbol, R_NilValue, rho);
	BCNPUSH(GET_BINDING_CELL(symbol, rho));

	SEXP value = RawVector::create(sizeof(R_loopinfo_t));
	R_loopinfo_t *loopinfo = (R_loopinfo_t *) RAW0(value);
	loopinfo->idx = -1;
#ifdef COMPACT_INTSEQ
	if (iscompact) {
	    int n1 = INTEGER(seq)[0];
	    int n2 = INTEGER(seq)[1];
	    loopinfo->len = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
	}
	else
#endif
	if (isVector(seq))
	  loopinfo->len = XLENGTH(seq);
	else if (isList(seq) || isNull(seq))
	  loopinfo->len = length(seq);
	else errorcall(GETCONST(constants, callidx), "%s",
		       _("invalid for() loop sequence"));
#ifdef COMPACT_INTSEQ
	loopinfo->type = iscompact ? INTSEQSXP : TYPEOF(seq);
#else
	loopinfo->type = TYPEOF(seq);
#endif
	loopinfo->symbol = symbol;
	BCNPUSH(value);

	/* bump up links count of seq to avoid modification by loop code */
	INCREMENT_LINKS(seq);

	/* place initial loop variable value object on stack */
	switch(TYPEOF(seq)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    value = allocVector(TYPEOF(seq), 1);
	    INCREMENT_NAMED(value);
	    BCNPUSH_NLNK(value);
	    break;
	default: BCNPUSH(R_NilValue);
	}
	/* the seq, binding cell, and value on the stack are now boxed */

	SET_FOR_LOOP_BCPROT_OFFSET(R_BCProtTop - R_BCNodeStackBase);
	INCLNK_stack(R_BCNodeStackTop);

	BC_CHECK_SIGINT();
	pc = codebase + label;
	NEXT();
      }
    OP(STEPFOR, 1):
      {
	int label = GETOP();
	R_loopinfo_t *loopinfo = GET_FOR_LOOP_INFO();
	R_xlen_t i = ++(loopinfo->idx);
	R_xlen_t n = loopinfo->len;
	if (i < n) {
	  BC_CHECK_SIGINT_LOOP(i);
	  pc = codebase + label;
	  int type = loopinfo->type;
	  SEXP seq = GET_FOR_LOOP_SEQ();
	  SEXP cell = GET_FOR_LOOP_BINDING();
	  SEXP value = NULL;
	  switch (type) {
	  case REALSXP:
	    if (BNDCELL_TAG_WR(cell) == REALSXP) {
		SET_BNDCELL_DVAL(cell,  REAL_ELT(seq, i));
		NEXT();
	    }
	    if (BNDCELL_WRITABLE(cell)) {
		NEW_BNDCELL_DVAL(cell, REAL_ELT(seq, i));
		NEXT();
	    }
	    GET_VEC_LOOP_VALUE(value);
	    SET_SCALAR_DVAL(value, REAL_ELT(seq, i));
	    SET_FOR_LOOP_VAR(value, cell, loopinfo, rho);
	    NEXT();
	  case INTSXP:
	    if (BNDCELL_TAG_WR(cell) == INTSXP) {
		SET_BNDCELL_IVAL(cell, INTEGER_ELT(seq, i));
		NEXT();
	    }
	    if (BNDCELL_WRITABLE(cell)) {
		NEW_BNDCELL_IVAL(cell, INTEGER_ELT(seq, i));
		NEXT();
	    }
	    GET_VEC_LOOP_VALUE(value);
	    SET_SCALAR_IVAL(value, INTEGER_ELT(seq, i));
	    SET_FOR_LOOP_VAR(value, cell, loopinfo, rho);
	    NEXT();
#ifdef COMPACT_INTSEQ
	  case INTSEQSXP:
	    {
		int *info = INTEGER(seq);
		int n1 = info[0];
		int n2 = info[1];
		int ii = (int) i;
		int ival = n1 <= n2 ? n1 + ii : n1 - ii;
		if (BNDCELL_TAG_WR(cell) == INTSXP) {
		    SET_BNDCELL_IVAL(cell,  ival);
		    NEXT();
		}
		if (BNDCELL_WRITABLE(cell)) {
		    NEW_BNDCELL_IVAL(cell, ival);
		    NEXT();
		}
		GET_VEC_LOOP_VALUE(value);
		SET_SCALAR_IVAL(value, ival);
		SET_FOR_LOOP_VAR(value, cell, loopinfo, rho);
		NEXT();
	    }
#endif
	  case LGLSXP:
	    if (BNDCELL_TAG_WR(cell) == LGLSXP) {
		SET_BNDCELL_LVAL(cell,  LOGICAL_ELT(seq, i));
		NEXT();
	    }
	    if (BNDCELL_WRITABLE(cell)) {
		NEW_BNDCELL_LVAL(cell, LOGICAL_ELT(seq, i));
		NEXT();
	    }
	    GET_VEC_LOOP_VALUE(value);
	    SET_SCALAR_LVAL(value, LOGICAL_ELT(seq, i));
	    SET_FOR_LOOP_VAR(value, cell, loopinfo, rho);
	    NEXT();
	  case CPLXSXP:
	    GET_VEC_LOOP_VALUE(value);
	    SET_SCALAR_CVAL(value, COMPLEX_ELT(seq, i));
	    break;
	  case STRSXP:
	    GET_VEC_LOOP_VALUE(value);
	    SET_STRING_ELT(value, 0, STRING_ELT(seq, i));
	    break;
	  case RAWSXP:
	    GET_VEC_LOOP_VALUE(value);
	    SET_SCALAR_BVAL(value, RAW(seq)[i]);
	    break;
	  case EXPRSXP:
	    value = XVECTOR_ELT(seq, i);
	    ENSURE_NAMEDMAX(value);
	    break;
	  case VECSXP:
	    value = VECTOR_ELT(seq, i);
	    ENSURE_NAMEDMAX(value);
	    break;
	  case LISTSXP:
	    value = CAR(seq);
	    SET_FOR_LOOP_SEQ(CDR(seq));
	    ENSURE_NAMEDMAX(value);
	    break;
	  default:
	    error("%s", _("invalid sequence argument in for loop"));
	  }
	  SET_FOR_LOOP_VAR(value, cell, loopinfo, rho);
	}
	NEXT();
      }
    OP(ENDFOR, 0):
      {
	size_t offset = GET_FOR_LOOP_BCPROT_OFFSET();
	DECLNK_stack(R_BCNodeStackBase + offset);
	SEXP seq = GET_FOR_LOOP_SEQ();
	DECREMENT_LINKS(seq);
	CXXR_POP(FOR_LOOP_STATE_SIZE - 1);
	SETSTACK(-1, R_NilValue);
	NEXT();
      }
    OP(SETLOOPVAL, 0):
      BCNPOP_IGNORE_VALUE(); SETSTACK(-1, R_NilValue); NEXT();
    OP(INVISIBLE,0): Evaluator::enableResultPrinting(false); NEXT();
    OP(LDCONST, 1):
      {
	Evaluator::enableResultPrinting(true);
	SEXP value = GETCONST(constants, GETOP());
	SEXPTYPE type = TYPEOF(value);
	switch(type) {
	case REALSXP:
	    if (IS_SIMPLE_SCALAR(value, REALSXP)) {
		BCNPUSH_REAL(REAL0(value)[0]);
		NEXT();
	    }
	    break;
	case INTSXP:
	    if (IS_SIMPLE_SCALAR(value, INTSXP)) {
		BCNPUSH_INTEGER(INTEGER0(value)[0]);
		NEXT();
	    }
	    break;
	case LGLSXP:
	    if (IS_SIMPLE_SCALAR(value, LGLSXP)) {
		BCNPUSH_LOGICAL(LOGICAL0(value)[0]);
		NEXT();
	    }
	    break;
	default:
	    break;
	}
	if (R_check_constants < 0)
	    value = duplicate(value);
	MARK_NOT_MUTABLE(value);
	BCNPUSH(value);
	NEXT();
      }
    OP(LDNULL, 0): Evaluator::enableResultPrinting(true); BCNPUSH(R_NilValue); NEXT();
    OP(LDTRUE, 0): Evaluator::enableResultPrinting(true); BCNPUSH_LOGICAL(TRUE); NEXT();
    OP(LDFALSE, 0): Evaluator::enableResultPrinting(true); BCNPUSH_LOGICAL(FALSE); NEXT();
    OP(GETVAR, 1): DO_GETVAR(false, false);
    OP(DDVAL, 1): DO_GETVAR(true, false);
    OP(SETVAR, 1):
      {
	int sidx = GETOP();
	SEXP loc;
	if (smallcache)
	    loc = GET_SMALLCACHE_BINDING_CELL(vcache, sidx);
	else {
	    SEXP symbol = GETCONST(constants, sidx);
	    loc = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	}

	R_bcstack_t *s = R_BCNodeStackTop - 1;
	SEXPTYPE tag = (SEXPTYPE) (s->tag);

	if (tag == BNDCELL_TAG_WR(loc))
	    switch (tag) {
	    case REALSXP: SET_BNDCELL_DVAL(loc, s->u.dval); NEXT();
	    case INTSXP: SET_BNDCELL_IVAL(loc, s->u.ival); NEXT();
	    case LGLSXP: SET_BNDCELL_LVAL(loc, s->u.lval); NEXT();
	    default: break;
	    }
	else if (BNDCELL_WRITABLE(loc))
	    switch (tag) {
	    case REALSXP: NEW_BNDCELL_DVAL(loc, s->u.dval); NEXT();
	    case INTSXP: NEW_BNDCELL_IVAL(loc, s->u.ival); NEXT();
	    case LGLSXP: NEW_BNDCELL_LVAL(loc, s->u.lval); NEXT();
	    default: break;
	    }

	SEXP value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	if (!SET_BINDING_VALUE(loc, value)) {
	    SEXP symbol = GETCONST(constants, sidx);
	    PROTECT(value);
	    defineVar(symbol, value, rho);
	    UNPROTECT(1);
	}
	NEXT();
      }
    OP(GETFUN, 1):
      {
	/* get the function */
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = findFun(symbol, rho);
	INIT_CALL_FRAME(value);
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	NEXT();
      }
    OP(GETGLOBFUN, 1):
      {
	/* get the function */
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = findFun(symbol, R_GlobalEnv);
	INIT_CALL_FRAME(value);
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	NEXT();
      }
    OP(GETSYMFUN, 1):
      {
	/* get the function */
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = SYMVALUE(symbol);
	if (Promise::isA(value)) {
	    value = SEXP_downcast<Promise *>(value)->force();
	}
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(GETBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = getPrimitive(symbol, BUILTINSXP);
//#define REPORT_OVERRIDEN_BUILTINS
#ifdef REPORT_OVERRIDEN_BUILTINS
	if (value != findFun(symbol, rho)) {
	    Rprintf("Possibly overridden builtin: %s\n", PRIMNAME(value));
	}
#endif
	if (RTRACE(value)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(GETINTLBUILTIN, 1):
      {
	/* get the function */
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = INTERNAL(symbol);
	if (TYPEOF(value) != BUILTINSXP)
	  error(_("there is no .Internal function '%s'"),
		CHAR(PRINTNAME(symbol)));
	INIT_CALL_FRAME(value);
	NEXT();
      }
    OP(CHECKFUN, 0):
      {
	/* check then the value on the stack is a function */
	SEXP value = GETSTACK(-1);
	if (!FunctionBase::isA(value))
	  error("%s", _("attempt to apply non-function"));
	INIT_CALL_FRAME_ARGS();
	NEXT();
      }
    OP(MAKEPROM, 1):
      {
	SEXP code = GETCONST(constants, GETOP());
	switch (CALL_FRAME_FTYPE()) {
	case CLOSXP:
	    PUSHCALLARG_RC(mkPROMISE(code, rho));
	    break;
	case BUILTINSXP:
	    if (TYPEOF(code) == BCODESXP)
		PUSHCALLARG(bcEval(code, rho, true));
	    else
		/* uncommon but possible, the compiler may decide not
		   to compile an argument expression */
		PUSHCALLARG(eval(code, rho));
	    break;
	case SPECIALSXP: break;
	default: break;
	}
	NEXT();
      }
    OP(DOMISSING, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP)
	  PUSHCALLARG(R_MissingArg);
	NEXT();
      }
    OP(SETTAG, 1):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	int tagidx = GETOP();
	if (ftype != SPECIALSXP) {
	    SEXP tag = GETCONST(constants, tagidx);
	    SETCALLARG_TAG(tag);
	}
	NEXT();
      }
    OP(DODOTS, 0):
      {
	SEXPTYPE ftype = CALL_FRAME_FTYPE();
	if (ftype != SPECIALSXP) {
	  SEXP h = R_findVar(R_DotsSymbol, rho);
	  if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
	    PROTECT(h);
	    for (; h != R_NilValue; h = CDR(h)) {
	      SEXP val;
	      if (ftype == BUILTINSXP)
	        val = eval(CAR(h), rho);
	      else if (CAR(h) == R_MissingArg)
	        val = CAR(h);
	      else
	        val = mkPROMISE(CAR(h), rho);
	      PUSHCALLARG(val);
	      SETCALLARG_TAG(TAG(h));
	    }
	    UNPROTECT(1); /* h */
	  }
	  else if (h != R_MissingArg)
	    error("%s", _("'...' used in an incorrect context"));
	}
	NEXT();
      }
    OP(PUSHARG, 0): PUSHCALLARG(BCNPOP()); NEXT();
    OP(PUSHCONSTARG, 1):
      {
	SEXP value = GETCONST(constants, GETOP());
	if (R_check_constants < 0)
	    value = duplicate(value);
	MARK_NOT_MUTABLE(value);
	PUSHCALLARG(value);
	NEXT();
      }
    OP(PUSHNULLARG, 0): PUSHCALLARG(R_NilValue); NEXT();
    OP(PUSHTRUEARG, 0): PUSHCALLARG(R_TrueValue); NEXT();
    OP(PUSHFALSEARG, 0): PUSHCALLARG(R_FalseValue); NEXT();
    OP(CALL, 1):
      {
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = GETCONST(constants, GETOP());
	SEXP args;
	SEXP value = NULL;
	int flag;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  args = BUILTIN_CALL_FRAME_ARGS();
	  checkForMissings(args, call);
	  flag = PRIMPRINT(fun);
	  Evaluator::enableResultPrinting(flag != 1);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
	  break;
	case SPECIALSXP:
	  flag = PRIMPRINT(fun);
	  Evaluator::enableResultPrinting(flag != 1);
	  value = PRIMFUN(fun) (call, fun, markSpecialArgs(CDR(call)), rho);
	  if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
	  break;
	case CLOSXP:
	  args = CLOSURE_CALL_FRAME_ARGS();
	  value = applyClosure(call, fun, args, rho, R_NilValue, true);
	  break;
	default: error("%s", _("bad function"));
	}
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(CALLBUILTIN, 1):
      {
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = GETCONST(constants, GETOP());
	SEXP args = BUILTIN_CALL_FRAME_ARGS();
	int flag;
	const void *vmax = vmaxget();
	if (TYPEOF(fun) != BUILTINSXP)
	  error(_("'%s' is not a '%s' function"), CHAR(PRINTNAME(fun)), "BUILTIN");
	flag = PRIMPRINT(fun);
	Evaluator::enableResultPrinting(flag != 1);
	SEXP value;
	if (Evaluator::profiling() && IS_TRUE_BUILTIN(fun)) {
	    SEXP oldref = R_Srcref;
	    RCNTXT cntxt(CTXT_BUILTIN, call,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    value = PRIMFUN(fun) (call, fun, args, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    value = PRIMFUN(fun) (call, fun, args, rho);
	}
	if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
	vmaxset(vmax);
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(CALLSPECIAL, 1):
      {
	SEXP call = GETCONST(constants, GETOP());
	SEXP symbol = CAR(call);
	SEXP fun = getPrimitive(symbol, SPECIALSXP);
	int flag;
	const void *vmax = vmaxget();
	if (RTRACE(fun)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	flag = PRIMPRINT(fun);
	Evaluator::enableResultPrinting(flag != 1);
	SEXP value = PRIMFUN(fun) (call, fun, markSpecialArgs(CDR(call)), rho);
	if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
	vmaxset(vmax);
	BCNPUSH(value);
	NEXT();
      }
    OP(MAKECLOSURE, 1):
      {
	SEXP fb = GETCONST(constants, GETOP());
	SEXP forms = VECTOR_ELT(fb, 0);
	SEXP body = VECTOR_ELT(fb, 1);
	SEXP value = mkCLOSXP(forms, body, rho);
	/* The LENGTH check below allows for byte code object created
	   by older versions of the compiler that did not record a
	   source attribute. */
	/* FIXME: bump bc version and don't check LENGTH? */
	if (LENGTH(fb) > 2) {
	  SEXP srcref = VECTOR_ELT(fb, 2);
	  if (!isNull(srcref)) setAttrib(value, R_SrcrefSymbol, srcref);
	}
	Evaluator::enableResultPrinting(true);
	BCNPUSH(value);
	NEXT();
      }
    OP(UMINUS, 1): FastUnary(-, R_SubSym);
    OP(UPLUS, 1): FastUnary(+, R_AddSym);
    OP(ADD, 1): FastBinary(R_ADD, PLUSOP, R_AddSym);
    OP(SUB, 1): FastBinary(R_SUB, MINUSOP, R_SubSym);
    OP(MUL, 1): FastBinary(R_MUL, TIMESOP, R_MulSym);
    OP(DIV, 1): FastBinary(R_DIV, DIVOP, R_DivSym);
    OP(EXPT, 1): FastBinary(R_POW, POWOP, R_ExptSym);
    OP(SQRT, 1): FastMath1(sqrt, R_SqrtSym);
    OP(EXP, 1): FastMath1(exp, R_ExpSym);
    OP(EQ, 1): FastRelop2(==, EQOP, R_EqSym);
    OP(NE, 1): FastRelop2(!=, NEOP, R_NeSym);
    OP(LT, 1): FastRelop2(<, LTOP, R_LtSym);
    OP(LE, 1): FastRelop2(<=, LEOP, R_LeSym);
    OP(GE, 1): FastRelop2(>=, GEOP, R_GeSym);
    OP(GT, 1): FastRelop2(>, GTOP, R_GtSym);
    OP(AND, 1): FastLogic2(&, ANDOP, R_AndSym);
    OP(OR, 1): FastLogic2(|, OROP, R_OrSym);
    OP(NOT, 1):
      {
	  Evaluator::enableResultPrinting(true);
	  R_bcstack_t *s = R_BCNodeStackTop - 1;
	  if (s->tag == LGLSXP) {
	      int lval = s->u.lval;
	      if (lval != NA_LOGICAL)
		  s->u.lval = lval ? FALSE : TRUE;
	      SKIP_OP();
	      NEXT();
	  }
	  Builtin1(do_logic, R_NotSym, rho);
      }
    OP(DOTSERR, 0): error("%s", _("'...' used in an incorrect context"));
    OP(STARTASSIGN, 1):
      {
	INCLNK_stack_commit();
	if (IS_STACKVAL_BOXED(-1)) {
	    SEXP saverhs = GETSTACK(-1);
	    FIXUP_RHS_NAMED(saverhs);
	    bool refrhs = MAYBE_REFERENCED(saverhs);
	    SETSTACK_FLAGS(-1, refrhs);
	    if (refrhs) INCREMENT_REFCNT(saverhs);
	}
	int sidx = GETOP();
	SEXP symbol = GETCONST(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	SEXP value = BINDING_VALUE(cell);
	R_varloc_t loc;
	if (value == R_UnboundValue ||
	    TYPEOF(value) == PROMSXP) {
	    value = EnsureLocal(symbol, rho, &loc);
	    if (loc.cell == NULL)
		loc.cell = R_NilValue;
	}
	else loc.cell = cell;

	bool maybe_in_assign = ASSIGNMENT_PENDING(loc.cell);
	SET_ASSIGNMENT_PENDING(loc.cell, true);
	BCNPUSH(loc.cell);

	if (maybe_in_assign || MAYBE_SHARED(value))
	    value = shallow_duplicate(value);
	BCNPUSH(value);

	BCNDUP3RD();
	/* top four stack entries are now
	   RHS value, LHS cell, LHS value, RHS value */
	NEXT();
      }
    OP(ENDASSIGN, 1):
      {
	SEXP lhscell = GETSTACK(-2);
	SET_ASSIGNMENT_PENDING(lhscell, false);

	int sidx = GETOP();
	SEXP symbol = GETCONST(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	SEXP value = GETSTACK(-1); /* leave on stack for GC protection */
	if (ALTREP(value)) {
	    SEXP v = try_assign_unwrap(value, symbol, rho, cell);
	    if (v != value) {
		SETSTACK(-1, v);
		value = v;
	    }
	}
	INCREMENT_NAMED(value);
	if (!SET_BINDING_VALUE(cell, value))
	    defineVar(symbol, value, rho);
	CXXR_POP(2); /* now pop cell and LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(GETSTACK(-1));
#else
	if (IS_STACKVAL_BOXED(-1)) {
	    SEXP saverhs = GETSTACK(-1);
	    INCREMENT_NAMED(saverhs);
	    bool refrhs = GETSTACK_FLAGS(-1);
	    if (refrhs) DECREMENT_REFCNT(saverhs);
	}
#endif
	NEXT();
      }
    OP(STARTSUBSET, 2): DO_STARTDISPATCH("[");
    OP(DFLTSUBSET, 0): DO_DFLTDISPATCH(do_subset_dflt, R_SubsetSym);
    OP(STARTSUBASSIGN, 2): DO_START_ASSIGN_DISPATCH("[<-");
    OP(DFLTSUBASSIGN, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign_dflt, R_SubassignSym);
    OP(STARTC, 2): DO_STARTDISPATCH("c");             /* no longer used */
    OP(DFLTC, 0): DO_DFLTDISPATCH(do_c_dflt, R_CSym); /* no longer used */
    OP(STARTSUBSET2, 2): DO_STARTDISPATCH("[[");
    OP(DFLTSUBSET2, 0): DO_DFLTDISPATCH(do_subset2_dflt, R_Subset2Sym);
    OP(STARTSUBASSIGN2, 2): DO_START_ASSIGN_DISPATCH("[[<-");
    OP(DFLTSUBASSIGN2, 0):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign2_dflt, R_Subassign2Sym);
    OP(DOLLAR, 2):
      {
	bool dispatched = false;
	SEXP call = GETCONST(constants, GETOP());
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP x = GETSTACK(-1);
	SEXP value = NULL;
	if (isObject(x)) {
	    SEXP ncall;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    dispatched = tryDispatch("$", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (dispatched)
	    SETSTACK(-1, value);
	else
	    SETSTACK(-1, R_subset3_dflt(x, PRINTNAME(symbol), call));
	Evaluator::enableResultPrinting(true);
	NEXT();
      }
    OP(DOLLARGETS, 2):
      {
	bool dispatched = false;
	SEXP call = GETCONST(constants, GETOP());
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP x = GETSTACK(-2);
	SEXP rhs = GETSTACK(-1);
	MARK_ASSIGNMENT_CALL(call);
	if (MAYBE_SHARED(x)) {
	    x = shallow_duplicate(x);
	    SETSTACK(-2, x);
	    ENSURE_NAMED(x);
	}
	SEXP value = NULL;
	if (isObject(x)) {
	    SEXP ncall, prom;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    prom = mkRHSPROMISE(CADDDR(ncall), rhs);
	    SETCAR(CDDDR(ncall), prom);
	    dispatched = tryDispatch("$<-", ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (!dispatched)
	  value = R_subassign3_dflt(call, x, symbol, rhs);
	CXXR_POP(1);
	SETSTACK(-1, value);
	NEXT();
      }
    OP(ISNULL, 0): DO_ISTEST(isNull);
    OP(ISLOGICAL, 0): DO_ISTYPE(LGLSXP);
    OP(ISINTEGER, 0):
      {
	SEXP arg = GETSTACK(-1);
	bool test = ((TYPEOF(arg) == INTSXP) && !inherits(arg, "factor"));
	SETSTACK(-1, test ? R_TrueValue : R_FalseValue);
	Evaluator::enableResultPrinting(true);
	NEXT();
      }
    OP(ISDOUBLE, 0): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0): DO_ISTEST(isNumericOnly);
    OP(VECSUBSET, 1): DO_VECSUBSET(rho, FALSE);
    OP(MATSUBSET, 1): DO_MATSUBSET(rho, FALSE); NEXT();
    OP(VECSUBASSIGN, 1): DO_VECSUBASSIGN(rho, FALSE);
    OP(MATSUBASSIGN, 1): DO_MATSUBASSIGN(rho, FALSE); NEXT();
    OP(AND1ST, 2):
      {
	int callidx = GETOP();
	int label = GETOP();
	FIXUP_SCALAR_LOGICAL(rho, callidx, "'x'", "&&", warn_lev);
	int val = GETSTACK_LOGICAL(-1);
	if (val == FALSE)
	    pc = codebase + label;
	Evaluator::enableResultPrinting(true);
	NEXT();
      }
    OP(AND2ND, 1):
      {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(rho, callidx, "'y'", "&&", warn_lev);
	int val = GETSTACK_LOGICAL(-1);
	/* The first argument is TRUE or NA. If the second argument is
	   not TRUE then its value is the result. If the second
	   argument is TRUE, then the first argument's value is the
	   result. */
	if (val == FALSE || val == NA_LOGICAL)
	    SETSTACK_LOGICAL(-2, val);
	CXXR_POP(1);
	Evaluator::enableResultPrinting(true);
	NEXT();
      }
    OP(OR1ST, 2):
      {
	int callidx = GETOP();
	int label = GETOP();
	FIXUP_SCALAR_LOGICAL(rho, callidx, "'x'", "||", warn_lev);
	int val = GETSTACK_LOGICAL(-1);
	if (val != NA_LOGICAL &&
	    val != FALSE) /* is true */
	    pc = codebase + label;
	Evaluator::enableResultPrinting(true);
	NEXT();
      }
    OP(OR2ND, 1):
      {
	int callidx = GETOP();
	FIXUP_SCALAR_LOGICAL(rho, callidx, "'y'", "||", warn_lev);
	int val = GETSTACK_LOGICAL(-1);
	/* The first argument is FALSE or NA. If the second argument is
	   not FALSE then its value is the result. If the second
	   argument is FALSE, then the first argument's value is the
	   result. */
	if (val != FALSE)
	    SETSTACK_LOGICAL(-2, val);
	CXXR_POP(1);
	Evaluator::enableResultPrinting(true);
	NEXT();
    }
    OP(GETVAR_MISSOK, 1): DO_GETVAR(false, true);
    OP(DDVAL_MISSOK, 1): DO_GETVAR(true, true);
    OP(VISIBLE, 0): Evaluator::enableResultPrinting(true); NEXT();
    OP(SETVAR2, 1):
      {
	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
	NEXT();
      }
    OP(STARTASSIGN2, 1):
      {
	INCLNK_stack_commit();
	SEXP symbol = GETCONST(constants, GETOP());
	R_varloc_t loc = R_findVarLoc(symbol, rho);

	if (loc.cell == NULL)
	    loc.cell = R_NilValue;
	bool maybe_in_assign = ASSIGNMENT_PENDING(loc.cell);
	SET_ASSIGNMENT_PENDING(loc.cell, true);
	BCNPUSH(loc.cell);

	SEXP value = getvar(symbol, ENCLOS(rho), false, false, NULL, 0);
	if (maybe_in_assign || MAYBE_SHARED(value))
	    value = shallow_duplicate(value);
	BCNPUSH(value);

	BCNDUP3RD();
	/* top four stack entries are now
	   RHS value, LHS cell, LHS value, RHS value */
	if (IS_STACKVAL_BOXED(-1)) {
	    FIXUP_RHS_NAMED(GETSTACK(-1));
	    INCREMENT_REFCNT(GETSTACK(-1));
	}
	NEXT();
      }
    OP(ENDASSIGN2, 1):
      {
	SEXP lhscell = GETSTACK(-2);
	SET_ASSIGNMENT_PENDING(lhscell, false);

	SEXP symbol = GETCONST(constants, GETOP());
	SEXP value = GETSTACK(-1); /* leave on stack for GC protection */
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
	CXXR_POP(2); /* now pop cell and LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = NAMEDMAX */
	ENSURE_NAMEDMAX(GETSTACK(-1));
#else
	INCREMENT_NAMED(GETSTACK(-1));
#endif
	DECREMENT_REFCNT(GETSTACK(-1));
	NEXT();
      }
    OP(SETTER_CALL, 2):
      {
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP rhs = GETSTACK_BELOW_CALL_FRAME(-1);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = GETCONST(constants, GETOP());
	SEXP vexpr = GETCONST(constants, GETOP());
	SEXP args, prom, last;
	MARK_ASSIGNMENT_CALL(call);
	if (MAYBE_SHARED(lhs)) {
	  lhs = shallow_duplicate(lhs);
	  SETSTACK_BELOW_CALL_FRAME(-2, lhs);
	  ENSURE_NAMED(lhs);
	}
	SEXP value = NULL;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with LHS value */
	  args = BUILTIN_CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and protect */
	  PROTECT(args = duplicate(CDR(call)));
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track references */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* insert evaluated promise for RHS as last argument */
	  last = args;
	  while (CDR(last) != R_NilValue)
	      last = CDR(last);
	  prom = mkRHSPROMISE(vexpr, rhs);
	  SETCAR(last, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  UNPROTECT(1);
	  break;
	case CLOSXP:
	  /* push evaluated promise for RHS onto arguments with 'value' tag */
	  /* This need to use a standard EVPROMISE so the reference
	     from the environment to the RHS value is counted. */
	  prom = R_mkEVPROMISE(vexpr, rhs);
	  PUSHCALLARG(prom);
	  SETCALLARG_TAG_SYMBOL(R_valueSym);
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  args = CLOSURE_CALL_FRAME_ARGS();
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_NilValue, true);
	  break;
	default: error("%s", _("bad function"));
	}
	POP_CALL_FRAME_PLUS(2, value);
	NEXT();
      }
    OP(GETTER_CALL, 1):
      {
	SEXP lhs = GETSTACK_BELOW_CALL_FRAME(-2);
	SEXP fun = CALL_FRAME_FUN();
	SEXP call = GETCONST(constants, GETOP());
	SEXP value = NULL;
	SEXP args, prom;
	switch (TYPEOF(fun)) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = BUILTIN_CALL_FRAME_ARGS();
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track references */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  args = CLOSURE_CALL_FRAME_ARGS();
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_NilValue, true);
	  break;
	default: error("%s", _("bad function"));
	}
	POP_CALL_FRAME(value);
	NEXT();
      }
    OP(SWAP, 0):
      {
	/* This instruction only occurs between accessor calls in
	   complex assignments. [It should probably be renamed to
	   reflect this.] It needs to make sure intermediate LHS
	   values in complex assignments are not shared by duplicating
	   the extracted value in tmp when necessary. Duplicating is
	   necessary if the value might be shared _or_ if the
	   container, which is in R_BCNodeStackTop[-3], has become
	   possibly shared by going through a closure in the preceding
	   accessor call.  This is taken to indicate that the
	   corresponding replacement function might be a closure and
	   will need to see an unmodified LHS value. This heuristic
	   fails if the accessor function called here is not a closure
	   but the replacement function is. */

	/* For the typed stack it might be OK just to force boxing at
	   this point, but for now this code tries to avoid doing
	   that. The macros make the code a little more readable. */
#define STACKVAL_MAYBE_REFERENCED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_REFERENCED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))
#define STACKVAL_MAYBE_SHARED(idx)				\
	(IS_STACKVAL_BOXED(idx) &&				\
	 MAYBE_SHARED(GETSTACK_SXPVAL_PTR(R_BCNodeStackTop + (idx))))

	if (STACKVAL_MAYBE_REFERENCED(-1) &&
	    (STACKVAL_MAYBE_SHARED(-1) ||
	     STACKVAL_MAYBE_SHARED(-3)))
	    SETSTACK(-1, shallow_duplicate(GETSTACK(-1)));

	R_bcstack_t tmp = R_BCNodeStackTop[-1];
	R_BCNodeStackTop[-1] = R_BCNodeStackTop[-2];
	R_BCNodeStackTop[-2] = tmp;
	NEXT();
    }
    OP(DUP2ND, 0): BCNDUP2ND(); NEXT();
    OP(SWITCH, 4):
      {
       SEXP call = GETCONST(constants, GETOP());
       SEXP names = GETCONST(constants, GETOP());
       SEXP coffsets = GETCONST(constants, GETOP());
       SEXP ioffsets = GETCONST(constants, GETOP());
       SEXP value = BCNPOP();
       if (!isVector(value) || length(value) != 1)
	   errorcall(call, "%s", _("EXPR must be a length 1 vector"));
       if (isFactor(value))
	   warningcall(call,
		       _("EXPR is a \"factor\", treated as integer.\n"
			 " Consider using '%s' instead."),
		       "switch(as.character( * ), ...)");
       if (TYPEOF(value) == STRSXP) {
	   if (names == R_NilValue) {
	       if (TYPEOF(ioffsets) != INTSXP)
		   errorcall(call, "%s", _("bad numeric 'switch' offsets"));
	       if (LENGTH(ioffsets) == 1) {
		   pc = codebase + INTEGER(ioffsets)[0]; /* returns NULL */
		   warningcall(call, "%s", _("'switch' with no alternatives"));
	       }
	       else
		   errorcall(call, "%s", _("numeric EXPR required for 'switch' without named alternatives"));
	   } else {
	       if (TYPEOF(coffsets) != INTSXP)
		   errorcall(call, "%s", _("bad character 'switch' offsets"));
	       if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(coffsets))
		   errorcall(call, "%s", _("bad 'switch' names"));
	       int n = LENGTH(names);
	       int which = n - 1;
	       for (int i = 0; i < n - 1; i++)
		   if (pmatch(STRING_ELT(value, 0),
			      STRING_ELT(names, i), TRUE /* exact */)) {
		       which = i;
		       break;
		   }
	       pc = codebase + INTEGER(coffsets)[which];
	   }
       }
       else {
	   if (TYPEOF(ioffsets) != INTSXP)
	       errorcall(call, "%s", _("bad numeric 'switch' offsets"));
	   int which = asInteger(value);
	   if (which != NA_INTEGER) which--;
	   if (which < 0 || which >= LENGTH(ioffsets))
	       which = LENGTH(ioffsets) - 1;
	   if (LENGTH(ioffsets) == 1)
	       warningcall(call, "%s", _("'switch' with no alternatives"));
	   pc = codebase + INTEGER(ioffsets)[which];
       }
       NEXT();
    }
    OP(RETURNJMP, 0):
      {
      SEXP value = BCNPOP();
      findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, value);
      }
    OP(STARTSUBSET_N, 2): DO_STARTDISPATCH_N("[");
    OP(STARTSUBASSIGN_N, 2): DO_START_ASSIGN_DISPATCH_N("[<-");
    OP(VECSUBSET2, 1): DO_VECSUBSET(rho, TRUE);
    OP(MATSUBSET2, 1): DO_MATSUBSET(rho, TRUE); NEXT();
    OP(VECSUBASSIGN2, 1): DO_VECSUBASSIGN(rho, TRUE);
    OP(MATSUBASSIGN2, 1): DO_MATSUBASSIGN(rho, TRUE); NEXT();
    OP(STARTSUBSET2_N, 2): DO_STARTDISPATCH_N("[[");
    OP(STARTSUBASSIGN2_N, 2): DO_START_ASSIGN_DISPATCH_N("[[<-");
    OP(SUBSET_N, 2): DO_SUBSET_N(rho, FALSE); NEXT();
    OP(SUBSET2_N, 2): DO_SUBSET_N(rho, TRUE); NEXT();
    OP(SUBASSIGN_N, 2): DO_SUBASSIGN_N(rho, FALSE); NEXT();
    OP(SUBASSIGN2_N, 2): DO_SUBASSIGN_N(rho, TRUE); NEXT();
    OP(LOG, 1): DO_LOG(); NEXT();
    OP(LOGBASE, 1): DO_LOGBASE(); NEXT();
    OP(MATH1, 2): DO_MATH1(); NEXT();
    OP(DOTCALL, 2): DO_DOTCALL(); NEXT();
    OP(COLON, 1): DO_COLON(); NEXT();
    OP(SEQALONG, 1): DO_SEQ_ALONG(); NEXT();
    OP(SEQLEN, 1): DO_SEQ_LEN(); NEXT();
    OP(BASEGUARD, 2): DO_BASEGUARD(); NEXT();
    OP(INCLNK, 0):
      INCLNK_stack_commit(); /* needed for pre version 12 byte code */
      INCLNK_STACK_PTR(R_BCNodeStackTop - 1);
      NEXT();
    OP(DECLNK, 0):
      DECLNK_STACK_PTR(R_BCNodeStackTop - 2);
      NEXT();
    OP(DECLNK_N, 1):
      for (int n = GETOP(), i = 0; i < n; i++)
	  DECLNK_STACK_PTR(R_BCNodeStackTop - 2 - i);
      NEXT();
    OP(INCLNKSTK, 0):
      {
	  int offset = (int)(R_BCProtTop - R_BCNodeStackBase);
	  INCLNK_stack(R_BCNodeStackTop);
	  BCNPUSH_INTEGER(offset);
	  NEXT();
      }
    OP(DECLNKSTK, 0):
      {
	  int offset = GETSTACK_IVAL_PTR(R_BCNodeStackTop - 2);
	  R_bcstack_t *ptop = R_BCNodeStackBase + offset;
	  DECLNK_stack(ptop);
	  R_BCNodeStackTop[-2] = R_BCNodeStackTop[-1];
	  CXXR_POP(1);
	  NEXT();
      }
    LASTOP;
  }
  return R_NilValue;
}

#ifdef THREADED_CODE
attribute_hidden SEXP R::R_bcEncode(SEXP bytes)
{
    BCODE *pc;

    R_xlen_t m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    R_xlen_t n = LENGTH(bytes);
    if (n == 0)
	return R_NilValue;
    int *ipc = INTEGER(bytes);

    int v = ipc[0];
    // Check version:
    if (v < R_bcMinVersion || v > R_bcVersion) {
	IntVector *code = IntVector::create(m * 2);
	pc = BCODE_PTR(code);
	pc[0].i = v;
	pc[1].v = opinfo[BCMISMATCH_OP].addr;
	return code;
    }

	IntVector *code = IntVector::create(m * n);
	memset(INTEGER(code), 0, m * n * sizeof(int));
	pc = BCODE_PTR(code);

	// Insert the current version number:
	pc[0].i = R_bcVersion;

	// First do a straight copy:
	for (R_xlen_t i = 1; i < n; i++) pc[i].i = ipc[i];

	/* Revert to version 2 to allow for some one compiling in a
	   new R, loading/saving in an old one, and then trying to run
	   in a new one. This has happened! Setting the version number
	   back tells ByteCode::interpret() to drop back to eval. */
	if (n == 2 && ipc[1] == BCMISMATCH_OP)
	    pc[0].i = 2;

    // Now replace the opcodes with the appropriate code addresses:
    {
	R_xlen_t i = 1;
	while (i < n) {
	    int op = pc[i].i;
	    if (op < 0 || op >= OPCOUNT)
		error("%s", _("unknown instruction code"));
	    pc[i].v = opinfo[op].addr;
	    i += opinfo[op].argc + 1;
	}
    }

	return code;
}

static int findOp(void *addr)
{
    for (int i = 0; i < OPCOUNT; i++)
	if (opinfo[i].addr == addr)
	    return i;
    error("%s", _("cannot find index for threaded code address"));
    return 0; /* not reached */
}

attribute_hidden SEXP R::R_bcDecode(SEXP code) {

    int m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    R_xlen_t n = LENGTH(code) / m;
    BCODE *pc = BCODE_PTR(code);

    IntVector *bytes = IntVector::create(n);
    int *ipc = INTEGER(bytes);

    /* copy the version number */
    ipc[0] = pc[0].i;

    for (R_xlen_t i = 1; i < n;) {
	int op = findOp(pc[i].v);
	int argc = opinfo[op].argc;
	ipc[i] = op;
	i++;
	for (int j = 0; j < argc; j++, i++)
	    ipc[i] = pc[i].i;
    }

    return bytes;
}
#else
attribute_hidden SEXP R::R_bcEncode(SEXP x) { return x; }
attribute_hidden SEXP R::R_bcDecode(SEXP x) { return duplicate(x); }
#endif

/* Add BCODESXP bc into the constants registry, performing a deep copy of the
   bc's constants */
#define CONST_CHECK_COUNT 1000
attribute_hidden void R::R_registerBC(SEXP bcBytes, SEXP bcode)
{
    if (R_check_constants <= 0)
	return;
    if (TYPEOF(bcBytes) != INTSXP)
	error("%s", _("R_registerBC requires integer vector as bcBytes"));
    if (TYPEOF(bcode) != BCODESXP)
	error("%s", _("R_registerBC requires BCODESXP object as bcode"));

    static int s_count = CONST_CHECK_COUNT;
    if (--s_count <= 0) {
	s_count = CONST_CHECK_COUNT;
	R_checkConstants(true);
    }

    /* The constants registry is a linked list of constant records. Each
       constant record is a generic vector, its first element is a pointer
       to the next constant record, the second element is a weak reference
       to the byte-code object, the third element is a reference to the whole
       constant pool, and the following elements are interleaved original and
       copied constants. A constant registry corresponds to a constant pool.
       When the weak reference gets cleared, the respective constant record
       can be removed from the list.

       One could simply compare/duplicate the lists of all constants (the whole
       constant pools), but that turned out too expensive */

    SEXP consts = BCODE_CONSTS(bcode); /* all constants, VECSXP */

#define CHECK_ALL_CONSTANTS
#ifndef CHECK_ALL_CONSTANTS
    int *ipc = INTEGER(bcBytes);
    int n = LENGTH(bcBytes);
    int loadableConsts = 0;

    /* add only constants loaded by certain instructions  */
    for (int i = 0; i < n; i += opinfo[ipc[i]].argc + 1)
        if (ipc[i] == LDCONST_OP || ipc[i] == PUSHCONSTARG_OP ||
		ipc[i] == CALLSPECIAL_OP)
            loadableConsts++;

    GCStackRoot<ListVector> constsRecord;
    constsRecord = ListVector::create(loadableConsts * 2 + 3);
    int crIdx = 3;
    for (int i = 0; i < n; i += opinfo[ipc[i]].argc + 1)
        if (ipc[i] == LDCONST_OP || ipc[i] == PUSHCONSTARG_OP ||
		ipc[i] == CALLSPECIAL_OP) {
            SEXP corig = VECTOR_ELT(consts, ipc[i + 1]);
            SET_VECTOR_ELT(constsRecord, crIdx++, corig);
            SET_VECTOR_ELT(constsRecord, crIdx++, duplicate(corig));
        }
#else
    /* add the whole constant pool */
    GCStackRoot<ListVector> constsRecord;
    constsRecord = ListVector::create(2 + 3);
    SET_VECTOR_ELT(constsRecord, 3, consts);
    /* the consts reference is in the record twice to make the code simpler */
    SET_VECTOR_ELT(constsRecord, 4, duplicate(consts));
#endif

    SEXP wref = R_MakeWeakRef(bcode, R_NilValue, R_NilValue, FALSE);
    SET_VECTOR_ELT(constsRecord, 0, VECTOR_ELT(R_ConstantsRegistry, 0));
    SET_VECTOR_ELT(constsRecord, 1, wref);
    SET_VECTOR_ELT(constsRecord, 2, consts);
    SET_VECTOR_ELT(R_ConstantsRegistry, 0, constsRecord);
}

/* A potentially very verbose report for modified compiler constant. */
static void reportModifiedConstant(SEXP crec, SEXP orig, SEXP copy, int idx)
{
    if (R_check_constants < 5)
	return;

    SEXP consts = VECTOR_ELT(crec, 2);
    int n = LENGTH(consts);
    if (idx == -1) {
	for (int i = 0; i < n; i++)
	    if (VECTOR_ELT(consts, i) == orig) {
		idx = i;
		break;
	    }
    }
    int oldout = R_OutputCon; /* redirect standard to error output */
    R_OutputCon = 2;
    int oldcheck = R_check_constants; /* guard against recursive invocation */
    R_check_constants = 0;
    if (idx != 0) {
	REprintf("%s", _("ERROR: the modified value of the constant is:\n"));
	PrintValue(orig);
	REprintf("%s", _("ERROR: the original value of the constant is:\n"));
	PrintValue(copy);
	REprintf(_("ERROR: the modified constant is at index %d\n"), idx);
	REprintf("%s", _("ERROR: the modified constant is in this function body:\n"));
	PrintValue(VECTOR_ELT(consts, 0));
    } else {
	REprintf("%s", _("ERROR: the modified constant is function body:\n"));
	PrintValue(orig);
	REprintf("%s", _("ERROR: the body was originally:\n"));
	PrintValue(copy);
    }
    findFunctionForBody(VECTOR_ELT(consts, 0));
    R_check_constants = oldcheck;
    R_OutputCon = oldout;
}

/* Checks whether compiler constants linked from the given record
   were modified. */
static bool checkConstantsInRecord(SEXP crec, bool abortOnError)
{
    int n = LENGTH(crec);
    bool constsOK = true;

    for (int i = 3; i < n;) {
	SEXP corig = VECTOR_ELT(crec, i++);
	SEXP ccopy = VECTOR_ELT(crec, i++);

	/* 39: not numerical comparison, not single NA, not attributes as
           set, do ignore byte-code, do ignore environments of closures,
           not ignore srcref

           srcref is not ignored because ignoring it is expensive
           (it triggers duplication)
        */
	if (!R_compute_identical(corig, ccopy, 39)) {

#ifndef CHECK_ALL_CONSTANTS
	    REprintf(_("ERROR: modification of compiler constant of type %s, length %d\n"),
		CHAR(type2str(TYPEOF(ccopy))), length(ccopy));
	    reportModifiedConstant(crec, corig, ccopy, -1);
#else
	    int nc = LENGTH(corig);
	    for (int ci = 0; ci < nc; ci++) {
		SEXP orig = VECTOR_ELT(corig, ci);
		SEXP copy = VECTOR_ELT(ccopy, ci);
		if (!R_compute_identical(orig, copy, 39)) {
		    REprintf(_("ERROR: modification of compiler constant of type %s, length %d\n"),
			CHAR(type2str(TYPEOF(copy))), length(copy));
		    reportModifiedConstant(crec, orig, copy, ci);
		}
	    }
#endif
	    constsOK = false;
        }
    }

    if (!constsOK && abortOnError) {
	/* turn off constant checking to avoid infinite recursion through
	   R_Suicide -> ... -> R_RunExitFinalizers -> R_checkConstants. */
	R_check_constants = 0;
	R_Suicide(_("compiler constants were modified!\n"));
    }

    return constsOK;
}

/* Checks if constants of any registered BCODESXP have been modified.
   Returns true if the constants are ok, otherwise returns false or aborts.*/
attribute_hidden bool R::R_checkConstants(bool abortOnError)
{
    if (R_check_constants <= 0 || R_ConstantsRegistry == NULL)
	return true;

    static bool s_checkingInProgress = false;

    if (s_checkingInProgress)
	/* recursive invocation is possible because of allocation
           in R_compute_identical */
	return true;

    bool constsOK = true;
    /* set up context to recover s_checkingInProgress */
    try {
    s_checkingInProgress = true;
    SEXP prev_crec = R_ConstantsRegistry;
    SEXP crec = VECTOR_ELT(prev_crec, 0);
    while(crec != R_NilValue) {
	SEXP wref = VECTOR_ELT(crec, 1);
	SEXP bc = R_WeakRefKey(wref);
	if (!checkConstantsInRecord(crec, abortOnError))
	    constsOK = false;
	if (bc == R_NilValue)
	    /* remove no longer needed record from the registry */
	    SET_VECTOR_ELT(prev_crec, 0, VECTOR_ELT(crec, 0));
	else
            prev_crec = crec;
	crec = VECTOR_ELT(crec, 0);
    }
    } catch (...) {
        s_checkingInProgress = false;
        throw;
	}
    s_checkingInProgress = false;
    return constsOK;
}

attribute_hidden SEXP do_mkcode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP bytes = CAR(args);
    SEXP consts = CADR(args);
    GCStackRoot<ByteCode> ans;
    ans = ByteCode::create();
    SET_CODE(ans, R_bcEncode(bytes));
    SET_CONSTS(ans, consts);
    R_registerBC(bytes, ans);

    return ans;
}

attribute_hidden SEXP do_bcclose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP forms = CAR(args);
    SEXP body = CADR(args);
    SEXP env = CADDR(args);

    CheckFormals(forms, "bcClose");

    if (!isByteCode(body))
	error("%s", _("invalid body"));

    if (isNull(env)) {
	error("%s", _("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if (!isEnvironment(env))
	error("%s", _("invalid environment"));

    return mkCLOSXP(forms, body, env);
}

attribute_hidden SEXP do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP symbol, i;

    checkArity(op, args);
    symbol = CAR(args);

    if (!isSymbol(symbol))
	error("%s", _("invalid symbol"));

    if ((i = INTERNAL(symbol)) != R_NilValue && TYPEOF(i) == BUILTINSXP)
	return R_TrueValue;
    else
	return R_FalseValue;
}

static SEXP disassemble(SEXP bc)
{
  GCStackRoot<ListVector> ans;
  SEXP dconsts;
  SEXP code = BCODE_CODE(bc);
  SEXP consts = BCODE_CONSTS(bc);
  SEXP expr = BCODE_EXPR(bc);
  int nc = LENGTH(consts);

  ans = ListVector::create(expr != R_NilValue ? 4 : 3);
  SET_VECTOR_ELT(ans, 0, install(".Code"));
  SET_VECTOR_ELT(ans, 1, R_bcDecode(code));
  SET_VECTOR_ELT(ans, 2, ListVector::create(nc));
  if (expr != R_NilValue)
      SET_VECTOR_ELT(ans, 3, duplicate(expr));

  dconsts = VECTOR_ELT(ans, 2);
  for (int i = 0; i < nc; i++) {
    SEXP c = VECTOR_ELT(consts, i);
    if (isByteCode(c))
      SET_VECTOR_ELT(dconsts, i, disassemble(c));
    else
      SET_VECTOR_ELT(dconsts, i, duplicate(c));
  }

  return ans;
}

attribute_hidden SEXP do_disassemble(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  checkArity(op, args);
  SEXP code = CAR(args);
  if (!isByteCode(code))
    error("%s", _("argument is not a byte code object"));
  return disassemble(code);
}

attribute_hidden SEXP do_bcversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  checkArity(op, args);
  IntVector *ans = IntVector::createScalar(R_bcVersion);
  return ans;
}

#ifdef UNUSED
#define R_COMPILED_EXTENSION ".Rc"

/* neither of these functions call R_ExpandFileName -- the caller
   should do that if it wants to */
const char *R_CompiledFileName(char *fname, char *buf, size_t bsize)
{
    /* find the base name and the extension */
    const char *basename = Rf_strrchr(fname, FILESEP[0]);
    if (basename == NULL) basename = fname;
    const char *ext = Rf_strrchr(basename, '.');

    if (ext != NULL && streql(ext, R_COMPILED_EXTENSION)) {
	/* the supplied file name has the compiled file extension, so
	   just copy it to the buffer and return the buffer pointer */
	if (snprintf(buf, bsize, "%s", fname) < 0)
	    error("%s", _("R_CompiledFileName: buffer too small"));
	return buf;
    }
    else if (ext == NULL) {
	/* if the requested file has no extension, make a name that
	   has the extension added on to the expanded name */
	if (snprintf(buf, bsize, "%s%s", fname, R_COMPILED_EXTENSION) < 0)
	    error("%s", _("R_CompiledFileName: buffer too small"));
	return buf;
    }
    else {
	/* the supplied file already has an extension, so there is no
	   corresponding compiled file name */
	return NULL;
    }
}

FILE *R_OpenCompiledFile(char *fname, char *buf, size_t bsize)
{
    const char *cname = R_CompiledFileName(fname, buf, bsize);

    if (cname != NULL && R_FileExists(cname) &&
	(streql(fname, cname) ||
	 !R_FileExists(fname) ||
	 R_FileMtime(cname) > R_FileMtime(fname)))
	/* the compiled file cname exists, and either fname does not
	   exist, or it is the same as cname, or both exist and cname
	   is newer */
	return R_fopen(buf, "rb");
    else return NULL;
}
#endif

attribute_hidden SEXP do_growconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	error("%s", _("constant buffer must be a generic vector"));

    int n = LENGTH(constBuf);
    ListVector *ans = ListVector::create(2 * n);
    for (int i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

attribute_hidden SEXP do_putconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	error("%s", _("constant buffer must be a generic vector"));

    int constCount = asInteger(CADR(args));
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	error("%s", _("bad constCount value"));

    SEXP x = CADDR(args);

    /* check for a match and return index if one is found */
    for (int i = 0; i < constCount; i++) {
	SEXP y = VECTOR_ELT(constBuf, i);
	/* 16 - take closure environments into account, this is necessary
	        as closures (closure literals) can get into the AST when
	        the AST is generated by a program (e.g. distr package)
	*/
	if (x == y || R_compute_identical(x, y, 16))
	    return ScalarInteger(i);
    }

    /* otherwise insert the constant and return index */
    SET_VECTOR_ELT(constBuf, constCount, x);
    return ScalarInteger(constCount);
}

attribute_hidden SEXP do_getconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;

    checkArity(op, args);
    constBuf = CAR(args);
    int n = asInteger(CADR(args));

    if (TYPEOF(constBuf) != VECSXP)
	error("%s", _("constant buffer must be a generic vector"));
    if (n < 0 || n > LENGTH(constBuf))
	error("%s", _("bad constant count"));

    ans = ListVector::create(n);
    for (int i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

#ifdef BC_PROFILING
attribute_hidden
SEXP do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP val = IntVector::create(OPCOUNT);
    for (int i = 0; i < OPCOUNT; i++)
	INTEGER(val)[i] = opcode_counts[i];
    return val;
}

static void dobcprof(int sig)
{
    if (s_current_opcode >= 0 && s_current_opcode < OPCOUNT)
	opcode_counts[s_current_opcode]++;
    signal(SIGPROF, dobcprof);
}

attribute_hidden
SEXP do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;
    int interval;
    double dinterval = 0.02;

    checkArity(op, args);
    if (Evaluator::profiling())
	error("%s", _("profile timer in use"));
    if (s_bc_profiling)
	error("%s", _("already byte code profiling"));

    /* according to man setitimer, it waits until the next clock
       tick, usually 10ms, so avoid too small intervals here */
    interval = 1e6 * dinterval + 0.5;

    /* initialize the profile data */
    s_current_opcode = NO_CURRENT_OPCODE;
    for (int i = 0; i < OPCOUNT; i++)
	opcode_counts[i] = 0;

    signal(SIGPROF, dobcprof);

    itv.it_interval.tv_sec = interval / 1000000;
    itv.it_interval.tv_usec =
	(suseconds_t) (interval - itv.it_interval.tv_sec * 1000000);
    itv.it_value.tv_sec = interval / 1000000;
    itv.it_value.tv_usec =
	(suseconds_t) (interval - itv.it_value.tv_sec * 1000000);
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	error("%s", _("setting profile timer failed"));

    s_bc_profiling = true;

    return R_NilValue;
}

static void dobcprof_null(int sig)
{
    signal(SIGPROF, dobcprof_null);
}

attribute_hidden
SEXP do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    struct itimerval itv;

    checkArity(op, args);
    if (!s_bc_profiling)
	error("%s", _("not byte code profiling"));

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, dobcprof_null);

    s_bc_profiling = false;

    return R_NilValue;
}
#else
NORET attribute_hidden
SEXP do_bcprofcounts(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error("%s", _("byte code profiling is not supported in this build"));
}
NORET attribute_hidden
SEXP do_bcprofstart(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error("%s", _("byte code profiling is not supported in this build"));
}
NORET attribute_hidden
SEXP do_bcprofstop(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    error("%s", _("byte code profiling is not supported in this build"));
}
#endif

/* end of byte code section */

attribute_hidden SEXP do_setnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_num_math_threads;
    checkArity(op, args);
    int new_ = asInteger(CAR(args));
    if (new_ >= 0 && new_ <= R_max_num_math_threads)
	R_num_math_threads = new_;
    return ScalarInteger(old);
}

attribute_hidden SEXP do_setmaxnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_max_num_math_threads, new_;
    checkArity(op, args);
    new_ = asInteger(CAR(args));
    if (new_ >= 0) {
	R_max_num_math_threads = new_;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return ScalarInteger(old);
}

attribute_hidden SEXP do_returnValue(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP val = R_ExitContext ? STACKVAL_TO_SEXP(R_ExitContext->returnValue) : nullptr;
    if (val) {
	MARK_NOT_MUTABLE(val);
	return val;
    }
    return CAR(args); /* default */
}

SEXP R_ParseEvalString(const char *str, SEXP env)
{
    SEXP s = PROTECT(mkString(str));

    ParseStatus status;
    SEXP ps = PROTECT(R_ParseVector(s, -1, &status, R_NilValue));
    if (status != PARSE_OK ||
	TYPEOF(ps) != EXPRSXP ||
	LENGTH(ps) != 1)
	error("%s", _("parse error"));

    SEXP val = XVECTOR_ELT(ps, 0);
    if (env != NULL)
	val = eval(val, env);

    UNPROTECT(2); /* s, ps */
    return val;
}

SEXP R_ParseString(const char *str)
{
    return R_ParseEvalString(str, NULL);
}

/* declare() SPECIALSXP */
attribute_hidden SEXP do_declare(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return R_NilValue;
}
