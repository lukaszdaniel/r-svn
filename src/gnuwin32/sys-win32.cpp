/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2023  The R Core Team
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

	 /* See ../unix/system.txt for a description of functions */

	/* Windows analogue of unix/sys-unix.c: often rather similar */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <cctype> /* for isalpha */
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/Evaluator.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <Rembedded.h>
#include <Startup.h>

using namespace R;
using namespace CXXR;

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

FILE *R_OpenInitFile(void)
{
    char  *buf, *p = getenv("R_PROFILE_USER");
    FILE *fp;

    fp = NULL;
    if (LoadInitFile) {
	if (p) {
	    if (!*p) return NULL;  /* set to "" */
	    return R_fopen(R_ExpandFileName(p), "r");
	}
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	p = getenv("R_USER");
	if (p) {
	    size_t needed = snprintf(NULL, 0, "%s/.Rprofile", p) + 1;
	    buf = (char *)malloc(needed);
	    if (!buf)
		return NULL;
	    snprintf(buf, needed, "%s/.Rprofile", p);
	    fp = R_fopen(buf, "r");
	    free(buf);
	}
    }
    return fp;
}
/*
 *  5) FILESYSTEM INTERACTION
 */


static int HaveHOME=-1;
static char *UserHOME = NULL;
#define NEWFILENAME_MAX 65536
static char newFileName[NEWFILENAME_MAX];

const char *R_ExpandFileName(const char *s)
{
    char *p, *q;

    if (s[0] != '~' || (s[0] && isalpha(s[1]))) return s;
    if (HaveHOME < 0) {
	HaveHOME = 0;
	p = getenv("R_USER"); /* should be set so the rest is a safety measure */
	if (p && strlen(p)) {
	    if ((UserHOME = (char *)malloc(strlen(p) + 1))) {
		strcpy(UserHOME, p);
		HaveHOME = 1;
	    }
	} else {
	    p = getenv("HOME");
	    if (p && strlen(p)) {
		if ((UserHOME = (char *)malloc(strlen(p) + 1))) {
		    strcpy(UserHOME, p);
		    HaveHOME = 1;
		}
	    } else {
		p = getenv("HOMEDRIVE");
		q = getenv("HOMEPATH");
		if (p && q &&
		   (UserHOME = (char *)malloc(strlen(p) + strlen(q) + 1))) {

		    strcpy(UserHOME, p);
		    strcat(UserHOME, q);
		    HaveHOME = 1;
		}
	    }
	}
    }
    if (HaveHOME > 0) {
	size_t len = strlen(UserHOME) + strlen(s+1);
	if (len < NEWFILENAME_MAX) {
	    strcpy(newFileName, UserHOME);
	    strcat(newFileName, s+1);
	    return newFileName;
	} else {
	    warning(_("expanded path length %llu would be too long for\n%s\n"),
		    (unsigned long long)len, s);
	    return s;
	}
    } else return s;
}

/* The following is a version of R_ExpandFileName that assumes
   s is in UTF-8 and returns the final result in that encoding as well. */
const char *R_ExpandFileNameUTF8(const char *s)
{
    if (s[0] !='~' || (s[0] && isalpha(s[1]))) return s;
    else {
	const char *native_home = R_ExpandFileName("~");
	/* a defensive guess, reEnc2 would throw error if not enough */
	size_t len = strlen(native_home) * 4;
    	char *utf8_home = (char *)malloc(len);
	if (!utf8_home) {
	    warning(_("expanded path length %llu would be too long for\n%s\n"),
	            (unsigned long long)len, s);
	    return s;
	}
    	reEnc2(native_home, utf8_home, len, CE_NATIVE, CE_UTF8, 3);
	len = strlen(utf8_home) + strlen(s+1) + 1;
    	if (len <= NEWFILENAME_MAX) {
    	    strcpy(newFileName, utf8_home);
    	    strcat(newFileName, s+1);
    	    return newFileName;
    	} else {
	    warning(_("expanded path length %llu would be too long for\n%s\n"),
	            (unsigned long long)len, s);
	    return s;
	}
    }
}

/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return mkString("Win32");
}

static DWORD StartTime;

static FILETIME Create, Exit, Kernel, User;

void R_setStartTime(void)
{
    StartTime = GetTickCount();
}

void R::R_getProcTime(double *data)
{
    DWORD elapsed;
    double kernel, user;

    /* This is in msec, but to clock-tick accuracy,
       said to be 10ms on NT and 55ms on Win95 */
    elapsed = (GetTickCount() - StartTime) / 10;

    /* These are in units of 100ns, but with an accuracy only
       in clock ticks.  So we round to 0.01s */
    GetProcessTimes(GetCurrentProcess(), &Create, &Exit, &Kernel, &User);
    user = 1e-5 * ((double) User.dwLowDateTime +
		   (double) User.dwHighDateTime * 4294967296.0);
    user = floor(user)/100.0;
    kernel = 1e-5 * ((double) Kernel.dwLowDateTime +
		     (double) Kernel.dwHighDateTime * 4294967296.0);
    kernel = floor(kernel)/100.0;
    data[0] = user;
    data[1] = kernel;
    data[2] = (double) elapsed / 100.0;
    data[3] = R_NaReal;
    data[4] = R_NaReal;
}

/* use in memory.c: increments for CPU times */
double R::R_getClockIncrement(void)
{
    return 1.0 / 100.0;
}

/*
 * Stderr, Stdout
 *   =FALSE .. drop output
 *   =TRUE  .. return output
 *   =""    .. print to standard error/output
 *   =fname .. redirect to file of that name
 *
 * Redirection and dropping is supported with all flag values. Printing is
 * supported with all flag values on non-RGui only (and happens via standard
 * handles). For returning output (anywhere) and printing (on RGui),
 * restrictions apply (below).
 * 
 * flag =0 don't wait
 *   returning of output not supported
 *   RGui: non-redirected standard error and standard output always dropped
 *         (printing not supported)
 *
 * flag =1 wait
 *   otherwise like flag =0
 *
 * flag =2 wait/printing in RGui
 *   returning of output not supported
 *   non-RGui: works like flag =1
 *   RGui: standard error and/or standard output is printed on console;
 *         flag=2 may only be used when at least one of the outputs
 *         is to be printed
 *
 * flag =3 wait/return output
 *   standard error and/or standard output is returned
 *   flag=3 may only be used when at least one of the outputs is to be returned
 *   RGui: printing is not supported (one cannot return one output and print
 *         the other)
 * 
 * Add 10 to flag to minimize application
 * Add 20 to flag make application "invisible"
*/

#include "run.h"

#define INTERN_BUFSIZE 8096
SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    rpipe *fp;
    char  buf[INTERN_BUFSIZE];
    const char *fout = "", *ferr = "";
    int   vis = 0, flag = 2, i = 0, j, ll = 0;
    SEXP  cmd, fin, Stdout, Stderr, tchar;
    GCStackRoot<> tlist(R_NilValue);
    int timeout = 0, timedout = 0;
    CXXR::RAllocStack::Scope rscope;

    checkArity(op, args);
    cmd = CAR(args);
    if (!isString(cmd) || LENGTH(cmd) != 1)
	errorcall(call, "%s", _("character string expected as first argument"));
    args = CDR(args);
    flag = asInteger(CAR(args)); args = CDR(args);
    if (flag >= 20) {vis = -1; flag -= 20;}
    else if (flag >= 10) {vis = 0; flag -= 10;}
    else vis = 1;

    fin = CAR(args);
    if (!isString(fin))
	errorcall(call, "%s", _("character string expected as third argument"));
    args = CDR(args);
    Stdout = CAR(args);
    args = CDR(args);
    Stderr = CAR(args);
    args = CDR(args);
    timeout = asInteger(CAR(args));
    if (timeout == NA_INTEGER || timeout < 0 || timeout > 2000000)
	/* the limit could be increased, but not much as in milliseconds it
	   has to fit into a 32-bit unsigned integer */
	errorcall(call, _("invalid '%s' argument"), "timeout");
    if (timeout && !flag)
	errorcall(call, "%s", _("Timeout with background running processes is not supported."));
    args = CDR(args);
    bool consignals = asLogicalNoNA(CAR(args), "receive.console.signals");

    if (CharacterMode == RGui) {
	/* This is a rather conservative approach: if
	   Rgui is launched from a console window it does have
	   standard handles -- but users might well not expect that.
	*/
	SetStdHandle(STD_INPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_OUTPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_ERROR_HANDLE, INVALID_HANDLE_VALUE);
	if (TYPEOF(Stdout) == STRSXP)
	    fout = translateCharFP(STRING_ELT(Stdout, 0));
	if (TYPEOF(Stderr) == STRSXP)
	    ferr = translateCharFP(STRING_ELT(Stderr, 0));
    } else {
	if (flag == 2) flag = 1; /* ignore std.output.on.console */
	if (TYPEOF(Stdout) == STRSXP)
	    fout = translateCharFP(STRING_ELT(Stdout, 0));
	else if (asLogical(Stdout) == 0)
	    fout = NULL;
	if (TYPEOF(Stderr) == STRSXP)
	    ferr = translateCharFP(STRING_ELT(Stderr, 0));
	else if (asLogical(Stderr) == 0)
	    ferr = NULL;
    }

    if (flag < 2) { /* Neither intern = TRUE nor
		       show.output.on.console for Rgui */
	ll = runcmd_timeout(translateCharFP(STRING_ELT(cmd, 0)),
		    CE_NATIVE,
		    flag, vis, translateCharFP(STRING_ELT(fin, 0)), fout, ferr,
		    timeout, &timedout, consignals);
	if (ll == NOLAUNCH) warning("%s", runerror());
    } else {
	/* read stdout +/- stderr from pipe */
	int m = -1;
	if ((TYPEOF(Stderr) == LGLSXP && asLogical(Stderr)) ||
	   (CharacterMode == RGui && TYPEOF(Stderr) == STRSXP && ferr && !ferr[0]))
	    /* read stderr from pipe */
	    m = 2;
	if ((TYPEOF(Stdout) == LGLSXP && asLogical(Stdout)) ||
	    (CharacterMode == RGui && TYPEOF(Stdout) == STRSXP && fout && !fout[0]))
	    /* read stdout from pipe */
	    m = (m == 2) ? 3 : 0;
	if (m == -1)
	    /* does not happen with system()/system2() */
	    error(_("invalid '%s' argument"), "flag");

	fp = rpipeOpen(translateCharFP(STRING_ELT(cmd, 0)), CE_NATIVE,
		       vis, translateCharFP(STRING_ELT(fin, 0)), m, fout, ferr,
	               timeout, 0);
	if (!fp) {
	    /* If intern = TRUE generate an error */
	    if (flag == 3) error("%s", runerror());
	    ll = NOLAUNCH;
	} else {
	    if (flag == 3) { /* intern */
		for (i = 0; rpipeGets(fp, buf, INTERN_BUFSIZE); i++) {
		    ll = strlen(buf) - 1;
		    if ((ll >= 0) && (buf[ll] == '\n')) buf[ll] = '\0';
		    tchar = mkChar(buf);
		    tlist = CONS(tchar, tlist);
		}
	    } else { /* print on RGui console */
		for (i = 0; rpipeGets(fp, buf, INTERN_BUFSIZE); i++)
		    R_WriteConsole(buf, strlen(buf));
	    }
	    ll = rpipeClose(fp, &timedout);
	}
    }
    if (timedout) {
	ll = 124;
	warning(_("command '%s' timed out after %ds"),
	        translateChar(STRING_ELT(cmd, 0)), timeout);
    } else if (flag == 3 && ll) {
	warning(_("running command '%s' had status %d"), 
	        translateChar(STRING_ELT(cmd, 0)), ll);
    }
    if (flag == 3) { /* intern = TRUE: convert pairlist to list */
	GCStackRoot<> rval;
	rval = allocVector(STRSXP, i);
	for (j = (i - 1); j >= 0; j--) {
	    SET_STRING_ELT(rval, j, CAR(tlist));
	    tlist = CDR(tlist);
	}
	if (ll) {
	    SEXP lsym = install("status");
	    setAttrib(rval, lsym, ScalarInteger(ll));
	}
	return rval;
    } else {
	SEXP rval = ScalarInteger(ll);
	Evaluator::enableResultPrinting(false);
	return rval;
    }
}
