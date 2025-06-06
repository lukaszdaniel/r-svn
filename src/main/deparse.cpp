/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  IMPLEMENTATION NOTES:
 *
 *  Deparsing has 3 layers.
 *  - The user interfaces, do_deparse(), do_dput(), and do_dump() should
 *    not be called from an internal function.
 *  - unless nlines > 0, the actual deparsing via deparse2() needs
 *    to be done twice, once to count things up and a second time to put
 *    them into the string vector for return.
 *  - Printing this to a file is handled by the calling routine.
 *
 *  Current call paths:
 *
 *    do_deparse() ------------> deparse1WithCutoff()
 *    do_dput() -> deparse1() -> deparse1WithCutoff()
 *    do_dump() -> deparse1() -> deparse1WithCutoff()
 *  ---------
 *  Workhorse: deparse1WithCutoff() -> deparse2() -> deparse2buff() --> {<itself>, ...}
 *  ---------  ~~~~~~~~~~~~~~~~~~  implicit arg R_BrowseLines == getOption("deparse.max.lines")
 *
 *  ./errors.c: PrintWarnings() | warningcall_dflt() ... -> deparse1s() -> deparse1WithCutoff()
 *  ./print.c : Print[Language|Closure|Expression]()    --> deparse1w() -> deparse1WithCutoff()
 *  bind.c,match.c,..: c|rbind(), match(), switch()...-> deparse1line() -> deparse1WithCutoff()
 *
 *  INDENTATION:
 *
 *  Indentation is carried out in the routine printtab2buff at the
 *  bottom of this file.  It seems like this should be settable via
 *  options.
 *
 *
 *  LocalParseData VARIABLES  (historically GLOBALs):
 *
 *  linenumber:	 counts the number of lines that have been written,
 *		 this is used to setup storage for deparsing.
 *
 *  len:	 counts the length of the current line, it will be
 *		 used to determine when to break lines.
 *
 *  incurly:	 keeps track of whether we are inside a curly or not,
 *		 this affects the printing of if-then-else.
 *
 *  inlist:	 keeps track of whether we are inside a list or not,
 *		 this affects the printing of if-then-else.
 *
 *  startline:	 indicator TRUE=start of a line (so we can tab out to
 *		 the correct place).
 *
 *  indent:	 how many tabs should be written at the start of
 *		 a line.
 *
 *  buff:	 contains the current string, we attempt to break
 *		 lines at cutoff, but can unlimited length.
 *
 *  lbreak:	 often used to indicate whether a line has been
 *		 broken, this makes sure that that indenting behaves
 *		 itself.
 */

/* DTL ('duncan'):
* The code here used to use static variables to share values
* across the different routines. These have now been collected
* into a struct named  LocalParseData and this is explicitly
* passed between the different routines. This avoids the needs
* for the global variables and allows multiple evaluators, potentially
* in different threads, to work on their own independent copies
* that are local to their call stacks. This avoids any issues
* with interrupts, etc. not restoring values.

* The previous issue with the global "cutoff" variable is now implemented
* by creating a deparse1WithCutoff() routine which takes the cutoff from
* the caller and passes this to the different routines as a member of the
* LocalParseData struct. Access to the deparse1() routine remains unaltered.
* This is exactly as Ross had suggested ...
*
* One possible fix is to restructure the code with another function which
* takes a cutoff value as a parameter.	 Then "do_deparse" and "deparse1"
* could each call this deeper function with the appropriate argument.
* I wonder why I didn't just do this? -- it would have been quicker than
* writing this note.  I guess it needs a bit more thought ...
*/

/** @file deparse.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat> /* for DBL_DIG */
#include <CXXR/RAllocStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <Print.h>
#include <Fileio.h>
#ifdef Win32
#include <trioremap.h>
#endif

#define BUFSIZE 512

#define MIN_Cutoff 20
#define DEFAULT_Cutoff 60
#define MAX_Cutoff (BUFSIZE - 12)
/* ----- MAX_Cutoff  <	BUFSIZE !! */

#include "RBufferUtils.h"

using namespace R;
using namespace CXXR;

typedef R_StringBuffer DeparseBuffer;

struct LocalParseData {
    int linenumber;
    size_t len;
    int incurly;
    int inlist;
    bool startline; /* = true; */
    int indent;
    SEXP strvec;
    int left;

    DeparseBuffer buffer;

    size_t cutoff;
    bool backtick;
    int opts;
    bool sourceable;
#ifdef longstring_WARN
    bool longstring;
#endif
    int maxlines;
    bool active;
    bool isS4;
    bool fnarg; /* fn argument, so parenthesize = as assignment */
};

static SEXP deparse1WithCutoff(SEXP call, bool abbrev, size_t cutoff,
			       bool backtick, int opts, int nlines);
static void args2buff(SEXP, int, int, LocalParseData *);
static void deparse2buff(SEXP, LocalParseData *);
static void print2buff(const char *, LocalParseData *);
static void printtab2buff(int, LocalParseData *);
static void writeline(LocalParseData *);
static void vec2buff(SEXP, LocalParseData *, bool do_names);
static void vector2buff(SEXP, LocalParseData *);
static void src2buff1(SEXP, LocalParseData *);
static bool src2buff(SEXP, int, LocalParseData *);
static void linebreak(bool *lbreak, LocalParseData *);
static void deparse2(SEXP, SEXP, LocalParseData *);

// .Internal(deparse(expr, width.cutoff, backtick, .deparseOpts(control), nlines))
attribute_hidden SEXP do_deparse(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP expr = CAR(args); args = CDR(args);
    int cut0 = DEFAULT_Cutoff;
    if(!isNull(CAR(args))) {
	cut0 = asInteger(CAR(args));
	if (cut0 == NA_INTEGER || cut0 < MIN_Cutoff || cut0 > MAX_Cutoff) {
	    warning("%s", _("invalid 'cutoff' value for 'deparse', using default"));
	    cut0 = DEFAULT_Cutoff;
	}
    }
    args = CDR(args);
    bool backtick = isNull(CAR(args)) ? 0 : asRbool(CAR(args), call);
    args = CDR(args);
    int opts = isNull(CAR(args)) ? SHOWATTRIBUTES : asInteger(CAR(args));
    args = CDR(args);
    int nlines = asInteger(CAR(args));
    if (nlines == NA_INTEGER) nlines = -1;
    return deparse1WithCutoff(expr, false, cut0, backtick, opts, nlines);
}

// deparse1() version *looking* at getOption("deparse.max.lines")
attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::deparse1m(SEXP call, bool abbrev, int opts)
{
    bool backtick = true;
    int old_bl = R_BrowseLines,
        blines = asInteger(GetOption1(install("deparse.max.lines")));
    if (blines != NA_INTEGER && blines > 0)
        R_BrowseLines = blines;
    SEXP result = deparse1WithCutoff(call, abbrev, DEFAULT_Cutoff, backtick,
				     opts, 0);
    R_BrowseLines = old_bl;
    return result;
}

// deparse1() version with R_BrowseLines := 0
SEXP R::deparse1(SEXP call, bool abbrev, int opts)
{
    bool backtick = true;
    int old_bl = R_BrowseLines;
    R_BrowseLines = 0;
    SEXP result = deparse1WithCutoff(call, abbrev, DEFAULT_Cutoff, backtick,
				     opts, 0);
    R_BrowseLines = old_bl;
    return result;
}


/* used for language objects in print(), in print.c */
attribute_hidden
SEXP R::deparse1w(SEXP call, bool abbrev, int opts)
{
    bool backtick = true;
    return deparse1WithCutoff(call, abbrev, R_print.cutoff, backtick, opts, -1);
}

static SEXP deparse1WithCutoff(SEXP call, bool abbrev, size_t cutoff,
			       bool backtick, int opts, int nlines)
{
/* Arg. abbrev:
	If abbrev is true, then the returned value
	is a STRSXP of length 1 with at most 13 characters.
	This is used for plot labelling etc.
*/
    SEXP svec;
    int savedigits;
    bool need_ellipses = false;
    LocalParseData localData;
	localData.linenumber = 0;
	localData.len = 0;
	localData.incurly = 0;
	localData.inlist = 0;
	localData.startline = true;
	localData.indent = 0;
	localData.strvec = NULL;
	localData.left = 0;
	localData.buffer = { NULL, 0, BUFSIZE };
	localData.cutoff = DEFAULT_Cutoff;
	localData.backtick = false;
	localData.opts = 0;
	localData.sourceable = true;
#ifdef longstring_WARN
	localData.longstring = false;
#endif
	localData.maxlines = INT_MAX;
	localData.active = true;
	localData.isS4 = 0;
	localData.fnarg = false;

    localData.cutoff = cutoff;
    localData.backtick = backtick;
    localData.opts = opts;
    localData.strvec = R_NilValue;

    PrintDefaults(); /* from global options() */
    savedigits = R_print.digits;
    R_print.digits = DBL_DIG;/* MAX precision */
    print2buff("", &localData); /* ensure allocation of buffer.data, PR#17876 */

    svec = R_NilValue;
    if (nlines > 0) {
	localData.linenumber = localData.maxlines = nlines;
    } else { // default: nlines = -1 (from R), or = 0 (from other C fn's)
	if(R_BrowseLines > 0)// not by default; e.g. from getOption("deparse.max.lines")
	    localData.maxlines = R_BrowseLines + 1; // enough to determine linenumber
	deparse2(call, svec, &localData);
	localData.active = true;
	if(R_BrowseLines > 0 && localData.linenumber > R_BrowseLines) {
	    localData.linenumber = R_BrowseLines + 1;
	    need_ellipses = true;
	}
    }
    PROTECT(svec = allocVector(STRSXP, localData.linenumber));
    deparse2(call, svec, &localData);
    if (abbrev) {
	char data[14];
	strncpy(data, CHAR(STRING_ELT(svec, 0)), 10);
	data[10] = '\0';
	if (strlen(CHAR(STRING_ELT(svec, 0))) > 10) strcat(data, "...");
	svec = mkString(data);
    } else if(need_ellipses) {
	SET_STRING_ELT(svec, R_BrowseLines, mkChar("  ..."));
    }
    if(nlines > 0 && localData.linenumber < nlines) {
	UNPROTECT(1); /* old svec value */
	PROTECT(svec);
	svec = lengthgets(svec, localData.linenumber);
    }
    UNPROTECT(1);
    PROTECT(svec); /* protect from warning() allocating, PR#14356 */
    R_print.digits = savedigits;
    /*: Don't warn anymore, we do deal with most (-> 'S4SXP' below)
    if ((opts & WARNINCOMPLETE) && localData.isS4)
	warning("%s", _("deparse of an S4 object may not always be source()able"));
	else */
    if ((opts & WARNINCOMPLETE) && !localData.sourceable)
	warning("%s", _("deparse may be incomplete"));
#ifdef longstring_WARN
    if ((opts & WARNINCOMPLETE) && localData.longstring)
	warning("%s", _("deparse may be not be source()able in R < 2.7.0"));
#endif
    /* somewhere lower down might have allocated ... */
    R_FreeStringBuffer(&(localData.buffer));
    UNPROTECT(1);
    return svec;
}

/* deparse1line(), e.g. for non-trivial list entries in as.character(<list>).
 * --------------
 * Concatenates all lines into one long one.
 * This is needed in terms.formula, where we must be able
 * to deparse a term label into a single line of text so
 * that it can be reparsed correctly */
// Used in coerce.c and relop.c
attribute_hidden
SEXP R::deparse1line_ex(SEXP call, bool abbrev, int opts)
{
    bool backtick = true;
    int lines;
    SEXP temp = PROTECT(
	    deparse1WithCutoff(call, abbrev, MAX_Cutoff, backtick, opts, -1));
    if ((lines = length(temp)) > 1) {
	char *buf;
	size_t len = 0;
	cetype_t enc = CE_NATIVE;
	for (int i = 0; i < length(temp); i++) {
	    SEXP s = STRING_ELT(temp, i);
	    cetype_t thisenc = getCharCE(s);
	    len += strlen(CHAR(s));  // FIXME: check for overflow?
	    if (thisenc != CE_NATIVE)
		enc = thisenc; /* assume only one non-native encoding */
	}
	CXXR::RAllocStack::Scope rscope;
	buf = R_alloc((size_t) len+lines, sizeof(char));
	*buf = '\0';
	for (int i = 0; i < length(temp); i++) {
	    if (i % 1000 == 999) R_CheckUserInterrupt();
	    strcat(buf, CHAR(STRING_ELT(temp, i)));
	    if (i < lines - 1)
		strcat(buf, "\n");
	}
	temp = ScalarString(mkCharCE(buf, enc));
    }
    UNPROTECT(1);
    return temp;
}

SEXP R::deparse1line(SEXP call, bool abbrev)
{
    return deparse1line_ex(call, abbrev, SIMPLEDEPARSE);
}


// called only from ./errors.c  for calls in warnings and errors :
attribute_hidden SEXP R::deparse1s(SEXP call)
{
   bool backtick = true;
   return
       deparse1WithCutoff(call, false, DEFAULT_Cutoff, backtick,
			  DEFAULTDEPARSE, /* nlines = */ 1);
}

#include <Rconnections.h>

// .Internal(dput(x, file, .deparseOpts(control)))
attribute_hidden SEXP do_dput(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    GCStackRoot<> tval; /* against Rconn_printf */
    tval = CAR(args);
    int opts = isNull(CADDR(args)) ? SHOWATTRIBUTES : asInteger(CADDR(args));

    if (TYPEOF(tval) == CLOSXP) {
	SEXP clo = PROTECT(duplicate(tval));
	SET_CLOENV(clo, R_GlobalEnv);
	tval = deparse1(clo, FALSE, opts);
	UNPROTECT(1);
    } else
	tval = deparse1(tval, FALSE, opts);

    if(!inherits(CADR(args), "connection"))
	error("%s", _("'file' must be a character string or connection"));
    int ifile = asInteger(CADR(args));
    if (ifile != 1) {
	Rconnection con = getConnection(ifile);
	bool wasopen = con->isopen;
	if(!wasopen) {
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "w");
	    if(!con->open(con)) error("%s", _("cannot open the connection"));
	    strcpy(con->mode, mode);
	}
	if(!con->canwrite) error("%s", _("cannot write to this connection"));
	/* Set up a context which will close the connection on error */
	try {
	bool havewarned = false;
	for (int i = 0; i < LENGTH(tval); i++) {
	    int res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, i)));
	    if(!havewarned &&
	       (size_t) res < strlen(CHAR(STRING_ELT(tval, i))) + 1) {
		warning("%s", _("wrote too few characters"));
		havewarned = true;
	    }
	}
	} catch (...) {
        if (!wasopen && con->isopen) con->close(con);
        throw;
	}
	if(!wasopen) { con->close(con); }
    }
    else { // ifile == 1 : "Stdout"
	for (int i = 0; i < LENGTH(tval); i++)
	    Rprintf("%s\n", CHAR(STRING_ELT(tval, i)));
    }

    return (CAR(args));
}

// .Internal(dump(list, file, envir, opts, evaluate))
attribute_hidden SEXP do_dump(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP names = CAR(args),
	 file = CADR(args);
    if(!inherits(file, "connection"))
	error(_("'%s' must be a character string or connection"), "file");
    if(!isString(names))
	error("%s", _("character arguments expected"));
    int nobjs = length(names);
    if(nobjs < 1 || length(file) < 1)
	error("%s", _("zero-length argument"));
    SEXP source = CADDR(args);
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    int opts = asInteger(CADDDR(args));
    /* <NOTE>: change this if extra options are added */
    if(opts == NA_INTEGER || opts < 0 || opts > 2048)
	error("%s", _("'opts' should be small non-negative integer"));
    // evaluate :
    if (!asLogical(CAD4R(args))) opts |= DELAYPROMISES;

    SEXP objs, o = PROTECT(objs = allocList(nobjs));
    int nout = 0;
    for (int i = 0; i < nobjs; i++, o = CDR(o)) {
	SET_TAG(o, installTrChar(STRING_ELT(names, i)));
	SETCAR(o, R_findVar(TAG(o), source));
	if (CAR(o) == R_UnboundValue)
	    warning(_("object '%s' not found"), EncodeChar(PRINTNAME(TAG(o))));
	else nout++;
    }
    o = objs;
    SEXP outnames = PROTECT(allocVector(STRSXP, nout)); // -> result
    if(nout > 0) {
	if(INTEGER(file)[0] == 1) {
	    for (int i = 0, nout = 0; i < nobjs; i++) {
		if (CAR(o) == R_UnboundValue) continue;
		const char *obj_name = translateChar(STRING_ELT(names, i));
		SET_STRING_ELT(outnames, nout++, STRING_ELT(names, i));
		if(isValidName(obj_name)) Rprintf("%s <-\n", obj_name);
		else if(opts & S_COMPAT) Rprintf("\"%s\" <-\n", obj_name);
		else Rprintf("`%s` <-\n", obj_name);
		SEXP tval = PROTECT(deparse1(CAR(o), FALSE, opts));
		for (int j = 0; j < LENGTH(tval); j++)
		    Rprintf("%s\n", CHAR(STRING_ELT(tval, j)));/* translated */
		UNPROTECT(1); /* tval */
		o = CDR(o);
	    }
	}
	else {
	    Rconnection con = getConnection(INTEGER(file)[0]);
	    bool wasopen = con->isopen;
	    if(!wasopen) {
		char mode[5];
		strcpy(mode, con->mode);
		strcpy(con->mode, "w");
		if(!con->open(con)) error("%s", _("cannot open the connection"));
		strcpy(con->mode, mode);
	    }
	    if(!con->canwrite) error("%s", _("cannot write to this connection"));
	    /* use try-catch to close the connection on error */
	    try {
	    bool havewarned = false;
	    for (int i = 0, nout = 0; i < nobjs; i++) {
		if (CAR(o) == R_UnboundValue) continue;
		SET_STRING_ELT(outnames, nout++, STRING_ELT(names, i));
		int res;
		const char *s = translateChar(STRING_ELT(names, i));
		unsigned int extra = 6;
		if(isValidName(s)) {
		    extra = 4;
		    res = Rconn_printf(con, "%s <-\n", s);
		} else if(opts & S_COMPAT)
		    res = Rconn_printf(con, "\"%s\" <-\n", s);
		else
		    res = Rconn_printf(con, "`%s` <-\n", s);
		if(!havewarned && (size_t) res < strlen(s) + extra)
		    warning("%s", _("wrote too few characters"));
		GCStackRoot<> tval;
		tval = deparse1(CAR(o), FALSE, opts);
		for (int j = 0; j < LENGTH(tval); j++) {
		    res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, j)));
		    if(!havewarned &&
		       (size_t) res < strlen(CHAR(STRING_ELT(tval, j))) + 1) {
			warning("%s", _("wrote too few characters"));
			havewarned = true;
		    }
		}
		o = CDR(o);
	    }
	    } catch (...) {
            if (!wasopen && con->isopen)
                con->close(con);
            throw;
	    }
	    if(!wasopen) { con->close(con); }
	}
    }

    UNPROTECT(2);
    return outnames;
}

static void linebreak(bool *lbreak, LocalParseData *d)
{
    if (d->len > d->cutoff) {
	if (!*lbreak) {
	    *lbreak = true;
	    d->indent++;
	}
	writeline(d);
    }
}

static void deparse2(SEXP what, SEXP svec, LocalParseData *d)
{
    d->strvec = svec;
    d->linenumber = 0;
    d->indent = 0;
    deparse2buff(what, d);
    writeline(d);
}


/* curlyahead looks at s to see if it is a list with
   the first op being a curly.  You need this kind of
   lookahead info to print if statements correctly.  */
static bool curlyahead(SEXP s)
{
    if (isList(s) || isLanguage(s))
	if (TYPEOF(CAR(s)) == SYMSXP && CAR(s) == R_BraceSymbol)
	    return true;
    return false;
}

/* needsparens looks at an arg to a unary or binary operator to
   determine if it needs to be parenthesized when deparsed
   mainop is a unary or binary operator,
   arg is an argument to it, on the left if left == 1 */

static bool needsparens(PPinfo mainop, SEXP arg, unsigned int left,
			    unsigned int deepLeft)
{
    PPinfo arginfo;
    if (TYPEOF(arg) == LANGSXP) {
	if (TYPEOF(CAR(arg)) == SYMSXP) {
	    if (Rf_isPrimitive(SYMVALUE(CAR(arg)))) {
		arginfo = PPINFO(SYMVALUE(CAR(arg)));

		/* Not all binary ops are binary! */
		switch(arginfo.kind) {
		case PP_BINARY:
		case PP_BINARY2:
		    switch(length(CDR(arg))) {
		    case 1:
			/* binary +/- precedence upgraded as unary */
			if (arginfo.precedence == PREC_SUM)
			    arginfo.precedence = PREC_SIGN;
			arginfo.kind = PP_UNARY;
			break;
		    case 2:
			break;
		    default:
			return false;
		    }
		default:
		    break;
		}

		switch(arginfo.kind) {
		case PP_SUBSET:
		    switch (mainop.kind) {
		    case PP_DOLLAR:
		    case PP_SUBSET:
			if (mainop.precedence > arginfo.precedence)
			    return false;
			/* else fall through */
		    default:
			break;
		    }
		case PP_BINARY:
		case PP_BINARY2:
		    if (mainop.precedence == PREC_COMPARE &&
			arginfo.precedence == PREC_COMPARE)
			return true;     /*   a < b < c   is not legal syntax */
		    /* else fall through */
		case PP_ASSIGN:
		case PP_ASSIGN2:
		case PP_DOLLAR:
		    if (mainop.precedence > arginfo.precedence
			|| (mainop.precedence == arginfo.precedence && left == mainop.rightassoc)) {
			return true;
		    }
		    break;
		case PP_UNARY:
		    return ((left && mainop.precedence > arginfo.precedence)
			|| (deepLeft && deepLeft > arginfo.precedence));
		case PP_FOR:
		case PP_IF:
		case PP_WHILE:
		case PP_REPEAT:
		    return (left || deepLeft);
		default:
		    return false;
		}
	    } else if (isUserBinop(CAR(arg))) {
		if (mainop.precedence > PREC_PERCENT
		    || (mainop.precedence == PREC_PERCENT && left == mainop.rightassoc)) {
		    return true;
		}
	    }
	}
    }
    else if ((TYPEOF(arg) == CPLXSXP) && (length(arg) == 1)) {
	if (mainop.precedence > PREC_SUM
	    || (mainop.precedence == PREC_SUM && left == mainop.rightassoc)) {
	    return true;
	}
    }
    return false;
}


/* does the character() vector x contain one `NA_character_` or is all "",
 * or if(isAtomic) does it have one "recursive" or "use.names" ?  */
static bool usable_nice_names(SEXP x, bool isAtomic)
{
    if (TYPEOF(x) == STRSXP) {
	R_xlen_t n = xlength(x);
	bool all_0 = true;
	if (isAtomic) // c(*, recursive=, use.names=): cannot use these as nice_names
	    for (R_xlen_t i = 0; i < n; i++) {
		if (STRING_ELT(x, i) == NA_STRING
		    || streql(CHAR(STRING_ELT(x, i)), "recursive")
		    || streql(CHAR(STRING_ELT(x, i)), "use.names"))
		    return false;
		else if (all_0 && *CHAR(STRING_ELT(x, i))) /* length test */
		    all_0 = false;
	    }
	else
	    for (R_xlen_t i = 0; i < n; i++) {
		if (STRING_ELT(x, i) == NA_STRING)
		    return false;
		else if (all_0 && *CHAR(STRING_ELT(x, i))) /* length test */
		    all_0 = false;
	    }

	return (!all_0);
    }
    return true;
}


typedef enum { UNKNOWN = -1,
	       SIMPLE = 0,
	       OK_NAMES,   // no structure(*); names written as  (n1 = v1, ..)
	       STRUC_ATTR, // use structure(*, <attr> = *, ..) for non-names only
	       STRUC_NMS_A // use structure(*, <attr> = *, ..)  for names, too
} attr_type;

#ifdef DEBUG_DEPARSE
static const char* attrT2char(attr_type typ) {
    switch(typ) {
    case UNKNOWN: return "UNKNOWN";
    case SIMPLE: return "SIMPLE";
    case OK_NAMES: return "OK_NAMES";
    case STRUC_ATTR: return "STRUC_ATTR";
    case STRUC_NMS_A: return "STRUC_NMS_A";
    default: return "_unknown_ attr_type -- should *NOT* happen!";
    }
}
# define ChTF(_logic_) (_logic_ ? "true" : "false")
#endif

/* Exact semantic of NICE_NAMES and SHOWATTRIBUTES i.e. "niceNames" and "showAttributes"

C|  depCtrl   | attr1() result
-| -----------+-----------------------------------------------------------------------------
1|  NN &&  SA | STRUCT_ATTR + NN  or  STRUC_NMS_A (if NN are not "allowed")
2| !NN &&  SA | if(has attr) STRUC_NMS_A  else "SIMPLE"
3|  NN && !SA | OK_NAMES   ||  SIMPLE  if(!has_names)
4| !NN && !SA | SIMPLE


C|  depCtrl   : what should   deparse(*, control = depCtrl)   do ?
-| -----------+-----------------------------------------------------------------------------
1|  NN &&  SA : all attributes(but srcref); names "NICE"ly (<nam> = <val>) if valid [no NA]
2| !NN &&  SA : all attributes( "    "   ) use structure(..) incl names but no _nice_ names
3|  NN && !SA : no attributes but names, names nicely even when "wrong" (i.e. NA in names(.))
4| !NN && !SA : no attributes shown, not even names

*/

// is *only* called  if (d->opts & SHOW_ATTR_OR_NMS) = d->opts & (SHOW_A | NICE_N)
static attr_type attr1(SEXP s, LocalParseData *d)
{
    SEXP a = ATTRIB(s), nm = getAttrib(s, R_NamesSymbol);
    attr_type attr = UNKNOWN;
    bool
	nice_names = (d->opts & NICE_NAMES),
	show_attr  = (d->opts & SHOWATTRIBUTES),
	has_names = (!isNull(nm)), ok_names;
#ifdef DEBUG_DEPARSE
    REprintf("  attr1(): has_names = %s", ChTF(has_names));
#endif
    if(has_names) {
	// ok only if there's no  NA_character_,.. in names() nor all """
	ok_names = (nice_names && usable_nice_names(nm, isVectorAtomic(s)));
#ifdef DEBUG_DEPARSE
	REprintf(", ok_names = %s", ChTF(ok_names));
#endif
	if(!ok_names)
	    attr = show_attr ? STRUC_NMS_A :
		/* nice_names */  OK_NAMES; // even when not ok
    }

    while(attr == UNKNOWN && !isNull(a)) {
	if(has_names && TAG(a) == R_NamesSymbol) {
	    // also  ok_names = true
	} else if(show_attr && TAG(a) != R_SrcrefSymbol) {
	    attr = STRUC_ATTR;
	    break;
	}
	// else
	a = CDR(a);
    }
    if(attr == UNKNOWN)
	attr = has_names ? OK_NAMES : SIMPLE;

    if(attr >= STRUC_ATTR) {
	print2buff("structure(", d);
    } else if(has_names) { // attr <= OK_NAMES
    }
#ifdef DEBUG_DEPARSE
    REprintf(", returning %s\n", attrT2char(attr));
#endif
    return attr;
}

static void attr2(SEXP s, LocalParseData *d, bool not_names)
{
    SEXP a = ATTRIB(s);
    while(!isNull(a)) {
	if(TAG(a) != R_SrcrefSymbol &&
	   !(TAG(a) == R_NamesSymbol && not_names)) {
	    print2buff(", ", d);
	    if(TAG(a) == R_DimSymbol) {
		print2buff("dim", d); // was .Dim
	    }
	    else if(TAG(a) == R_DimNamesSymbol) {
		print2buff("dimnames", d); // was .Dimnames
	    }
	    else if(TAG(a) == R_NamesSymbol) {
		print2buff("names", d); // was .Names
	    }
	    else if(TAG(a) == R_TspSymbol) {
		print2buff("tsp", d); // was .Tsp
	    }
	    else if(TAG(a) == R_LevelsSymbol) {
		print2buff("levels", d); // was .Label
	    }
	    else {
		/* TAG(a) might contain spaces etc */
		const char *tag = CHAR(PRINTNAME(TAG(a)));
		int d_opts_in = d->opts;
		d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
		if(isValidName(tag))
		    deparse2buff(TAG(a), d);
		else {
		    print2buff("\"", d);
		    deparse2buff(TAG(a), d);
		    print2buff("\"", d);
		}
		d->opts = d_opts_in;
	    }
	    print2buff(" = ", d);
	    bool fnarg = d->fnarg;
	    d->fnarg = true;
	    deparse2buff(CAR(a), d);
	    d->fnarg = fnarg;
	}
	a = CDR(a);
    }
    print2buff(")", d);
}

static const char *quotify(SEXP name, int quote)
{
    const char *s = CHAR(name);

    /* If a symbol is not a valid name, put it in quotes, escaping
     * any quotes in the string itself */

    if (isValidName(s) || *s == '\0') return s;

    return EncodeString(name, 0, quote, Rprt_adj_none);
}

/* check for whether we need to parenthesize a caller.  The unevaluated ones
   are tricky:
   We want
     x$f(z)
     x[n](z)
     base::mean(x)
   but
     (f+g)(z)
     (function(x) 1)(x)
     etc.
*/
static bool parenthesizeCaller(SEXP s)
{
    SEXP op, sym;
    if (TYPEOF(s) == LANGSXP) { /* unevaluated */
	op = CAR(s);
	if (TYPEOF(op) == SYMSXP) {
	    if (isUserBinop(op)) return true;   /* %foo% */
	    sym = SYMVALUE(op);
	    if (Rf_isPrimitive(sym)) {
		if (PPINFO(sym).precedence >= PREC_SUBSET
		    || PPINFO(sym).kind == PP_FUNCALL
		    || PPINFO(sym).kind == PP_PAREN
		    || PPINFO(sym).kind == PP_CURLY) return false; /* x$f(z) or x[n](z) or f(z) or (f) or {f} */
		else return true;		/* (f+g)(z) etc. */
	    }
	    return false;			/* regular function call */
	 } else
	    return true;			/* something strange, like (1)(x) */
    } else
	return (TYPEOF(s) == CLOSXP);
}

/* This is the recursive part of deparsing. */

#define SIMPLE_OPTS (~QUOTEEXPRESSIONS & ~SHOWATTRIBUTES & ~DELAYPROMISES)
/* keep KEEPINTEGER | USESOURCE | KEEPNA | S_COMPAT, also
   WARNINCOMPLETE but that is not used below this point. */
#define SHOW_ATTR_OR_NMS (SHOWATTRIBUTES | NICE_NAMES)

static void deparse2buff(SEXP s, LocalParseData *d)
{
    bool lookahead = false, lbreak = false, fnarg = d->fnarg;
    attr_type attr = STRUC_ATTR;
    SEXP t;
    int d_opts_in = d->opts, i, n;

    d->fnarg = false;

    /* This flag should only be set when recursing through the LHS
       of binary ops, so by default we reset to zero */
    int prevLeft = d->left;
    d->left = 0;

    if (!d->active) return;

    if (IS_S4_OBJECT(s)) {
	d->isS4 = true;
	/* const void *vmax = vmaxget(); */
	SEXP class_ = getAttrib(s, R_ClassSymbol),
	    cl_def = TYPEOF(class_) == STRSXP ? STRING_ELT(class_, 0) : R_NilValue;
	if(TYPEOF(cl_def) == CHARSXP) { // regular S4 objects
	    print2buff("new(\"", d);
	    print2buff(translateChar(cl_def), d);
	    print2buff("\", ", d);
	    SEXP slotNms; // ---- slotNms := methods::.slotNames(s)  ---------
	    // computed alternatively, slotNms := names(getClassDef(class)@slots) :
	    static SEXP R_getClassDef = NULL, R_slots = NULL, R_asS3 = NULL;
	    if(R_getClassDef == NULL)
		R_getClassDef = findFun(install("getClassDef"), R_MethodsNamespace);
	    if(R_slots == NULL) R_slots = install("slots");
	    if(R_asS3  == NULL) R_asS3  = install("asS3");
	    SEXP e = PROTECT(lang2(R_getClassDef, class_));
	    cl_def = PROTECT(eval(e, R_BaseEnv)); // correct env?
	    slotNms = // names( cl_def@slots ) :
		getAttrib(R_do_slot(cl_def, R_slots), R_NamesSymbol);
	    UNPROTECT(2); // (e, cl_def)
	    int n;
	    bool has_Data = false;// does it have ".Data" slot?
	    bool hasS4_t = (TYPEOF(s) == OBJSXP);
	    if(TYPEOF(slotNms) == STRSXP && (n = LENGTH(slotNms))) {
		PROTECT(slotNms);
		SEXP slotlist = PROTECT(allocVector(VECSXP, n));
		// := structure(lapply(slotNms, slot, object=s), names=slotNms)
		for(int i=0; i < n; i++) {
		    SEXP slot_i = STRING_ELT(slotNms, i);
		    SET_VECTOR_ELT(slotlist, i, R_do_slot(s, installTrChar(slot_i)));
		    if(!hasS4_t && !has_Data)
			has_Data = (streql(CHAR(slot_i), ".Data"));
		}
		setAttrib(slotlist, R_NamesSymbol, slotNms);
		vec2buff(slotlist, d, true);
		/*-----------------*/
		UNPROTECT(2); // (slotNms, slotlist)
	    }
	    if(!hasS4_t && !has_Data) {
		// may have *non*-slot contents, (i.e., not in .Data)
		// ==> additionally deparse asS3(s) :
		e = PROTECT(lang2(R_asS3, s)); // = asS3(s)
		SEXP S3_s = PROTECT(eval(e, R_BaseEnv)); // correct env?
		print2buff(", ", d);
		deparse2buff(S3_s, d);
		UNPROTECT(2); // (e, S3_s)
	    }
	    print2buff(")", d);
	}
	else { // exception: class_ is not CHARSXP
	    if(isNull(cl_def) && isNull(ATTRIB(s))) // special
		print2buff("getClass(\"S4\")@prototype", d);
	    else { // irregular S4 ((does this ever trigger ??))
		d->sourceable = false;
		print2buff("<S4 object of class ", d);
		deparse2buff(class_, d);
		print2buff(">", d);
	    }
	}
	/* vmaxset(vmax); */
	return;
    } // if( S4 )

    // non-S4 cases:
    switch (TYPEOF(s)) {
    case NILSXP:
	print2buff("NULL", d);
	break;
    case SYMSXP: {
	bool doquote = ((d_opts_in & QUOTEEXPRESSIONS) && strlen(CHAR(PRINTNAME(s))));
	if (doquote) {
	    attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	    print2buff("quote(", d);
	}
	if (d_opts_in & S_COMPAT) {
	    print2buff(quotify(PRINTNAME(s), '"'), d);
	} else if (d->backtick)
	    print2buff(quotify(PRINTNAME(s), '`'), d);
	else
	    print2buff(CHAR(PRINTNAME(s)), d);
	if (doquote) {
	    print2buff(")", d);
	    if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	}
	break;
    }
    case CHARSXP:
    {
	CXXR::RAllocStack::Scope rscope;
	const char *ts = translateChar(s);
#ifdef longstring_WARN
	/* versions of R < 2.7.0 cannot parse strings longer than 8192 chars */
	if(strlen(ts) >= 8192) d->longstring = true;
#endif
	print2buff(ts, d);
	break;
    }
    case SPECIALSXP:
    case BUILTINSXP:
	print2buff(".Primitive(\"", d);
	print2buff(PRIMNAME(s), d);
	print2buff("\")", d);
	break;
    case PROMSXP:
	if(d->opts & DELAYPROMISES) {
	    d->sourceable = false;
	    print2buff("<promise: ", d);
	    d->opts &= ~QUOTEEXPRESSIONS; /* don't want delay(quote()) */
	    deparse2buff(PREXPR(s), d);
	    d->opts = d_opts_in;
	    print2buff(">", d);
	} else {
	    PROTECT(s = eval(s, R_EmptyEnv)); /* eval uses env of promise */
	    deparse2buff(s, d);
	    UNPROTECT(1);
	}
	break;
    case CLOSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	if ((d->opts & USESOURCE)
	    && !isNull(t = getAttrib(s, R_SrcrefSymbol)))
		src2buff1(t, d);
	else {
	    /* We have established that we don't want to use the
	       source for this function */
	    d->opts &= SIMPLE_OPTS & ~USESOURCE;
	    print2buff("function (", d);
	    args2buff(FORMALS(s), 0, 1, d);
	    print2buff(") ", d);

	    writeline(d);
	    deparse2buff(BODY_EXPR(s), d);
	    d->opts = d_opts_in;
	}
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	break;
    case ENVSXP:
	d->sourceable = false;
	print2buff("<environment>", d);
	break;
    case VECSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	print2buff("list(", d);
	d->opts = d_opts_in;// vec2buff() must use unchanged d
	vec2buff(s, d, (attr == OK_NAMES || attr == STRUC_ATTR));
	d->opts |= NICE_NAMES;
	print2buff(")", d);
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	d->opts = d_opts_in;
	break;
    case EXPRSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	if(length(s) <= 0)
	    print2buff("expression()", d);
	else {
	    int locOpts = d->opts;
	    print2buff("expression(", d);
	    d->opts &= SIMPLE_OPTS;
	    vec2buff(s, d, (attr == OK_NAMES || attr == STRUC_ATTR));
	    d->opts = locOpts;
	    print2buff(")", d);
	}
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	d->opts = d_opts_in;
	break;
    case LISTSXP: {
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	/* pairlist(x=) cannot be evaluated, hence with missings we use
	   as.pairlist(alist(...)) to allow evaluation of deparsed formals */
	bool missing = false;
	for(t=s; t != R_NilValue; t=CDR(t))
	    if (CAR(t) == R_MissingArg) {
		missing = true;
		break;
	    }
	if (missing)
	    print2buff("as.pairlist(alist(", d);
	else
	    print2buff("pairlist(", d);
	d->inlist++;
	for (t=s ; CDR(t) != R_NilValue ; t=CDR(t) ) {
	    if( TAG(t) != R_NilValue ) {
		d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
		deparse2buff(TAG(t), d);
		d->opts = d_opts_in;
		print2buff(" = ", d);
	    }
	    deparse2buff(CAR(t), d);
	    print2buff(", ", d);
	}
	if( TAG(t) != R_NilValue ) {
	    d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
	    deparse2buff(TAG(t), d);
	    d->opts = d_opts_in;
	    print2buff(" = ", d);
	}
	deparse2buff(CAR(t), d);
	if (missing)
	    print2buff("))", d);
	else
	    print2buff(")", d);
	d->inlist--;
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	break;
    }
    case LANGSXP:
	{
	if (!isNull(ATTRIB(s)))
	    d->sourceable = false;
	SEXP op = CAR(s);
	bool doquote = false;
	bool maybe_quote = (d_opts_in & QUOTEEXPRESSIONS);
	if (maybe_quote) {
	    // do *not* quote() formulas:
	    doquote = // := op is not `~` (tilde) :
		(!((TYPEOF(op) == SYMSXP) &&
		  streql(CHAR(PRINTNAME(op)), "~")));
	    if (doquote) {
		print2buff("quote(", d);
		d->opts &= SIMPLE_OPTS;
	    } else { // `~`
		d->opts &= ~QUOTEEXPRESSIONS;
	    }
	}

	if (TYPEOF(op) == SYMSXP) {
	    int userbinop = 0;
	    if (Rf_isPrimitive(SYMVALUE(op)) ||
		(userbinop = isUserBinop(op))) {
		PPinfo fop;
		bool parens;
		s = CDR(s);
		if (userbinop) {
		    if (isNull(getAttrib(s, R_NamesSymbol))) {
			// not quite right for spacing, but can't be unary :
			fop.kind = PP_BINARY2;
			fop.precedence = PREC_PERCENT;
			fop.rightassoc = 0;
		    } else
			// if args are named, deparse as function call (PR#15350):
			fop.kind = PP_FUNCALL;
		} else
		    fop = PPINFO(SYMVALUE(op));

		switch (fop.kind) {
		case PP_BINARY:
		    switch (length(s)) {
		    case 1:
			fop.kind = PP_UNARY;
			if (fop.precedence == PREC_SUM)
			    // binary +/- precedence upgraded as unary
			    fop.precedence = PREC_SIGN;
			break;
		    case 2:
			break;
		    default:
			fop.kind = PP_FUNCALL;
			break;
		    }
		    break;
		case PP_BINARY2:
		    if (length(s) != 2)
			fop.kind = PP_FUNCALL;
		    else if (userbinop)
			fop.kind = PP_BINARY;
		    break;
		case PP_DOLLAR: {
		    if (length(s) != 2) {
			fop.kind = PP_FUNCALL;
			break;
		    }
		    SEXP rhs = CADR(s);
		    if (TYPEOF(rhs) != SYMSXP && !(isValidString(rhs)
						   && STRING_ELT(rhs, 0) != NA_STRING))
			fop.kind = PP_FUNCALL;
		    break;
		}
		default:
		    break;
		}
		switch (fop.kind) {
		case PP_IF:
		    print2buff("if (", d);
		    /* print the predicate */
		    deparse2buff(CAR(s), d);
		    print2buff(") ", d);
		    if (d->incurly && !d->inlist ) {
			lookahead = curlyahead(CADR(s));
			if (!lookahead) {
			    writeline(d);
			    d->indent++;
			}
		    }
		    /* need to find out if there is an else */
		    if (length(s) > 2) {
			deparse2buff(CADR(s), d);
			if (d->incurly && !d->inlist) {
			    writeline(d);
			    if (!lookahead)
				d->indent--;
			}
			else
			    print2buff(" ", d);
			print2buff("else ", d);
			deparse2buff(CADDR(s), d);
		    }
		    else {
			deparse2buff(CADR(s), d);
			if (d->incurly && !lookahead && !d->inlist )
			    d->indent--;
		    }
		    break;
		case PP_WHILE:
		    print2buff("while (", d);
		    deparse2buff(CAR(s), d);
		    print2buff(") ", d);
		    deparse2buff(CADR(s), d);
		    break;
		case PP_FOR:
		    print2buff("for (", d);
		    deparse2buff(CAR(s), d);
		    print2buff(" in ", d);
		    deparse2buff(CADR(s), d);
		    print2buff(") ", d);
		    deparse2buff(CADDR(s), d);
		    break;
		case PP_REPEAT:
		    print2buff("repeat ", d);
		    deparse2buff(CAR(s), d);
		    break;
		case PP_CURLY:
		    print2buff("{", d);
		    d->incurly += 1;
		    d->indent++;
		    writeline(d);
		    while (s != R_NilValue) {
			deparse2buff(CAR(s), d);
			writeline(d);
			s = CDR(s);
		    }
		    d->indent--;
		    print2buff("}", d);
		    d->incurly -= 1;
		    break;
		case PP_PAREN:
		    print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    print2buff(")", d);
		    break;
		case PP_SUBSET:
		    if ((parens = needsparens(fop, CAR(s), 1, prevLeft)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("[", d);
		    else
			print2buff("[[", d);
		    args2buff(CDR(s), 0, 0, d);
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("]", d);
		    else
			print2buff("]]", d);
		    break;
		case PP_FUNCALL:
		case PP_RETURN:
		    if (d->backtick)
			print2buff(quotify(PRINTNAME(op), '`'), d);
		    else
			print2buff(quotify(PRINTNAME(op), '"'), d);
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 0, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FOREIGN:
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 1, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FUNCTION:
		    if (!(d->opts & USESOURCE) || !isString(CADDR(s))) {
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("(", d);
			args2buff(CAR(s), 0, 1, d);
			print2buff(") ", d);
			deparse2buff(CADR(s), d);
		    } else {
			s = CADDR(s);
			n = length(s);
			CXXR::RAllocStack::Scope rscope;
			for(i = 0 ; i < n ; i++) {
			    print2buff(translateChar(STRING_ELT(s, i)), d);
			    writeline(d);
			}
		    }
		    break;
		case PP_ASSIGN:
		case PP_ASSIGN2: {
		    bool outerparens = (fnarg && streql(CHAR(PRINTNAME(op)), "="));
		    if (outerparens)
		    	print2buff("(", d);
		    if ((parens = needsparens(fop, CAR(s), 1, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : fop.precedence;
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff(" ", d);
		    if ((parens = needsparens(fop, CADR(s), 0, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : prevLeft;
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (outerparens)
		    	print2buff(")", d);
		    d->left = 0;
		    break;
		}
		case PP_DOLLAR:
		    if ((parens = needsparens(fop, CAR(s), 1, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : fop.precedence;
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    /*temp fix to handle printing of x$a's */
		    if( isString(CADR(s)) &&
			isValidName(CHAR(STRING_ELT(CADR(s), 0))))
			deparse2buff(STRING_ELT(CADR(s), 0), d);
		    else {
			if ((parens = needsparens(fop, CADR(s), 0, prevLeft)))
			    print2buff("(", d);
			d->left = parens ? 0 : prevLeft;
			deparse2buff(CADR(s), d);
			if (parens)
			    print2buff(")", d);
		    }
		    d->left = 0;
		    break;
		case PP_BINARY:
		    if ((parens = needsparens(fop, CAR(s), 1, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : fop.precedence;
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff(" ", d);
		    linebreak(&lbreak, d);

		    if ((parens = needsparens(fop, CADR(s), 0, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : prevLeft;
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (lbreak) {
			d->indent--;
			lbreak = false;
		    }
		    d->left = 0;
		    break;
		case PP_BINARY2:	/* no space between op and args */
		    if ((parens = needsparens(fop, CAR(s), 1, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : fop.precedence;
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);

		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    if ((parens = needsparens(fop, CADR(s), 0, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : prevLeft;
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    d->left = 0;
		    break;
		case PP_UNARY:
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    if ((parens = needsparens(fop, CAR(s), 0, prevLeft)))
			print2buff("(", d);
		    d->left = parens ? 0 : prevLeft;
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    d->left = 0;
		    break;
		case PP_BREAK:
		    print2buff("break", d);
		    break;
		case PP_NEXT:
		    print2buff("next", d);
		    break;
		case PP_SUBASS:
		    if(d->opts & S_COMPAT) {
			print2buff("\"", d);
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("\'(", d);
		    } else {
			print2buff("`", d);
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("`(", d);
		    }
		    args2buff(s, 0, 0, d);
		    print2buff(")", d);
		    break;
		default:
		    d->sourceable = false;
		    UNIMPLEMENTED("deparse2buff");
		}
	    }
	    else {
		SEXP val = R_NilValue; /* -Wall */
		if (isSymbol(CAR(s))) {
		    val = SYMVALUE(CAR(s));
		    if (TYPEOF(val) == PROMSXP)
			val = eval(val, R_BaseEnv);
		}
		if ( isSymbol(CAR(s))
		  && TYPEOF(val) == CLOSXP
		  && streql(CHAR(PRINTNAME(CAR(s))), "::") ) { //  :: is special case
		    deparse2buff(CADR(s), d);
		    print2buff("::", d);
		    deparse2buff(CADDR(s), d);
		}
		else if ( isSymbol(CAR(s))
		  && TYPEOF(val) == CLOSXP
		  && streql(CHAR(PRINTNAME(CAR(s))), ":::") ) { // ::: is special case
		    deparse2buff(CADR(s), d);
		    print2buff(":::", d);
		    deparse2buff(CADDR(s), d);
		}
		else {
		    if ( isSymbol(CAR(s)) ){
			if(d->opts & S_COMPAT)
			    print2buff(quotify(PRINTNAME(CAR(s)), '\''), d);
			else
			    print2buff(quotify(PRINTNAME(CAR(s)), '`'), d);
		    }
		    else
			deparse2buff(CAR(s), d);
		    print2buff("(", d);
		    args2buff(CDR(s), 0, 0, d);
		    print2buff(")", d);
		}
	    }
	} // end{op : SYMSXP }
	else if (FunctionBase::isA(op)) {
	    if (parenthesizeCaller(op)) {
		print2buff("(", d);
		deparse2buff(op, d);
		print2buff(")", d);
	    } else
		deparse2buff(op, d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	else { /* we have a lambda expression */
	    if (parenthesizeCaller(op)) {
		print2buff("(", d);
		deparse2buff(op, d);
		print2buff(")", d);
	    } else
		deparse2buff(op, d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	if (maybe_quote) {
	    d->opts = d_opts_in;
	    if(doquote)
		print2buff(")", d);
	}
	}
	break; // end{case LANGSXP} ---------------------------------------------
    case STRSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
	vector2buff(s, d);
	break;
    case EXTPTRSXP:
    {
	char tpb[32]; /* need 12+2+2*sizeof(void*) */
	d->sourceable = false;
	snprintf(tpb, 32, "<pointer: %p>", R_ExternalPtrAddr(s));
	tpb[31] = '\0';
	print2buff(tpb, d);
    }
	break;
    case BCODESXP:
	d->sourceable = false;
	print2buff("<bytecode>", d);
	break;
    case WEAKREFSXP:
	d->sourceable = false;
	print2buff("<weak reference>", d);
	break;
    case OBJSXP: {
	/*
	print2buff("object(", d);
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	 print2buff(")", d);
	*/
	d->sourceable = false;
	print2buff("<object>", d);
	break;
    }
    default:
	d->sourceable = false;
	UNIMPLEMENTED_TYPE("deparse2buff", s);
    }

    d->left = prevLeft;
}


/* If there is a string array active point to that, and */
/* otherwise we are counting lines so don't do anything. */

static void writeline(LocalParseData *d)
{
    if (d->strvec != R_NilValue && d->linenumber < d->maxlines)
	SET_STRING_ELT(d->strvec, d->linenumber, mkChar(d->buffer.data));
    d->linenumber++;
    if (d->linenumber >= d->maxlines) d->active = false;
    /* reset */
    d->len = 0;
    d->buffer.data[0] = '\0';
    d->startline = true;
}

static void print2buff(const char *strng, LocalParseData *d)
{
    size_t tlen, bufflen;

    if (d->startline) {
	d->startline = false;
	printtab2buff(d->indent, d);	/*if at the start of a line tab over */
    }
    tlen = strlen(strng);
    R_AllocStringBuffer(0, &(d->buffer));
    bufflen = strlen(d->buffer.data);
    R_AllocStringBuffer(bufflen + tlen, &(d->buffer));
    strcat(d->buffer.data, strng);
    d->len += (int) tlen;
}

/*
 * Encodes a complex value as a syntactically correct
 * string that can be reparsed by R. This is required
 * because by default strings like '1+Infi' or '3+NaNi'
 * are produced which are not valid complex literals.
 */

#define NB 1000  /* Same as printutils.c */
#define NB2 2*NB+25
static const char *EncodeNonFiniteComplexElement(Rcomplex x, char* buff)
{
    int w, d, e, wi, di, ei;

    // format a first time to get width/decimals
    formatComplex(&x, 1, &w, &d, &e, &wi, &di, &ei, 0);

    char Re[NB];
    char Im[NB];

    strcpy(Re, EncodeReal0(x.r, w, d, e, "."));
    strcpy(Im, EncodeReal0(x.i, wi, di, ei, "."));

    snprintf(buff, NB2, "complex(real=%s, imaginary=%s)", Re, Im);
    buff[NB2-1] = '\0';
    return buff;
}

static void deparse2buf_name(SEXP nv, int i, LocalParseData *d) {
    if (!isNull(nv) && !isNull(STRING_ELT(nv, i))
	&& *CHAR(STRING_ELT(nv, i))) { /* length test */
	/* d->opts = SIMPLEDEPARSE; This seems pointless */
	if(isValidName(translateChar(STRING_ELT(nv, i))))
	    deparse2buff(STRING_ELT(nv, i), d);
	else if(d->backtick) {
	    print2buff("`", d);
	    deparse2buff(STRING_ELT(nv, i), d);
	    print2buff("`", d);
	} else {
	    print2buff("\"", d);
	    deparse2buff(STRING_ELT(nv, i), d);
	    print2buff("\"", d);
	}
	/* d->opts = d_opts_in; */
	print2buff(" = ", d);
    }
}

// deparse atomic vectors :
static void vector2buff(SEXP vector, LocalParseData *d)
{
    // Known here:  TYPEOF(vector)  is one of the 6 atomic *SXPs
    const char *strp;
    char *buff = 0, hex[64]; // 64 is more than enough
    int i, d_opts_in = d->opts,
	tlen = length(vector),
	quote = isString(vector) ? '"' : 0;
    bool surround = false, allNA,
	intSeq = false; // := true iff integer sequence 'm:n' (up *or* down)
    if(TYPEOF(vector) == INTSXP && tlen > 1) {
	int *vec = INTEGER(vector);
	// vec[1] - vec[0] could overflow, and does in package Rmpfr
	double d_i = (double) vec[1] - (double)vec[0];
	intSeq = (vec[0] != NA_INTEGER &&
		  vec[1] != NA_INTEGER &&
		  fabs(d_i) == 1);
	if(intSeq) for(i = 2; i < tlen; i++) {
	    if((vec[i] == NA_INTEGER) ||
	       ((double)vec[i] - (double)vec[i-1]) != d_i) {
		intSeq = false;
		break;
	    }
	}
    }

    SEXP nv = R_NilValue;
    bool do_names = (d_opts_in & SHOW_ATTR_OR_NMS);// iff true use '<tag_i> = <comp_i>'
    if(do_names) {
	nv = getAttrib(vector, R_NamesSymbol); // only "do names" if have names:
	if(isNull(nv))
	    do_names = false;
    }
    PROTECT(nv);
    bool
	STR_names, // if true, use structure(.,*) for names even if(nice_names)
	need_c = (tlen > 1); // (?) only true iff SHOW_ATTR_OR_NMS
    STR_names = (do_names && (intSeq || tlen == 0));
#ifdef DEBUG_DEPARSE
    REprintf("vector2buff(v): length(v) = %d; initial (do|STR)_names) = (%s,%s)\n",
	     tlen, ChTF(do_names), ChTF(STR_names));
#endif
    if (STR_names) // use structure(.,*) for names even if(nice_names)
	d->opts &= ~NICE_NAMES;
    attr_type attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(vector, d) : SIMPLE;
    if(do_names) do_names = (attr == OK_NAMES || attr == STRUC_ATTR);
    if(!need_c) need_c = do_names; // c(a = *) but not c(1)
#ifdef DEBUG_DEPARSE
    REprintf(" -> final (do|STR)_names) = (%s,%s), attr = %s\n",
	     ChTF(do_names), ChTF(STR_names), attrT2char(attr));
#endif
    if (tlen == 0) {
	switch(TYPEOF(vector)) {
	case LGLSXP: print2buff("logical(0)", d); break;
	case INTSXP: print2buff("integer(0)", d); break;
	case REALSXP: print2buff("numeric(0)", d); break;
	case CPLXSXP: print2buff("complex(0)", d); break;
	case STRSXP: print2buff("character(0)", d); break;
	case RAWSXP: print2buff("raw(0)", d); break;
	default: UNIMPLEMENTED_TYPE("vector2buff", vector);
	}
    }
    else if(TYPEOF(vector) == INTSXP) {
	/* We treat integer separately, as S_compatible is relevant.

	   Also, it is neat to deparse m:n in that form,
	   so we do so as from 2.5.0, and for m > n, from 3.5.0
	 */
	if(intSeq) { // m:n
		strp = EncodeElement(vector, 0, '"', '.');
		print2buff(strp, d);
		print2buff(":", d);
		strp = EncodeElement(vector, tlen - 1, '"', '.');
		print2buff(strp, d);
	} else {
	    int *vec = INTEGER(vector);
	    bool addL = (d->opts & KEEPINTEGER && !(d->opts & S_COMPAT));
	    allNA = ((d->opts & KEEPNA) || addL);
	    for(i = 0; i < tlen; i++)
		if(vec[i] != NA_INTEGER) {
		    allNA = false;
		    break;
		}
	    if((d->opts & KEEPINTEGER && (d->opts & S_COMPAT))) {
		print2buff("as.integer(", d); surround = true;
	    }
	    allNA = (allNA && !(d->opts & S_COMPAT));
	    if(need_c) print2buff("c(", d);
	    for (i = 0; i < tlen; i++) {
		if(do_names) // put '<tag> = '
		    deparse2buf_name(nv, i, d);
		if(allNA && vec[i] == NA_INTEGER) {
		    print2buff("NA_integer_", d);
		} else {
		    strp = EncodeElement(vector, i, quote, '.');
		    print2buff(strp, d);
		    if(addL && vec[i] != NA_INTEGER) print2buff("L", d);
		}
		if (i < (tlen - 1)) print2buff(", ", d);
		if (tlen > 1 && d->len > d->cutoff) writeline(d);
		if (!d->active) break;
	    }
	    if(need_c)   print2buff(")", d);
	    if(surround) print2buff(")", d);
	}
    } else { // tlen > 0;  _not_ INTSXP
	allNA = (d->opts & KEEPNA);
	if((d->opts & KEEPNA) && TYPEOF(vector) == REALSXP) {
	    for(i = 0; i < tlen; i++)
		if(!ISNA(REAL(vector)[i])) {
		    allNA = false;
		    break;
		}
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.double(", d); surround = true;
	    }
	} else if((d->opts & KEEPNA) && TYPEOF(vector) == CPLXSXP) {
	    Rcomplex *vec = COMPLEX(vector);
	    for(i = 0; i < tlen; i++) {
		if( !ISNA(vec[i].r) && !ISNA(vec[i].i) ) {
		    allNA = false;
		    break;
		}
	    }
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.complex(", d); surround = true;

	    }
	} else if((d->opts & KEEPNA) && TYPEOF(vector) == STRSXP) {
	    for(i = 0; i < tlen; i++)
		if(STRING_ELT(vector, i) != NA_STRING) {
		    allNA = false;
		    break;
		}
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.character(", d); surround = true;
	    }
	} else if(TYPEOF(vector) == RAWSXP) {
	    print2buff("as.raw(", d); surround = true;
 	}
	if(need_c) print2buff("c(", d);
	allNA = (allNA && !(d->opts & S_COMPAT));
	for (i = 0; i < tlen; i++) {
	    if(do_names) // put '<tag> = '
		deparse2buf_name(nv, i, d);
	    if(allNA && TYPEOF(vector) == REALSXP &&
	       ISNA(REAL(vector)[i])) {
		strp = "NA_real_";
	    } else if (TYPEOF(vector) == CPLXSXP &&
		       (ISNA(COMPLEX(vector)[i].r)
			&& ISNA(COMPLEX(vector)[i].i)) ) {
		strp = allNA ? "NA_complex_" : EncodeElement(vector, i, quote, '.');
	    } else if(TYPEOF(vector) == CPLXSXP &&
		      (ISNAN(COMPLEX(vector)[i].r) || !R_FINITE(COMPLEX(vector)[i].i)) ) {
		if (!buff)
		    buff = (char*) alloca(NB2);
		strp = EncodeNonFiniteComplexElement(COMPLEX(vector)[i], buff);
	    } else if (allNA && TYPEOF(vector) == STRSXP &&
		       STRING_ELT(vector, i) == NA_STRING) {
		strp = "NA_character_";
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & S_COMPAT)) {
		int w, d, e;
		formatReal(&REAL(vector)[i], 1, &w, &d, &e, 0);
		strp = EncodeReal2(REAL(vector)[i], w, d, e);
	    } else if (TYPEOF(vector) == STRSXP) {
		CXXR::RAllocStack::Scope rscope;
#ifdef longstring_WARN
		const char *ts = translateChar(STRING_ELT(vector, i));
		/* versions of R < 2.7.0 cannot parse strings longer than 8192 chars */
		if(strlen(ts) >= 8192) d->longstring = true;
#endif
		strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == RAWSXP) {
		strp = EncodeRaw(RAW(vector)[i], "0x");
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & HEXNUMERIC)) {
		double x = REAL(vector)[i];
		// Windows warns here, but incorrectly as this is C99
		// and the snprintf used from trio is compliant.
		if (R_FINITE(x)) {
		    snprintf(hex, 32, "%a", x);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & DIGITS17)) {
		double x = REAL(vector)[i];
		if (R_FINITE(x)) {
		    snprintf(hex, 32, "%.17g", x);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == CPLXSXP && (d->opts & HEXNUMERIC)) {
		Rcomplex z =  COMPLEX(vector)[i];
		if (R_FINITE(z.r) && R_FINITE(z.i)) {
		    snprintf(hex, 64, "%a + %ai", z.r, z.i);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == CPLXSXP && (d->opts & DIGITS17)) {
		Rcomplex z =  COMPLEX(vector)[i];
		if (R_FINITE(z.r) && R_FINITE(z.i)) {
		    snprintf(hex, 64, "%.17g%+.17gi", z.r, z.i);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else
		strp = EncodeElement(vector, i, quote, '.');
	    print2buff(strp, d);
	    if (i < (tlen - 1)) print2buff(", ", d);
	    if (tlen > 1 && d->len > d->cutoff) writeline(d);
	    if (!d->active) break;
	} // for(i in 1:tlen)
	if(need_c  ) print2buff(")", d);
	if(surround) print2buff(")", d);
    }
    if(attr >= STRUC_ATTR) attr2(vector, d, (attr == STRUC_ATTR));
    if (STR_names) d->opts = d_opts_in;
    UNPROTECT(1); /* nv */
} // vector2buff()


/* src2buff1: Deparse one source ref to buffer */

static void src2buff1(SEXP srcref, LocalParseData *d)
{
    CXXR::RAllocStack::Scope rscope;
    PROTECT(srcref);

    PROTECT(srcref = lang2(R_AsCharacterSymbol, srcref));
    PROTECT(srcref = eval(srcref, R_BaseEnv));
    int n = length(srcref);
    for (int i = 0 ; i < n ; i++) {
	/* FIXME: does not embed UTF-8 for RGui */
	print2buff(translateChar(STRING_ELT(srcref, i)), d);
	if(i < n-1) writeline(d);
    }
    UNPROTECT(3);
}

/* src2buff : Deparse source element k to buffer, if possible; return false on failure */

static bool src2buff(SEXP sv, int k, LocalParseData *d)
{
    SEXP t;

    if (TYPEOF(sv) == VECSXP && length(sv) > k && !isNull(t = VECTOR_ELT(sv, k))) {
	src2buff1(t, d);
	return true;
    }
    else return false;
}

/* Deparse vectors of S-expressions, i.e., list() and expression() objects.
   In particular, this deparses objects of mode expression. */
static void vec2buff(SEXP v, LocalParseData *d,
		     bool do_names) // iff true use '<tag_i> = <comp_i>'
{
    bool lbreak = false;
    CXXR::RAllocStack::Scope rscope;
    int n = length(v);
    SEXP nv = R_NilValue;
    if(do_names) {
	nv = getAttrib(v, R_NamesSymbol); // only "do names" if have names:
	if (isNull(nv))
	    do_names = false;
    }
    PROTECT(nv);
    SEXP sv; // Srcref or NULL
    if (d->opts & USESOURCE) {
	sv = getAttrib(v, R_SrcrefSymbol);
	if (TYPEOF(sv) != VECSXP)
	    sv = R_NilValue;
    } else
	sv = R_NilValue;

    for(int i = 0 ; i < n ; i++) {
	if (i > 0)
	    print2buff(", ", d);
	linebreak(&lbreak, d);
	if(do_names) // put '<tag> = '
	    deparse2buf_name(nv, i, d);
	if (!src2buff(sv, i, d)) {
	    if (TYPEOF(v) == EXPRSXP)
		    deparse2buff(XVECTOR_ELT(v, i), d);
	    else
		    deparse2buff(VECTOR_ELT(v, i), d);
	}
    }
    if (lbreak)
	d->indent--;
    UNPROTECT(1); /* nv */
}

static void args2buff(SEXP arglist, int lineb, int formals, LocalParseData *d)
{
    bool lbreak = false;

    while (arglist != R_NilValue) {
	if (TYPEOF(arglist) != LISTSXP && TYPEOF(arglist) != LANGSXP)
	    error("%s", _("badly formed function expression"));
	if (TAG(arglist) != R_NilValue) {
	    SEXP s = TAG(arglist);

	    if( s == R_DotsSymbol )
		print2buff(CHAR(PRINTNAME(s)), d);
	    else if(d->backtick)
		print2buff(quotify(PRINTNAME(s), '`'), d);
	    else
		print2buff(quotify(PRINTNAME(s), '"'), d);

	    if(formals) {
		if (CAR(arglist) != R_MissingArg) {
		    print2buff(" = ", d);
		    d->fnarg = true;
		    deparse2buff(CAR(arglist), d);
		}
	    }
	    else {
		print2buff(" = ", d);
		if (CAR(arglist) != R_MissingArg) {
		    d->fnarg = true;
		    deparse2buff(CAR(arglist), d);
		}
	    }
	}
	else {
	  d->fnarg = true;
	  deparse2buff(CAR(arglist), d);
	}
	arglist = CDR(arglist);
	if (arglist != R_NilValue) {
	    print2buff(", ", d);
	    linebreak(&lbreak, d);
	}
    }
    if (lbreak)
	d->indent--;
}

/* This code controls indentation.  Used to follow the S style, */
/* (print 4 tabs and then start printing spaces only) but I */
/* modified it to be closer to emacs style (RI). */

static void printtab2buff(int ntab, LocalParseData *d)
{
    for (int i = 1; i <= ntab; i++)
	if (i <= 4)
	    print2buff("    ", d);
	else
	    print2buff("  ", d);
}
