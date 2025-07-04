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
 *  it under the terms of the GNU General Pulic License as published by
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

/*

Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

strsplit grep [g]sub [g]regexpr
  handle UTF-8 directly if fixed/perl = TRUE, via wchar_t for extended

As from R 4.1.0 we translate latin1 strings in a non-latin1-locale to UTF-8.

*/

/* It is possible to use TRE for fixed = TRUE.
   The main benefit would be code simplification: however, the
   special-purpose code is substantially faster, so we no longer
   plan to do so.
*/

/* PCRE supports only single-byte locales and UTF-8, so we convert
   inputs in all other MBCS locales to UTF-8.

   In [g]sub and [g]regexpr we need to know match positions in
   characters.  To avoid yet more cases we handle all MBCS locales in
   wchar in ERE for those functions.  (Byte positions suffice for
   [g]sub(fixed = TRUE), and [g]regexpr needs to convert to char
   positions for all MBCSs.)
*/

/** @file grep.cpp
 *
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h> // for size_t
#endif

/* interval at which to check interrupts */
/*   if re-enabling, consider a power of two */
/* #define NINTERRUPT 1000000 */

/* How many encoding warnings to give */
#define NWARN 5

#include <cctype>
#include <cwchar>
#include <cwctype>    /* for wctrans_t */
#include <R_ext/Minmax.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/RS.h>  /* for R_Calloc/R_Free */

/* As from TRE 0.8.0, tre.h replaces regex.h */
#include <tre/tre.h>

#ifdef HAVE_PCRE2
  /* PCRE2_CODE_UNIT_WIDTH is defined to 8 via config.h */
# include <pcre2.h>
#else
  /*
  Some systems might have pcre headers in a subdirectory -- not seen recently.
  */
# ifdef HAVE_PCRE_PCRE_H
#  include <pcre/pcre.h>
# else
#  include <pcre.h>
# endif
#endif

using namespace R;
using namespace CXXR;

/*
   Default maximum stack size: note this is reserved but not allocated
   until needed.  The help says 1M suffices, but we found more was
   needed for strings around a million bytes.
*/
#define JIT_STACK_MAX 64*1024*1024
/*
   This will stay reserved until the end of the session, but at 64MB
   that is not an issue -- and most sessions will not use PCRE with
   more than 10 strings.
 */

#ifdef HAVE_PCRE2
static pcre2_jit_stack *jit_stack = NULL; // allocated at first use.
#else
static pcre_jit_stack *jit_stack = NULL; // allocated at first use.
#endif

static int jit_stack_size(void)
{
    int stmax = JIT_STACK_MAX;
    char *p = getenv("R_PCRE_JIT_STACK_MAXSIZE");
    if (p) {
	char *endp;
	double xdouble = R_strtod(p, &endp);
	if (xdouble >= 0 && xdouble <= 1000)
	    stmax = (int)(xdouble*1024*1024);
	else warning ("R_PCRE_JIT_STACK_MAXSIZE invalid and ignored");
    }
    return stmax;
}

#ifdef HAVE_PCRE2
static void setup_jit(pcre2_match_context *mcontext)
{
    if (!jit_stack)
	jit_stack = pcre2_jit_stack_create(32*1024, jit_stack_size(), NULL);
    if (jit_stack)
	pcre2_jit_stack_assign(mcontext, NULL, jit_stack);
}
#else
static void setup_jit(pcre_extra *re_pe)
{
    if (!jit_stack)
	jit_stack = pcre_jit_stack_alloc(32*1024, jit_stack_size());
    if (jit_stack)
	pcre_assign_jit_stack(re_pe, NULL, jit_stack);
}
#endif

/* we allow pat == NULL if the regex cannot be safely expressed
   as a string (e.g., when using grepRaw) */
NORET static void reg_report(int rc,  regex_t *reg, const char *pat)
{
    char errbuf[1001];
    tre_regerror(rc, reg, errbuf, 1001);
    if (pat) {
	/* PR#16600 - the regex may be so long that the TRE error description
	   is truncated out from the message, so give also a warning */
	warning(_("TRE pattern compilation error '%s'"), errbuf);
	error(_("invalid regular expression '%s', reason '%s'"), pat, errbuf);
    } else
	error(_("invalid regular expression, reason '%s'"), errbuf);
}

/* wc must be zero-terminated, nc = wsclen(wc), maybe_ascii is TRUE if the input
   is probably pure ASCII. maybe_ascii is useful for regular expressions over
   long vectors (lines of text) where most lines are ASCII, but not all, to
   reduce the overhead of encoding conversions. */
static SEXP mkCharWLenASCII(const wchar_t *wc, int nc, bool maybe_ascii)
{
    if (maybe_ascii) {
	char *xi = R_Calloc(nc, char);
	for(int i = 0; i < nc ; i++) {
	    unsigned int u = (unsigned int) wc[i];
	    if (u > 127) {
		R_Free(xi);
		return mkCharWLenASCII(wc, nc, FALSE);
	    }
	    xi[i] = (char) u;
	}
	SEXP ans = mkCharLenCE(xi, nc, CE_UTF8);
	R_Free(xi);
	return ans;
    }

    /* possibly not ASCII */
    size_t nb = ((size_t)nc + 1) * 4;
    if (nb <= 8192) {
	char xi[8192];
	nb = wcstoutf8(xi, wc, nb);
	return mkCharLenCE(xi, (int)(nb-1), CE_UTF8);
    }

    nb = wcstoutf8(NULL, wc, (size_t)INT_MAX+2);
    if (nb-1 > INT_MAX)
	error("%s", _("R character strings are limited to 2^31-1 bytes"));
    char *xi = R_Calloc(nb, char);
    nb = wcstoutf8(xi, wc, nb);
    SEXP ans = mkCharLenCE(xi, (int)(nb-1), CE_UTF8);
    R_Free(xi);
    return ans;
}

static SEXP markBytesOld(SEXP x, bool useBytes, bool haveBytesInput)
{
    /* If 1, mark results of gsub, sub, strsplit as "bytes" when using bytes
       and replacement, split did not happen. If 0, keep their original
       encoding flag (pre-82589 behavior, see markBytesResultIfNew).

       The intention of the marking (82589) was to avoid encoding flag
       instability caused by 82587, e.g.
       in gsub(weird_thing, "", useBytes=TRUE). The option is experimental
       and intended to be removed.

       Currently the default is the old behavior.
    */
    static int markBytesResultIfOld = -1; /* -1 to allow setting */

    if (markBytesResultIfOld == -1) {
	const char *p = getenv("_R_REGEX_MARK_OLD_RESULT_AS_BYTES_");
	markBytesResultIfOld = (p && StringTrue(p)) ? 1 : 0;
    }
    if (!markBytesResultIfOld || !haveBytesInput ||
        !useBytes || IS_ASCII(x) || IS_BYTES(x) || x == NA_STRING)

	return x;
    else
	return mkCharLenCE(CHAR(x), LENGTH(x), CE_BYTES);
}

static SEXP mkBytesNew(const char *name, bool haveBytesInput)
{
    /* If 1, mark results of gsub, sub, strsplit as "bytes" when using bytes
       and replacement or split happened. If 0 and no input was marked as
       "bytes", mark results as native/unknown, possibly creating an invalid
       string (pre-82587 behavior). This option is experimental and intended
       to be removed.

       Currently the default is the old behavior when no input is marked as
       "bytes". However, the new behavior applies when the input includes
       a string marked as "bytes".
    */
    static int markBytesResultIfNew = -1;

    if (markBytesResultIfNew == -1) {
	const char *p = getenv("_R_REGEX_MARK_NEW_RESULT_AS_BYTES_");
	markBytesResultIfNew = (p && StringTrue(p)) ? 1 : 0;
    }
    if (haveBytesInput || markBytesResultIfNew)
	return mkCharCE(name, CE_BYTES);
    else
	return mkCharCE(name, CE_NATIVE);
}

#ifdef HAVE_PCRE2
static void R_pcre_exec_error(int rc, R_xlen_t i)
{
    if (rc >= 0 || rc == PCRE2_ERROR_NOMATCH)
	return;
    // too much effort to handle long-vector indices, including on Windows
    char buf[256];
    pcre2_get_error_message(rc, (PCRE2_UCHAR *)buf, sizeof(buf));
    if(streql(buf, "recursion limit exceeded"))
	strcat(buf, ": consider increasing the C stack size for the R process");
    warning(_("PCRE error\n\t'%s'\n\tfor element %d"), buf, (int) i + 1);
}
#else
static void R_pcre_exec_error(int rc, R_xlen_t i)
{
    if (rc > -2) return;
    // too much effort to handle long-vector indices, including on Windows
    switch (rc) {
#  ifdef PCRE_ERROR_JIT_STACKLIMIT
    case PCRE_ERROR_JIT_STACKLIMIT:
	warning(_("JIT stack limit reached in PCRE for element %d"),
		(int) i + 1);
	break;
#  endif
    case PCRE_ERROR_MATCHLIMIT:
	warning(_("back-tracking limit reached in PCRE for element %d"),
		(int) i + 1);
	break;
    case PCRE_ERROR_RECURSIONLIMIT:
	warning(_("recursion limit reached in PCRE for element %d\n  consider increasing the C stack size for the R process"),
		(int) i + 1);
	break;
    case PCRE_ERROR_INTERNAL:
    case PCRE_ERROR_UNKNOWN_OPCODE:
	warning(_("unexpected internal error in PCRE for element %d"),
		(int) i + 1);
	break;
#  ifdef PCRE_ERROR_RECURSELOOP
   case PCRE_ERROR_RECURSELOOP:
	warning(_("PCRE detected a recursive loop in the pattern for element %d"),
		(int) i + 1);
	break;
#  endif
    }
}
#endif

/* returns value allocated on R_alloc stack */
static const char *to_native(const char *str, bool use_UTF8)
{
    return use_UTF8 ? reEnc(str, CE_UTF8, CE_NATIVE, 1) : str;
}

static bool only_ascii(SEXP x, R_xlen_t len)
{
    for (R_xlen_t i = 0; i < len; i++) 
	if (!IS_ASCII(STRING_ELT(x, i)) && STRING_ELT(x, i) != NA_STRING)  
	    return false;
    return true;
}

static bool have_bytes(SEXP x, R_xlen_t len)
{
    for (R_xlen_t i = 0; i < len; i++) 
	if (IS_BYTES(STRING_ELT(x, i))) return true;
    return false;
}

static bool have_utf8(SEXP x, R_xlen_t len)
{
    for (R_xlen_t i = 0; i < len; i++) 
	if (IS_UTF8(STRING_ELT(x, i))) return true;
    return false;
}

static bool have_latin1(SEXP x, R_xlen_t len)
{
    for (R_xlen_t i = 0; i < len; i++) 
	if (IS_LATIN1(STRING_ELT(x, i))) return true;
    return false;
}

#ifdef HAVE_PCRE2
# if PCRE2_MAJOR < 10 || (PCRE2_MAJOR == 10 && PCRE2_MINOR < 30)
#  define R_PCRE_LIMIT_RECURSION
# endif
#else
# define R_PCRE_LIMIT_RECURSION
#endif

#ifdef R_PCRE_LIMIT_RECURSION
static bool use_recursion_limit(SEXP subject)
{
    bool use_limit = false;
    if (R_PCRE_limit_recursion == NA_LOGICAL) {
	// use recursion limit only on long strings
	R_xlen_t i;
	R_xlen_t len = XLENGTH(subject);
	for (i = 0 ; i < len ; i++)
	    if (strlen(CHAR(STRING_ELT(subject, i))) >= 1000) {
		use_limit = true;
		break;
	    }
    } else if (R_PCRE_limit_recursion)
	use_limit = true;
    return use_limit;
}

static long R_pcre_max_recursions()
{
    uintptr_t ans, stack_used, current_frame;
    /* Approximate size of stack frame in PCRE match(), actually
       platform / compiler dependent.  Estimate found at
       https://bugs.r-project.org/show_bug.cgi?id=16757
       However, it seems that on Solaris compiled with cc, the size is
       much larger (not too surprising as that happens with R's
       parser). OTOH, OpenCSW's builds of PCRE are built to use the
       heap for recursion.
    */
    const uintptr_t recursion_size = 600;

    const uintptr_t fallback_used = 10000;
    /* This is about 6MB stack, reasonable since stacks are usually >= 8MB
       OTOH, the out-of-box limit is 10000000.
    */
    const long fallback_limit = 10000;
    /* Was PCRE compiled to use stack or heap for recursion? 1=stack */
    int use_recursion;
# ifdef HAVE_PCRE2
    pcre2_config(PCRE2_CONFIG_STACKRECURSE, &use_recursion);
    /* from PCRE2 10.30, use_recursion is always false */
# else
    pcre_config(PCRE_CONFIG_STACKRECURSE, &use_recursion);
# endif
    if (!use_recursion) return -1L;
    if (R_CStackLimit == -1) return fallback_limit;
    current_frame = (uintptr_t) &ans;
    /* Approximate number of bytes used in the stack, or fallback */
    if (R_CStackDir == 1) {
	stack_used =  (R_CStackStart >= current_frame) ?
	    R_CStackStart - current_frame : fallback_used;
    } else {
	stack_used = (current_frame >= R_CStackStart) ?
	    current_frame - R_CStackStart : fallback_used;
    }
    if (stack_used >= R_CStackLimit) return 0L;
    ans = (R_CStackLimit - stack_used) / recursion_size;
    return (long) ((ans <= LONG_MAX) ? ans : -1L);
}
#endif

#ifdef HAVE_PCRE2
static void R_pcre2_prepare(const char *pattern, SEXP subject, bool use_UTF8,
                bool caseless, const unsigned char **tables,
                pcre2_code **re, pcre2_match_context **mcontext)
{
    int errcode;
    PCRE2_SIZE erroffset;
    uint32_t options = 0;
    pcre2_compile_context *ccontext = NULL;

    if (use_UTF8)
	options |= PCRE2_UTF | PCRE2_NO_UTF_CHECK;
    else {
	ccontext = pcre2_compile_context_create(NULL);
	/* PCRE2 internal tables by default are only for ASCII characters.
	   They are needed for lower/upper case distinction and character
	   classes in non-UTF mode. */
	if (!*tables)
	    *tables = pcre2_maketables(NULL);
	pcre2_set_character_tables(ccontext, *tables);
    }
    if (caseless)
	options |= PCRE2_CASELESS;

    *re = pcre2_compile((PCRE2_SPTR) pattern, PCRE2_ZERO_TERMINATED, options,
                         &errcode, &erroffset, ccontext);
    if (!*re) {
	/* not managing R_alloc stack because this ends in error */
	char buf[256];
	pcre2_get_error_message(errcode, (PCRE2_UCHAR *)buf, sizeof(buf));
	pcre2_compile_context_free(ccontext);
	warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"), buf,
	        to_native(pattern + erroffset, use_UTF8));
	error(_("invalid regular expression '%s'"),
	      to_native(pattern, use_UTF8));
    }
    pcre2_compile_context_free(ccontext);
    *mcontext = pcre2_match_context_create(NULL);
    if (R_PCRE_use_JIT) {
	int rc = pcre2_jit_compile(*re, 0);
	if (rc && rc != PCRE2_ERROR_JIT_BADOPTION) {
	    /* PCRE2_ERROR_JIT_BADOPTION is returned when JIT support is not
	       compiled in PCRE2 library */
	    char buf[256];
	    pcre2_get_error_message(rc, (PCRE2_UCHAR *)buf, sizeof(buf));
	    warning(_("PCRE JIT compilation error\n\t'%s'"), buf);
	}
	if (!rc)
	    setup_jit(*mcontext);
    }
# ifdef R_PCRE_LIMIT_RECURSION
    if (use_recursion_limit(subject))
	pcre2_set_recursion_limit(*mcontext, (uint32_t) R_pcre_max_recursions());

    /* we could use set_depth_limit() in newer versions, but the memory limit
       imposed then depends on the regular expression, and the values have
       different meaning from those for recursion limit in versions before
       10.30 */
# endif
}
#else /* ! HAVE_PCRE2 */
static void set_pcre_recursion_limit(pcre_extra **re_pe_ptr, const long limit)
{
    if (limit >= 0) {
	pcre_extra *re_pe = *re_pe_ptr;
	if (!re_pe) {
	    // this will be freed by pcre_free_study so cannot use R_Calloc
	    re_pe = (pcre_extra *) calloc(1, sizeof(pcre_extra));
	    if (!re_pe) {
		warning(_("allocation error in '%s'"), "set_pcre_recursion_limit");
		return;
	    }
	    re_pe->flags = PCRE_EXTRA_MATCH_LIMIT_RECURSION;
	    *re_pe_ptr = re_pe;
	} else
	    re_pe->flags |= PCRE_EXTRA_MATCH_LIMIT_RECURSION;
	re_pe->match_limit_recursion = (unsigned long) limit;
    }
}

static void R_pcre_prepare(const char *pattern, SEXP subject, bool use_UTF8,
               bool caseless, bool always_study,
               const unsigned char **tables, pcre **re, pcre_extra **re_extra)
{
    int erroffset;
    const char *errorptr;
    int options = 0;
    R_xlen_t len = XLENGTH(subject);
    bool pcre_st = always_study ||
                       (R_PCRE_study == -2 ? false : len >= R_PCRE_study);

    if (use_UTF8)
	options |= PCRE_UTF8;
    if (caseless)
	options |= PCRE_CASELESS;
    if (!*tables)
	// PCRE docs say this is not needed, but it is on Windows
	*tables = pcre_maketables();
    *re = pcre_compile(pattern, options, &errorptr, &erroffset, *tables);
    if (!*re) {
	if (errorptr)
	    warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
	            errorptr, to_native(pattern + erroffset, use_UTF8));
	/* in R 3.6 and earlier strsplit reported "invalid split pattern" */
	error(_("invalid regular expression '%s'"),
	      to_native(pattern, use_UTF8));
    }
    if (pcre_st) {
	*re_extra = pcre_study(*re,
	                       R_PCRE_use_JIT ? PCRE_STUDY_JIT_COMPILE : 0,
	                       &errorptr);
	if (errorptr)
	    warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	else if (R_PCRE_use_JIT)
	    setup_jit(*re_extra);
    }
    /* FIXME: do not set recursion limit when JIT compilation succeeded? */
    if (use_recursion_limit(subject))
	set_pcre_recursion_limit(re_extra, R_pcre_max_recursions());
}
#endif

// FIXME: Protect PCRE/PCRE2 data via contexts (as well as other data).
// FIXME: Do not rebuild locale tables repeatedly.
// FIXME: There is no documented way to free locale tables with PCRE2.
//        Using free() would not work on Windows if PCRE2 is dynamically
//        linked but uses a different C runtime from R.

/* strsplit is going to split the strings in the first argument into
 * tokens depending on the second argument. The characters of the second
 * argument are used to split the first argument.  A list of vectors is
 * returned of length equal to the input vector x, each element of the
 * list is the collection of splits for the corresponding element of x.
*/

attribute_hidden SEXP do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP args0 = args, ans, tok, x;
    R_xlen_t i, itok, len, tlen;
    size_t j, ntok;
    bool fixed_opt, perl_opt, useBytes;
    char *pt = NULL; wchar_t *wpt = NULL;
    const char *buf, *split = "", *bufp;
    const unsigned char *tables = NULL;
    bool use_UTF8 = false;
    const void *vmax, *vmax2;
    int nwarn = 0;
    bool haveBytesInput;

    checkArity(op, args);
    x = CAR(args); args = CDR(args);
    tok = CAR(args); args = CDR(args);
    fixed_opt = asBool2(CAR(args), call); args = CDR(args);
    perl_opt = asBool2(CAR(args), call); args = CDR(args);
    useBytes = asBool2(CAR(args), call);
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(x) || !isString(tok)) error("%s", _("non-character argument"));


    len = XLENGTH(x);
    tlen = XLENGTH(tok);

    /* treat split = NULL as split = "" */
    if (!tlen) { tlen = 1; SETCADR(args0, tok = mkString("")); }
    PROTECT(tok);

    haveBytesInput = (have_bytes(tok, tlen) || have_bytes(x, len));
    if (!useBytes)
	useBytes = only_ascii(tok, tlen) && only_ascii(x, len);
    if (!useBytes) 
	useBytes = haveBytesInput;
    if (!useBytes) {
	// use_UTF8 means use wchar_t* for the TRE engine
	if (!fixed_opt && mbcslocale) use_UTF8 = true;
	if (!use_UTF8)
	    use_UTF8 = (have_utf8(tok, tlen) || have_utf8(x, len));
	if (!use_UTF8 && !latin1locale)
	    use_UTF8 = (have_latin1(tok, tlen) || have_latin1(x, len));
    }

    /* group by token for efficiency with PCRE/TRE versions */
    PROTECT(ans = allocVector(VECSXP, len));
    vmax = vmaxget();
    for (itok = 0; itok < tlen; itok++) {
	SEXP this_ = STRING_ELT(tok, itok);

	if (this_ == NA_STRING) { /* NA token doesn't split */
	    for (i = itok; i < len; i += tlen)
		SET_VECTOR_ELT(ans, i,
		               ScalarString(markBytesOld(STRING_ELT(x, i),
		                                         useBytes,
		                                         haveBytesInput)));
	    continue;
	} else if (!CHAR(this_)[0]) { /* empty */
	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = trCharUTF82(STRING_ELT(x, i));
		    if (!buf || !utf8Valid(buf)) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid UTF-8"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateCharFP2(STRING_ELT(x, i));
		    if (!buf || (mbcslocale && !mbcsValid(buf))) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid in this locale"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		if (!useBytes && (use_UTF8 || mbcslocale) &&
		    !IS_ASCII(STRING_ELT(x, i))) {

		    /* split into individual characters (not bytes) */
		    char bf[20 /* > R_MB_CUR_MAX */];
		    const char *p = buf;
		    size_t used;
		    mbstate_t mb_st;
		    ssize_t nt;  /* need to check error on size_t */

		    if (use_UTF8) {
			for (ntok = 0; *p; p += used, ntok++)
			    used = utf8clen(*p);
			p = buf;
			PROTECT(t = allocVector(STRSXP, ntok));
			for (j = 0; j < ntok; j++, p += used) {
			    used = utf8clen(*p);
			    memcpy(bf, p, used); bf[used] = '\0';
			    SET_STRING_ELT(t, j, mkCharCE(bf, CE_UTF8));
			}
		    } else if ((nt = mbstowcs(NULL, buf, 0)) < 0) {
			PROTECT(t = ScalarString(NA_STRING));
		    } else {
			ntok = nt;
			mbs_init(&mb_st);
			PROTECT(t = allocVector(STRSXP, ntok));
			for (j = 0; j < ntok; j++, p += used) {
			    /* This is valid as we have already checked */
			    used = mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st);
			    memcpy(bf, p, used); bf[used] = '\0';
			    SET_STRING_ELT(t, j, markKnown(bf, STRING_ELT(x, i)));
			}
		    }
		} else {
		    /* useBytes or ASCII or
		       single-byte locale and not marked as UTF-8 */
		    char bf[2];
		    ntok = strlen(buf);
		    PROTECT(t = allocVector(STRSXP, ntok));
		    bf[1] = '\0';
		    if(useBytes) {
			for (j = 0; j < ntok; j++) {
			    bf[0] = buf[j];
			    SET_STRING_ELT(t, j, mkBytesNew(bf, haveBytesInput));
			}
		    } else {
			for (j = 0; j < ntok; j++) {
			    bf[0] = buf[j];
			    SET_STRING_ELT(t, j, markKnown(bf, STRING_ELT(x, i)));
			}
		    }
		}
		SET_VECTOR_ELT(ans, i, t);
		UNPROTECT(1);
		vmaxset(vmax2);
	    }
	} else if (fixed_opt) {
	    const char *laststart, *ebuf;
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else if (use_UTF8) { // includes Latin-1 support
		split = trCharUTF82(STRING_ELT(tok, itok));
		if (!split || !utf8Valid(split))
		    error(_("'split' string %lld is invalid UTF-8"),
		          (long long)itok+1);
	    } else {
		split = translateCharFP2(STRING_ELT(tok, itok));
		if (!split || (mbcslocale && !mbcsValid(split)))
		    error(_("'split' string %lld is invalid in this locale"),
			  (long long)itok+1);
	    }
	    int slen = (int) strlen(split);

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}

		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = trCharUTF82(STRING_ELT(x, i));
		    if (!buf || !utf8Valid(buf)) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid UTF-8"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateCharFP2(STRING_ELT(x, i));
		    if (!buf || (mbcslocale && !mbcsValid(buf))) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid in this locale"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		/* find out how many splits there will be */
		size_t ntok = 0;
		/* This is UTF-8 safe since it compares whole strings */
		laststart = buf;
		ebuf = buf + strlen(buf);
		for (bufp = buf; bufp < ebuf; bufp++) {
		    if ((slen == 1 && *bufp != *split) ||
			(slen > 1 && !streqln(bufp, split, slen))) continue;
		    ntok++;
		    bufp += std::max(slen - 1, 0);
		    laststart = bufp+1;
		}
		bufp = laststart;
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		laststart = bufp = buf;
		pt = R_Realloc(pt, strlen(buf)+1, char);
		for (size_t j = 0; j < ntok; j++) {
		    /* This is UTF-8 safe since it compares whole
		       strings, but <MBCS-FIXME> it would be more
		       efficient to skip along by chars.
		    */
		    for (; bufp < ebuf; bufp++) {
			if ((slen == 1 && *bufp != *split) ||
			    (slen > 1 && !streqln(bufp, split, slen))) continue;
			if (slen) {
			    if (bufp > laststart)
				memcpy(pt, laststart, bufp - laststart);
			    pt[bufp - laststart] = '\0';
			} else {
			    pt[0] = *bufp; pt[1] ='\0';
			}
			bufp += std::max(slen-1, 0);
			laststart = bufp+1;
			if (useBytes)
			    SET_STRING_ELT(t, j, mkBytesNew(pt, haveBytesInput));
			else if (use_UTF8)
			    SET_STRING_ELT(t, j, mkCharCE(pt, CE_UTF8));
			else
			    SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
			break;
		    }
		    bufp = laststart;
		}
		if (*bufp) {
		    if (useBytes)
			SET_STRING_ELT(t, ntok, mkBytesNew(bufp, haveBytesInput));
		    else if (use_UTF8)
			SET_STRING_ELT(t, ntok, mkCharCE(bufp, CE_UTF8));
		    else
			SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		}
		vmaxset(vmax2);
	    }
	} else if (perl_opt) {
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else if (use_UTF8) {
		split = trCharUTF82(STRING_ELT(tok, itok));
		if (!split || !utf8Valid(split))
		    error(_("'split' string %lld is invalid UTF-8"),
		          (long long)itok+1);
	    } else {
		split = translateCharFP2(STRING_ELT(tok, itok));
		if (!split || (mbcslocale && !mbcsValid(split)))
		    error(_("'split' string %lld is invalid in this locale"),
		          (long long)itok+1);
	    }
#ifdef HAVE_PCRE2
	    pcre2_code *re = NULL;
	    pcre2_match_context *mcontext = NULL;
	    PCRE2_SIZE *ovector = NULL;
	    uint32_t ovecsize = 10;
	    R_pcre2_prepare(split, x, use_UTF8, false, &tables, &re, &mcontext);
	    pcre2_match_data *mdata = pcre2_match_data_create(ovecsize, NULL);
	    uint32_t eflag = 0;
	    if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;
#else
	    pcre *re_pcre = NULL;
	    pcre_extra *re_pe = NULL;
	    int ovecsize = 30;
	    int ovector[ovecsize];

	    R_pcre_prepare(split, x, use_UTF8, false, true, &tables, &re_pcre,
	                 &re_pe);
#endif
	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}

		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = trCharUTF82(STRING_ELT(x, i));
		    if (!buf || !utf8Valid(buf)) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid UTF-8"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateCharFP2(STRING_ELT(x, i));
		    if (!buf || (mbcslocale && !mbcsValid(buf))) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid in this locale"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		/* find out how many splits there will be */
		ntok = 0;
		bufp = buf;
		if (*bufp) {
		    int rc;
#ifdef HAVE_PCRE2
		    while((rc = pcre2_match(re, (PCRE2_SPTR) bufp,
		                            PCRE2_ZERO_TERMINATED,
					    0, eflag, mdata, mcontext)) >= 0) {
			ovector = pcre2_get_ovector_pointer(mdata);
#else
		    while((rc = pcre_exec(re_pcre, re_pe, bufp,
					  (int) strlen(bufp),
					  0, 0, ovector, ovecsize)) >= 0) {
#endif
			/* Empty matches get the next char, so move by one. */
			if (ovector[1] > 0)
			    bufp += ovector[1];
			else if (*bufp)
			    bufp += utf8clen(*bufp);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		    R_pcre_exec_error(rc, i);
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		bufp = buf;
		pt = R_Realloc(pt, strlen(buf)+1, char);
		for (j = 0; j < ntok; j++) {
#ifdef HAVE_PCRE2
		    int rc = pcre2_match(re, (PCRE2_SPTR) bufp,
				       PCRE2_ZERO_TERMINATED,
		                       0, eflag, mdata, mcontext);
#else
		    int rc = pcre_exec(re_pcre, re_pe, bufp,
				       (int) strlen(bufp), 0, 0,
				       ovector, ovecsize);
#endif
		    R_pcre_exec_error(rc, i);
		    if (ovector[1] > 0) {
			/* Match was non-empty. */
			if (ovector[0] > 0)
			    strncpy(pt, bufp, ovector[0]);
			pt[ovector[0]] = '\0';
			bufp += ovector[1];
		    } else {
			/* Match was empty. */
			int clen = utf8clen(*bufp);
			strncpy(pt, bufp, clen);
			pt[clen] = '\0';
			bufp += clen;
		    }
		    if (useBytes)
			SET_STRING_ELT(t, j, mkBytesNew(pt, haveBytesInput));
		    else if (use_UTF8)
			SET_STRING_ELT(t, j, mkCharCE(pt, CE_UTF8));
		    else
			SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
		}
		if (*bufp) {
		    if (useBytes)
			SET_STRING_ELT(t, ntok, mkBytesNew(bufp, haveBytesInput));
		    else if (use_UTF8)
			SET_STRING_ELT(t, ntok, mkCharCE(bufp, CE_UTF8));
		    else
			SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		}
		vmaxset(vmax2);
	    }
#ifdef HAVE_PCRE2
	    pcre2_match_data_free(mdata);
	    pcre2_code_free(re);
	    pcre2_match_context_free(mcontext);
#else
	    if(re_pe) pcre_free_study(re_pe);
	    pcre_free(re_pcre);
#endif
	} else if (!useBytes && use_UTF8) { /* ERE in wchar_t */
	    regex_t reg;
	    regmatch_t regmatch[1];
	    int rc;
	    int cflags = REG_EXTENDED;
	    const wchar_t *wbuf, *wbufp, *wsplit;

	    /* Careful: need to distinguish empty (rm_eo == 0) from
	       non-empty (rm_eo > 0) matches.  In the former case, the
	       token extracted is the next character.  Otherwise, it is
	       everything before the start of the match, which may be
	       the empty string (not a ``token'' in the strict sense).
	    */

	    wsplit = wtransChar2(STRING_ELT(tok, itok));
	    if (!wsplit)
		error(_("'split' string %lld is invalid"), (long long)itok+1);
	    if ((rc = tre_regwcomp(&reg, wsplit, cflags)))
		reg_report(rc, &reg, translateChar(STRING_ELT(tok, itok)));
	    bool ascii_split = IS_ASCII(STRING_ELT(tok, itok));

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		wbuf = wtransChar2(STRING_ELT(x, i));
		bool ascii_xi = IS_ASCII(STRING_ELT(x, i));
		if (!wbuf) {
		    if(nwarn++ < NWARN)
			warning(_("input string %lld is invalid"),
			        (long long)i+1);
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}

		/* find out how many splits there will be */
		ntok = 0;
		wbufp = wbuf;
		if (*wbufp) {
		    while(tre_regwexec(&reg, wbufp, 1, regmatch, 0) == 0) {
			/* Empty matches get the next char, so move by one. */
			wbufp += std::max(regmatch[0].rm_eo, 1);
			ntok++;
			if (!*wbufp) break;
		    }
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*wbufp ? 1 : 0)));
		/* and fill with the splits */
		wbufp = wbuf;
		wpt = R_Realloc(wpt, wcslen(wbuf)+1, wchar_t);
		for (j = 0; j < ntok; j++) {
		    tre_regwexec(&reg, wbufp, 1, regmatch, 0);
		    if (regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if (regmatch[0].rm_so > 0)
			    wcsncpy(wpt, wbufp, regmatch[0].rm_so);
			wpt[regmatch[0].rm_so] = 0;
			wbufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			wpt[0] = *wbufp;
			wpt[1] = 0;
			wbufp++;
		    }
		    SET_STRING_ELT(t, j,
				   mkCharWLenASCII(wpt, regmatch[0].rm_so,
				                   (ascii_split && ascii_xi)));
		}
		if (*wbufp)
		    SET_STRING_ELT(t, ntok,
				   mkCharWLenASCII(wbufp, (int) wcslen(wbufp),
				                   (ascii_split && ascii_xi)));
		vmaxset(vmax2);
	    }
	    tre_regfree(&reg);
	} else { /* ERE in normal chars -- single byte */
	         /* previously used also with MBCS */
	    regex_t reg;
	    regmatch_t regmatch[1];
	    int rc;
	    int cflags = REG_EXTENDED;

	    /* Careful: need to distinguish empty (rm_eo == 0) from
	       non-empty (rm_eo > 0) matches.  In the former case, the
	       token extracted is the next character.  Otherwise, it is
	       everything before the start of the match, which may be
	       the empty string (not a ``token'' in the strict sense).
	    */
	    /* never use_UTF8 */
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else {
		split = translateCharFP2(STRING_ELT(tok, itok));
		if (!split || (mbcslocale && !mbcsValid(split)))
		    error(_("'split' string %lld is invalid in this locale"),
		          (long long)itok+1);
	    }
	    if (useBytes)
		rc = tre_regcompb(&reg, split, cflags);
	    else
		rc = tre_regcomp(&reg, split, cflags);

	    if(rc) reg_report(rc, &reg, split);

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		/* never use_UTF8 */
		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else {
		    buf = translateCharFP2(STRING_ELT(x, i));
		    if (!buf || (mbcslocale && !mbcsValid(buf))) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid in this locale"),
			            (long long)i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}

		/* find out how many splits there will be */
		ntok = 0;
		bufp = buf;
		if (*bufp) {
		    if (useBytes) {
			while(!(rc = tre_regexecb(&reg, bufp, 1, regmatch, 0))) {
			/* Empty matches get the next char, so move by one. */
			bufp += std::max(regmatch[0].rm_eo, 1);
			ntok++;
			if (*bufp == '\0') break;
		    }
		    } else {
			while(!(rc = tre_regexec(&reg, bufp, 1, regmatch, 0))) {
			    /* Empty matches get the next char, so move by one. */
                            /* Not necessarily correct with MBCS, but only used
			       with single bytes */
			    bufp += std::max(regmatch[0].rm_eo, 1);
			    ntok++;
			    if (*bufp == '\0') break;
			}
		    }
		    // AFAICS the only possible error report is REG_ESPACE
		    if (rc == REG_ESPACE)
			warning(_("Out-of-memory error in regexp matching for element %d"),
				(int) i + 1);
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		bufp = buf;
		pt = R_Realloc(pt, strlen(buf)+1, char);
		for (j = 0; j < ntok; j++) {
		    int rc;
		    if(useBytes) rc = tre_regexecb(&reg, bufp, 1, regmatch, 0);
		    else rc = tre_regexec(&reg, bufp, 1, regmatch, 0);

		    // AFAICS the only possible error report is REG_ESPACE
		    if (rc == REG_ESPACE)
			warning(_("Out-of-memory error in regexp matching for element %d"),
				(int) i + 1);
		    if (regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if (regmatch[0].rm_so > 0)
			    strncpy(pt, bufp, regmatch[0].rm_so);
			pt[regmatch[0].rm_so] = '\0';
			bufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			/* Not necessarily correct with MBCS, but only used
			   with single bytes */
			bufp++;
		    }
		    if (useBytes)
			SET_STRING_ELT(t, j, mkBytesNew(pt, haveBytesInput));
		    else
			SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
		}
		if (*bufp) {
		    if (useBytes)
			SET_STRING_ELT(t, ntok, mkBytesNew(bufp, haveBytesInput));
		    else
			SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		}
		vmaxset(vmax2);
	    }
	    tre_regfree(&reg);
	}
	vmaxset(vmax);
    }

    if (getAttrib(x, R_NamesSymbol) != R_NilValue)
	namesgets(ans, getAttrib(x, R_NamesSymbol));
    UNPROTECT(1);
    R_Free(pt); R_Free(wpt);
#ifdef HAVE_PCRE2
    if (tables) free((void *)tables);
    /* new PCRE2 will have pcre2_maketables_free() */
#else
    if (tables) pcre_free((void *)tables);
#endif
    UNPROTECT(1); /* tok */
    return ans;
}

/* Used by grep[l] and [g]regexpr, with return value the match
   position in characters */
/* This could be faster for plen > 1 particularly in non-UTF8 mbcs, but
   uses in R are for small strings and such mbcs should no longer be common */
static int fgrep_one(const char *pat, const char *target,
		     bool useBytes, bool use_UTF8, int *next)
{
    int plen = (int) strlen(pat), len = (int) strlen(target);
    int i = -1;
    const char *p;

    if (plen == 0) {
	if (next != NULL) *next = 1;
	return 0;
    }
    if (plen == 1 && (useBytes || !(mbcslocale || use_UTF8))) {
	/* a single byte is a common case */
	for (i = 0, p = target; *p; p++, i++)
	    if (*p == pat[0]) {
		if (next != NULL) *next = i + 1;
		return i;
	    }
	return -1;
    }
    if (!useBytes && use_UTF8) {
	const char *pos = strstr(target, pat);
	if (pos) {
	    int ib = (int) (pos-target);
	    int jb;

	    if (next != NULL) *next = ib + plen;
	    for (jb = 0, i = 0; jb < ib; jb++)
		/* count leading UTF-8 bytes */
		if ((target[jb] & 0xc0) != 0x80) i++;
	    return i;
	}
    } else if (!useBytes && mbcslocale) { /* skip along by chars */
	mbstate_t mb_st;
	int ib, used;
	mbs_init(&mb_st);
	for (ib = 0, i = 0; ib <= len-plen; i++) {
	    if (streqln(pat, target+ib, plen)) {
		if (next != NULL) *next = ib + plen;
		return i;
	    }
	    used = (int) Mbrtowc(NULL,  target+ib, R_MB_CUR_MAX, &mb_st);
	    if (used <= 0) break;
	    ib += used;
	}
    } else {
	const char *pos = strstr(target, pat);
	if (pos) {
	    i = (int) (pos-target);
	    if (next != NULL) *next = i + plen;
	    return i;
	}
    }
    return -1;
}

/* Returns the match position in bytes, for use in [g]sub.
   len is the length (strlen) of target.
   patlen is the length (strlen) of pat. (FIXME: should be size_t)
*/

static int fgrep_one_bytes(const char *pat, size_t patlen, const char *target,
                           int len, bool useBytes, bool use_UTF8)
{
    int i = -1;
    const char *p;

    if (patlen == 0) return 0;

    /* UTF-8 allows for byte-based search of valid pattern string inside
       valid target string, because lead and continuation bytes are taken
       from distinct ranges. */
    if (patlen == 1 && (useBytes || use_UTF8 || !mbcslocale)) {
	/* a single byte is a common case */
	for (i = 0, p = target; *p; p++, i++)
	    if (*p == pat[0]) return i;
	return -1;
    }
    if (!use_UTF8 && !useBytes && mbcslocale) { /* skip along by chars */
	mbstate_t mb_st;
	int ib, used;
	mbs_init(&mb_st);
	for (ib = 0, i = 0; (size_t) ib <= len-patlen; i++) {
	    if (streqln(pat, target+ib, patlen)) return ib;
	    used = (int) Mbrtowc(NULL, target+ib, R_MB_CUR_MAX, &mb_st);
	    if (used <= 0) break;
	    ib += used;
	}
    } else {
	const char *pos = strstr(target, pat);
	if (pos) return (int) (pos-target);
    }
    return -1;
}

attribute_hidden SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ind, ans;
    regex_t reg;
    R_xlen_t i, j, n;
    int nmatches = 0, rc;
    const char *spat = NULL;
    const wchar_t *wpat = NULL;
    const unsigned char *tables = NULL /* -Wall */;
#ifdef HAVE_PCRE2
    pcre2_code *re = NULL;
    pcre2_match_context *mcontext = NULL;
    uint32_t ovecsize = 1;
    pcre2_match_data *mdata = NULL;
#else
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    int ovecsize = 3;
    int ov[ovecsize];
#endif
    bool use_UTF8 = false, use_WC = false;
    const void *vmax;
    int nwarn = 0;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    bool igcase_opt = asBool2(CAR(args), call); args = CDR(args);
    bool value_opt = asBool2(CAR(args), call); args = CDR(args);
    bool perl_opt = asBool2(CAR(args), call); args = CDR(args);
    bool fixed_opt = asBool2(CAR(args), call); args = CDR(args);
    bool useBytes = asBool2(CAR(args), call); args = CDR(args);
    bool invert = asBool2(CAR(args), call);
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(pat) || LENGTH(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (LENGTH(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(text);
    if (STRING_ELT(pat, 0) == NA_STRING) {
	if (value_opt) {
	    SEXP nmold = PROTECT(getAttrib(text, R_NamesSymbol));
	    PROTECT(ans = allocVector(STRSXP, n));
	    for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);
	    if (!isNull(nmold))
		setAttrib(ans, R_NamesSymbol, duplicate(nmold));
	    UNPROTECT(2); /* ans, nmold */
	} else if (PRIMVAL(op)) { // grepl case
	    ans = allocVector(LGLSXP, n);
	    for (i = 0; i < n; i++)  LOGICAL(ans)[i] = NA_LOGICAL;
	} else {
	    ans = allocVector(INTSXP, n);
	    for (i = 0; i < n; i++)  INTEGER(ans)[i] = NA_INTEGER;
	}
	return ans;
    }

    if (!useBytes) 
	useBytes = only_ascii(pat, 1) && only_ascii(text, n);
    if (!useBytes) 
	useBytes = have_bytes(pat, 1) || have_bytes(text, n);
    if (!useBytes) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales */
	/* if we have non-ASCII text in a DBCS locale, we need to use wchar in TRE */
	if (!fixed_opt && mbcslocale) use_UTF8 = true;
	if (!use_UTF8)
	    use_UTF8 = (have_utf8(pat, 1) || have_utf8(text, n));
	if (!use_UTF8 && !latin1locale)
	    use_UTF8 = (have_latin1(pat, 1) || have_latin1(text, n));
    }

    if (!fixed_opt && !perl_opt) {
	use_WC = use_UTF8; use_UTF8 = false;
    }
    if (useBytes)
	spat = CHAR(STRING_ELT(pat, 0));
    else if (use_WC) {
	wpat = wtransChar2(STRING_ELT(pat, 0));
	if (!wpat) error("%s", _("regular expression is invalid"));
    } else if (use_UTF8) {
	spat = trCharUTF82(STRING_ELT(pat, 0));
	if (!spat || !utf8Valid(spat)) error("%s", _("regular expression is invalid UTF-8"));
    } else {
	spat = translateCharFP2(STRING_ELT(pat, 0));
	if (!spat || (mbcslocale && !mbcsValid(spat)))
	    error("%s", _("regular expression is invalid in this locale"));
    }

    if (fixed_opt) ;
    else if (perl_opt) {
#ifdef HAVE_PCRE2
	R_pcre2_prepare(spat, text, use_UTF8, igcase_opt, &tables, &re,
	                &mcontext);
	mdata = pcre2_match_data_create(ovecsize, NULL);
#else
	R_pcre_prepare(spat, text, use_UTF8, igcase_opt, false, &tables,
                     &re_pcre, &re_pe);
#endif
    } else {
	int cflags = REG_NOSUB | REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC)
	    rc = tre_regcompb(&reg, spat, cflags);
	else
	    rc = tre_regwcomp(&reg, wpat, cflags);
	if (rc) reg_report(rc, &reg, spat);
    }

    PROTECT(ind = allocVector(LGLSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	LOGICAL(ind)[i] = 0;
	if (STRING_ELT(text, i) != NA_STRING) {
	    const char *s = NULL;
	    const wchar_t *ws = NULL;
	    if (useBytes)
		s = CHAR(STRING_ELT(text, i));
	    else if (use_WC) {
		ws = wtransChar2(STRING_ELT(text, i));
		if (!ws) {
		    if(nwarn++ < NWARN)
			warning(_("input string %lld is invalid"),
			        (long long)i+1);
		    continue;
		}
	    } else if (use_UTF8) {
		s = trCharUTF82(STRING_ELT(text, i));
		if (!s || !utf8Valid(s)) {
		    if(nwarn++ < NWARN)
			warning(_("input string %lld is invalid UTF-8"),
			        (long long)i+1);
		    continue;
		}
	    } else {
		s = translateCharFP2(STRING_ELT(text, i));
		if (!s || (mbcslocale && !mbcsValid(s))) {
		    if(nwarn++ < NWARN)
			warning(_("input string %lld is invalid in this locale"),
			        (long long)i+1);
		    continue;
		}
	    }

	    if (fixed_opt)
		LOGICAL(ind)[i] = fgrep_one(spat, s, useBytes, use_UTF8, NULL) >= 0;
	    else if (perl_opt) {
#ifdef HAVE_PCRE2
		uint32_t eflag = 0;
		if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;

		int rc = pcre2_match(re, (PCRE2_SPTR) s, PCRE2_ZERO_TERMINATED,
		                     0, eflag, mdata, mcontext);
#else
		int rc =
		    pcre_exec(re_pcre, re_pe, s, (int) strlen(s), 0, 0, ov, 0);
#endif
		if(rc >= 0) LOGICAL(ind)[i] = 1;
		else {
		    LOGICAL(ind)[i] = 0;
		    R_pcre_exec_error(rc, i);
		}
	    } else {
		if (!use_WC)
		    rc = tre_regexecb(&reg, s, 0, NULL, 0);
		else
		    rc = tre_regwexec(&reg, ws, 0, NULL, 0);
		if (rc == 0) LOGICAL(ind)[i] = 1;
		// AFAICS the only possible error report is REG_ESPACE
		if (rc == REG_ESPACE)
		    warning(_("Out-of-memory error in regexp matching for element %d"),
			    (int) i + 1);
	    }
	}
	vmaxset(vmax);
	if (invert ^ LOGICAL(ind)[i]) nmatches++;
    }

    if (fixed_opt);
    else if (perl_opt) {
#ifdef HAVE_PCRE2
	pcre2_match_data_free(mdata);
	pcre2_code_free(re);
	pcre2_match_context_free(mcontext);
	if (tables)
	    /* new PCRE2 will have pcre2_maketables_free() */
	    free((void *)tables);
#else
	if (re_pe) pcre_free_study(re_pe);
	pcre_free(re_pcre);
	pcre_free((unsigned char *) tables);
#endif
    } else
	tre_regfree(&reg);

    if (PRIMVAL(op)) {/* grepl case */
	UNPROTECT(1); /* ind */
	return ind;
    }

    if (value_opt) {
	SEXP nmold = PROTECT(getAttrib(text, R_NamesSymbol)), nm;
	PROTECT(ans = allocVector(STRSXP, nmatches));
	for (i = 0, j = 0; i < n ; i++)
	    if (invert ^ LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(text, i));
	/* copy across names and subset */
	if (!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
	UNPROTECT(2); /* ans, nmold */
    } else {
#ifdef LONG_VECTOR_SUPPORT
	if (n > INT_MAX) {
	    ans = allocVector(REALSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i]) REAL(ans)[j++] = (double)(i + 1);
	} else
#endif
	{
	    ans = allocVector(INTSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i])
		    INTEGER(ans)[j++] = (int) (i + 1);
	}
    }
    UNPROTECT(1); /* ind */
    return ans;
}


/* fixed, single binary search, no error checking; -1 = no match, otherwise offset
   NOTE: all offsets here (in & out) are 0-based !! */
static R_size_t fgrepraw1(SEXP pat, SEXP text, R_size_t offset) {
    Rbyte *haystack = RAW(text), *needle = RAW(pat);
    R_size_t n = LENGTH(text);
    R_size_t ncmp = LENGTH(pat);
    if (n < ncmp)
	return (R_size_t) -1;
    switch (ncmp) { /* it may be silly but we optimize small needle
		       searches, because they can be used to match
		       single UTF8 chars (up to 3 bytes) */
    case 1:
	{
	    Rbyte c = needle[0];
	    while (offset < n) {
		if (haystack[offset] == c)
		    return offset;
		offset++;
	    }
	    return (R_size_t) -1;
	}
    case 2:
	{
	    n--;
	    while (offset < n) {
		if (haystack[offset    ] == needle[0] &&
		    haystack[offset + 1] == needle[1])
		    return offset;
		offset++;
	    }
	    return (R_size_t) -1;
	}
    case 3:
	{
	    n -= 2;
	    while (offset < n) {
		if (haystack[offset    ] == needle[0] &&
		    haystack[offset + 1] == needle[1] &&
		    haystack[offset + 2] == needle[2])
		    return offset;
		offset++;
	    }
	    return (R_size_t) -1;
	}
    default:
	{
	    ncmp--;
	    n -= ncmp;
	    while (offset < n) {
		if (haystack[offset] == needle[0] &&
		    !memcmp(haystack + offset + 1, needle + 1, ncmp))
		    return offset;
		offset++;
	    }
	}
    }
    return (R_size_t) -1;
}

/* grepRaw(pattern, text, offset, ignore.case, fixed, value, all, invert) */
// FIXME:  allow long vectors.
attribute_hidden SEXP do_grepraw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, res_head, res_tail;
    regex_t reg;
    int nmatches = 0, rc, cflags, eflags = 0;
    int *res_val;
    int res_alloc = 512; /* must be divisible by 2 since we may store
			    offset+length it is the initial size of
			    the integer vector of matches */
    R_size_t res_ptr, offset;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    offset = asInteger(CAR(args)); args = CDR(args);
    bool igcase_opt = asBool2(CAR(args), call); args = CDR(args);
    bool fixed_opt = asBool2(CAR(args), call); args = CDR(args);
    bool value = asBool2(CAR(args), call); args = CDR(args);
    bool all = asBool2(CAR(args), call); args = CDR(args);
    bool invert = asBool2(CAR(args), call);
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");

    /* invert=TRUE, value=FALSE will really give you a headache
       thinking about it so we better not go there (the code below
       will actually respect it for all cases except for fixed=FALSE,
       all=TRUE so we could support it at some point but I fail to see
       any real use of it) */
    if (invert && !value) {
	warning(_("argument '%s' will be ignored"), "invert = TRUE");
	invert = 0;
    }

    /* currently we support only offset >= 1 */
    if (offset < 1)
	error(_("invalid '%s' argument"), "offset");
    if (!isRaw(pat))
	error(_("invalid '%s' argument"), "pattern");
    if (!isRaw(text))
	error(_("invalid '%s' argument"), "text");
    if (offset > (R_size_t) LENGTH(text))
	return allocVector(INTSXP, 0);

    offset--; /* reduce offset to base 0 */

    /* TRE fails miserably for REG_LITERAL -- not only is it slow but
       it doesn't handle embedded NULs properly (e.g., compile
       goes into an infinite loop with "\00" pattern) -- so we have
       to do it by hand */
    if (fixed_opt) {
	if (LENGTH(pat) == 0)
	    return allocVector(value ? (all ? VECSXP : RAWSXP) : INTSXP, 0);
	if (!all) {
	    R_size_t res = fgrepraw1(pat, text, offset);
	    if (invert) {
		Rbyte *ansp;
		if (res == (R_size_t) -1) return value ? text : ScalarInteger(1);
		if (!value) return ScalarInteger(((res == 0) ? LENGTH(pat) : 0) + 1);
		ans = allocVector(RAWSXP, LENGTH(text) - LENGTH(pat));
		ansp = RAW(ans);
		if (res) {
		    memcpy(ansp, RAW(text), res);
		    ansp += res;
		}
		res += LENGTH(pat);
		if (res < (R_size_t) LENGTH(text))
		    memcpy(ansp, RAW(text) + res, LENGTH(text) - res);
		return ans;
	    }
	    if (res == (R_size_t) -1) return allocVector(value ? RAWSXP : INTSXP, 0);
	    if (!value) return ScalarInteger((int)(res + 1));
	    /* value=TRUE doesn't really make sense for anything other than
	       match/nomatch detection since we just return the pattern */
	    return pat;
	} else {
	    /* There are two ways to do it: two pass or one pass. We
	       use the latter with TRE below, but for a sequential
	       search I assume it's fast enough so it's not worth the
	       hassle.  We just special-case really tiny matches which
	       should be the most common case anyway.
	    */
#define MAX_MATCHES_MINIBUF 32
	    int matches[MAX_MATCHES_MINIBUF];
	    int n = LENGTH(text);
	    while (offset < (R_size_t) n) {
		offset = fgrepraw1(pat, text, offset);
		if (offset == (R_size_t) -1)
		    break;
		if (nmatches < MAX_MATCHES_MINIBUF)
		    matches[nmatches] = (int)(offset + 1);
		nmatches++;
		offset += LENGTH(pat);
	    }
	    if (value) {
		if (invert) { /* invert is actually useful here as it
				 is performing something like strsplit */
		    R_size_t pos = 0;
		    SEXP elt, mvec = NULL;
		    int *fmatches = (int*) matches; /* either the minbuffer or an allocated maxibuffer */
		    int nprotect = 0;

		    if (!nmatches) return text;

		    /* if there are more matches than in the buffer,
		       we actually need to get them first */
		    if (nmatches > MAX_MATCHES_MINIBUF) {
			PROTECT(mvec = allocVector(INTSXP, nmatches));
			nprotect++;
			fmatches = INTEGER(mvec);
			memcpy(fmatches, matches, sizeof(matches));
			nmatches = MAX_MATCHES_MINIBUF;
			offset = matches[MAX_MATCHES_MINIBUF - 1] + LENGTH(pat) - 1;
			while (offset < (R_size_t) n) {
			    offset = fgrepraw1(pat, text, offset);
			    if (offset == (R_size_t) -1)
				break;
			    INTEGER(mvec)[nmatches++] = (int)(offset + 1);
			    offset += LENGTH(pat);
			}
		    }

		    /* there are always nmatches + 1 pieces (unlike strsplit) */
		    ans = PROTECT(allocVector(VECSXP, nmatches + 1));
		    nprotect++;
		    /* add all pieces before matches */
		    for (int i = 0; i < nmatches; i++) {
			R_size_t elt_size = fmatches[i] - 1 - pos;
			elt = allocVector(RAWSXP, elt_size);
			SET_VECTOR_ELT(ans, i, elt);
			if (elt_size)
			    memcpy(RAW(elt), RAW(text) + pos, elt_size);
			pos = fmatches[i] - 1 + LENGTH(pat);
		    }
		    /* add the rest after last match */
		    elt = allocVector(RAWSXP, LENGTH(text) - (fmatches[nmatches - 1] - 1 + LENGTH(pat)));
		    SET_VECTOR_ELT(ans, nmatches, elt);
		    if (LENGTH(elt))
			memcpy(RAW(elt), RAW(text) + LENGTH(text) - LENGTH(elt), LENGTH(elt));
		    UNPROTECT(nprotect);
		    return ans;
		}

		/* value=TRUE is pathetic for fixed=TRUE without
		   invert as it is just rep(pat, nmatches) */
		ans = PROTECT(allocVector(VECSXP, nmatches));
		for (int i = 0; i < nmatches; i++)
		    SET_VECTOR_ELT(ans, i, pat);
		UNPROTECT(1);
		return ans;
	    }
	    ans = allocVector(INTSXP, nmatches);
	    if (nmatches <= MAX_MATCHES_MINIBUF) { /* our min-buffer was enough, great */
		if (nmatches) memcpy(INTEGER(ans), matches, nmatches * sizeof(int));
		return ans;
	    }
	    /* more matches than we could remember, time for pass 2 */
	    memcpy(INTEGER(ans), matches, sizeof(matches));
	    /* but we are not completely stupid - we can continue
	       where amnesia hit us */
	    nmatches = MAX_MATCHES_MINIBUF;
	    offset = matches[MAX_MATCHES_MINIBUF - 1] + LENGTH(pat) - 1; /* matches are 1-based, we are 0-based hence - 1 */
	    while (offset < (R_size_t) n) {
		offset = fgrepraw1(pat, text, offset);
		if (offset == (R_size_t) -1)
		    break;
		INTEGER(ans)[nmatches++] = (int)(offset + 1);
		offset += LENGTH(pat);
	    }
	    return ans;
	}
    }

    cflags = REG_EXTENDED;
    if (igcase_opt) cflags |= REG_ICASE;

    rc = tre_regncompb(&reg, (const char*) RAW(pat), LENGTH(pat), cflags);
    if (rc) reg_report(rc, &reg, NULL /* pat is not necessarily a C string */ );

    if (!all) { /* match only once */
	regmatch_t ptag;
	rc = tre_regnexecb(&reg, (const char*) RAW(text) + offset, LENGTH(text) - offset, 1, &ptag, 0);
	tre_regfree(&reg);
	if (value) {
	    if (rc != REG_OK || ptag.rm_eo == ptag.rm_so) /* TODO: is this good enough? it is the same as matching an empty string ... */
		return invert ? text : allocVector(RAWSXP, 0);
	    if (invert) {
		Rbyte *ansp;
		R_size_t len;
		ans = allocVector(RAWSXP, LENGTH(text) - (ptag.rm_eo - ptag.rm_so));
		ansp = RAW(ans);
		if (ptag.rm_so) {
		    memcpy(ansp, RAW(text), ptag.rm_so);
		    ansp += ptag.rm_so;
		}
		len = LENGTH(text) - ptag.rm_eo;
		if (len)
		    memcpy(ansp, RAW(text) + ptag.rm_eo, len);
	    } else {
		ans = allocVector(RAWSXP, ptag.rm_eo - ptag.rm_so);
		memcpy(RAW(ans), RAW(text) + offset + ptag.rm_so, ptag.rm_eo - ptag.rm_so);
	    }
	    return ans;
	}
	return (rc == REG_OK) ? ScalarInteger((int)(ptag.rm_so + 1 + offset)) : allocVector(INTSXP, 0);
    }

    /* match all - we use a pairlist of integer arrays to expand the result
       to allow use on big binary strings with many matches (it could be done
       by re-allocating a temp buffer but I chose sequential allocations to
       reduce possible fragmentation) */
    res_head = res_tail = PROTECT(list1(allocVector(INTSXP, res_alloc)));
    res_val = INTEGER(CAR(res_tail));
    res_ptr = 0;
    while (1) {
	regmatch_t ptag;
	rc = tre_regnexecb(&reg, (const char*) RAW(text) + offset, LENGTH(text) - offset, 1, &ptag, eflags);
	if (rc)
	    break;
	if (!nmatches) eflags |= REG_NOTBOL;
	if (res_ptr >= (R_size_t) res_alloc) {
	    /* double the buffer size, but limit to 32Mb */
	    if (res_alloc < 33554432) res_alloc <<= 1;
	    SETCDR(res_tail, list1(allocVector(INTSXP, res_alloc)));
	    res_tail = CDR(res_tail);
	    res_val = INTEGER(CAR(res_tail));
	    res_ptr = 0;
	}
	res_val[res_ptr++] = (int)(ptag.rm_so + 1 + offset);
	if (value) res_val[res_ptr++] = ptag.rm_eo - ptag.rm_so;
	offset += ptag.rm_eo;
	nmatches++;
	if (ptag.rm_eo == 0) { /* empty string matched => trouble; FIXME: we may want to consider just advancing anyway */
	    int infinite_match = 1;
	    /* the only place where this is acceptable is "^" as that will go away in the next step */
	    if (nmatches == 1) { /* to see if that is true, re-run the match with REG_NOTBOL (added above) */
		rc = tre_regnexecb(&reg, (const char*) RAW(text) + offset, LENGTH(text) - offset, 1, &ptag, eflags);
		if (rc != REG_OK || ptag.rm_eo != 0)
		    infinite_match = 0;
	    }
	    if (infinite_match)
		warning("%s", _("pattern matches an empty string infinitely, returning first match only"));
	    break;
	}
	if (offset >= (R_size_t) LENGTH(text)) break;
    }

    if (value) { /* for values we store in fact the absolute start offsets and length in the integer vector */
	SEXP vec = CAR(res_head);
	R_size_t entry = 0, cptr = 0, clen = (CDR(res_head) == R_NilValue) ? res_ptr : LENGTH(vec);
	R_size_t inv_start = 0; /* 0-based start position of the pieces for invert */
	res_val = INTEGER(vec);
	ans = PROTECT(allocVector(VECSXP, invert ? (nmatches + 1) : nmatches));
	while (entry < (R_size_t) nmatches) {
	    if (invert) { /* for invert=TRUE store the current piece up to the match */
		SEXP rvec = allocVector(RAWSXP, res_val[cptr] - 1 - inv_start);
		SET_VECTOR_ELT(ans, entry, rvec);
		entry++;
		if (LENGTH(rvec))
		    memcpy(RAW(rvec), RAW(text) + inv_start, LENGTH(rvec));
		inv_start = res_val[cptr] - 1 + res_val[cptr + 1];
	    } else { /* for invert=FALSE store the matched piece */
		SEXP rvec = allocVector(RAWSXP, res_val[cptr + 1]);
		SET_VECTOR_ELT(ans, entry, rvec);
		entry++;
		if (LENGTH(rvec))
		    memcpy(RAW(rvec), RAW(text) + res_val[cptr] - 1, LENGTH(rvec));
	    }
	    /* advance in the elements -- possibly jumping to the next list block */
	    cptr += 2;
	    if (cptr >= clen) {
		res_head = CDR(res_head);
		if (res_head == R_NilValue) break;
		vec = CAR(res_head);
		res_val = INTEGER(vec);
		cptr = 0;
		clen = (CDR(res_head) == R_NilValue) ? res_ptr : LENGTH(vec);
	    }
	}
	if (invert) { /* add the last piece after the last match */
	    SEXP lvec = allocVector(RAWSXP, LENGTH(text) - inv_start);
	    SET_VECTOR_ELT(ans, nmatches, lvec);
	    if (LENGTH(lvec))
		memcpy(RAW(lvec), RAW(text) + inv_start, LENGTH(lvec));
	}
	UNPROTECT(1);
    } else { /* if values are not needed, we just collect all the start offsets */
	ans = allocVector(INTSXP, nmatches);
	res_val = INTEGER(ans);
	while (res_head != R_NilValue) {
	    SEXP vec = CAR(res_head);
	    R_size_t len = (CDR(res_head) == R_NilValue) ? res_ptr : LENGTH(vec);
	    if (len) memcpy(res_val, INTEGER(vec), len * sizeof(int));
	    res_val += len;
	    res_head = CDR(res_head);
	}
    }
    UNPROTECT(1);

    tre_regfree(&reg);

    return ans;
}

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static char *string_adj(char *target, const char *orig, const char *repl,
			regmatch_t *regmatch)
{
    int i, k;
    const char *p = repl; char *t = target;

    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) p++; else {p++; *t++ = *p++;}
	}
	else *t++ = *p++;
    }
    return t;
}

/* used for single-byte locales, and UTF-8 for perl = TRUE */
static int count_subs(const char *repl)
{
    int i = 0;
    const char *p = repl;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {i++; p += 2;}
	    else if (p[1] == 0) p++; else p += 2;
	}
	else p++;
    }
    return i;
}

/* FIXME: use UCP for upper/lower conversion
          could use pcre2_substitute which will take care of that and also
          supports \u, \l
*/
#ifdef HAVE_PCRE2
static char *R_pcre_string_adj(char *target, const char *orig, const char *repl,
		      PCRE2_SIZE *ovec, bool use_UTF8, int ncap)
#else
static char *R_pcre_string_adj(char *target, const char *orig, const char *repl,
		      int *ovec, bool use_UTF8, int ncap)
#endif
{
    uint64_t i, nb;
    int k;
    const char *p = repl;
    char *t = target, c;
    bool upper = false, lower = false;

    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		if (k >= ncap) {
		    /* back-reference to a group that has not been captured,
		       treat it as an empty string; the special case is needed
		       for PCRE2, but not for PCRE where the length of the
		       matched group will appear to be zero because ovector
		       is zeroed */
		    p += 2;
		    continue;
		}
		/* Here we need to work in chars */
		nb = ovec[2*k+1] - ovec[2*k];
		/* unused patterns will have nb == 0, both offsets -1 with PCRE
		   and PCRE2_UNSET with PCRE2 */
		if (nb > 0 && use_UTF8 && (upper || lower)) {
		    wctrans_t tr = wctrans(upper ? "toupper" : "tolower");
		    uint64_t j;
		    int nc;
		    char *xi, *p;
		    wchar_t *wc;
		    R_CheckStack2((nb+1)*sizeof(char));
		    p = xi = (char *) alloca((nb+1)*sizeof(char));
		    for (j = 0; j < nb; j++) *p++ = orig[ovec[2*k]+j];
		    *p = '\0';
		    nc = (int) utf8towcs(NULL, xi, 0);
		    if (nc >= 0) {
			R_CheckStack2((nc+1)*sizeof(wchar_t));
			wc = (wchar_t *) alloca((nc+1)*sizeof(wchar_t));
			utf8towcs(wc, xi, nc + 1);
			for (int j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = (int) wcstoutf8(NULL, wc, INT_MAX);
			wcstoutf8(xi, wc, nb);
			for (j = 0; j < nb - 1; j++) *t++ = *xi++;
		    }
		} else
		    for (i = ovec[2*k] ; i < ovec[2*k+1] ; i++) {
			c = orig[i];
			*t++ = (char) (upper ? toupper(c) : (lower ? tolower(c) : c));
		    }
		p += 2;
	    } else if (p[1] == 'U') {
		p += 2;
		upper = true; lower = false;
	    } else if (p[1] == 'L') {
		p += 2;
		upper = false; lower = true;
	    } else if (p[1] == 'E') { /* end case modification */
		p += 2;
		upper = false; lower = false;
	    } else if (p[1] == 0) {
		p += 1;
	    } else {
		p += 1;
		*t++ = *p++;
	    }
	} else *t++ = *p++;
    }
    return t;
}

static wchar_t *wstring_adj(wchar_t *target, const wchar_t *orig,
			    const wchar_t *repl, regmatch_t *regmatch)
{
    int i, k;
    const wchar_t *p = repl;
    wchar_t *t = target;

    while (*p) {
	if (*p == L'\\') {
	    if (L'1' <= p[1] && p[1] <= L'9') {
		k = p[1] - L'0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) p++; else {p++; *t++ = *p++;}
	}
	else *t++ = *p++;
    }
    return t;
}

static int wcount_subs(const wchar_t *repl)
{
    int i = 0;
    const wchar_t *p = repl;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {i++; p += 2;}
	    else if (p[1] == 0) p++; else p += 2;
	}
	else p++;
    }
    return i;
}

static int sub_buffer_check_overflow(double d)
{
    /* 2147483647 is a length limit for R strings and 32-bit ints can be
       precisely represented in IEEE double (but not 64-bit ints) */
    if (!(d < INT_MAX) || !(d < 2147483647))
	error("%s", _("result string is too long"));
    return (int)d;
}

static void sub_buffer_size_init(size_t replen, int ns, int nsubs, int global,
                     int *nns, int *maxrep)
{
   /* worst possible scenario is to put a copy of the
      replacement after every character, unless there are
      backrefs */
    *maxrep = sub_buffer_check_overflow((double)replen + (ns-2.) * nsubs);
    if (global) {
	double dnns = (double)ns * (*maxrep + 1.) + 1000.;
	if (dnns > 10000) dnns = 2.*ns + (double)replen + 1000.;
	*nns = sub_buffer_check_overflow(dnns);
    } else
	*nns = sub_buffer_check_overflow((double)ns +
	                                 (double)*maxrep + 1000.);
}

static int sub_buffer_size_expand(double needed, int *nns)
{
    int ineeded = sub_buffer_check_overflow(needed);
    if (*nns < ineeded) {
	/* This could fail at smaller value on a 32-bit platform:
	   it is merely an integer overflow check */
	if (*nns < INT_MAX/2)
	    (*nns) *= 2;
	if (*nns < ineeded)
	    (*nns) = ineeded;
	return 1; 
    } else
	return 0;
}

static void sub_buffer_expand(double needed, int *nns, char **cbuf, char **u)
{
    if (sub_buffer_size_expand(needed, nns)) {
       char *tmp;
       tmp = R_Realloc(*cbuf, *nns, char);
       *u = tmp + (*u - *cbuf);
       *cbuf = tmp;
   }
}

static void wsub_buffer_expand(double needed, int *nns, wchar_t **cbuf, wchar_t **u)
{
    if (sub_buffer_size_expand(needed, nns)) {
       wchar_t *tmp;
       tmp = R_Realloc(*cbuf, *nns, wchar_t);
       *u = tmp + (*u - *cbuf);
       *cbuf = tmp;
   }
}

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

attribute_hidden SEXP do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, text, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    R_xlen_t i, n;
    int j, ns, nns, nmatch, offset, rc;
    bool global;
    int eflags, last_end;
    char *u, *cbuf;
    const char *spat = NULL, *srep = NULL, *s = NULL;
    size_t patlen = 0, replen = 0;
    bool use_UTF8 = false, use_WC = false;
    bool ascii_patrep = false;
    const wchar_t *wpat = NULL, *wrep = NULL, *ws = NULL;
    const unsigned char *tables = NULL;
#ifdef HAVE_PCRE2
    uint32_t ovecsize = 10;
    pcre2_code *re = NULL;
    pcre2_match_context *mcontext = NULL;
    pcre2_match_data *mdata = NULL;
#else
    int ovecsize = 30;
    pcre *re_pcre = NULL;
    pcre_extra *re_pe  = NULL;
#endif
    const void *vmax = vmaxget();
    bool haveBytesInput;

    checkArity(op, args);

    global = (bool) PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    bool igcase_opt = asBool2(CAR(args), call); args = CDR(args);
    bool perl_opt = asBool2(CAR(args), call); args = CDR(args);
    bool fixed_opt = asBool2(CAR(args), call); args = CDR(args);
    bool useBytes = asBool2(CAR(args), call); args = CDR(args);
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(pat) || LENGTH(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (LENGTH(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");
    if (!isString(rep) || LENGTH(rep) < 1)
	error(_("invalid '%s' argument"), "replacement");
    if (LENGTH(rep) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "replacement");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(text);
    /* This contradicts the code below that has NA matching NA */
    if (STRING_ELT(pat, 0) == NA_STRING) {
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);

	goto exit_gsub;
    }
    haveBytesInput = (have_bytes(pat, 1) || have_bytes(rep, 1) ||
                     have_bytes(text, n));

    if (!useBytes) 
	useBytes = only_ascii(pat, 1) && only_ascii(rep, 1) && 
	           only_ascii(text, n);
    if (!useBytes) 
	useBytes = haveBytesInput;
    if (!useBytes) {
	/* if we have non-ASCII text in a DBCS locale, we need to use wchar in TRE */
	if (!fixed_opt && mbcslocale) use_UTF8 = true;
	if (!use_UTF8)
	    use_UTF8 = (have_utf8(pat, 1) || have_utf8(rep, 1) ||
	               have_utf8(text, n));
	if (!use_UTF8 && !latin1locale)
	    use_UTF8 = (have_latin1(pat, 1) || have_latin1(rep, 1) ||
	               have_latin1(text, n));
    }

    if (!fixed_opt && !perl_opt) {
	use_WC = use_UTF8; use_UTF8 = false;
    }

    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	srep = CHAR(STRING_ELT(rep, 0));
    } else if (use_WC) {
	wpat = wtransChar2(STRING_ELT(pat, 0));
	if (!wpat) error(_("invalid '%s' argument"), "pattern");
	wrep = wtransChar2(STRING_ELT(rep, 0));
	if (!wrep) error(_("invalid '%s' argument"), "replacement");
	ascii_patrep = (IS_ASCII(STRING_ELT(pat, 0)) &&
	                IS_ASCII(STRING_ELT(rep, 0)));
    } else if (use_UTF8) {
	spat = trCharUTF82(STRING_ELT(pat, 0));
	if (!spat || !utf8Valid(spat)) error("%s", _("'pattern' is invalid UTF-8"));
	srep = trCharUTF82(STRING_ELT(rep, 0));
	if (!srep || !utf8Valid(srep)) error("%s", _("'replacement' is invalid UTF-8"));
    } else {
	spat = translateCharFP2(STRING_ELT(pat, 0));
	if (!spat || (mbcslocale && !mbcsValid(spat)))
	    error("%s", _("'pattern' is invalid in this locale"));
	srep = translateCharFP2(STRING_ELT(rep, 0));
	if (!srep || (mbcslocale && !mbcsValid(srep)))
	    error("%s", _("'replacement' is invalid in this locale"));
    }

    if (fixed_opt) {
	patlen = strlen(spat);
	if (!patlen) error("%s", _("zero-length pattern"));
	replen = strlen(srep);
    } else if (perl_opt) {
#ifdef HAVE_PCRE2
	R_pcre2_prepare(spat, text, use_UTF8, igcase_opt, &tables, &re,
	                &mcontext);
	mdata = pcre2_match_data_create(ovecsize, NULL);
#else
	R_pcre_prepare(spat, text, use_UTF8, igcase_opt, false, &tables,
	             &re_pcre, &re_pe);
#endif
	replen = strlen(srep);
    } else {
	int cflags = REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC) {
	    rc =  tre_regcompb(&reg, spat, cflags);
	    if (rc) reg_report(rc, &reg, spat);
	    replen = strlen(srep);
	} else {
	    rc  = tre_regwcomp(&reg, wpat, cflags);
	    if (rc) reg_report(rc, &reg, translateChar(STRING_ELT(pat, 0)));
	    replen = wcslen(wrep);
	}
    }

    PROTECT(ans = allocVector(STRSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	/* NA pattern was handled above */
	if (STRING_ELT(text, i) == NA_STRING) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	    continue;
	}

	if (useBytes)
	    s = CHAR(STRING_ELT(text, i));
	else if (use_WC) {
	    ws = wtransChar2(STRING_ELT(text, i));
	    if (!ws)
		error(_("input string %lld is invalid"), (long long)i+1);
	} else if (use_UTF8) {
	    s = trCharUTF82(STRING_ELT(text, i));
	    if (!s || !utf8Valid(s))
		error(_("input string %lld is invalid UTF-8"),
		     (long long)i+1);
	} else {
	    s = translateCharFP2(STRING_ELT(text, i));
	    if (!s || (mbcslocale && !mbcsValid(s)))
		error(_("input string %lld is invalid in this locale"),
		      (long long)i+1);
	}

	if (fixed_opt) {
	    int st, nr, slen = (int) strlen(s);
	    ns = slen;
	    st = fgrep_one_bytes(spat, patlen, s, ns, useBytes, use_UTF8);
	    if (st < 0)
		SET_STRING_ELT(ans, i, markBytesOld(STRING_ELT(text, i),
		                                    useBytes, haveBytesInput));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		if (global) { /* need to find max number of matches */
		    const char *ss= s;
		    int sst = st;
		    nr = 0;
		    do {
			nr++;
			ss += sst+patlen;
			slen -= (int)(sst+patlen);
		    } while((sst = fgrep_one_bytes(spat, patlen, ss, slen,
		                                   useBytes, use_UTF8)) >= 0);
		} else nr = 1;
		cbuf = u = R_Calloc(ns + nr*(replen - patlen) + 1, char);
		*u = '\0';
		slen = ns;
		do {
		    strncpy(u, s, st);
		    u += st;
		    s += st+patlen;
		    slen -= (int)(st+patlen);
		    strncpy(u, srep, replen);
		    u += replen;
		} while(global && (st = fgrep_one_bytes(spat, patlen, s, slen,
		                                        useBytes, use_UTF8)) >= 0);
		strcpy(u, s);
		if (useBytes)
		    SET_STRING_ELT(ans, i, mkBytesNew(cbuf, haveBytesInput));
		else if (use_UTF8)
		    SET_STRING_ELT(ans, i, mkCharCE(cbuf, CE_UTF8));
		else
		    SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
		R_Free(cbuf);
	    }
	} else if (perl_opt) {
	   int ncap, maxrep;
#ifdef HAVE_PCRE2
	   uint32_t eflag;
	   PCRE2_SIZE *ovector = NULL;
#else
	   int eflag;
	   int ovector[ovecsize];
	   /* zero for unknown patterns; this is done to make sure that back
              references to unset groups return an empty string, but it is
	      not needed anymore as ncap is being checked due to PCRE2 */
	   memset(ovector, 0, ovecsize*sizeof(int));
#endif
	   ns = (int) strlen(s);
	   sub_buffer_size_init(replen, ns, count_subs(srep), global,
	                        &nns, &maxrep);
	   u = cbuf = R_Calloc(nns, char);
	   offset = 0; nmatch = 0; eflag = 0; last_end = -1;
	   /* ncap is one more than the number of capturing patterns */
#ifdef HAVE_PCRE2
	   if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;
	   /* PCRE2 has also pcre2_substitute */
	   while ((rc = pcre2_match(re, (PCRE2_SPTR) s, (PCRE2_SIZE) ns,
				    (PCRE2_SIZE) offset, eflag, mdata,
				    mcontext)) >= 0 ) {

	       ovector = pcre2_get_ovector_pointer(mdata);
#else
	   while ((rc = pcre_exec(re_pcre, re_pe, s, ns, offset, eflag,
				  ovector, 30)) >= 0) {
#endif
	       /* printf("%s, %d, %d %d\n", s, offset,
		  ovector[0], ovector[1]); */
	       nmatch++;
	       for (size_t j = offset; j < ovector[0]; j++) *u++ = s[j];
	       ncap = rc > 0 ? rc : 10;
	       if (last_end == -1 /* for PCRE2 */ || ovector[1] > (R_size_t) last_end) {
		   u = R_pcre_string_adj(u, s, srep, ovector, use_UTF8, ncap);
		   last_end = (int) ovector[1];
	       }
	       offset = (int) ovector[1];
	       if (s[offset] == '\0' || !global) break;
	       if (ovector[1] == ovector[0]) {
		   /* advance by a char */
		   if (use_UTF8) {
		       int used, pos = 0;
		       while( (used = utf8clen(s[pos])) ) {
			   pos += used;
			   if (pos > offset) {
			       for (j = offset; j < pos; j++) *u++ = s[j];
			       offset = pos;
			       break;
			   }
		       }
		   } else
		       *u++ = s[offset++];
	       }
	       double needed = (double)(u-cbuf) + (double)(ns-offset)
	                       + (double)maxrep + 100.;
	       sub_buffer_expand(needed, &nns, &cbuf, &u);
#ifdef HAVE_PCRE2
	       eflag = PCRE2_NOTBOL;  /* probably not needed */
	       if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;
#else
	       eflag = PCRE_NOTBOL;  /* probably not needed */
#endif
	   }
	   R_pcre_exec_error(rc, i);
	   if (nmatch == 0)
	       SET_STRING_ELT(ans, i, markBytesOld(STRING_ELT(text, i),
		                                   useBytes, haveBytesInput));
	   else if (STRING_ELT(rep, 0) == NA_STRING)
	       SET_STRING_ELT(ans, i, NA_STRING);
	   else {
	       /* copy the tail */
	       double needed = (double)(u-cbuf) + (double)(ns-offset) + 1.0;
	       sub_buffer_expand(needed, &nns, &cbuf, &u);
	       for (j = offset ; s[j] ; j++) *u++ = s[j];
	       *u = '\0';
	       if (useBytes)
		   SET_STRING_ELT(ans, i, mkBytesNew(cbuf, haveBytesInput));
	       else if (use_UTF8)
		   SET_STRING_ELT(ans, i, mkCharCE(cbuf, CE_UTF8));
	       else
		   SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
	   }
	   R_Free(cbuf);
       } else if (!use_WC) {
	    int maxrep;
	    /* extended regexp in bytes */

	    ns = (int) strlen(s);
	    sub_buffer_size_init(replen, ns, count_subs(srep), global,
	                         &nns, &maxrep);
	    u = cbuf = R_Calloc(nns, char);
	    offset = 0; nmatch = 0; eflags = 0; last_end = -1;
	    while ((rc = tre_regexecb(&reg, s+offset, 10, regmatch, eflags))
		   == 0) {
		/* printf("%s, %d %d\n", &s[offset],
		   regmatch[0].rm_so, regmatch[0].rm_eo); */
		nmatch++;
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = s[offset+j];
		if (offset+regmatch[0].rm_eo > last_end) {
		    u = string_adj(u, s+offset, srep, regmatch);
		    last_end = offset+regmatch[0].rm_eo;
		}
		offset += regmatch[0].rm_eo;
		if (s[offset] == '\0' || !global) break;
		if (regmatch[0].rm_eo == regmatch[0].rm_so)
		    *u++ = s[offset++];
		double needed = (double)(u-cbuf) + (double)(ns-offset)
		                + (double)maxrep + 100.;
		sub_buffer_expand(needed, &nns, &cbuf, &u);
		eflags = REG_NOTBOL;
	    }
	    // AFAICS the only possible error report is REG_ESPACE
	    if (rc == REG_ESPACE)
		warning(_("Out-of-memory error in regexp matching for element %d"),
			(int) i + 1);

	    if (nmatch == 0)
		SET_STRING_ELT(ans, i, markBytesOld(STRING_ELT(text, i),
		                                    useBytes, haveBytesInput));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		/* copy the tail */
		double needed = (double)(u-cbuf) + (double)(ns-offset) + 1.0;
		sub_buffer_expand(needed, &nns, &cbuf, &u);
		for (j = offset ; s[j] ; j++) *u++ = s[j];
		*u = '\0';
		if (useBytes)
		    SET_STRING_ELT(ans, i, mkBytesNew(cbuf, haveBytesInput));
		else
		    SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
	    }
	    R_Free(cbuf);
	} else  {
	    /* extended regexp in wchar_t */
	    wchar_t *u, *cbuf;
	    int maxrep;
	    bool ascii_texti = IS_ASCII(STRING_ELT(text, i));

	    ns = (int) wcslen(ws);
	    sub_buffer_size_init(replen, ns, wcount_subs(wrep), global,
	                         &nns, &maxrep);
	    u = cbuf = R_Calloc(nns, wchar_t);
	    offset = 0; nmatch = 0; eflags = 0; last_end = -1;
	    while (tre_regwexec(&reg, ws+offset, 10, regmatch, eflags) == 0) {
		nmatch++;
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = ws[offset+j];
		if (offset+regmatch[0].rm_eo > last_end) {
		    u = wstring_adj(u, ws+offset, wrep, regmatch);
		    last_end = offset+regmatch[0].rm_eo;
		}
		offset += regmatch[0].rm_eo;
		if (ws[offset] == L'\0' || !global) break;
		if (regmatch[0].rm_eo == regmatch[0].rm_so)
		    *u++ = ws[offset++];
		double needed = (double)(u-cbuf) + (double)(ns-offset)
		                + (double)maxrep + 100.;
		wsub_buffer_expand(needed, &nns, &cbuf, &u);
		eflags = REG_NOTBOL;
	    }
	    if (nmatch == 0)
		SET_STRING_ELT(ans, i, markBytesOld(STRING_ELT(text, i),
		                                    useBytes, haveBytesInput));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		/* copy the tail */
		double needed = (double)(u-cbuf) + (double)(ns-offset) + 1.;
		wsub_buffer_expand(needed, &nns, &cbuf, &u);
		for (j = offset ; ws[j] ; j++) *u++ = ws[j];
		*u = L'\0';
		SET_STRING_ELT(ans, i,
		               mkCharWLenASCII(cbuf, (int)(u-cbuf),
		                               (ascii_patrep && ascii_texti)));
	    }
	    R_Free(cbuf);
	}
	vmaxset(vmax);
    }

    if (fixed_opt) ;
    else if (perl_opt) {
#ifdef HAVE_PCRE2
	pcre2_match_data_free(mdata);
	pcre2_code_free(re);
	pcre2_match_context_free(mcontext);
	if (tables)
	    /* new PCRE2 will have pcre2_maketables_free() */
	    free((void *)tables);
#else
	if (re_pe) pcre_free_study(re_pe);
	pcre_free(re_pcre);
	pcre_free((unsigned char *) tables);
#endif
    } else tre_regfree(&reg);

  exit_gsub: 
    SHALLOW_DUPLICATE_ATTRIB(ans, text);
    /* This also copied the class, if any */
    UNPROTECT(1);
    return ans;
}

static int getNc(const char *s, int st)
{
    int nc = 0;
    for(int i = 0; i < st; i += utf8clen(s[i]))
	nc++;
    return nc;
}

static SEXP gregexpr_BadStringAns(void);

static SEXP gregexpr_Regexc(const regex_t *reg, SEXP sstr, int useBytes, int use_WC,
		R_xlen_t i, SEXP itype, int *nwarn)
{
    int matchIndex = -1, j, st, foundAll = 0, foundAny = 0;
    size_t len, offset = 0;
    regmatch_t regmatch[10];
    SEXP ans, matchlen;         /* Return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* Buffers for storing multiple matches */
    int bufsize = 1024;         /* Starting size for buffers */
    int eflags = 0;
    const char *string = NULL;
    const wchar_t *ws = NULL;

    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));

    if (useBytes) {
	string = CHAR(sstr);
	len = strlen(string);
	use_WC = false; /* to be sure */
    } else if (!use_WC) {
	string = translateCharFP2(sstr);
	if (!string || (mbcslocale && !mbcsValid(string))) {
	    if ((*nwarn)++ < NWARN)
		warning(_("input string %lld is invalid in this locale"),
		        (long long)i+1);
	    return gregexpr_BadStringAns();
	}
	len = strlen(string);
    } else {
	ws = wtransChar2(sstr);
	if (!ws) {
	    if ((*nwarn)++ < NWARN)
		warning(_("input string %lld is invalid"), (long long)i+1);
	    return gregexpr_BadStringAns();
	}
	len = wcslen(ws);
    }

    while (!foundAll) {
        int rc = REG_OK; // in case offset>=len (e.g., when len==0)
	if ( offset < len &&
	     (rc = !use_WC ? tre_regexecb(reg, string+offset, 1, regmatch, eflags) :
	      tre_regwexec(reg, ws+offset, 1, regmatch, eflags))
	     == REG_OK) {
	    if ((matchIndex + 1) == bufsize) {
		/* Reallocate match buffers */
		int newbufsize = bufsize * 2;
		SEXP tmp;
		tmp = allocVector(INTSXP, 2 * bufsize);
		for (j = 0; j < bufsize; j++)
		    INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
		UNPROTECT(1);
		matchlenbuf = tmp;
		PROTECT(matchlenbuf);
		tmp = allocVector(INTSXP, 2 * bufsize);
		for (j = 0; j < bufsize; j++)
		    INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
		matchbuf = tmp;
		UNPROTECT(2);
		PROTECT(matchbuf);
		PROTECT(matchlenbuf);
		bufsize = newbufsize;
	    }
	    matchIndex++;
	    foundAny = 1;
	    st = regmatch[0].rm_so;
	    INTEGER(matchbuf)[matchIndex] = (int)(offset + st + 1); /* index from one */
	    INTEGER(matchlenbuf)[matchIndex] = regmatch[0].rm_eo - st;
	    if (INTEGER(matchlenbuf)[matchIndex] == 0)
		offset += st + 1;
	    else
		offset += regmatch[0].rm_eo;
	} else {
	    foundAll = 1;
	    if (!foundAny) {
		matchIndex++;
		INTEGER(matchbuf)[matchIndex] = -1;
		INTEGER(matchlenbuf)[matchIndex] = -1;
	    }
	}
	eflags = REG_NOTBOL;
	// AFAICS the only possible error report is REG_ESPACE
	if (rc == REG_ESPACE)
	    warning(_("Out-of-memory error in regexp matching for element %d"),
		    (int) i + 1);
    }
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    /* copy from buffers */
    for (j = 0; j <= matchIndex; j++) {
	INTEGER(ans)[j] = INTEGER(matchbuf)[j];
	INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
    }
    setAttrib(ans, install("match.length"), matchlen);
    if(useBytes) {
	setAttrib(ans, install("index.type"), itype);
	setAttrib(ans, install("useBytes"), R_TrueValue);
    }
    UNPROTECT(4);
    return ans;
}

static SEXP gregexpr_fixed(const char *pattern, const char *string,
	       bool useBytes, bool use_UTF8, SEXP itype)
{
    int patlen, matchIndex, st = 0, foundAll = 0, foundAny = 0, j,
	ansSize, nb = 0;
    size_t curpos = 0, slen;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    if (!useBytes && use_UTF8)
	patlen = (int) utf8towcs(NULL, pattern, 0);
    else if (!useBytes && mbcslocale)
	patlen = (int) mbstowcs(NULL, pattern, 0);
    else
	patlen = (int) strlen(pattern);
    slen = strlen(string);
    st = fgrep_one(pattern, string, useBytes, use_UTF8, &nb);
    matchIndex = -1;
    if (st < 0) {
	INTEGER(matchbuf)[0] = -1;
	INTEGER(matchlenbuf)[0] = -1;
    } else {
	foundAny = 1;
	matchIndex++;
	INTEGER(matchbuf)[matchIndex] = st + 1; /* index from one */
	INTEGER(matchlenbuf)[matchIndex] = patlen;
	while(!foundAll) {
	    string += nb;
	    if (patlen == 0)
		curpos += st + 1;
	    else
		curpos += st + patlen;
	    if (curpos >= slen)
		break;
	    st = fgrep_one(pattern, string, useBytes, use_UTF8, &nb);
	    if (st >= 0) {
		if ((matchIndex + 1) == bufsize) {
		    /* Reallocate match buffers */
		    int newbufsize = bufsize * 2;
		    SEXP tmp;
		    tmp = allocVector(INTSXP, 2 * bufsize);
		    for (j = 0; j < bufsize; j++)
			INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
		    UNPROTECT(1);
		    matchlenbuf = tmp;
		    PROTECT(matchlenbuf);
		    tmp = allocVector(INTSXP, 2 * bufsize);
		    for (j = 0; j < bufsize; j++)
			INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
		    matchbuf = tmp;
		    UNPROTECT(2);
		    PROTECT(matchbuf);
		    PROTECT(matchlenbuf);
		    bufsize = newbufsize;
		}
		matchIndex++;
		/* index from one */
		INTEGER(matchbuf)[matchIndex] = (int)(curpos + st + 1);
		INTEGER(matchlenbuf)[matchIndex] = patlen;
	    } else foundAll = 1;
	}
    }
    ansSize = foundAny ? (matchIndex + 1) : 1;
    PROTECT(ans = allocVector(INTSXP, ansSize));
    PROTECT(matchlen = allocVector(INTSXP, ansSize));
    /* copy from buffers */
    for (j = 0; j < ansSize; j++) {
	INTEGER(ans)[j] = INTEGER(matchbuf)[j];
	INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
    }
    setAttrib(ans, install("match.length"), matchlen);
    if(useBytes) {
	setAttrib(ans, install("index.type"), itype);
	setAttrib(ans, install("useBytes"), R_TrueValue);
    }
    UNPROTECT(4);
    return ans;
}

/* This function is used to convert a single ovector (match_start,
   match_end) pair (in bytes) to a pair of (match_start in 1-indexed
   unicode characters stored in mptr, match_length in number of
   unicode characters stored in lenptr)

   We have to do this once for the match and once for every group, so
   I generalized the method and call it twice from
   extract_match_and_groups to avoid repetitive code.

   Toby Dylan Hocking 2011-03-10
*/
#ifdef HAVE_PCRE2
static bool ovector_extract_start_length(bool use_UTF8,PCRE2_SIZE *ovector,
			     int *mptr,int *lenptr,const char *string)
#else
static bool ovector_extract_start_length(bool use_UTF8,int *ovector,
			     int *mptr,int *lenptr,const char *string)
#endif
{
    bool foundAll = false;
    /* FIXME: what if the match is unused? */
    int st = (int) ovector[0];
    *mptr = st + 1; /* index from one */
    *lenptr = (int) ovector[1] - st;
    if (use_UTF8) {
	/* Unfortunately these are in bytes */
	if (st > 0) {
	    *mptr = 1 + getNc(string, st);
	    if (*mptr <= 0) { /* an invalid string */
		/* FIXME: seems unreachable */
		*mptr = NA_INTEGER;
		foundAll = true; /* if we get here, we are done */
	    }
	}
	*lenptr = getNc(string + st, *lenptr);
	if (*lenptr < 0) {/* an invalid string */
	   /* FIXME: seems unreachable */
	    *lenptr = NA_INTEGER;
	    foundAll = true;
	}
    }
    return foundAll;
}

/* this function generalizes the parsing of the "ovector" from pcre
   which contains the match and group start and end bytes. it is
   organized as follows: match_start match_end group1_start group1_end
   group2_start group2_end ... we process these in regexpr and
   gregexpr, so I made this function to avoid duplicating code between
   the 2.

   Toby Dylan Hocking 2011-03-10 */
#ifdef HAVE_PCRE2
static bool extract_match_and_groups(bool use_UTF8, PCRE2_SIZE *ovector, int capture_count,
			 int *mptr, int *lenptr, int *cptr, int *clenptr,
			 const char *string, int capture_stride)
#else
static bool extract_match_and_groups(bool use_UTF8, int *ovector, int capture_count,
			 int *mptr, int *lenptr, int *cptr, int *clenptr,
			 const char *string, int capture_stride)
#endif
{
    bool foundAll =
	ovector_extract_start_length(use_UTF8, ovector, mptr, lenptr, string);
    /* also extract capture locations */
    for(int i = 0; i < capture_count; i++) {
	int ind = capture_stride*i;
	ovector_extract_start_length(use_UTF8, ovector+2*(i+1),
				     cptr+ind, clenptr+ind, string);
    }
    return foundAll;
}

#ifdef HAVE_PCRE2
static SEXP R_pcre2_gregexpr(const char *pattern, const char *string,
	         pcre2_code *re, bool useBytes, bool use_UTF8,
	         pcre2_match_data *mdata, pcre2_match_context *mcontext,
                 int capture_count, SEXP capture_names, R_xlen_t n, SEXP itype)
#else
static SEXP R_pcre_gregexpr(const char *pattern, const char *string,
	        pcre *re_pcre, pcre_extra *re_pe,
	        bool useBytes, bool use_UTF8,
	        int *ovector, int ovector_size,
	        int capture_count, SEXP capture_names, R_xlen_t n,
	        SEXP itype)
#endif
{
    bool foundAll = false, foundAny = false;
    int matchIndex = -1, start = 0;
    SEXP ans, matchlen;         /* return vect and its attribute */
    GCStackRoot<> capturebuf, capturelenbuf;
    GCStackRoot<> matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    int slen = (int) strlen(string);

    capturebuf = allocVector(INTSXP, bufsize*capture_count);
    capturelenbuf = allocVector(INTSXP, bufsize*capture_count);
    matchbuf = allocVector(INTSXP, bufsize);
    matchlenbuf = allocVector(INTSXP, bufsize);

    while (!foundAll) {
#ifdef HAVE_PCRE2
	uint32_t eflag = 0;
	if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;
	int rc = pcre2_match(re, (PCRE2_SPTR) string, slen, start, eflag,
		             mdata, mcontext);
	PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(mdata);
#else
	int rc = pcre_exec(re_pcre, re_pe, string, slen, start, 0, ovector,
		       ovector_size);
#endif
	R_pcre_exec_error(rc, n);
	if (rc >= 0) {
	    if ((matchIndex + 1) == bufsize) {
		/* Reallocate match buffers */
		int newbufsize = bufsize * 2;
		SEXP tmp;
		tmp = allocVector(INTSXP, newbufsize);
		for (int j = 0; j < bufsize; j++) /* or use memcpy */
		    INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
		matchlenbuf = tmp;
		tmp = allocVector(INTSXP, newbufsize);
		for (int j = 0; j < bufsize; j++)  /* or use memcpy */
		    INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
		matchbuf = tmp;
		if (capture_count) {
		    tmp = allocVector(INTSXP, newbufsize*capture_count);
		    for(int j = 0; j < bufsize; j++)
			for(int i = 0; i < capture_count; i++)
			    INTEGER(tmp)[j + newbufsize*i] =
				INTEGER(capturebuf)[j + bufsize*i];
		    capturebuf = tmp;
		    tmp = allocVector(INTSXP, newbufsize*capture_count);
		    for(int j = 0; j < bufsize; j++)
			for(int i = 0; i < capture_count; i++)
			    INTEGER(tmp)[j + newbufsize*i] =
				INTEGER(capturelenbuf)[j + bufsize*i];
		    capturelenbuf =  tmp;
		}
		bufsize = newbufsize;
	    }
	    matchIndex++;
	    foundAny = true;
	    foundAll =
		extract_match_and_groups(use_UTF8, ovector, capture_count,
					 INTEGER(matchbuf) + matchIndex,
					 INTEGER(matchlenbuf) + matchIndex,
					 INTEGER(capturebuf) + matchIndex,
					 INTEGER(capturelenbuf) + matchIndex,
					 string, bufsize);
	    /* we need to advance 'start' in bytes */
	    if (ovector[1] - ovector[0] == 0)
		start = (int) ovector[0] + 1;
	    else
		start = (int) ovector[1];
	    if (start >= slen) foundAll = TRUE;
	} else {
	    foundAll = true;
	    if (!foundAny) matchIndex = 0;
	}
    }
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    /* Protect in case install("match.length") allocates */
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    setAttrib(ans, install("match.length"), matchlen);
    if(useBytes) {
	setAttrib(ans, install("index.type"), itype);
	setAttrib(ans, install("useBytes"), R_TrueValue);
    }
    UNPROTECT(1);
    if (foundAny) {
	for (int j = 0; j <= matchIndex; j++) {
	    INTEGER(ans)[j] = INTEGER(matchbuf)[j];
	    INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
	}
    } else
	INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;

    if (capture_count) {
	GCStackRoot<> capture, capturelen, dmn;
	capture = allocMatrix(INTSXP, matchIndex+1, capture_count);
	capturelen = allocMatrix(INTSXP, matchIndex+1, capture_count);
	dmn = allocVector(VECSXP, 2);
	SET_VECTOR_ELT(dmn, 1, capture_names);
	setAttrib(capture, R_DimNamesSymbol, dmn);
	setAttrib(capturelen, R_DimNamesSymbol, dmn);
	if (foundAny) {
	    for (int j = 0; j <= matchIndex; j++)
		for(int i = 0; i < capture_count; i++) {
		    int return_index = j + (matchIndex+1) * i;
		    int buffer_index = j + bufsize * i;
		    INTEGER(capture)[return_index] =
			INTEGER(capturebuf)[buffer_index];
		    INTEGER(capturelen)[return_index] =
			INTEGER(capturelenbuf)[buffer_index];
		}
	} else
	    for(int i = 0; i < capture_count; i++)
		INTEGER(capture)[i] = INTEGER(capturelen)[i] = -1;
	setAttrib(ans, install("capture.start"), capture);
	setAttrib(ans, install("capture.length"), capturelen);
	setAttrib(ans, install("capture.names"), capture_names);
    }
    UNPROTECT(1); /* ans */
    return ans;
}

static SEXP gregexpr_NAInputAns(void)
{
    SEXP ans, matchlen;
    PROTECT(ans = allocVector(INTSXP, 1));
    PROTECT(matchlen = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = INTEGER(matchlen)[0] = R_NaInt;
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}

static SEXP gregexpr_BadStringAns(void)
{
    SEXP ans, matchlen;
    PROTECT(ans = allocVector(INTSXP, 1));
    PROTECT(matchlen = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, itype;
    regex_t reg;
    regmatch_t regmatch[10];
    R_xlen_t i, n;
    int rc;
    const char *spat = NULL; /* -Wall */
    const wchar_t *wpat = NULL;
    const char *s = NULL;
    const wchar_t *ws = NULL;
    const unsigned char *tables = NULL /* -Wall */;
#ifdef HAVE_PCRE2
    pcre2_code *re = NULL;
    pcre2_match_context *mcontext = NULL;
    pcre2_match_data *mdata = NULL;
    PCRE2_SIZE *ovector = NULL;
    uint32_t name_count, name_entry_size, capture_count;
#else
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    int *ovector = NULL, name_count, name_entry_size, capture_count;
#endif
    bool use_UTF8 = false, use_WC = false;
    char *name_table;
    const void *vmax;
    int info_code, ovector_size = 0; /* -Wall */
    SEXP capture_names = R_NilValue;
    int nwarn = 0;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    bool igcase_opt = asBool2(CAR(args), call); args = CDR(args);
    bool perl_opt = asBool2(CAR(args), call); args = CDR(args);
    bool fixed_opt = asBool2(CAR(args), call); args = CDR(args);
    bool useBytes = asBool2(CAR(args), call); args = CDR(args);
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    /* Note that excluding NAs differs from grep/sub */
    if (!isString(pat) || LENGTH(pat) < 1 || STRING_ELT(pat, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "pattern");
    if (LENGTH(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(text);
    if (!useBytes) 
	useBytes = have_bytes(pat, 1) || have_bytes(text, n);
    PROTECT(itype = ScalarString(mkChar(useBytes ? "bytes" : "chars")));

    if (!useBytes) 
	useBytes = only_ascii(pat, 1) && only_ascii(text, n);
    if (!useBytes) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales,
	   and as from 2.11.0 in TRE too. */
	/* if we have non-ASCII text in a DBCS locale, we need to use wchar in TRE */
	if (!fixed_opt && mbcslocale) use_UTF8 = true;
	if (!use_UTF8)
	    use_UTF8 = (have_utf8(pat, 1) || have_utf8(text, n));
	if (!use_UTF8 && !latin1locale)
	    use_UTF8 = (have_latin1(pat, 1) || have_latin1(text, n));
    }

    if (!fixed_opt && !perl_opt) {
	use_WC = use_UTF8; use_UTF8 = false;
    }

    if (useBytes)
	spat = CHAR(STRING_ELT(pat, 0));
    else if (use_WC) {
	wpat = wtransChar2(STRING_ELT(pat, 0));
	if (!wpat) error("%s", _("regular expression is invalid"));
    } else if (use_UTF8) {
	spat = trCharUTF82(STRING_ELT(pat, 0));
	if (!spat || !utf8Valid(spat)) error("%s", _("regular expression is invalid UTF-8"));
    } else {
	spat = translateCharFP2(STRING_ELT(pat, 0));
	if (!spat || (mbcslocale && !mbcsValid(spat)))
	    error("%s", _("regular expression is invalid in this locale"));
    }

    if (fixed_opt) ;
    else if (perl_opt) {
#ifdef HAVE_PCRE2
	R_pcre2_prepare(spat, text, use_UTF8, igcase_opt, &tables, &re,
	                &mcontext);

	/* also extract info for named groups */
	pcre2_pattern_info(re, PCRE2_INFO_NAMECOUNT, &name_count);
	pcre2_pattern_info(re, PCRE2_INFO_NAMEENTRYSIZE, &name_entry_size);
	pcre2_pattern_info(re, PCRE2_INFO_NAMETABLE, (PCRE2_SPTR**) &name_table);
	info_code =
	    pcre2_pattern_info(re, PCRE2_INFO_CAPTURECOUNT, &capture_count);
	if(info_code < 0)
	    /* this should not happen, but  */
	    error(_("'pcre2_patterninfo' returned '%d' "), info_code);
	ovector_size = (int) capture_count + 1;
	/* PCRE2 also has pcre2_match_data_create_from_pattern() */
	mdata = pcre2_match_data_create(ovector_size, NULL);
	/* ovector_size not used below */
#else
	R_pcre_prepare(spat, text, use_UTF8, igcase_opt, false, &tables,
	               &re_pcre, &re_pe);

	/* also extract info for named groups */
	pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMECOUNT, &name_count);
	pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMEENTRYSIZE, &name_entry_size);
	pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_NAMETABLE, &name_table);
	info_code =
	    pcre_fullinfo(re_pcre, re_pe, PCRE_INFO_CAPTURECOUNT,
			  &capture_count);
	if(info_code < 0)
	    error(_("'pcre_fullinfo' returned '%d' "), info_code);
	ovector_size = (capture_count + 1) * 3;
	ovector = (int *) malloc(ovector_size*sizeof(int));
#endif
	SEXP thisname;
	PROTECT(capture_names = allocVector(STRSXP, (int) capture_count));
	for(i = 0; i < name_count; i++) {
	    char *entry = name_table + name_entry_size * i;
	    PROTECT(thisname = mkChar(entry + 2));
	    int capture_num = ((unsigned char)entry[0]<<8)
	                     + (unsigned char)entry[1] - 1;
	    SET_STRING_ELT(capture_names, capture_num, thisname);
	    UNPROTECT(1);
	}
    } else {
	int cflags = REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC)
	    rc = tre_regcompb(&reg, spat, cflags);
	else
	    rc = tre_regwcomp(&reg, wpat, cflags);
	if (rc) reg_report(rc, &reg, spat);
    }

    if (PRIMVAL(op) == 0) { /* regexpr */
	SEXP matchlen, capture_start, capturelen;
	int *is, *il;
	PROTECT(ans = allocVector(INTSXP, n));
	/* Protect in case install("match.length") allocates */
	PROTECT(matchlen = allocVector(INTSXP, n));
	setAttrib(ans, install("match.length"), matchlen);
	if(useBytes) {
	    setAttrib(ans, install("index.type"), itype);
	    setAttrib(ans, install("useBytes"), R_TrueValue);
	}
	UNPROTECT(1);
	if (perl_opt && capture_count) {
	    if (n > INT_MAX) error("%s", _("too long a vector"));
	    int nn = (int) n;
	    SEXP dmn;
	    PROTECT(dmn = allocVector(VECSXP, 2));
	    SET_VECTOR_ELT(dmn, 1, capture_names);
	    PROTECT(capture_start = allocMatrix(INTSXP, nn, (int) capture_count));
	    setAttrib(capture_start, R_DimNamesSymbol, dmn);
	    setAttrib(ans, install("capture.start"), capture_start);
	    PROTECT(capturelen = allocMatrix(INTSXP, nn, (int) capture_count));
	    setAttrib(capturelen, R_DimNamesSymbol, dmn);
	    setAttrib(ans, install("capture.length"), capturelen);
	    setAttrib(ans, install("capture.names"), capture_names);
	    UNPROTECT(3);
	    is = INTEGER(capture_start);
	    il = INTEGER(capturelen);
	    // initiialization needed for NA inputs: PR#16484
	    for (i = 0 ; i < n * (int) capture_count ; i++)
		is[i] = il[i] = NA_INTEGER;
	} else is = il = INTEGER(ans); // not actually used, but is + i has to be legal
	vmax = vmaxget();
	for (i = 0 ; i < n ; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    if (STRING_ELT(text, i) == NA_STRING) {
		INTEGER(matchlen)[i] = INTEGER(ans)[i] = NA_INTEGER;
	    } else {
		if (useBytes)
		    s = CHAR(STRING_ELT(text, i));
		else if (use_WC) {
		    ws = wtransChar2(STRING_ELT(text, i));
		    if (!ws) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid"),
			            (long long)i+1);
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			continue;
		    }
		} else if (use_UTF8) {
		    s = trCharUTF82(STRING_ELT(text, i));
		    if (!s || !utf8Valid(s)) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid UTF-8"),
			            (long long)i+1);
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			continue;
		    }
		} else {
		    s = translateCharFP2(STRING_ELT(text, i));
		    if (!s || (mbcslocale && !mbcsValid(s))) {
			if(nwarn++ < NWARN)
			    warning(_("input string %lld is invalid in this locale"),
			            (long long)i+1);
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			continue;
		    }
		}
		if (fixed_opt) {
		    int st = fgrep_one(spat, s, useBytes, use_UTF8, NULL);
		    INTEGER(ans)[i] = (st > -1)?(st+1):-1;
		    if (!useBytes && use_UTF8) {
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    (int) utf8towcs(NULL, spat, 0):-1;
		    } else if (!useBytes && mbcslocale) {
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    (int) mbstowcs(NULL, spat, 0):-1;
		    } else
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    (int) strlen(spat):-1;
		} else if (perl_opt) {
		    int rc;
#ifdef HAVE_PCRE2
		    uint32_t eflag = 0;
		    if (use_UTF8) eflag |= PCRE2_NO_UTF_CHECK;
		    rc = pcre2_match(re, (PCRE2_SPTR) s, PCRE2_ZERO_TERMINATED,
		                     0, eflag, mdata, mcontext);
		    ovector = pcre2_get_ovector_pointer(mdata);
#else
		    rc = pcre_exec(re_pcre, re_pe, s, (int) strlen(s), 0, 0,
				   ovector, ovector_size);
#endif
		    R_pcre_exec_error(rc, i);
		    if (rc >= 0) {
			extract_match_and_groups(use_UTF8, ovector,
						 (int) capture_count,
						 // don't use this for large i
						 INTEGER(ans) + i,
						 INTEGER(matchlen) + i,
						 is + i, il + i,
						 s, (int) n);
		    } else {
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			for(int cn = 0; cn < (int) capture_count; cn++) {
			    R_xlen_t ind = i + cn*n;
			    is[ind] = il[ind] = -1;
			}
		    }
		} else {
		    int rc;
		    if (!use_WC)
			rc = tre_regexecb(&reg, s, 1, regmatch, 0);
		    else
			rc = tre_regwexec(&reg, ws, 1, regmatch, 0);
		    if (rc == 0) {
			int st = regmatch[0].rm_so;
			INTEGER(ans)[i] = st + 1; /* index from one */
			INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
		    } else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
		    // AFAICS the only possible error report is REG_ESPACE
		    if (rc == REG_ESPACE)
			warning(_("Out-of-memory error in regexp matching for element %d"),
				(int) i + 1);
		}
	    }
	    vmaxset(vmax);
	}
    } else {
	SEXP elt;
	PROTECT(ans = allocVector(VECSXP, n));
	vmax = vmaxget();
	for (i = 0 ; i < n ; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    if (STRING_ELT(text, i) == NA_STRING) {
		elt = gregexpr_NAInputAns();
	    } else {
		if (fixed_opt || perl_opt) {
		    if (useBytes)
			s = CHAR(STRING_ELT(text, i));
		    else if (use_UTF8) {
			s = trCharUTF82(STRING_ELT(text, i));
			if (!s || !utf8Valid(s)) {
			    if (nwarn++ < NWARN)
				warning(_("input string %lld is invalid UTF-8"),
				        (long long)i+1);
			    s = NULL;
			}
		    } else {
			s = translateCharFP2(STRING_ELT(text, i));
			if (!s || (mbcslocale && !mbcsValid(s))) {
			    if (nwarn++ < NWARN)
				warning(_("input string %lld is invalid in this locale"),
				        (long long)i+1);
			    s = NULL;
			}
		    }
		    if (!useBytes && !s) {
			elt = gregexpr_BadStringAns();
		    } else {
			if (fixed_opt)
			    elt = gregexpr_fixed(spat, s, useBytes, use_UTF8,
				                 itype);
			else
#ifdef HAVE_PCRE2
			    elt = R_pcre2_gregexpr(spat, s, re, useBytes,
			                           use_UTF8, mdata, mcontext,
			                           (int) capture_count,
						   capture_names, i, itype);
#else
			    elt = R_pcre_gregexpr(spat, s, re_pcre, re_pe,
						  useBytes, use_UTF8, ovector,
						  ovector_size,
			                          (int) capture_count,
						  capture_names, i, itype);
#endif
		    }
		} else
		    elt = gregexpr_Regexc(&reg, STRING_ELT(text, i),
					  useBytes, use_WC, i, itype, &nwarn);
	    }
	    SET_VECTOR_ELT(ans, i, elt);
	    vmaxset(vmax);
	}
    }

    if (fixed_opt) ;
    else if (perl_opt) {
#ifdef HAVE_PCRE2
	pcre2_match_data_free(mdata);
	pcre2_code_free(re);
	pcre2_match_context_free(mcontext);
	if (tables)
	    /* new PCRE2 will have pcre2_maketables_free() */
	    free((void *)tables);
#else
	if (re_pe) pcre_free_study(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
	free(ovector);
#endif
	UNPROTECT(1);
    } else
	tre_regfree(&reg);

    UNPROTECT(2);
    return ans;
}

// .Internal(regexec(pattern, text, ignore.case, fixed, useBytes)) :
attribute_hidden SEXP do_regexec(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchpos, matchlen, itype;
    bool opt_icase, opt_fixed, useBytes;

    bool use_WC = false;
    const char *s, *t;
    const wchar_t *ws, *wt;
    const void *vmax = NULL;

    regex_t reg;
    size_t nmatch;
    regmatch_t *pmatch;
    R_xlen_t i, n;
    int so;
    int rc, cflags = REG_EXTENDED;

    checkArity(op, args);

    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    opt_icase = asLogicalNAFalse(CAR(args)); args = CDR(args);
    opt_fixed = asLogicalNAFalse(CAR(args)); args = CDR(args);
    useBytes = asLogicalNAFalse(CAR(args));

    if(opt_fixed && opt_icase) {
	warning(_("argument '%s' will be ignored"),
		"ignore.case = TRUE");
	opt_icase = 0;
    }
    if(opt_fixed) cflags |= REG_LITERAL;
    if(opt_icase) cflags |= REG_ICASE;

    if(!isString(pat) ||
       (LENGTH(pat) < 1) ||
       (STRING_ELT(pat, 0) == NA_STRING))
	error(_("invalid '%s' argument"), "pattern");
    if(LENGTH(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if(!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(text);

    if(!useBytes) 
	useBytes = have_bytes(pat, 1) || have_bytes(text, n);
    PROTECT(itype = ScalarString(mkChar(useBytes ? "bytes" : "chars")));

    if(!useBytes) 
	useBytes = only_ascii(pat, 1) && only_ascii(text, n);

    if(!useBytes) {
	if (!opt_fixed && mbcslocale) use_WC = true;
	if (!use_WC)
	    use_WC = (have_utf8(pat, 1) || have_utf8(text, n));
	if (!use_WC && !latin1locale)
	    use_WC = (have_latin1(pat, 1) || have_latin1(text, n));
    }

    if(useBytes)
	rc = tre_regcompb(&reg, CHAR(STRING_ELT(pat, 0)), cflags);
    else if (use_WC) {
	ws = wtransChar2(STRING_ELT(pat, 0));
	if(!ws)
	    error("%s", _("regular expression is invalid"));
	rc = tre_regwcomp(&reg, ws, cflags);
    } else {
	s = translateCharFP2(STRING_ELT(pat, 0));
	if(!s || (mbcslocale && !mbcsValid(s)))
	    error("%s", _("regular expression is invalid in this locale"));
	rc = tre_regcomp(&reg, s, cflags);
    }
    if(rc) {
	char errbuf[1001];
	tre_regerror(rc, &reg, errbuf, 1001);
	error(_("regcomp error: '%s'"), errbuf);
    }

    nmatch = reg.re_nsub + 1;

    pmatch = (regmatch_t *) malloc(nmatch * sizeof(regmatch_t));

    PROTECT(ans = allocVector(VECSXP, n));

    for(i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if(STRING_ELT(text, i) == NA_STRING) {
	    PROTECT(matchpos = ScalarInteger(NA_INTEGER));
	    SEXP s_match_length = install("match.length");
	    setAttrib(matchpos, s_match_length ,
		      ScalarInteger(NA_INTEGER));
	    SET_VECTOR_ELT(ans, i, matchpos);
	    UNPROTECT(1);
	} else {
	    vmax = vmaxget();
	    if(useBytes)
		rc = tre_regexecb(&reg, CHAR(STRING_ELT(text, i)),
				  nmatch, pmatch, 0);
	    else if(use_WC) {
		wt = wtransChar2(STRING_ELT(text, i));
		if (!wt)
		    error(_("input string %lld is invalid in this locale"),
		          (long long)i + 1);
		rc = tre_regwexec(&reg, wt, nmatch, pmatch, 0);
		vmaxset(vmax);
	    } else {
		t = translateCharFP2(STRING_ELT(text, i));
		if (!t || (mbcslocale && !mbcsValid(t)))
		    error(_("input string %lld is invalid in this locale"),
			  (long long)i + 1);
		rc = tre_regexec(&reg, t,
				 nmatch, pmatch, 0);
		vmaxset(vmax);
	    }
	    if(rc == REG_OK) {
		PROTECT(matchpos = allocVector(INTSXP, nmatch));
		PROTECT(matchlen = allocVector(INTSXP, nmatch));
		for(size_t j = 0; j < nmatch; j++) {
		    so = pmatch[j].rm_so;
		    INTEGER(matchpos)[j] = so + 1;
		    INTEGER(matchlen)[j] = pmatch[j].rm_eo - so;
		}
		setAttrib(matchpos, install("match.length"), matchlen);
		if(useBytes) {
		    setAttrib(matchpos, install("index.type"), itype);
		    setAttrib(matchpos, install("useBytes"), R_TrueValue);
		}
		SET_VECTOR_ELT(ans, i, matchpos);
		UNPROTECT(2);
	    } else {
		/* No match (or could there be an error?). */
		/* Alternatively, could return nmatch -1 values.
		 */
		// AFAICS the only possible error report is REG_ESPACE
		if (rc == REG_ESPACE)
		    warning(_("Out-of-memory error in regexp matching for element %d"),
			    (int) i + 1);
		PROTECT(matchpos = ScalarInteger(-1));
		PROTECT(matchlen = ScalarInteger(-1));
		setAttrib(matchpos, install("match.length"), matchlen);
		if(useBytes) {
		    setAttrib(matchpos, install("index.type"), itype);
		    setAttrib(matchpos, install("useBytes"), R_TrueValue);
		}
		SET_VECTOR_ELT(ans, i, matchpos);
		UNPROTECT(2);
	    }
	}
    }

    free(pmatch);

    tre_regfree(&reg);

    UNPROTECT(2);

    return ans;
}

/* pcre_config was added in PCRE 4.0, with PCRE_CONFIG_UTF8 .
   PCRE_CONFIG_UNICODE_PROPERTIES had been added by 8.10,
   the earliest version we allowed when coding this.
 */
#ifdef HAVE_PCRE2
attribute_hidden SEXP do_pcre_config(SEXP call, SEXP op, SEXP args, SEXP env)
{
    uint32_t res;

    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(LGLSXP, 4));
    int *lans = LOGICAL(ans);
    SEXP nm = allocVector(STRSXP, 4);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("UTF-8"));
    pcre2_config(PCRE2_CONFIG_UNICODE, &res);
    lans[0] = res;
    SET_STRING_ELT(nm, 1, mkChar("Unicode properties"));
    lans[1] = res;
    SET_STRING_ELT(nm, 2, mkChar("JIT"));
    pcre2_config(PCRE2_CONFIG_JIT, &res);
    lans[2] = res;
    pcre2_config(PCRE2_CONFIG_STACKRECURSE, &res);
    lans[3] = res;
    SET_STRING_ELT(nm, 3, mkChar("stack"));
    UNPROTECT(1);
    return ans;
}
#else
attribute_hidden SEXP do_pcre_config(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int res;

    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(LGLSXP, 4));
    int *lans = LOGICAL(ans);
    SEXP nm = allocVector(STRSXP, 4);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("UTF-8"));
    pcre_config(PCRE_CONFIG_UTF8, &res); lans[0] = res;
    SET_STRING_ELT(nm, 1, mkChar("Unicode properties"));
    pcre_config(PCRE_CONFIG_UNICODE_PROPERTIES, &res); lans[1] = res;
    SET_STRING_ELT(nm, 2, mkChar("JIT"));
#ifdef PCRE_CONFIG_JIT
    // added (and JIT support) in 8.20.
    pcre_config(PCRE_CONFIG_JIT, &res);
#else
    res = false;
#endif
    lans[2] = res;
    pcre_config(PCRE_CONFIG_STACKRECURSE, &res); lans[3] = res;
    SET_STRING_ELT(nm, 3, mkChar("stack"));
    UNPROTECT(1);
    return ans;
}
#endif
