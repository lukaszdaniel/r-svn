/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2025  The R Core Team
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
 */


/* =========
 * Printing:
 * =========
 *
 * All printing in R is done via the functions Rprintf and REprintf
 * or their (v) versions Rvprintf and REvprintf.
 * These routines work exactly like (v)printf(3).  Rprintf writes to
 * ``standard output''.	 It is redirected by the sink() function,
 * and is suitable for ordinary output.	 REprintf writes to
 * ``standard error'' and is useful for error messages and warnings.
 * It is not redirected by sink().
 *
 *  See ./format.c  for the  format_FOO_  functions which provide
 *	~~~~~~~~~~  the	 length, width, etc.. that are used here.
 *  See ./print.c  for do_printdefault, do_prmatrix, etc.
 *
 *
 * Here, the following UTILITIES are provided:
 *
 * The utilities EncodeLogical, EncodeInteger, EncodeReal
 * and EncodeString can be used to convert R objects to a form suitable
 * for printing.  These print the values passed in a formatted form
 * or, in the case of NA values, an NA indicator.  EncodeString takes
 * care of printing all the standard ANSI escapes \a, \t \n etc.
 * so that these appear in their backslash form in the string.	There
 * is also a routine called Rstrlen which computes the length of the
 * string in its escaped rather than literal form.
 *
 * Finally there is a routine called EncodeElement which will encode
 * a single R-vector element.  This is used in deparse and write.table.
 */

/* if ESC_BARE_QUOTE is defined, " in an unquoted string is replaced
   by \".  " in a quoted string is always replaced by \". */

/** @file printutils.cpp
 *
 * General remarks on Printing and the Encode.. utils.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Minmax.h>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/String.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Rmath.h>
#include <Print.h>
#include <R_ext/RS.h>
#include <Rconnections.h>

#include "RBufferUtils.h"

/* At times we want to convert marked UTF-8 strings to wchar_t*. We
 * can use our facilities to do so in a UTF-8 locale or system
 * facilities if the platform tells us that wchar_t is UCS-4 or we
 * know that about the platform.
 * Add __OpenBSD__ and  __NetBSD__ ?
 */
#if !defined(__STDC_ISO_10646__) && (defined(__APPLE__) || defined(__FreeBSD__) || defined(__sun))
/* This may not be 100% true (see the comment in rlocale.h),
   but it seems true in normal locales */
# define __STDC_ISO_10646__
#endif

#ifdef Win32
#include <trioremap.h>
#endif

using namespace std;
using namespace R;

#define BUFSIZE 8192  /* used by Rprintf etc */

attribute_hidden
R_size_t R::R_Decode2Long(char *p, int *ierr)
{
    R_size_t v = strtol(p, &p, 10);
    *ierr = 0;
    if(p[0] == '\0') return v;
    /* else look for letter-code ending : */
    if(R_Verbose)
	REprintf("R_Decode2Long(): v=%ld\n", (long)v);
    // NOTE: currently, positive *ierr are not differentiated in the callers:
    if(p[0] == 'G') {
	if((Giga * (double)v) > (double) R_SIZE_T_MAX) { *ierr = 4; return v; }
	return (R_size_t) Giga * v;
    }
    else if(p[0] == 'M') {
	if((Mega * (double)v) > (double) R_SIZE_T_MAX) { *ierr = 1; return v; }
	return (R_size_t) Mega * v;
    }
    else if(p[0] == 'K') {
	if((1024 * (double)v) > (double) R_SIZE_T_MAX) { *ierr = 2; return v; }
	return (1024*v);
    }
    else if(p[0] == 'k') {
	if((1000 * (double)v) > (double) R_SIZE_T_MAX) { *ierr = 3; return v; }
	return (1000*v);
    }
    else {
	*ierr = -1;
	return v;
    }
}

/* There is no documented (or enforced) limit on 'w' here,
   so use snprintf */
#define NB 1000
const char *Rf_EncodeLogical(int x, int w)
{
    /* fast path when 'w' fits exactly */
    if(x == NA_LOGICAL) {
	if(w == R_print.na_width) return CHAR(R_print.na_string);
    } else if(x) {
	if(w == 4) return "TRUE";
    } else
	if(w == 5) return "FALSE";
    /* general case */
    static char buff[NB];
    if(x == NA_LOGICAL) snprintf(buff, NB, "%*s", min(w, (NB-1)), CHAR(R_print.na_string));
    else if(x) snprintf(buff, NB, "%*s", min(w, (NB-1)), "TRUE");
    else snprintf(buff, NB, "%*s", min(w, (NB-1)), "FALSE");
    buff[NB-1] = '\0';
    return buff;
}

const char *Rf_EncodeInteger(int x, int w)
{
    static char buff[NB];
    if(x == NA_INTEGER) snprintf(buff, NB, "%*s", min(w, (NB-1)), CHAR(R_print.na_string));
    else snprintf(buff, NB, "%*d", min(w, (NB-1)), x);
    buff[NB-1] = '\0';
    return buff;
}

attribute_hidden
const char *R::EncodeRaw(Rbyte x, const char * prefix)
{
    static char buff[10];
    snprintf(buff, 10, "%s%02x", prefix, x);
    return buff;
}

attribute_hidden
const char *R::EncodeEnvironment(SEXP x)
{
    CXXR::RAllocStack::Scope rscope;
    static char ch[1000];
    if (x == R_GlobalEnv)
	snprintf(ch, 1000,  "<environment: R_GlobalEnv>");
    else if (x == R_BaseEnv)
	snprintf(ch, 1000, "<environment: base>");
    else if (x == R_EmptyEnv)
	snprintf(ch, 1000, "<environment: R_EmptyEnv>");
    else if (R_IsPackageEnv(x))
	snprintf(ch, 1000, "<environment: %s>",
		translateChar(STRING_ELT(R_PackageEnvName(x), 0)));
    else if (R_IsNamespaceEnv(x))
	snprintf(ch, 1000, "<environment: namespace:%s>",
		translateChar(STRING_ELT(R_NamespaceEnvSpec(x), 0)));
    else snprintf(ch, 1000, "<environment: %p>", (void *)x);

    return ch;
}

attribute_hidden
const char *R::EncodeExtptr(SEXP x)
{
    static char buf[1000];
    snprintf(buf, 1000, "<pointer: %p>", R_ExternalPtrAddr(x));
    return buf;
}

#define strwidth(x) R::Rstrwid(x, (int) strlen(x), CE_NATIVE, 0)

attribute_hidden const char *Rf_EncodeReal(double x, int w, int d, int e, char cdec)
{
    char dec[2];
    dec[0] = cdec; dec[1] = '\0';
    return EncodeReal0(x, w, d, e, dec);
}

const char *Rf_EncodeReal0(double x, int w, int d, int e, const char *dec)
{
    static char buff[NB], buff2[2*NB];
    char fmt[20], *out = buff;

    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x)) snprintf(buff, NB, "%*s", min(w, (NB-1)), CHAR(R_print.na_string));
	else if(ISNAN(x)) snprintf(buff, NB, "%*s", min(w, (NB-1)), "NaN");
	else if(x > 0) snprintf(buff, NB, "%*s", min(w, (NB-1)), "Inf");
	else snprintf(buff, NB, "%*s", min(w, (NB-1)), "-Inf");
    }
    else if (e) {
	if(d) { // '#' flag
	    snprintf(fmt, 20, "%%#%d.%de", min(w, (NB-1)), d);
	}
	else {
	    snprintf(fmt, 20, "%%%d.%de", min(w, (NB-1)), d);
	}
	snprintf(buff, NB, fmt, x);
    }
    else { /* e = 0 */
	snprintf(fmt, 20, "%%%d.%df", min(w, (NB-1)), d);
	snprintf(buff, NB, fmt, x);
    }
    buff[NB-1] = '\0';

    if (!streql(dec, ".")) { /* replace "." by dec */
	int len = strwidth(dec); /* 3·14 must work */
	if (len != 1) warning("%s", (len > 1) ?
	    _("the decimal mark is more than one character wide; this will become an error") :
	    _("the decimal mark is less than one character wide; this will become an error"));

	char *p, *q;
	for(p = buff, q = buff2; *p; p++) {
	    if(*p == '.') for(const char *r = dec; *r; r++) *q++ = *r;
	    else *q++ = *p;
	}
	*q = '\0';
	out = buff2;
    }

    return out;
}

// A copy of EncodeReal0() -- additionally dropping trailing zeros:
static const char *EncodeRealDrop0(double x, int w, int d, int e, const char *dec)
{
    static char buff[NB], buff2[2*NB];
    char fmt[20], *out = buff;

    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x))       snprintf(buff, NB, "%*s", min(w, (NB-1)), CHAR(R_print.na_string));
	else if(ISNAN(x)) snprintf(buff, NB, "%*s", min(w, (NB-1)), "NaN");
	else if(x > 0)    snprintf(buff, NB, "%*s", min(w, (NB-1)), "Inf");
	else              snprintf(buff, NB, "%*s", min(w, (NB-1)), "-Inf");
    }
    else if (e) {
	if(d) {
	    snprintf(fmt, 20, "%%#%d.%de", min(w, (NB-1)), d);
	}
	else {
	    snprintf(fmt, 20, "%%%d.%de", min(w, (NB-1)), d);
	}
	snprintf(buff, NB, fmt, x);
    }
    else { /* e = 0 */
	snprintf(fmt, 20, "%%%d.%df", min(w, (NB-1)), d);
	snprintf(buff, NB, fmt, x);
    }
    buff[NB-1] = '\0';

    // Drop trailing zeroes
    for (char *p = buff; *p; p++) {
	if(*p == '.') {
	    char *replace = p++;
	    while ('0' <= *p  &&  *p <= '9')
		if(*(p++) != '0')
		    replace = p;
	    if(replace != p)
		while((*(replace++) = *(p++)))
		    ;
	    break;
	}
    }

    if (!streql(dec, ".")) { /* replace "." by dec */
	int len = strwidth(dec); /* 3·14 must work */
	if (len != 1) warning("%s", (len > 1) ?
	    _("the decimal mark is more than one character wide; this will become an error") :
	    _("the decimal mark is less than one character wide; this will become an error"));

	char *p, *q;
	for(p = buff, q = buff2; *p; p++) {
	    if(*p == '.') for(const char *r = dec; *r; r++) *q++ = *r;
	    else *q++ = *p;
	}
	*q = '\0';
	out = buff2;
    }

    return out;
}

attribute_hidden SEXP R::StringFromReal(double x, int *warn)
{
    int w, d, e;
    formatReal(&x, 1, &w, &d, &e, 0);
    if (ISNA(x)) return NA_STRING;
    else return mkChar(EncodeRealDrop0(x, w, d, e, OutDec));
}


attribute_hidden
const char *R::EncodeReal2(double x, int w, int d, int e)
{
    static char buff[NB];
    char fmt[20];

    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x)) snprintf(buff, NB, "%*s", min(w, (NB-1)), CHAR(R_print.na_string));
	else if(ISNAN(x)) snprintf(buff, NB, "%*s", min(w, (NB-1)), "NaN");
	else if(x > 0) snprintf(buff, NB, "%*s", min(w, (NB-1)), "Inf");
	else snprintf(buff, NB, "%*s", min(w, (NB-1)), "-Inf");
    }
    else if (e) {
	if(d) {
	    snprintf(fmt, 20 ,"%%#%d.%de", min(w, (NB-1)), d);
	    snprintf(buff, NB, fmt, x);
	}
	else {
	    snprintf(fmt, 20, "%%%d.%de", min(w, (NB-1)), d);
	    snprintf(buff, NB, fmt, x);
	}
    }
    else { /* e = 0 */
	snprintf(fmt, 20, "%%#%d.%df", min(w, (NB-1)), d);
	snprintf(buff, NB, fmt, x);
    }
    buff[NB-1] = '\0';
    return buff;
}

#define NB3 NB+3
const char *Rf_EncodeComplex(Rcomplex x, int wr, int dr, int er, int wi, int di, int ei,
	       const char *dec)
{
    static char buff[NB3];

    /* IEEE allows signed zeros; strip these here */
    if (x.r == 0.0) x.r = 0.0;
    if (x.i == 0.0) x.i = 0.0;

    if (ISNA(x.r) || ISNA(x.i)) {
	snprintf(buff, NB,
		 "%*s", /* was "%*s%*s", R_print.gap, "", */
		 min(wr+wi+2, (NB-1)), CHAR(R_print.na_string));
    } else {
	char Re[NB];
	const char *Im, *tmp;
	int flagNegIm = 0;
	/* EncodeReal has static buffer, so copy */
	tmp = EncodeReal0(x.r, wr, dr, er, dec);
	strcpy(Re, tmp);
	/* If x.i is very slightly negative, we printed -Oi, and that
	   will often be platform dependent */
	if ( (flagNegIm = (x.i < 0)) ) x.i = -x.i;
	Im = EncodeReal0(x.i, wi, di, ei, dec);
	if (streql(Im, "0")) flagNegIm = FALSE;
	snprintf(buff, NB3, "%s%s%si", Re, flagNegIm ? "-" : "+", Im);
    }
    buff[NB3-1] = '\0';
    return buff;
}

/* <FIXME>
   encodeString and Rstrwid assume that the wchar_t representation
   used to hold multibyte chars is Unicode.  This is usually true, and
   we warn if it is not known to be true.  Potentially looking at
   wchar_t ranges as we do is incorrect, but that is even less likely to
   be problematic.

   On Windows with surrogate pairs it will not be canonical, but AFAIK
   they do not occur in any MBCS (so it would only matter if we implement
   UTF-8, and then only if Windows has surrogate pairs switched on.).
*/

#include <rlocale.h> /* redefines isw* functions */

#ifdef Win32
#include "rgui_UTF8.h"
#endif

/* strlen() using escaped rather than literal form.
   In MBCS locales it works in characters, and reports in display width.
   Rstrwid is also used in printarray.c.

   This supported embedded nuls when we had those.
 */
attribute_hidden
int R::Rstrwid(const char *str, int slen, cetype_t ienc, int quote)
{
    const char *p = str;
    int len = 0, i;

    if(ienc == CE_BYTES) { // not currently used for that encoding
	for (i = 0; i < slen; i++) {
	    unsigned char k = str[i];
	    if (k >= 0x20 && k < 0x80) len += 1;
	    else len += 4;
	}
	return len;
    }
    /* Future-proof: currently that is all Rstrlen calls it with,
       and printarray has CE_NATIVE explicitly */
    if(ienc > 2) // CE_NATIVE, CE_UTF8, CE_BYTES are supported
	warning(_("unsupported encoding (%d) in Rstrwid"), ienc);
    if(mbcslocale || ienc == CE_UTF8) {
	bool useUTF8 = (ienc == CE_UTF8);
	mbstate_t mb_st;

	if(!useUTF8)  mbs_init(&mb_st);
	for (i = 0; i < slen; i++) {
	    unsigned int k; /* not wint_t as it might be signed */
	    wchar_t wc;
	    int res = useUTF8 ? (int) utf8toucs(&wc, p):
		(int) mbrtowc(&wc, p, R_MB_CUR_MAX, &mb_st);
	    if(res >= 0) {
		if (useUTF8 && IS_HIGH_SURROGATE(wc))
		    k = utf8toucs32(wc, p);
		else
		    k = wc;
		if(0x20 <= k && k < 0x7f && iswprint(k)) {
		    switch(wc) {
		    case L'\\':
			len += 2;
			break;
		    case L'\'':
		    case L'"':
		    case L'`':
			len += (quote == *p) ? 2 : 1;
			break;
		    default:
			len++; /* assumes these are all width 1 */
			break;
		    }
		    p++;
		} else if (k < 0x80) {
		    switch(wc) {
		    case L'\a':
		    case L'\b':
		    case L'\f':
		    case L'\n':
		    case L'\r':
		    case L'\t':
		    case L'\v':
		    case L'\0':
			len += 2; break;
		    default:
			/* print in octal */
			len += 4; break;
		    }
		    p++;
		} else {
		    /* no need to worry about truncation as iswprint
		     * and wcwidth get replaced on Windows */
		    // conceivably an invalid \U escape could use 11 or 12
		    len += iswprint(k) ?
#ifdef USE_RI18N_WIDTH
			Ri18n_wcwidth(k) :
#else
			/* this is expected to return -1 for
			   non-printable (including unassigned)
			   characters: that is unlikely to occur,
			   although the system's idea of 'printing'
			   may differ from the internal tables */
			imax2(wcwidth((wchar_t) k), 0) :
#endif
		    	(k > 0xffff ? 10 : 6);
		    i += (res - 1);
		    p += res;
		}
	    } else { /* invalid char */
		len += 4;
		p++;
	    }
	}
    } else // not MBCS nor marked as UTF-8
	for (i = 0; i < slen; i++) {
	    if((unsigned char) *p < 0x80) {
		/* ASCII */
		if(isprint((int)*p)) {
		    switch(*p) {
		    case '\\':
			len += 2; break;
		    case '\'':
		    case '"':
		    case '`':
			len += (quote == *p)? 2 : 1; break;
		    default:
			len++; break;
		    }
		} else switch(*p) {
		    case '\a':
		    case '\b':
		    case '\f':
		    case '\n':
		    case '\r':
		    case '\t':
		    case '\v':
		    case '\0':
			len += 2; break;
		    default:
			/* print in octal */
			len += 4; break;
		    }
		p++;
	    } else { /* 8 bit char */
#ifdef Win32 /* It seems Windows does not know what is printable! */
		len++;
#else
		len += isprint((int)*p) ? 1 : 4;
#endif
		p++;
	    }
	}

    return len;
}

/* Match what EncodeString does with encodings */
attribute_hidden
int R::Rstrlen(SEXP s, int quote)
{
    cetype_t ienc = getCharCE(s);
    if (ienc == CE_UTF8 || ienc == CE_BYTES)
	return Rstrwid(CHAR(s), LENGTH(s), ienc, quote);
    CXXR::RAllocStack::Scope rscope;
    const char *p = translateChar(s);
    int len = Rstrwid(p, (int)strlen(p), CE_NATIVE, quote);

    return len;
}

/* Here w is the minimum field width
   If 'quote' is non-zero the result should be quoted (and internal quotes
   escaped and NA strings handled differently).

   EncodeString is called from EncodeElement, EncodeChar, cat() (for labels
   when filling), to (auto)print character vectors, arrays, names and
   CHARSXPs.  It is also called by do_encodeString, but not from
   format().
 */

attribute_hidden
const char *R::EncodeString(SEXP s, int w, int quote, Rprt_adj justify)
{
    int i, cnt;
    const char *p; char *q, buf[13];
    cetype_t ienc = getCharCE(s);
    bool useUTF8 = (w < 0);
    CXXR::RAllocStack::Scope rscope;

    if (w < 0) w = w + 1000000;

    /* We have to do something like this as the result is returned, and
       passed on by EncodeElement -- so no way could be end user be
       responsible for freeing it.  However, this is not thread-safe. */

    static R_StringBuffer gBuffer = {NULL, 0, BUFSIZE};
    R_StringBuffer *buffer = &gBuffer;

    if (s == NA_STRING) {
	p = quote ? CHAR(R_print.na_string) : CHAR(R_print.na_string_noquote);
	cnt = (int)(quote ? strlen(CHAR(R_print.na_string)) :
		strlen(CHAR(R_print.na_string_noquote)));
	i = (quote ? Rstrlen(R_print.na_string, 0) :
		Rstrlen(R_print.na_string_noquote, 0));
	quote = 0;
    } else {
	if(IS_BYTES(s)) {
	    ienc = CE_NATIVE;
#ifdef Win32
	    if (WinUTF8out)
		ienc = CE_UTF8;
#endif
	    p = CHAR(s);
	    cnt = (int) strlen(p);
	    const char *q;
	    char *pp = R_alloc(4*cnt+1, 1), *qq = pp, buf[5];
	    for (q = p; *q; q++) {
		unsigned char k = (unsigned char) *q;
		if (k >= 0x20 && k < 0x80) {
		    *qq++ = *q;
		    if (quote && *q == '"') cnt++;
		} else {
		    snprintf(buf, 5, "\\x%02x", k);
		    for(int j = 0; j < 4; j++) *qq++ = buf[j];
		    cnt += 3;
		}
	    }
	    *qq = '\0';
	    p = pp;
	    i = cnt;
#ifdef Win32
	} else if(WinUTF8out) {
	    if(ienc == CE_UTF8) {
		p = CHAR(s);
		i = Rstrlen(s, quote);
		cnt = LENGTH(s);
	    } else {
		p = translateCharUTF8(s);
		if(p == CHAR(s)) {
		    i = Rstrlen(s, quote);
		    cnt = LENGTH(s);
		} else {
		    cnt = (int) strlen(p);
		    i = Rstrwid(p, cnt, CE_UTF8, quote);
		}
		ienc = CE_UTF8;
	    }
#endif
	} else {
	    if (useUTF8 && ienc == CE_UTF8) {
		p = CHAR(s);
		i = Rstrlen(s, quote);
		cnt = LENGTH(s);
	    } else {
		ienc = CE_NATIVE;
		p = translateChar(s);
		if(p == CHAR(s)) {
		    i = Rstrlen(s, quote);
		    cnt = LENGTH(s);
		} else {
		    cnt = (int) strlen(p);
		    i = Rstrwid(p, cnt, CE_NATIVE, quote);
		}
	    }
	}
    }

    /* We need enough space for the encoded string, including escapes.
       Octal encoding turns one byte into four.
       \u encoding can turn a multibyte into six or ten,
       but it turns 2/3 into 6, and 4 (and perhaps 5/6) into 10.
       Let's be wasteful here (the worst case appears to be an MBCS with
       one byte for an upper-plane Unicode point output as ten bytes,
       but I doubt that such an MBCS exists: two bytes is plausible).

       +2 allows for quotes, +6 for UTF_8 escapes.
     */
    if(5.*cnt + 8 > (double) SIZE_MAX)
	error(_("too large string (nchar=%d) => 5*nchar + 8 > SIZE_MAX"),
	      cnt);
    size_t q_len = 5*(size_t)cnt + 8;
    if(q_len < (size_t) w) q_len = (size_t) w;
    q = (char *) R_AllocStringBuffer(q_len, buffer);

    int b = w - i - (quote ? 2 : 0); /* total amount of padding */
    if(justify == Rprt_adj_none) b = 0;
    if(b > 0 && justify != Rprt_adj_left) {
	int b0 = (justify == Rprt_adj_centre) ? b/2 : b;
	for(i = 0 ; i < b0 ; i++) *q++ = ' ';
	b -= b0;
    }
    if(quote) *q++ = (char) quote;
    if(mbcslocale || ienc == CE_UTF8) {
	bool useUTF8 = (ienc == CE_UTF8);
	bool wchar_is_ucs_or_utf16 = TRUE;
	mbstate_t mb_st;
#ifndef __STDC_ISO_10646__
	bool Unicode_warning = FALSE;
#endif
# if !defined (__STDC_ISO_10646__) && !defined (Win32)
	wchar_is_ucs_or_utf16 = FALSE;
# endif
	if(!useUTF8)  mbs_init(&mb_st);
#ifdef Win32
	else if(WinUTF8out) { memcpy(q, UTF8in, 3); q += 3; }
#endif
	for (i = 0; i < cnt; i++) {
	    wchar_t wc;
	    int res = (int)(useUTF8 ? utf8toucs(&wc, p):
			    mbrtowc(&wc, p, R_MB_CUR_MAX, &mb_st));
	    /* res = 0 is a terminator
	     * some mbrtowc implementations return wc past end of UCS */
	    if(res >= 0 &&
	       ((0 <= wc && (unsigned long)wc <= 0x10FFFF) || !wchar_is_ucs_or_utf16)) {
		unsigned int k; /* not wint_t as it might be signed */
		if (useUTF8 && IS_HIGH_SURROGATE(wc))
		    k = utf8toucs32(wc, p);
		else
		    k = wc;
		/* To be portable, treat \0 explicitly */
		if(res == 0) {k = 0; wc = L'\0';}
		if(0x20 <= k && k < 0x7f && iswprint(k)) {
		    switch(wc) {
		    case L'\\': *q++ = '\\'; *q++ = '\\'; p++; break;
		    case L'\'':
		    case L'"':
		    case L'`':
			{
			    if(quote == *p)  *q++ = '\\';
			    *q++ = *p++;
			    break;
			}
		    default:
			for(int j = 0; j < res; j++) *q++ = *p++;
			break;
		    }
		} else if (k < 0x80) {
		    /* ANSI Escapes */
		    switch(wc) {
		    case L'\a': *q++ = '\\'; *q++ = 'a'; break;
		    case L'\b': *q++ = '\\'; *q++ = 'b'; break;
		    case L'\f': *q++ = '\\'; *q++ = 'f'; break;
		    case L'\n': *q++ = '\\'; *q++ = 'n'; break;
		    case L'\r': *q++ = '\\'; *q++ = 'r'; break;
		    case L'\t': *q++ = '\\'; *q++ = 't'; break;
		    case L'\v': *q++ = '\\'; *q++ = 'v'; break;
		    case L'\0': *q++ = '\\'; *q++ = '0'; break;

		    default:
			/* print in octal */
			// gcc 7 requires cast here
			snprintf(buf, 5, "\\%03o", (unsigned char)k);
			for(int j = 0; j < 4; j++) *q++ = buf[j];
			break;
		    }
		    p++;
		} else {
		    /* wc could be an unpaired surrogate and this does
		     * not do the same as Rstrwid */
		    /* no need to worry about truncation as iswprint
		     * gets replaced on Windows */
		    if(iswprint(k)) {
			/* The problem here is that wc may be
			   printable according to the Unicode tables,
			   but it may not be printable on the output
			   device concerned.

			   And the system iswprintf may not correspond
			   to the latest Unicode tables.
			*/
			for(int j = 0; j < res; j++) *q++ = *p++;
		    } else {
# if !defined (__STDC_ISO_10646__) && !defined (Win32)
			Unicode_warning = TRUE;
# endif
			if(k > 0xffff)
			    snprintf(buf, 13, "\\U{%06x}", k);
			else
			    snprintf(buf, 11, "\\u%04x", k);
			int j = (int) strlen(buf);
			memcpy(q, buf, j);
			q += j;
			p += res;
		    }
		    i += (res - 1);
		}
	    } else { /* invalid char */
		snprintf(q, 5, "\\x%02x", *((unsigned char *)p));
		q += 4; p++;
	    }
	}
#ifndef __STDC_ISO_10646__
	if(Unicode_warning)
	    warning("%s", _("it is not known that wchar_t is Unicode on this platform"));
#endif

    } else
	for (i = 0; i < cnt; i++) {

	    /* ASCII */
	    if((unsigned char) *p < 0x80) {
		if(*p != '\t' && isprint((int)*p)) { /* Windows has \t as printable */
		    switch(*p) {
		    case '\\': *q++ = '\\'; *q++ = '\\'; break;
		    case '\'':
		    case '"':
		    case '`':
		    {
			if(quote == *p)  *q++ = '\\';
			*q++ = *p;
			break;
		    }
		    default: *q++ = *p; break;
		    }
		} else switch(*p) {
			/* ANSI Escapes */
		    case '\a': *q++ = '\\'; *q++ = 'a'; break;
		    case '\b': *q++ = '\\'; *q++ = 'b'; break;
		    case '\f': *q++ = '\\'; *q++ = 'f'; break;
		    case '\n': *q++ = '\\'; *q++ = 'n'; break;
		    case '\r': *q++ = '\\'; *q++ = 'r'; break;
		    case '\t': *q++ = '\\'; *q++ = 't'; break;
		    case '\v': *q++ = '\\'; *q++ = 'v'; break;
		    case '\0': *q++ = '\\'; *q++ = '0'; break;

		    default:
			/* print in octal */
			snprintf(buf, 5, "\\%03o", (unsigned char) *p);
			for(int j = 0; j < 4; j++) *q++ = buf[j];
			break;
		    }
		p++;
	    } else {  /* 8 bit char */
		if(!isprint((int)*p & 0xff)) {
		    /* print in octal */
		    snprintf(buf, 5, "\\%03o", (unsigned char) *p);
		    for(int j = 0; j < 4; j++) *q++ = buf[j];
		    p++;
		} else *q++ = *p++;
	    }
	}

#ifdef Win32
    if(WinUTF8out && ienc == CE_UTF8)  { memcpy(q, UTF8out, 3); q += 3; }
#endif
    if(quote) *q++ = (char) quote;
    if(b > 0 && justify != Rprt_adj_right) {
	for(i = 0 ; i < b ; i++) *q++ = ' ';
    }
    *q = '\0';

    return buffer->data;
}

/* EncodeElement is called by cat(), write.table() and deparsing. */

/* NB this is called by R.app even though it is in no public header, so
   alter there if you alter this */
const char *R::EncodeElement(SEXP x, int indx, int quote, char cdec)
{
    char dec[2];
    dec[0] = cdec; dec[1] = '\0';
    return EncodeElement0(x, indx, quote, dec);
}

const char *R::EncodeElement0(SEXP x, R_xlen_t indx, int quote, const char *dec)
{
    int w, d, e, wi, di, ei;
    const char *res;

    switch(TYPEOF(x)) {
    case LGLSXP:
	formatLogical(&LOGICAL_RO(x)[indx], 1, &w);
	res = EncodeLogical(LOGICAL_RO(x)[indx], w);
	break;
    case INTSXP:
	formatInteger(&INTEGER_RO(x)[indx], 1, &w);
	res = EncodeInteger(INTEGER_RO(x)[indx], w);
	break;
    case REALSXP:
	formatReal(&REAL_RO(x)[indx], 1, &w, &d, &e, 0);
	res = EncodeReal0(REAL_RO(x)[indx], w, d, e, dec);
	break;
    case STRSXP:
	formatString(&STRING_PTR_RO(x)[indx], 1, &w, quote);
	res = EncodeString(STRING_ELT(x, indx), w, quote, Rprt_adj_left);
	break;
    case CPLXSXP:
	formatComplex(&COMPLEX_RO(x)[indx], 1, &w, &d, &e, &wi, &di, &ei, 0);
	res = EncodeComplex(COMPLEX_RO(x)[indx], w, d, e, wi, di, ei, dec);
	break;
    case RAWSXP:
	res = EncodeRaw(RAW_RO(x)[indx], "");
	break;
    default:
	res = NULL; /* -Wall */
	UNIMPLEMENTED_TYPE("EncodeElement", x);
    }
    return res;
}

/* EncodeChar is a simple wrapper for EncodeString
   called by error messages to display CHARSXP values.

   The pointer returned by EncodeChar points into an internal buffer
   which is overwritten by subsequent calls to EncodeChar/EncodeString.
   It is the responsibility of the caller to copy the result before
   any subsequent call to EncodeChar/EncodeString may happen. Note that
   particularly it is NOT safe to pass the result of EncodeChar as 3rd
   argument to errorcall (errorcall_cpy can be used instead). */
//attribute_hidden
const char *R::EncodeChar(SEXP x)
{
    return EncodeString(x, 0, 0, Rprt_adj_left);
}


void Rprintf(const char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    Rvprintf(format, ap);
    va_end(ap);
}

/*
  REprintf is used by the error handler do not add
  anything unless you're sure it won't
  cause problems
*/
void REprintf(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    REvprintf(format, ap);
    va_end(ap);
}

void RWprintf(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    RWvprintf(format, ap);
    va_end(ap);
}

#if defined(HAVE_VASPRINTF) && !HAVE_DECL_VASPRINTF
#ifdef __cplusplus
extern "C"
#endif
int vasprintf(char **ret, const char *format, va_list args);
#endif

# define R_BUFSIZE BUFSIZE
// similar to dummy_vfprintf in connections.c
attribute_hidden
int R::Rcons_vprintf(const char *format, va_list arg)
{
    char buf[R_BUFSIZE], *p = buf;
    int res;
    CXXR::RAllocStack::Scope rscope;
    bool usedVasprintf = FALSE;
    va_list aq;

    va_copy(aq, arg);
    res = Rvsnprintf_mbcs(buf, R_BUFSIZE, format, aq);
    va_end(aq);
#ifdef HAVE_VASPRINTF
    if(res >= R_BUFSIZE || res < 0) {
	res = vasprintf(&p, format, arg);
	if (res < 0) {
	    p = buf;
	    warning("%s", _("printing of extremely long output is truncated"));
	} else usedVasprintf = TRUE;
    }
#else
    if(res >= R_BUFSIZE) { /* res is the desired output length */
	/* dummy_vfprintf protects against `res` being counted short; we do not
	   do that here */
	p = R_alloc(res+1, sizeof(char));
	vnsprintf(p, res + 1, format, arg);
    } else if(res < 0) {
	/* Some non-C99 conforming vsnprintf implementations return -1 on
	   truncation instead of only on error. */
	p = R_alloc(10*R_BUFSIZE, sizeof(char));
	res = Rvsnprintf_mbcs(p, 10*R_BUFSIZE, format, arg);
	if (res < 0 || res >= 10*R_BUFSIZE)
	    warning("%s", _("printing of extremely long output is truncated"));
    }
#endif /* HAVE_VASPRINTF */
    res = (int) strlen(p);
    R_WriteConsole(p, res);
    if(usedVasprintf) free(p);
    return res;
}

void Rvprintf(const char *format, va_list arg)
{
    int i=0, con_num=R_OutputCon;
    Rconnection con;
    va_list argcopy;
    static int printcount = 0;

    if (++printcount > 100) {
	R_CheckUserInterrupt();
	printcount = 0;
    }

    do{
      con = getConnection(con_num);
      va_copy(argcopy, arg);
      con->m_type = otype_t::INFORMATION_;
      /* Parentheses added for Fedora with -D_FORTIFY_SOURCE=2 */
      (con->vfprintf)(con, format, argcopy);
      va_end(argcopy);
      con->fflush(con);
      con_num = getActiveSink(i++);
    } while(con_num>0);


}

/*
   REvprintf is part of the error handler.
   Do not change it unless you are SURE that
   your changes are compatible with the
   error handling mechanism.

   It is also used in R_Suicide on Unix.
*/

attribute_hidden
int R::REvprintf_internal(otype_t otype, const char *format, va_list arg)
{
    static char *malloc_buf = NULL;
    int res;

    if (malloc_buf) {
	char *tmp = malloc_buf;
	malloc_buf = NULL;
	free(tmp);
    }
    if(R_ErrorCon != 2) {
	Rconnection con = getConnection_no_err(R_ErrorCon);
	if(con == NULL) {
	    /* should never happen, but in case of corruption... */
	    R_ErrorCon = 2;
	} else {
	    con->m_type = otype;
	    /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
	    res = (con->vfprintf)(con, format, arg);
	    con->fflush(con);
	    return res;
	}
    }
    if(R_Consolefile) {
	/* try to interleave stdout and stderr carefully */
	if(R_Outputfile && (R_Outputfile != R_Consolefile)) {
	    fflush(R_Outputfile);
	    res = vfprintf(R_Consolefile, format, arg);
	    /* normally R_Consolefile is stderr and so unbuffered, but
	       it can be something else (e.g. stdout on Win9x) */
	    fflush(R_Consolefile);
	} else
	    res = vfprintf(R_Consolefile, format, arg);
    } else {
	char buf[BUFSIZE];
	bool printed = false;
	va_list aq;

	va_copy(aq, arg);
	res = Rvsnprintf_mbcs(buf, BUFSIZE, format, aq);
	va_end(aq);
	if (res >= BUFSIZE) {
	    /* A very long string has been truncated. Try to allocate a large
	       buffer for it to print it in full. Do not use R_alloc() as this
	       can be run due to memory allocation error from the R heap.
	       Do not use contexts and do not throw any errors nor warnings
	       as this may be run from error handling. */
	    int size = res + 1;
	    malloc_buf = (char *)malloc(size * sizeof(char));
	    if (malloc_buf) {
		res = vsnprintf(malloc_buf, size, format, arg);
		if (res == size - 1) {
		    R_WriteConsoleEx(malloc_buf, res, otype);
		    printed = true;
		}
		char *tmp = malloc_buf;
		malloc_buf = NULL;
		free(tmp);
	    }
	}
	if (!printed) {
	    res = (int) strlen(buf);
	    R_WriteConsoleEx(buf, res, otype);
	}
    }
    return res;
}

void REvprintf(const char *format, va_list arg)
{
    REvprintf_internal(otype_t::ERROR_, format, arg);
}

void RWvprintf(const char *format, va_list arg)
{
    REvprintf_internal(otype_t::WARNING_, format, arg);
}

attribute_hidden int Rf_IndexWidth(R_xlen_t n)
{
    return (int) (log10((double)n + 0.5) + 1);
}

attribute_hidden void Rf_VectorIndex(R_xlen_t i, int w)
{
/* print index label "[`i']" , using total width `w' (left filling blanks) */
    Rprintf("%*s[%ld]", w-IndexWidth(i)-2, "", (long)i);
}
