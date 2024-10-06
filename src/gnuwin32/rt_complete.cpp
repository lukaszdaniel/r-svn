/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file rt_complete.c
 *  Copyright (C) 2007-2024 The R Core Team.
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <getline/getline.h>
#include <memory>
#include <cstring>
#include <cstdlib> /* for getenv */
#include <R_ext/Minmax.h>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp> // for streql, streqln
#include <Defn.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include "console.h"

using namespace R;

static int completion_available = -1;

static int gl_tab(char *buf, int offset, size_t *loc)
/* default tab handler, acts like tabstops every 8 cols */
{
    int i, count, len;

    len = strlen(buf);
    count = 8 - (offset + *loc) % 8;
    for (size_t i=len; i >= *loc; i--)
	buf[i+count] = buf[i];
    for (i=0; i < count; i++)
	buf[*loc+i] = ' ';
    i = *loc;
    *loc = i + count;
    return i;
}

static int rt_completion(char *buf, int offset, size_t *loc)
{
    int alen, cursor_position = *loc;
    char *partial_line = buf;
    const char *additional_text;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;

    if (!completion_available) return gl_tab(buf, offset, loc);

    if (completion_available < 0) {
	char *p = getenv("R_COMPLETION");
	if (p && streql(p, "FALSE")) {
	    completion_available = 0;
	    return gl_tab(buf, offset, loc);
	}
	/* First check if namespace is loaded */
	if (R_findVarInFrame(R_NamespaceRegistry, install("utils"))
	   != R_UnboundValue) completion_available = 1;
	else { /* Then try to load it */
	    const char *p = "try(loadNamespace('utils'), silent=TRUE)";
	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if (status == PARSE_OK) {
		for (int i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if (R_findVarInFrame(R_NamespaceRegistry, install("utils"))
	       != R_UnboundValue) completion_available = 1;
	    else {
		completion_available = 0;
		return -1; /* no change */
	    }
	}
    }

    alen = strlen(partial_line);
    std::unique_ptr<char[]> tmp = std::make_unique<char[]>(alen + 1);
    std::unique_ptr<char[]> tmpline = std::make_unique<char[]>(2*alen + 1);
    char *orig = tmp.get();
    char *pline = tmpline.get();
    char *pchar = pline, achar;
    strcpy(orig, partial_line);
    for (int i = 0; i < alen; i++) {
        achar = orig[i];
	if (achar == '"' || achar == '\\') *pchar++ = '\\';
	*pchar++ = achar;
    }
    *pchar = 0;
    size_t plen = strlen(pline);
    size_t len = plen + 100; 
    std::unique_ptr<char[]> tmp2 = std::make_unique<char[]>(len);
    char *cmd = tmp2.get();
    snprintf(cmd, len,
	     "utils:::.win32consoleCompletion(\"%.*s\", %d)",
	     (int)plen, pline, cursor_position);
    PROTECT(cmdSexp = mkString(cmd));
    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	/* Uncomment next line to debug */
	/* Rprintf("failed: %s \n", cmd); */
	/* otherwise pretend that nothing happened and return */
	return -1; /* no change */
    }
    /* Loop is needed here as EXPRSEXP will be of length > 1 */
    for (int i = 0; i < length(cmdexpr); i++)
	ans = eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
    UNPROTECT(2);
    PROTECT(ans);

    /* ans has the form list(addition, possible), where 'addition' is
       unique additional text if any, and 'possible' is a character
       vector holding possible completions if any (already formatted
       for linewise printing in the current implementation).  If
       'possible' has any content, we want to print those (or show in
       status bar or whatever).  Otherwise add the 'additional' text
       at the cursor */

#define ADDITION 0
#define POSSIBLE 1

    CXXR::RAllocStack::Scope rscope;
    alen = length(VECTOR_ELT(ans, POSSIBLE));
    if (alen) {
	int max_show = 10;
	printf("\n"); /* finish current line */
	for (int i = 0; i < std::min(alen, max_show); i++) {
	    printf("%s\n", translateChar(STRING_ELT(VECTOR_ELT(ans, POSSIBLE), i)));
	}
	if (alen > max_show)
	    printf("\n[...truncated]\n");
	cursor_position = -2; /* Need to redisplay whole line */
    }
    additional_text = translateChar(STRING_ELT( VECTOR_ELT(ans, ADDITION), 0 ));
    alen = strlen(additional_text);
    if (alen) {
	int cp = *loc;
	memcpy(buf+cp, additional_text, alen+1);
	*loc = cp + alen;
    }

    UNPROTECT(1); /* ans */
    return cursor_position;
}


void R_gl_tab_set(void)
{
    gl_tab_hook = rt_completion;
}
