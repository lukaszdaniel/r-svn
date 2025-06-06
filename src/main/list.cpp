/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001, 2006  The R Core Team
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
 *
 *  Basic List Handling Features
 *
 *  These remain here to show that R is truly descended from Lisp :-).
 *  There is one real function "allnames" shich should probably be
 *  elsewhere.
 */

/** @file list.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/ProtectStack.hpp>
#include <Defn.h>
#include <Internal.h>

using namespace R;

/* Utility functions moved to Rinlinedfuns.h */

/* The following code is used to recursive traverse a block */
/* of code and extract all the symbols present in that code. */

typedef struct {
 SEXP	ans;
 bool	UniqueNames;
 bool	IncludeFunctions;
 int	StoreValues;
 int	ItemCounts;
 int	MaxCount;
} NameWalkData;

static void namewalk(SEXP s, NameWalkData *d)
{
    SEXP name;

    switch (TYPEOF(s)) {
    case SYMSXP:
	name = PRINTNAME(s);
	/* skip blank symbols */
	if (CHAR(name)[0] == '\0') goto ignore;
	if (d->ItemCounts < d->MaxCount) {
	    if (d->StoreValues) {
		if (d->UniqueNames) {
		    for (int j = 0 ; j < d->ItemCounts ; j++) {
			if (STRING_ELT(d->ans, j) == name)
			    goto ignore;
		    }
		}
		SET_STRING_ELT(d->ans, d->ItemCounts, name);
	    }
	    d->ItemCounts++;
	}
    ignore:
	break;
    case LANGSXP:
	if (!d->IncludeFunctions) s = CDR(s);
	while (s != R_NilValue) {
	    namewalk(CAR(s), d);
	    s = CDR(s);
	}
	break;
    case EXPRSXP:
	for(R_xlen_t i = 0 ; i < XLENGTH(s) ; i++)
	    namewalk(XVECTOR_ELT(s, i), d);
	break;
    default:
	/* it seems the intention is to do nothing here! */
	break;
    }
}

/* Also does all.vars with functions=FALSE
   .Internal(all.names(expr, functions, max.names, unique)) */
attribute_hidden SEXP do_allnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP expr;
    int savecount;
    NameWalkData data = {NULL, 0, 0, 0, 0, 0};

    checkArity(op, args);

    expr = CAR(args);
    args = CDR(args);

    data.IncludeFunctions = asLogical(CAR(args));
    if (data.IncludeFunctions == NA_LOGICAL)
	data.IncludeFunctions = 0;
    args = CDR(args);

    data.MaxCount = asInteger(CAR(args));
    if (data.MaxCount == -1) data.MaxCount = R_INT_MAX;
    if (data.MaxCount < 0 || data.MaxCount == NA_INTEGER)
	data.MaxCount = 0;
    args = CDR(args);

    data.UniqueNames = asLogical(CAR(args));
    if (data.UniqueNames == NA_LOGICAL)
	data.UniqueNames = 1;

    namewalk(expr, &data);
    savecount = data.ItemCounts;

    data.ans = allocVector(STRSXP, data.ItemCounts);

    data.StoreValues = 1;
    data.ItemCounts = 0;
    namewalk(expr, &data);

    if (data.ItemCounts != savecount) {
	PROTECT(expr = data.ans);
	data.ans = allocVector(STRSXP, data.ItemCounts);
	for (int i = 0 ; i < data.ItemCounts ; i++)
	    SET_STRING_ELT(data.ans, i, STRING_ELT(expr, i));
	UNPROTECT(1);
    }

    return data.ans;
}
