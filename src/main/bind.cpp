/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 2002--2020  The R Foundation
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
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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

/** @file bind.cpp
 *
 */

/* Code to handle list / vector switch */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <R_ext/Minmax.h>
#include <Localization.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/PrtUtil.h> // for IndexWidth
#include <R_ext/Itermacros.h>

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#include "duplicate.h"

using namespace R;
using namespace CXXR;

#define LIST_ASSIGN(x) {SET_VECTOR_ELT(data->ans_ptr, data->ans_length, x); data->ans_length++;}

static SEXP cbind(SEXP, SEXP, SEXPTYPE, SEXP, int);
static SEXP rbind(SEXP, SEXP, SEXPTYPE, SEXP, int);

/* The following code establishes the return type for the */
/* functions  unlist, c, cbind, and rbind and also determines */
/* whether the returned object is to have a names attribute. */

struct BindData {
 int  ans_flags;
 SEXP ans_ptr;
 R_xlen_t ans_length;
 SEXP ans_names;
 R_xlen_t  ans_nnames;
/* int  deparse_level; Initialize to 1. */
};

static bool HasNames(SEXP x)
{
    if (isVector(x)) {
	if (!isNull(getAttrib(x, R_NamesSymbol)))
	    return true;
    }
    else if (isList(x)) {
	while (!isNull(x)) {
	    if (!isNull(TAG(x))) return 1;
	    x = CDR(x);
	}
    }
    return false;
}

// Determine result type of unlist() or c();  called from  do_c()  and  do_unlist()
static void AnswerType(SEXP x, bool recurse, bool usenames, struct BindData *data, SEXP call)
{
    switch (TYPEOF(x)) {
    case NILSXP: // NULL entries are dropped
	break;
    case RAWSXP:
	data->ans_flags |= 1;
	data->ans_length += XLENGTH(x);
	break;
    case LGLSXP:
	data->ans_flags |= 2;
	data->ans_length += XLENGTH(x);
	break;
    case INTSXP:
	data->ans_flags |= 16;
	data->ans_length += XLENGTH(x);
	break;
    case REALSXP:
	data->ans_flags |= 32;
	data->ans_length += XLENGTH(x);
	break;
    case CPLXSXP:
	data->ans_flags |= 64;
	data->ans_length += XLENGTH(x);
	break;
    case STRSXP:
	data->ans_flags |= 128;
	data->ans_length += XLENGTH(x);
	break;

#ifdef DO_C_Symbol
/* new case : */
    case SYMSXP:
    case LANGSXP:
	data->ans_flags |= 512; /* as.expression() implicitly */
	data->ans_length += 1;
	break;
#endif
    case VECSXP:
	if (recurse) {
	    R_xlen_t i, n = XLENGTH(x);
	    if (usenames && !data->ans_nnames &&
		!isNull(getAttrib(x, R_NamesSymbol)))
		data->ans_nnames = 1;
	    for (i = 0; i < n; i++) {
		if (usenames && !data->ans_nnames)
		    data->ans_nnames = HasNames(VECTOR_ELT(x, i));
		AnswerType(VECTOR_ELT(x, i), recurse, usenames, data, call);
	    }
	}
	else {
	    data->ans_flags |= 256;
	    data->ans_length += XLENGTH(x);
	}
	break;
    case EXPRSXP:
	if (recurse) {
	    R_xlen_t i, n = XLENGTH(x);
	    if (usenames && !data->ans_nnames &&
		!isNull(getAttrib(x, R_NamesSymbol)))
		data->ans_nnames = 1;
	    for (i = 0; i < n; i++) {
		if (usenames && !data->ans_nnames)
		    data->ans_nnames = HasNames(XVECTOR_ELT(x, i));
		AnswerType(XVECTOR_ELT(x, i), recurse, usenames, data, call);
	    }
	}
	else {
	    data->ans_flags |= 512;
	    data->ans_length += XLENGTH(x);
	}
	break;
    case LISTSXP:
	if (recurse) {
	    while (x != R_NilValue) {
		if (usenames && !data->ans_nnames) {
		    if (!isNull(TAG(x))) data->ans_nnames = 1;
		    else data->ans_nnames = HasNames(CAR(x));
		}
		AnswerType(CAR(x), recurse, usenames, data, call);
		x = CDR(x);
	    }
	}
	else {
	    data->ans_flags |= 256;
	    data->ans_length += length(x);
	}
	break;
    default:
	data->ans_flags |= 256;
	data->ans_length += 1;
	break;
    }

    /* check for overflow in ans_length. Objects are added one at a
       time for each call to AnswerType so it is safe to check here.
       Since sizes are signed, positive numbers, the overflow will
       manifest itself as a negative result (both numbers will be
       31-bit so we cannot overflow across the 32-bit boundary). If
       our assumption (all lengths are signed) is violated, this won't
       work so check when switching length types! */

#ifndef LONG_VECTOR_SUPPORT
    if (data->ans_length < 0)
	errorcall(call, _("resulting vector exceeds vector length limit in '%s'"), "AnswerType");
#endif
}


/* The following functions are used to coerce arguments to the
 * appropriate type for inclusion in the returned value. */

static void ListAnswer(SEXP x, int recurse, struct BindData *data, SEXP call)
{
    R_xlen_t i;

    switch(TYPEOF(x)) {
    case NILSXP: // NULL entries are dropped
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarLogical(LOGICAL(x)[i]));
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarRaw(RAW(x)[i]));
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarInteger(INTEGER(x)[i]));
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarReal(REAL(x)[i]));
	break;
    case CPLXSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarComplex(COMPLEX(x)[i]));
	break;
    case STRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LIST_ASSIGN(ScalarString(STRING_ELT(x, i)));
	break;
    case VECSXP:
	if (recurse) {
	    for (i = 0; i < XLENGTH(x); i++)
		ListAnswer(VECTOR_ELT(x, i), recurse, data, call);
	}
	else {
	    for (i = 0; i < XLENGTH(x); i++)
		LIST_ASSIGN(lazy_duplicate(VECTOR_ELT(x, i)));
	}
	break;
    case EXPRSXP:
	if (recurse) {
	    for (i = 0; i < XLENGTH(x); i++)
		ListAnswer(XVECTOR_ELT(x, i), recurse, data, call);
	}
	else {
	    for (i = 0; i < XLENGTH(x); i++)
		LIST_ASSIGN(lazy_duplicate(XVECTOR_ELT(x, i)));
	}
	break;
    case LISTSXP:
	if (recurse) {
	    while (x != R_NilValue) {
		ListAnswer(CAR(x), recurse, data, call);
		x = CDR(x);
	    }
	}
	else
	    while (x != R_NilValue) {
		LIST_ASSIGN(lazy_duplicate(CAR(x)));
		x = CDR(x);
	    }
	break;
    default:
	LIST_ASSIGN(lazy_duplicate(x));
	break;
    }
}

static void StringAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    StringAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    StringAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    StringAnswer(VECTOR_ELT(x, i), data, call);
	break;
    default:
	PROTECT(x = coerceVector(x, STRSXP));
	for (i = 0; i < XLENGTH(x); i++)
	    SET_STRING_ELT(data->ans_ptr, data->ans_length++, STRING_ELT(x, i));
	UNPROTECT(1);
	break;
    }
}

static void LogicalAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    LogicalAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LogicalAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LogicalAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LOGICAL(data->ans_ptr)[data->ans_length++] = LOGICAL(x)[i];
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    int v = INTEGER(x)[i];
	    LOGICAL(data->ans_ptr)[data->ans_length++] = (v == NA_INTEGER) ? NA_LOGICAL : ( v != 0 );
	}
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    LOGICAL(data->ans_ptr)[data->ans_length++] = (int)RAW(x)[i] != 0;
	break;
    default:
	errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  R_typeToChar(x), "LogicalAnswer");
    }
}

static void IntegerAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    IntegerAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    IntegerAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    IntegerAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = LOGICAL(x)[i];
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = INTEGER(x)[i];
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    INTEGER(data->ans_ptr)[data->ans_length++] = (int)RAW(x)[i];
	break;
    default:
	errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  R_typeToChar(x), "IntegerAnswer");
    }
}

static void RealAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    int xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    RealAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RealAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RealAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    REAL(data->ans_ptr)[data->ans_length++] = REAL(x)[i];
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = LOGICAL(x)[i];
	    if (xi == NA_LOGICAL)
		REAL(data->ans_ptr)[data->ans_length++] = NA_REAL;
	    else REAL(data->ans_ptr)[data->ans_length++] = xi;
	}
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = INTEGER(x)[i];
	    if (xi == NA_INTEGER)
		REAL(data->ans_ptr)[data->ans_length++] = NA_REAL;
	    else REAL(data->ans_ptr)[data->ans_length++] = xi;
	}
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    REAL(data->ans_ptr)[data->ans_length++] = (int)RAW(x)[i];
	break;
    default:
	errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  R_typeToChar(x), "RealAnswer");
    }
}

static void ComplexAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    int xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    ComplexAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    ComplexAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    ComplexAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case REALSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    COMPLEX(data->ans_ptr)[data->ans_length].r = REAL(x)[i];
	    COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    data->ans_length++;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    COMPLEX(data->ans_ptr)[data->ans_length++] = COMPLEX(x)[i];
	break;
    case LGLSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = LOGICAL(x)[i];
	    if (xi == NA_LOGICAL) {
		COMPLEX(data->ans_ptr)[data->ans_length].r = NA_REAL;
#ifdef NA_TO_COMPLEX_NA
		COMPLEX(data->ans_ptr)[data->ans_length].i = NA_REAL;
#else
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
#endif
	    }
	    else {
		COMPLEX(data->ans_ptr)[data->ans_length].r = xi;
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    }
	    data->ans_length++;
	}
	break;
    case INTSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    xi = INTEGER(x)[i];
	    if (xi == NA_INTEGER) {
		COMPLEX(data->ans_ptr)[data->ans_length].r = NA_REAL;
#ifdef NA_TO_COMPLEX_NA
		COMPLEX(data->ans_ptr)[data->ans_length].i = NA_REAL;
#else
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
#endif
	    }
	    else {
		COMPLEX(data->ans_ptr)[data->ans_length].r = xi;
		COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    }
	    data->ans_length++;
	}
	break;

    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++) {
	    COMPLEX(data->ans_ptr)[data->ans_length].r = (int)RAW(x)[i];
	    COMPLEX(data->ans_ptr)[data->ans_length].i = 0.0;
	    data->ans_length++;
	}
	break;

    default:
	errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  R_typeToChar(x), "ComplexAnswer");
    }
}

static void RawAnswer(SEXP x, struct BindData *data, SEXP call)
{
    R_xlen_t i;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    RawAnswer(CAR(x), data, call);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RawAnswer(XVECTOR_ELT(x, i), data, call);
	break;
    case VECSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RawAnswer(VECTOR_ELT(x, i), data, call);
	break;
    case RAWSXP:
	for (i = 0; i < XLENGTH(x); i++)
	    RAW(data->ans_ptr)[data->ans_length++] = RAW(x)[i];
	break;
    default:
	errorcall(call, _("type '%s' is unimplemented in '%s'"),
		  R_typeToChar(x), "RawAnswer");
    }
}

static SEXP NewBase(SEXP base, SEXP tag)
{
    SEXP ans;
    char *cbuf;
    base = EnsureString(base);
    tag = EnsureString(tag);
    if (*CHAR(base) && *CHAR(tag)) { /* test of length */
	CXXR::RAllocStack::Scope rscope;
	const char *sb = translateCharUTF8(base), *st = translateCharUTF8(tag);
	size_t sz = strlen(st) + strlen(sb) + 1;
	cbuf = (char *) R_AllocStringBuffer(sz, &cbuff);
	snprintf(cbuf, sz + 1, "%s.%s", sb, st);
	/* This isn't strictly correct as we do not know that all the
	   components of the name were correctly translated. */
	ans = mkCharCE(cbuf, CE_UTF8);
    }
    else if (*CHAR(tag)) {
	ans = tag;
    }
    else if (*CHAR(base)) {
	ans = base;
    }
    else ans = R_BlankString;
    return ans;
}

static SEXP NewName(SEXP base, SEXP tag, R_xlen_t seqno, int count)
{
/* Construct a new Name/Tag, using
 *	base.tag
 *	base
 *	base<seqno>	or
 *	tag
 *
 */
    SEXP ans;
    base = EnsureString(base);
    tag = EnsureString(tag);
    if (*CHAR(base)) {
	if (*CHAR(tag)) {
	    CXXR::RAllocStack::Scope rscope;
	    const char
		*sb = translateCharUTF8(base),
		*st = translateCharUTF8(tag);
	    size_t sz = strlen(sb) + strlen(st) + 1;
	    char *cbuf = (char *) R_AllocStringBuffer(sz, &cbuff);
	    snprintf(cbuf, sz + 1, "%s.%s", sb, st);
	    ans = mkCharCE(cbuf, CE_UTF8);
	}
	else if (count == 1)
	    ans = base;
	else {
	    CXXR::RAllocStack::Scope rscope;
	    const char *sb = translateCharUTF8(base);
	    char *cbuf;
	    size_t sz = strlen(sb) + (size_t) IndexWidth(seqno) + 1;
	    cbuf = (char *) R_AllocStringBuffer(sz, &cbuff);
#ifdef LONG_VECTOR_SUPPORT
	    if (seqno > INT_MAX)
		snprintf(cbuf, sz + 1, "%s%.0f", sb, (double) seqno);
	    else
#endif
		snprintf(cbuf, sz + 1, "%s%d", sb, (int) seqno);
	    ans = mkCharCE(cbuf, CE_UTF8);
	}
    }
    else if (*CHAR(tag)) {
	ans = tag;
    } else
	ans = R_BlankString;
    return ans;
}

/* also used in coerce.c */
attribute_hidden SEXP R::ItemName(SEXP names, R_xlen_t i)
{
  /* return  names[i]  if it is a character (>= 1 char), or NULL otherwise */
    if (names != R_NilValue &&
	STRING_ELT(names, i) != R_NilValue &&
	CHAR(STRING_ELT(names, i))[0] != '\0') /* length test */
	return STRING_ELT(names, i);
    else
	return R_NilValue;
}

/* NewExtractNames(v, base, tag, recurse):  For c() and	 unlist().
 * On entry, "base" is the naming component we have acquired by
 * recursing down from above.
 *	If we have a list and we are recursing, we append a new tag component
 * to the base tag (either by using the list tags, or their offsets),
 * and then we do the recursion.
 *	If we have a vector, we just create the tags for each element. */

struct NameData {
 int count;
 R_xlen_t seqno;
 // int firstpos;
};


// count names in (branch) v, recursively if(recurse) :
static void namesCount(SEXP v, int recurse, struct NameData *nameData)
{
    R_xlen_t i, n = xlength(v);
    SEXP names = PROTECT(getAttrib(v, R_NamesSymbol)), namei;

    /* The  "<= 1"  in every for() loop, i.e., for all "vector" cases,
       makes this much faster for large vector 'v'
       _and_ prevents ("almost surely") overflow of nameData->count.
       -->  PR#17284 and PR#17292, with thanks to Suharto Anggono.
    */
    switch(TYPEOF(v)) {
    case NILSXP:
	break;
    case LISTSXP:
	if (recurse) {
	    for (i = 0; i < n && nameData->count <= 1; i++) {
		PROTECT(namei = ItemName(names, i));
		if (namei == R_NilValue)
		    namesCount(CAR(v), recurse, nameData);
		v = CDR(v);
		UNPROTECT(1); /*namei*/
	    }
	    break;
	} /* else fall through */
    case VECSXP:
	if (recurse) {
	    for (i = 0; i < n && nameData->count <= 1; i++) {
		namei = ItemName(names, i);
		if (namei == R_NilValue)
		    namesCount(VECTOR_ELT(v, i), recurse, nameData);
	    }
	    break;
	} /* else fall through */
    case EXPRSXP:
	if (recurse) {
	    for (i = 0; i < n && nameData->count <= 1; i++) {
		namei = ItemName(names, i);
		if (namei == R_NilValue)
		    namesCount(XVECTOR_ELT(v, i), recurse, nameData);
	    }
	    break;
	} /* else fall through */
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	for (i = 0; i < n && nameData->count <= 1; i++)
	    nameData->count++;
	break;
    default:
	nameData->count++;
    }
    UNPROTECT(1); /*names*/
}

static void NewExtractNames(SEXP v, SEXP base, SEXP tag, int recurse,
			     struct BindData *data, struct NameData *nameData)
{
    SEXP names, namei;
    R_xlen_t i, n;
    int savecount=0;
    R_xlen_t saveseqno;

    /* If we have a new tag, we reset the index
     * sequence and create the new basename string : */
    if (tag != R_NilValue) {
	PROTECT(base = NewBase(base, tag));
	saveseqno = nameData->seqno;
	savecount = nameData->count;
	nameData->count = 0;
	namesCount(v, recurse, nameData);
	nameData->seqno = 0;
    }
    else saveseqno = 0;

    n = xlength(v);
    PROTECT(names = getAttrib(v, R_NamesSymbol));

    switch(TYPEOF(v)) {
    case NILSXP:
	break;
    case LISTSXP:
	for (i = 0; i < n; i++) {
	    PROTECT(namei = ItemName(names, i));
	    if (recurse) {
		NewExtractNames(CAR(v), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	    v = CDR(v);
	    UNPROTECT(1); /*namei*/
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    namei = ItemName(names, i);
	    if (recurse) {
		NewExtractNames(VECTOR_ELT(v, i), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	}
	break;
    case EXPRSXP:
	for (i = 0; i < n; i++) {
	    namei = ItemName(names, i);
	    if (recurse) {
		NewExtractNames(XVECTOR_ELT(v, i), base, namei, recurse, data, nameData);
	    }
	    else {
		namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	}
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	for (i = 0; i < n; i++) {
	    namei = ItemName(names, i);
	    namei = NewName(base, namei, ++(nameData->seqno), nameData->count);
	    SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	}
	break;
    default:
	namei = NewName(base, R_NilValue, ++(nameData->seqno), nameData->count);
	SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
    }
    if (tag != R_NilValue) {
	nameData->count = savecount;
	UNPROTECT(1);
    }
    UNPROTECT(1); /*names*/
    nameData->seqno = nameData->seqno + saveseqno;
}

/* Code to extract the optional arguments to c().  We do it this */
/* way, rather than having an interpreted front-end do the job, */
/* because we want to avoid duplication at the top level. */
/* FIXME : is there another possibility? */
static SEXP c_Extract_opt(SEXP ans, bool *recurse, bool *usenames,
			  SEXP call)
{
    SEXP a, n, last = NULL, next = NULL;
    int v, n_recurse = 0, n_usenames = 0;

    for (a = ans; a != R_NilValue; a = next) {
	n = TAG(a);
	next = CDR(a);
	if (n != R_NilValue && pmatch(R_RecursiveSymbol, n, TRUE)) {
	    if (n_recurse++ == 1)
		errorcall(call, "%s", _("repeated formal argument 'recursive'"));
	    if ((v = asLogical(CAR(a))) != NA_INTEGER) {
		*recurse = v;
	    }
	    if (last == NULL)
		ans = next;
	    else
		SETCDR(last, next);
	}
	else if (n != R_NilValue && pmatch(R_UseNamesSymbol, n, TRUE)) {
	    if (n_usenames++ == 1)
		errorcall(call, "%s", _("repeated formal argument 'use.names'"));
	    if ((v = asLogical(CAR(a))) != NA_INTEGER) {
		*usenames = v;
	    }
	    if (last == NULL)
		ans = next;
	    else
		SETCDR(last, next);
	}
	else last = a;
    }
    return ans;
}


/* The change to lists based on dotted pairs has meant that it was
   necessary to separate the internal code for "c" and "unlist".
   Although the functions are quite similar, they operate on very
   different data structures.
*/

/* The major difference between the two functions is that the value of
   the "recursive" argument is FALSE by default for "c" and TRUE for
   "unlist".  In addition, "c" takes ... while "unlist" takes a single
   argument.
*/

/* This is a primitive SPECIALSXP */
attribute_hidden SEXP do_c(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    /* Remove any NULL elements before dispatch so
       they never influence the method implementation. */
    args = R_listCompact(args, /* keep_first */ TRUE);

    /* Attempt method dispatch. */

    /* DispatchOrEval internal generic: c */
    auto dgroup = DispatchAnyOrEval(call, op, "c", args, env, 1, 1);
    //                    ^^^ "Any" => all args are eval()ed and checked => correct multi-arg dispatch
    if (dgroup.first)
        return (dgroup.second);

    return do_c_dflt(call, op, dgroup.second, env);
}

attribute_hidden SEXP do_c_dflt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* Method dispatch has failed; run the default code. */
    /* By default we do not recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    bool
	usenames = true,
	recurse = false;
    /* this was only done for length(args) > 1 prior to 1.5.0,
       _but_ `recursive' might be the only argument */
    PROTECT(args = c_Extract_opt(args, &recurse, &usenames, call));

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a pair based list. */

    struct BindData data;
/*    data.deparse_level = 1;  Initialize this early. */
    data.ans_flags  = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;

    SEXP t, ans;
    for (t = args; t != R_NilValue; t = CDR(t)) {
	if (usenames && !data.ans_nnames) {
	    if (!isNull(TAG(t))) data.ans_nnames = 1;
	    else data.ans_nnames = HasNames(CAR(t));
	}
	AnswerType(CAR(t), recurse, usenames, &data, call);
    }

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive is FALSE) then we must return a list.	Otherwise, */
    /* we use the natural coercion for vector types. */

    SEXPTYPE mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = allocVector(mode, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;
    t = args;

    if (mode == VECSXP || mode == EXPRSXP) {
	if (!recurse) {
	    while (args != R_NilValue) {
		ListAnswer(CAR(args), 0, &data, call);
		args = CDR(args);
	    }
	}
	else ListAnswer(args, recurse, &data, call);
	data.ans_length = xlength(ans);
    }
    else if (mode == STRSXP)
	StringAnswer(args, &data, call);
    else if (mode == CPLXSXP)
	ComplexAnswer(args, &data, call);
    else if (mode == REALSXP)
	RealAnswer(args, &data, call);
    else if (mode == RAWSXP)
	RawAnswer(args, &data, call);
    else if (mode == LGLSXP)
	LogicalAnswer(args, &data, call);
    else /* integer */
	IntegerAnswer(args, &data, call);
    args = t;

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
	PROTECT(data.ans_names = allocVector(STRSXP, data.ans_length));
	data.ans_nnames = 0;
	while (args != R_NilValue) {
	    struct NameData nameData;
	    nameData.seqno = 0;
	    nameData.count = 0;
	    NewExtractNames(CAR(args), R_NilValue, TAG(args), recurse, &data, &nameData);
	    args = CDR(args);
	}
	setAttrib(ans, R_NamesSymbol, data.ans_names);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return ans;
} /* do_c */


attribute_hidden SEXP do_unlist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, t;
    R_xlen_t i, n = 0;
    struct BindData data;

/*    data.deparse_level = 1; */
    checkArity(op, args);

    /* Attempt method dispatch. */

    /* DispatchOrEval internal generic: unlist */
    if (DispatchOrEval(call, op, "unlist", args, env, &ans, 0, 1))
	return ans;

    /* Method dispatch has failed; run the default code. */
    /* By default we recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    PROTECT(args = CAR(ans));
    bool recurse = asBool2(CADR(ans), call);
    bool usenames = asBool2(CADDR(ans), call);
    bool lenient = true; // was (implicitly!) FALSE  up to R 3.0.1

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a generic vector. */

    data.ans_flags  = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;

    if (isNewList(args)) {
	n = xlength(args);
	if (usenames && getAttrib(args, R_NamesSymbol) != R_NilValue)
	    data.ans_nnames = 1;
	for (i = 0; i < n; i++) {
	    if (usenames && !data.ans_nnames)
		data.ans_nnames = HasNames(VECTOR_ELT(args, i));
	    AnswerType(VECTOR_ELT(args, i), recurse, usenames, &data, call);
	}
    }
    else if (isList(args)) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    if (usenames && !data.ans_nnames) {
		if (!isNull(TAG(t))) data.ans_nnames = 1;
		else data.ans_nnames = HasNames(CAR(t));
	    }
	    AnswerType(CAR(t), recurse, usenames, &data, call);
	}
    }
    else {
	UNPROTECT(1);
	if (lenient || isVector(args)) return args;
	else error("%s", _("argument not a list"));
    }

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive is FALSE) then we must return a list.  Otherwise, */
    /* we use the natural coercion for vector types. */

    SEXPTYPE mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = allocVector(mode, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;
    t = args;

    if (mode == VECSXP || mode == EXPRSXP) {
	if (!recurse) {
	    if (TYPEOF(args) == VECSXP)
		for (i = 0; i < n; i++)
		    ListAnswer(VECTOR_ELT(args, i), 0, &data, call);
	    else if (TYPEOF(args) == LISTSXP)
		for ( ; args != R_NilValue; args = CDR(args))
		    ListAnswer(CAR(args), 0, &data, call);
	}
	else ListAnswer(args, recurse, &data, call);
	data.ans_length = xlength(ans);
    }
    else if (mode == STRSXP)
	StringAnswer(args, &data, call);
    else if (mode == CPLXSXP)
	ComplexAnswer(args, &data, call);
    else if (mode == REALSXP)
	RealAnswer(args, &data, call);
    else if (mode == RAWSXP)
	RawAnswer(args, &data, call);
    else if (mode == LGLSXP)
	LogicalAnswer(args, &data, call);
    else /* integer */
	IntegerAnswer(args, &data, call);
    args = t;

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
	struct NameData nameData;
	PROTECT(data.ans_names = allocVector(STRSXP, data.ans_length));
	if (!recurse) {
	    if (TYPEOF(args) == VECSXP) {
		SEXP names = getAttrib(args, R_NamesSymbol);
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.count = 0;
		for (i = 0; i < n; i++) {
		    NewExtractNames(VECTOR_ELT(args, i), R_NilValue,
				    ItemName(names, i), recurse, &data, &nameData);
		}
	    }
	    else if (TYPEOF(args) == LISTSXP) {
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.count = 0;
		while (args != R_NilValue) {
		    NewExtractNames(CAR(args), R_NilValue,
				    TAG(args), recurse, &data, &nameData);
		    args = CDR(args);
		}
	    }
	}
	else {
	    data.ans_nnames = 0;
	    nameData.seqno = 0;
	    nameData.count = 0;
	    NewExtractNames(args, R_NilValue, R_NilValue, recurse, &data, &nameData);
	}
	setAttrib(ans, R_NamesSymbol, data.ans_names);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return ans;
} /* do_unlist */


/* cbind(deparse.level, ...) and rbind(deparse.level, ...) : */
/* This is a special .Internal */
attribute_hidden SEXP do_bind(SEXP call, SEXP op, SEXP args_, SEXP env)
{
    // missing(deparse.level) :
    bool missingDL = (isSymbol(CAR(args_)) && R_missing(CAR(args_), env));
    /* since R 2.2.0: first argument "deparse.level" */
    int deparse_level = asInteger(eval(CAR(args_), env));
    bool tryS4 = (deparse_level >= 0);
    /* NB: negative deparse_level should otherwise be equivalent to deparse_level == 0,
     * --  as cbind(), rbind() below only check for '== 1' and '== 2'
     * {FIXME: methods should do same} */

    /* Lazy evaluation and method dispatch based on argument types are
     * fundamentally incompatible notions.  The results here are
     * ghastly.
     *
     * We build promises to evaluate the arguments and then force the
     * promises so that if we dispatch to a closure below, the closure
     * is still in a position to use "substitute" to get the actual
     * expressions which generated the argument (for naming purposes).
     *
     * The dispatch rule here is as follows:
     *
     * 1) For each argument we get the list of possible class
     *	  memberships from the class attribute.
     *
     * 2) We inspect each class in turn to see if there is an
     *	  applicable method.
     *
     * 3) If we find a method, we use it.  Otherwise, if there was an S4
     *    object among the arguments, we try S4 dispatch; otherwise, we
     *    use the default code.
     *
     * In versions of R up to 3.6.x, we used the following rule instead:
     *
     * 3) If we find an applicable method we make sure that it is
     *	  identical to any method determined for prior arguments.
     *	  If it is identical, we proceed, otherwise we immediately
     *	  drop through to the default code.
     */

    GCStackRoot<> args;
    args = promiseArgs(args_, env);

    const char *generic = ((PRIMVAL(op) == 1) ? "cbind" : "rbind");
    GCStackRoot<> method(R_NilValue);
    bool anyS4 = FALSE;
    char buf[512];

    for (SEXP a = CDR(args); a != R_NilValue && method == R_NilValue; a = CDR(a)) {
	GCStackRoot<> obj;
	obj = eval(CAR(a), env);
	if (tryS4 && !anyS4 && isS4(obj)) anyS4 = TRUE;
	if (isObject(obj)) {
	    GCStackRoot<> classlist;
	    classlist = R_data_class2(obj);
	    for (int i = 0; i < length(classlist); i++) {
		const char *s = translateChar(STRING_ELT(classlist, i));
		if (strlen(generic) + strlen(s) + 2 > 512)
		    error(_("class name too long in '%s'"), generic);
		snprintf(buf, 512, "%s.%s", generic, s);
		SEXP classmethod = R_LookupMethod(install(buf), env, env,
						  R_BaseNamespace);
		if (classmethod != R_UnboundValue) {
		    method = classmethod;
		    break;
		}
	    }
	}
    }

    tryS4 = (anyS4 && (method == R_NilValue));
    if (tryS4) {
	/* use methods:::cbind / rbind */
	method = findFun(install(generic), R_MethodsNamespace);
    }
    if (method != R_NilValue) { // found an S3 or S4 method
	if (missingDL)
	    args = CDR(args); /* discard 'deparse.level' */
	else
	    SET_TAG(args, install("deparse.level")); /* tag 'deparse.level' */
	SEXP ans = applyClosure(call, method, args, env, R_NilValue, true);
	return ans;
    } else
	args = CDR(args); /* discard 'deparse.level' */

    /* Dispatch based on class membership has failed. */
    /* The default code for rbind/cbind.default follows */
    /* First, extract the evaluated arguments. */
    SEXP rho = env;
    struct BindData data;
    data.ans_flags = 0;
    data.ans_length = 0;
    data.ans_nnames = 0;
    for (SEXP t = args; t != R_NilValue; t = CDR(t)) {
	if (TYPEOF(CAR(t)) == SYMSXP) {
	    AnswerType(PRINTNAME(CAR(t)), FALSE, FALSE, &data, call);
	} else 
	    AnswerType(PRVALUE(CAR(t)), FALSE, FALSE, &data, call);
    }

    /* zero-extent matrices shouldn't give NULL, but cbind(NULL) should: */
    if (!data.ans_flags && !data.ans_length) {
	return R_NilValue;
    }

    SEXPTYPE mode = NILSXP;
    if      (data.ans_flags & 512) mode = EXPRSXP;
    else if (data.ans_flags & 256) mode = VECSXP;
    else if (data.ans_flags & 128) mode = STRSXP;
    else if (data.ans_flags &  64) mode = CPLXSXP;
    else if (data.ans_flags &  32) mode = REALSXP;
    else if (data.ans_flags &  16) mode = INTSXP;
    else if (data.ans_flags &	2) mode = LGLSXP;
    else if (data.ans_flags &	1) mode = RAWSXP;

    switch(mode) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case RAWSXP:
	break;
	/* we don't handle expressions: we could, but coercion of a matrix
	   to an expression is not ideal.
	   FIXME?  had  cbind(y ~ x, 1) work using lists, before */
    default:
	error(_("cannot create a matrix from type '%s'"),
	      type2char(mode)); /* mode can only be EXPRSXP here */
    }

    if (PRIMVAL(op) == 1)
        return cbind(call, args, mode, rho, deparse_level);
    else
        return rbind(call, args, mode, rho, deparse_level);
}


static void SetRowNames(SEXP dimnames, SEXP x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 0, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCAR(dimnames, x);
}

static void SetColNames(SEXP dimnames, SEXP x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 1, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCADR(dimnames, x);
}

/*
 * Apparently i % 0 could occur here (PR#2541).  But it should not,
 * as zero-length vectors are ignored and
 * zero-length matrices must have zero columns,
 * unless the result has zero rows, hence is of length zero and no
 * copying will be done.
 */
static SEXP cbind(SEXP call, SEXP args, SEXPTYPE mode, SEXP rho,
		  int deparse_level)
{
    bool have_rnames = false, have_cnames = false, warned = false;
    int nnames, mnames;
    int rows, cols, mrows, lenmin = 0;
    SEXP dn, t, u, result, dims, expr;

    nnames = 0;
    mnames = 0;
    rows = 0;
    cols = 0;
    mrows = -1;

    /* check if we are in the zero-row case */

    for (t = args; t != R_NilValue; t = CDR(t)) {
	if (TYPEOF(CAR(t)) == SYMSXP) {
	    u = PRINTNAME(CAR(t));
	} else 
	    u = PRVALUE(CAR(t));
	if ((isMatrix(u) ? nrows(u) : length(u)) > 0) {
	    lenmin = 1;
	    break;
	}
    }

    /* check conformability of matrix arguments */

    int na = 0;
    for (t = args; t != R_NilValue; t = CDR(t), na++) {
	if (TYPEOF(CAR(t)) == SYMSXP) {
	    u = PRINTNAME(CAR(t));
	} else 
	    u = PRVALUE(CAR(t));
	dims = getAttrib(u, R_DimSymbol);
	if (length(dims) == 2) {
	    if (mrows == -1)
		mrows = INTEGER(dims)[0];
	    else if (mrows != INTEGER(dims)[0])
		error(_("number of rows of matrices must match (see arg %d)"),
		      na + 1);
	    cols += INTEGER(dims)[1];
	}
	else if (length(u) >= lenmin) {
	    rows = std::max(rows, length(u));
	    cols += 1;
	}
    }
    if (mrows != -1) rows = mrows;

    /* Check conformability of vector arguments. -- Look for dimnames. */

    for (t = args, na = 0; t != R_NilValue; t = CDR(t), na++) {
	u = PRVALUE(CAR(t));
	dims = getAttrib(u, R_DimSymbol);
	if (length(dims) == 2) {
	    dn = getAttrib(u, R_DimNamesSymbol);
	    if (length(dn) == 2) {
		if (VECTOR_ELT(dn, 1) != R_NilValue)
		    have_cnames = true;
		if (VECTOR_ELT(dn, 0) != R_NilValue)
		    mnames = mrows;
	    }
	} else {
	    int k = length(u);
	    if (!warned && k > 0 && (k > rows || rows % k)) {
		warned = true;
		warning(_("number of rows of result is not a multiple of vector length (arg %d)"), na + 1);
	    }
	    PROTECT(dn = getAttrib(u, R_NamesSymbol));
	    if (k >= lenmin && (TAG(t) != R_NilValue ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 isSymbol(substitute(CAR(t),R_NilValue)))))
		have_cnames = true;
	    nnames = std::max(nnames, length(dn));
	    UNPROTECT(1); /* dn */
	}
    }
    if (mnames || nnames == rows)
	have_rnames = true;

    PROTECT(result = allocMatrix(mode, rows, cols));
    R_xlen_t n = 0; // index, possibly of long vector

    if (mode == STRSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, STRSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!isMatrix(u)) ? rows : k;
		xcopyStringWithRecycle(result, u, n, idx, k);
		n += idx;
	    }
	}
    }
    else if (mode == VECSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    int umatrix = isMatrix(u); /* might be lost in coercion to VECSXP */
	    if (umatrix || length(u) >= lenmin) {
		/* we cannot assume here that coercion will work */
		switch(TYPEOF(u)) {
		case NILSXP:
		case LANGSXP:
		case RAWSXP:
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
		case VECSXP:
		case LISTSXP:
		{
		    PROTECT(u = coerceVector(u, mode));
		    R_xlen_t k = XLENGTH(u);
		    if (k > 0) {
			R_xlen_t idx = (!umatrix) ? rows : k;
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    SET_VECTOR_ELT(result, n++,
				lazy_duplicate(VECTOR_ELT(u, i1)));
			});
		    }
		    UNPROTECT(1);
		    break;
		}
		default:
		    for (int i = 0; i < rows; i++)
			SET_VECTOR_ELT(result, n++, lazy_duplicate(u));
		}
	    }
	}
    }
    else if (mode == CPLXSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, CPLXSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!isMatrix(u)) ? rows : k;
		xcopyWithRecycle(COMPLEX(result), COMPLEX(u), n, idx, k);
		n += idx;
	    }
	}
    }
    else if (mode == RAWSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, RAWSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (!isMatrix(u)) ? rows : k;
		xcopyWithRecycle(RAW(result), RAW(u), n, idx, k);
		n += idx;
	    }
	}
    }
    else { /* everything else, currently NILSXP, REALSXP, INTSXP, LGLSXP */
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t)); /* type of u can be any of: RAW, LGL, INT, REAL, or NULL */
	    if (isMatrix(u) || length(u) >= lenmin) {
		R_xlen_t k = xlength(u); /* use xlength since u can be NULL */
		R_xlen_t idx = (!isMatrix(u)) ? rows : k;
		if (idx > 0 && TYPEOF(u) <= INTSXP) {
		    /* NILSXP or INT or LGL
		     * taking INTERER(NILSXP) should segfault, and
		     * sometimes does.  But if cbind-ing a NULL, there
		     * are zero rows and u is not a matrix, so nothing to do. */
		    if (mode <= INTSXP) {
			xcopyWithRecycle(INTEGER(result), INTEGER(u),
						n, idx, k);
			n += idx;
		    }
		    else {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    REAL(result)[n++] =
				(INTEGER(u)[i1]) == NA_INTEGER ? NA_REAL : INTEGER(u)[i1];
			});
		    }
		}
		else if (TYPEOF(u) == REALSXP) {
		    xcopyWithRecycle(REAL(result), REAL(u), n, idx, k);
		    n += idx;
		}
		else { /* u is a RAWSXP */
		    /* FIXME: I'm not sure what the author intended when the sequence was
		       defined as raw < logical -- it is possible to represent logical as
		       raw losslessly but not vice versa. So due to the way this was
		       defined the raw -> logical conversion is bound to be lossy .. 
		       But it is not: logicals include NAs, raws do not.
*/
		    if (mode == LGLSXP) {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    LOGICAL(result)[n++] = RAW(u)[i1] ? TRUE : FALSE;
			});
		    } else if (mode == INTSXP) {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    INTEGER(result)[n++] = (unsigned char) RAW(u)[i1];
			});
		    } else if (mode == REALSXP) {
			R_xlen_t i, i1;
			MOD_ITERATE1(idx, k, i, i1, {
			    REAL(result)[n++] = (unsigned char) RAW(u)[i1];
			});
		    } else
			/* not sure this can be reached, but to be safe: */
                        /* `mode` is created in do_bind(), it can only
                         be one of: NILSXP, LGLSXP, INTSXP, REALSXP,
                         CPLXSXP, STRSXP, VECSXP, RAWSXP */
			error(_("cannot create a matrix of type '%s'"),
			      type2char(mode));
		}
	    }
	}
    }

    /* Adjustment of dimnames attributes. */
    if (have_cnames || have_rnames) {
	SEXP nam, tnam,v;
	PROTECT(dn = allocVector(VECSXP, 2));
	if (have_cnames)
	    nam = SET_VECTOR_ELT(dn, 1, allocVector(STRSXP, cols));
	else
	    nam = R_NilValue;	/* -Wall */
	int j = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u)) {
		v = getAttrib(u, R_DimNamesSymbol);

		if (have_rnames &&
		    GetRowNames(dn) == R_NilValue &&
		    GetRowNames(v) != R_NilValue)
		    SetRowNames(dn, lazy_duplicate(GetRowNames(v)));

		/* rbind() does this only  if(have_?names) .. : */
		/* but if tnam is non-null, have_cnames = TRUE: see above */
		tnam = GetColNames(v);
		if (tnam != R_NilValue) {
		    for (int i = 0; i < length(tnam); i++)
			SET_STRING_ELT(nam, j++, STRING_ELT(tnam, i));
		}
		else if (have_cnames) {
		    for (int i = 0; i < ncols(u); i++)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    } else if (length(u) >= lenmin) {
		u = getAttrib(u, R_NamesSymbol);

		if (have_rnames && GetRowNames(dn) == R_NilValue
		    && u != R_NilValue && length(u) == rows)
		    SetRowNames(dn, lazy_duplicate(u));

		if (TAG(t) != R_NilValue)
		    SET_STRING_ELT(nam, j++, PRINTNAME(TAG(t)));
		else {
		    expr = substitute(CAR(t), R_NilValue);
		    if (deparse_level == 1 && isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
			PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(deparse1line(expr, true), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_cnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	setAttrib(result, R_DimNamesSymbol, dn);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return result;
} /* cbind */

static SEXP rbind(SEXP call, SEXP args, SEXPTYPE mode, SEXP rho,
		  int deparse_level)
{
    bool have_rnames = false, have_cnames = false, warned = false;
    int nnames, mnames;
    int rows, cols, mcols, lenmin = 0;
    SEXP dn, t, u, result, dims, expr;

    nnames = 0;
    mnames = 0;
    rows = 0;
    cols = 0;
    mcols = -1;

    /* check if we are in the zero-cols case */

    for (t = args; t != R_NilValue; t = CDR(t)) {
	if (TYPEOF(CAR(t)) == SYMSXP) {
	    u = PRINTNAME(CAR(t));
	} else 
	    u = PRVALUE(CAR(t));
	if ((isMatrix(u) ? ncols(u) : length(u)) > 0) {
	    lenmin = 1;
	    break;
	}
    }

    /* check conformability of matrix arguments */

    int na = 0;
    for (t = args; t != R_NilValue; t = CDR(t), na++) {
	if (TYPEOF(CAR(t)) == SYMSXP) {
	    u = PRINTNAME(CAR(t));
	} else 
	    u = PRVALUE(CAR(t));
	dims = getAttrib(u, R_DimSymbol);
	if (length(dims) == 2) {
	    if (mcols == -1)
		mcols = INTEGER(dims)[1];
	    else if (mcols != INTEGER(dims)[1])
		error(_("number of columns of matrices must match (see arg %d)"),
		      na + 1);
	    rows += INTEGER(dims)[0];
	}
	else if (length(u) >= lenmin){
	    cols = std::max(cols, length(u));
	    rows += 1;
	}
    }
    if (mcols != -1) cols = mcols;

    /* Check conformability of vector arguments. -- Look for dimnames. */

    na = 0;
    for (t = args; t != R_NilValue; t = CDR(t), na++) {
	u = PRVALUE(CAR(t));
	dims = getAttrib(u, R_DimSymbol);
	if (length(dims) == 2) {
	    dn = getAttrib(u, R_DimNamesSymbol);
	    if (length(dn) == 2) {
		if (VECTOR_ELT(dn, 0) != R_NilValue)
		    have_rnames = true;
		if (VECTOR_ELT(dn, 1) != R_NilValue)
		    mnames = mcols;
	    }
	}
	else {
	    int k = length(u);
	    if (!warned && k>0 && (k > cols || cols % k)) {
		warned = true;
		warning(_("number of columns of result is not a multiple of vector length (arg %d)"), na + 1);
	    }
	    PROTECT(dn = getAttrib(u, R_NamesSymbol));
	    if (k >= lenmin && (TAG(t) != R_NilValue ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 isSymbol(substitute(CAR(t),R_NilValue)))))
		have_rnames = true;
	    nnames = std::max(nnames, length(dn));
	    UNPROTECT(1); /* dn */
	}
    }
    if (mnames || nnames == cols)
	have_cnames = true;

    PROTECT(result = allocMatrix(mode, rows, cols));

    R_xlen_t n = 0;

    if (mode == STRSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, STRSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		xfillStringMatrixWithRecycle(result, u, n, rows, idx, cols, k);
		n += idx;
	    }
	}
    }
    else if (mode == VECSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    int umatrix = isMatrix(u), urows = umatrix ? nrows(u) : 1; /* coercing to VECSXP will lose these. PR#15468 */
	    if (umatrix || length(u) >= lenmin) {
		PROTECT(u = coerceVector(u, mode));
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = umatrix ? urows : (k > 0);
		FILL_MATRIX_ITERATE(n, rows, idx, cols, k)
		    SET_VECTOR_ELT(result, didx,
			lazy_duplicate(VECTOR_ELT(u, sidx)));
		n += idx;
		UNPROTECT(1);
	    }
	}
    }
    else if (mode == RAWSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, RAWSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		xfillMatrixWithRecycle(RAW(result), RAW(u), n, rows, idx,
					  cols, k);
		n += idx;
	    }
	}
    }
    else if (mode == CPLXSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, CPLXSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		xfillMatrixWithRecycle(COMPLEX(result), COMPLEX(u), n,
					      rows, idx, cols, k);
		n += idx;
	    }
	}
    }
    else if (mode == INTSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, INTSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		xfillMatrixWithRecycle(INTEGER(result), INTEGER(u), n, rows, idx,
					  cols, k);
		n += idx;
	    }
	}
    }
     else if (mode == LGLSXP) {
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u) || length(u) >= lenmin) {
		u = coerceVector(u, LGLSXP);
		R_xlen_t k = XLENGTH(u);
		R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		xfillMatrixWithRecycle(LOGICAL(result), LOGICAL(u), n, rows, idx,
					  cols, k);
		n += idx;
	    }
	}
     }
     else if (mode == REALSXP) {
	 for (t = args; t != R_NilValue; t = CDR(t)) {
	     u = PRVALUE(CAR(t));
	     if (isMatrix(u) || length(u) >= lenmin) {
		 u = coerceVector(u, REALSXP);
		 R_xlen_t k = XLENGTH(u);
		 R_xlen_t idx = (isMatrix(u)) ? nrows(u) : (k > 0);
		 xfillMatrixWithRecycle(REAL(result), REAL(u), n, rows, idx,
					  cols, k);
		 n += idx;
	     }
	 }
     }
     else { /* everything else, currently NILSXP so do nothing */
     }

    /* Adjustment of dimnames attributes. */
    if (have_rnames || have_cnames) {
	SEXP nam, tnam,v;
	PROTECT(dn = allocVector(VECSXP, 2));
	if (have_rnames)
	    nam = SET_VECTOR_ELT(dn, 0, allocVector(STRSXP, rows));
	else
	    nam = R_NilValue;	/* -Wall */
	int j = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
	    u = PRVALUE(CAR(t));
	    if (isMatrix(u)) {
		v = getAttrib(u, R_DimNamesSymbol);

		if (have_cnames &&
		    GetColNames(dn) == R_NilValue &&
		    GetColNames(v) != R_NilValue)
		    SetColNames(dn, lazy_duplicate(GetColNames(v)));

		/* cbind() doesn't test have_?names BEFORE tnam!=Nil..:*/
		/* but if tnam is non-null, have_rnames = TRUE: see above */
		tnam = GetRowNames(v);
		if (have_rnames) {
		    if (tnam != R_NilValue) {
			for (int i = 0; i < length(tnam); i++)
			    SET_STRING_ELT(nam, j++, STRING_ELT(tnam, i));
		    }
		    else {
			for (int i = 0; i < nrows(u); i++)
				SET_STRING_ELT(nam, j++, R_BlankString);
		    }
		}
	    }
	    else if (length(u) >= lenmin) {
		u = getAttrib(u, R_NamesSymbol);

		if (have_cnames && GetColNames(dn) == R_NilValue
		    && u != R_NilValue && length(u) == cols)
		    SetColNames(dn, lazy_duplicate(u));

		if (TAG(t) != R_NilValue)
		    SET_STRING_ELT(nam, j++, PRINTNAME(TAG(t)));
		else {
		    expr = substitute(CAR(t), R_NilValue);
		    if (deparse_level == 1 && isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
			PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(deparse1line(expr, true), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_rnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	setAttrib(result, R_DimNamesSymbol, dn);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return result;
} /* rbind */
