/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-2014  The R Core Team
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

/** @file dstruct.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <Localization.h>
#include <Defn.h>

using namespace R;

/*  mkPRIMSXP - return a builtin function      */
/*              either "builtin" or "special"  */

/*  The value produced is cached do avoid the need for GC protection
    in cases where a .Primitive is produced by unserializing or
    reconstructed after a package has clobbered the value assigned to
    a symbol in the base package. */

attribute_hidden SEXP R::mkPRIMSXP(int offset, bool evaluate)
{
    SEXP result;
    SEXPTYPE type = evaluate ? BUILTINSXP : SPECIALSXP;
    static SEXP PrimCache = NULL;
    static int FunTabSize = 0;
    
    if (PrimCache == NULL) {
	/* compute the number of entries in R_FunTab */
	while (R_FunTab[FunTabSize].name)
	    FunTabSize++;

	/* allocate and protect the cache */
	PrimCache = allocVector(VECSXP, FunTabSize);
	R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
	error("offset is out of R_FunTab range");

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue) {
	result = allocSExp(type);
	SET_PRIMOFFSET(result, offset);
	SET_VECTOR_ELT(PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
	error("requested primitive type is not consistent with cached value");

    return result;
}

/* This is called by function() {}, where an invalid
   body should be impossible. When called from
   other places (eg do_asfunction) they
   should do this checking in advance */

/*  mkCLOSXP - return a closure with formals f,  */
/*             body b, and environment rho       */

/*attribute_hidden*/ SEXP R::mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    PROTECT(formals);
    PROTECT(body);
    PROTECT(rho);
    SEXP c = allocSExp(CLOSXP);

#ifdef not_used_CheckFormals
    if(isList(formals))
	SET_FORMALS(c, formals);
    else
	error("%s", _("invalid formal arguments for 'function'"));
#else
    SET_FORMALS(c, formals);
#endif
    switch (TYPEOF(body)) {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
	error("%s", _("invalid body argument for 'function'"));
	break;
    default:
	SET_BODY(c, body);
	break;
    }

    if(rho == R_NilValue)
	SET_CLOENV(c, R_GlobalEnv);
    else
	SET_CLOENV(c, rho);
    UNPROTECT(3);
    return c;
}

/* version for the API with more checking */
SEXP R_mkClosure(SEXP formals, SEXP body, SEXP rho)
{
    CheckFormals(formals, "R_mkClosure");
    if (! isEnvironment(rho))
	error("%s", _("invalid environment"));
    return mkCLOSXP(formals, body, rho);
}

/* mkChar - make a character (CHARSXP) variable -- see Rinlinedfuns.h */

/*  mkSYMSXP - return a symsxp with the string  */
/*             name inserted in the name field  */

static bool isDDName(SEXP name)
{
    char *endp;

    const char *buf = CHAR(name);
    if (streqln(buf, "..", 2) && strlen(buf) > 2) {
	buf += 2;
	strtol(buf, &endp, 10); // discard value
	if (*endp != '\0')
	    return 0;
	else
	    return 1;
    }
    return 0;
}

attribute_hidden SEXP R::mkSYMSXP(SEXP name, SEXP value)
{
    PROTECT(name);
    PROTECT(value);
    bool i = isDDName(name);
    SEXP c = allocSExp(SYMSXP);
    SET_PRINTNAME(c, name);
    SET_SYMVALUE(c, value);
    SET_DDVAL(c, i);
    UNPROTECT(2);
    return c;
}
