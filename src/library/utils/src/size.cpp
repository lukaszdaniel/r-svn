/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2025  The R Core Team
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


/* We need to know the sizes of certain internal structures */
#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/GCStackRoot.hpp>
#include <Defn.h>

using namespace R;
using namespace CXXR;

/* A count of the memory used by an object. The following assumptions
   are made.

   1) this is called from user-level, so only some types of objects are
      important.
   2) an object gets charged for all the space allocated on the heap
      and all the nodes specifically due to it, but not for the
      space for its name nor for .Internals it references.
*/

static R_size_t objectsize(SEXP s)
{
    R_size_t cnt = 0, vcnt = 0;
    SEXP tmp;
    bool isVec = false;

    switch (TYPEOF(s)) {
    case NILSXP:
	return(0);
	break;
    case SYMSXP:
	break;
    case BCODESXP:
	R_CheckStack();
	for (bool done = FALSE; ! done; ) {
	    cnt += objectsize(EXPR(s));
	    cnt += objectsize(CODE0(s));
	    cnt += sizeof(RObject);
	    cnt += objectsize(ATTRIB(s));
	    s = CONSTS(s);
	    switch (TYPEOF(s)) {
	    case LISTSXP:
	    case LANGSXP:
	    case BCODESXP:
	    case DOTSXP: break;
	    case NILSXP: return cnt;
	    default: done = TRUE;
	    }
	}
	cnt += objectsize(s);
	break;
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	R_CheckStack();
	for (bool done = false; ! done; ) {
	    cnt += objectsize(TAG(s));
	    cnt += objectsize(CAR(s));
	    cnt += sizeof(RObject);
	    cnt += objectsize(ATTRIB(s));
	    s = CDR(s);
	    switch (TYPEOF(s)) {
	    case LISTSXP:
	    case LANGSXP:
	    case BCODESXP:
	    case DOTSXP: break;
	    case NILSXP: return cnt;
	    default: done = true;
	    }
	}
	cnt += objectsize(s);
	break;
    case CLOSXP:
	R_CheckStack();
	cnt += objectsize(FORMALS(s));
	cnt += objectsize(BODY(s));
	/* no charge for the environment */
	break;
    case ENVSXP:
	R_CheckStack(); /* in case attributes might lead to a cycle */
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
	break;
    case CHARSXP:
	vcnt = BYTE2VEC(length(s)+1);
	isVec = true;
	break;
    case LGLSXP:
    case INTSXP:
	vcnt = INT2VEC(xlength(s));
	isVec = true;
	break;
    case REALSXP:
	vcnt = FLOAT2VEC(xlength(s));
	isVec = true;
	break;
    case CPLXSXP:
	vcnt = COMPLEX2VEC(xlength(s));
	isVec = true;
	break;
    case STRSXP:
	{
	R_CheckStack();
	vcnt = PTR2VEC(xlength(s));
	GCStackRoot<> dup;
	dup = csduplicated(s);
	for (R_xlen_t i = 0; i < xlength(s); i++) {
	    tmp = STRING_ELT(s, i);
	    if(tmp != NA_STRING && !LOGICAL(dup)[i])
		cnt += objectsize(tmp);
	}
	isVec = true;
	break;
	}
    case ANYSXP:
	/* we don't know about these */
	break;
    case EXPRSXP:
	/* Generic Vector Objects */
	R_CheckStack();
	vcnt = PTR2VEC(xlength(s));
	for (R_xlen_t i = 0; i < xlength(s); i++)
	    cnt += objectsize(XVECTOR_ELT(s, i));
	isVec = TRUE;
	break;
    case VECSXP:
	/* Generic Vector Objects */
	R_CheckStack();
	vcnt = PTR2VEC(xlength(s));
	for (R_xlen_t i = 0; i < xlength(s); i++)
	    cnt += objectsize(VECTOR_ELT(s, i));
	isVec = true;
	break;
    case WEAKREFSXP:
	R_CheckStack();
	cnt += objectsize(WEAKREF_FINALIZER(s));
	cnt += objectsize(WEAKREF_KEY(s));
	cnt += objectsize(WEAKREF_VALUE(s));
	break;
    case EXTPTRSXP:
	R_CheckStack();
	cnt += sizeof(void *);  /* the actual pointer */
	cnt += objectsize(EXTPTR_PROT(s));
	cnt += objectsize(EXTPTR_TAG(s));
	break;
    case RAWSXP:
	vcnt = BYTE2VEC(xlength(s));
	isVec = true;
	break;
    case OBJSXP:
	/* Has S4TAG and ATRIB but no CAR nor CDR */
	R_CheckStack();
	cnt += objectsize(S4TAG(s));
	break;
    default:
	UNIMPLEMENTED_TYPE("object.size", s);
    }
    /* add in node space:
       we need to take into account the rounding up that goes on
       in the node classes. */
    if(isVec) {
	cnt += sizeof(VectorBase);
	if (vcnt > 16) cnt += 8*vcnt;
	else if (vcnt > 8) cnt += 128;
	else if (vcnt > 6) cnt += 64;
	else if (vcnt > 4) cnt += 48;
	else if (vcnt > 2) cnt += 32;
	else if (vcnt > 1) cnt += 16;
	else if (vcnt > 0) cnt += 8;
    } else cnt += sizeof(RObject);
    /* add in attributes: these are fake for CHARSXPs */
    if(TYPEOF(s) != CHARSXP) cnt += objectsize(ATTRIB(s));
    return cnt;
}


SEXP objectSize(SEXP x)
{
    return ScalarReal( (double) objectsize(x) );
}
