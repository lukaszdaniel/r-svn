/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2009-2023 The R Core Team.
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

/* This is an experimental facility for printing low-level information
   about R objects. It is not intended to be exposed at the top level
   but rather used as a debugging/inspection facility. It is not
   necessarily complete - feel free to add missing pieces. */

/** @file inspect.cpp
 *
 */

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Localization.h>
#include <CXXR/RAllocStack.hpp>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Print.h>

using namespace R;

/* FIXME: envir.c keeps this private - it should probably go to Defn.h */
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define GLOBAL_FRAME_MASK (1<<15)
#define IS_GLOBAL_FRAME(e) (ENVFLAGS(e) & GLOBAL_FRAME_MASK)

/* based on EncodeEnvironment in  printutils.c */
static void PrintEnvironment(SEXP x)
{
    CXXR::RAllocStack::Scope rscope;
    if (x == R_GlobalEnv)
	Rprintf("<R_GlobalEnv>");
    else if (x == R_BaseEnv)
	Rprintf("<base>");
    else if (x == R_EmptyEnv)
	Rprintf("<R_EmptyEnv>");
    else if (R_IsPackageEnv(x))
	Rprintf("<%s>",
		translateChar(STRING_ELT(R_PackageEnvName(x), 0)));
    else if (R_IsNamespaceEnv(x))
	Rprintf("<namespace:%s>",
		translateChar(STRING_ELT(R_NamespaceEnvSpec(x), 0)));
    else Rprintf("<%p>", (void *)x);
}

/* print prefix */
static void pp(int pre) {
    /* this is sort of silly, I know, but it saves at least some output
       calls (and we can replace \t by spaces if desired) ... */
    while (pre >= 8) { Rprintf("\t"); pre -= 8; }
    while (pre-- > 0) Rprintf(" ");
}

/** @brief Translate RObject's SEXPTYPE enum to a character string
 * 
 * @param v examined RObject
 * 
 * @return name of RObject's type
 */
const char *R::typeName(SEXP v) {
    if (TYPEOF(v) == OBJSXP && IS_S4_OBJECT(v))
	return "S4SXP";
    return sexptype2char(TYPEOF(v)); // -> memory.c
}

static void inspect_tree(int, SEXP, int, int);
static void inspect_subtree(SEXP x, int pre, int deep, int pvec)
{
    inspect_tree(pre + 2, x, deep - 1, pvec);
}

/* pre is the prefix, v is the object to inspect, deep specifies
   the recursion behavior (0 = no recursion, -1 = [sort of] unlimited
   recursion, positive numbers define the maximum recursion depth)
   and pvec is the max. number of vector elements to show  */
static void inspect_tree(int pre, SEXP v, int deep, int pvec) {
    int a = 0;
    pp(pre);
    /* the use of %lx is deliberate because I hate the output of %p,
       but if this causes portability issues, it could be changed.
       SU

       It is invalid on 64-bit Windows.
    */
#ifdef _WIN64
    Rprintf("@%p %02d %s g%dc%d [", (void *)v, TYPEOF(v), typeName(v),
	    NODE_GENERATION(v), NODE_CLASS(v));
#else
    Rprintf("@%lx %02d %s g%dc%d [", (long) v, TYPEOF(v), typeName(v),
	    NODE_GENERATION(v), NODE_CLASS(v));
#endif
    if (OBJECT(v)) { a = 1; Rprintf("OBJ"); }
    if (MARK(v)) { if (a) Rprintf(","); Rprintf("MARK"); a = 1; }
#ifndef SWITCH_TO_REFCNT
    if (NAMED(v)) { if (a) Rprintf(","); Rprintf("NAM(%d)",NAMED(v)); a = 1; }
#endif
    if (REFCNT(v)) { if (a) Rprintf(","); Rprintf("REF(%d)",REFCNT(v)); a = 1; }
    if (RDEBUG(v)) { if (a) Rprintf(","); Rprintf("DBG"); a = 1; }
    if (RTRACE(v)) { if (a) Rprintf(","); Rprintf("TR"); a = 1; }
    if (RSTEP(v)) { if (a) Rprintf(","); Rprintf("STP"); a = 1; }
    if (IS_S4_OBJECT(v)) { if (a) Rprintf(","); Rprintf("S4"); a = 1; }
    if (TYPEOF(v) == SYMSXP || TYPEOF(v) == LISTSXP) {
	if (IS_ACTIVE_BINDING(v)) { if (a) Rprintf(","); Rprintf("AB"); a = 1; }
	if (BINDING_IS_LOCKED(v)) { if (a) Rprintf(","); Rprintf("LCK"); a = 1; }
    }
    if (TYPEOF(v) == ENVSXP) {
	if (FRAME_IS_LOCKED(v)) { if (a) Rprintf(","); Rprintf("LCK"); a = 1; }
	if (IS_GLOBAL_FRAME(v)) { if (a) Rprintf(","); Rprintf("GL"); a = 1; }
    }
    if (TYPEOF(v) == PROMSXP) {
	if (PROMISE_IS_EVALUATED(v)) { if (a) Rprintf(","); Rprintf("VAL"); a = 1; }
    }
    if (LEVELS(v)) { if (a) Rprintf(","); Rprintf("gp=0x%x", LEVELS(v)); a = 1; }
    if (ATTRIB(v) && ATTRIB(v) != R_NilValue) { if (a) Rprintf(","); Rprintf("ATT"); a = 1; }
    Rprintf("] ");

    if (ALTREP(v) && ALTREP_INSPECT(v, pre, deep, pvec, inspect_subtree)) {
	if (ATTRIB(v) && ATTRIB(v) != R_NilValue && TYPEOF(v) != CHARSXP) {
	    pp(pre);
	    Rprintf("ATTRIB:\n");
	    inspect_tree(pre+2, ATTRIB(v), deep, pvec);
	}
	return;
    }

    switch (TYPEOF(v)) {
    case VECSXP: case STRSXP: case LGLSXP: case INTSXP: case RAWSXP:
    case REALSXP: case CPLXSXP: case EXPRSXP:
	Rprintf("(len=%ld, tl=%ld)", (long)XLENGTH(v), (long)XTRUELENGTH(v));
    default:
	break;
    }
    if (TYPEOF(v) == ENVSXP) /* NOTE: this is not a trivial OP since it involves looking up things
				in the environment, so for a low-level debugging we may want to
				avoid it .. */
	PrintEnvironment(v);
    if (TYPEOF(v) == CHARSXP) {
	if (IS_BYTES(v)) Rprintf("[bytes] ");
	if (IS_LATIN1(v)) Rprintf("[latin1] ");
	if (IS_UTF8(v)) Rprintf("[UTF8] ");
	if (IS_ASCII(v)) Rprintf("[ASCII] ");
	if (IS_CACHED(v)) Rprintf("[cached] ");
	Rprintf("\"%s\"", CHAR(v));
	if (v == R_NaString) Rprintf(" [NA]");
    }
    if (TYPEOF(v) == SYMSXP) {
	if (v == R_UnboundValue)
	    Rprintf("[unbound value]");
	else if (v == R_MissingArg)
	    Rprintf("[missing argument]");
	else if (v == R_RestartToken)
	    Rprintf("[restart token]");
	else
	    Rprintf("\"%s\"%s", EncodeChar(PRINTNAME(v)), (SYMVALUE(v) == R_UnboundValue) ? "" : " (has value)");
    }
    if (TYPEOF(v) == EXTPTRSXP)
	Rprintf("<%p>", R_ExternalPtrAddr(v));
    switch (TYPEOF(v)) { /* for native vectors print the first elements in-line */
    case LGLSXP:
	if (XLENGTH(v) > 0) {
		R_xlen_t i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    Rprintf("%s%d", (i > 0) ? "," : " ",
			    (int) LOGICAL_ELT(v, i));
		    i++;
		}
		if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case INTSXP:
	if (XLENGTH(v) > 0) {
	    R_xlen_t i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%d", (i > 0) ? "," : " ", INTEGER_ELT(v, i));
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case RAWSXP:
	if (XLENGTH(v) > 0) {
	    R_xlen_t i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%02x", (i > 0) ? "," : " ", (int) ((unsigned char) RAW(v)[i]));
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    case REALSXP:
	if (XLENGTH(v) > 0) {
	    R_xlen_t i = 0;
	    while (i < XLENGTH(v) && i < pvec) {
		Rprintf("%s%g", (i > 0) ? "," : " ", REAL_ELT(v, i));
		i++;
	    }
	    if (i < XLENGTH(v)) Rprintf(",...");
	}
	break;
    default:
	break;
    }
    Rprintf("\n");
    if (deep) switch (TYPEOF(v)) {
	case VECSXP:
	    {
		R_xlen_t i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    inspect_tree(pre+2, VECTOR_ELT(v, i), deep - 1, pvec);
		    i++;
		}
		if (i < XLENGTH(v)) { pp(pre+2); Rprintf("...\n"); }
	    }
	    break;
	case EXPRSXP:
	    {
		R_xlen_t i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    inspect_tree(pre+2, XVECTOR_ELT(v, i), deep - 1, pvec);
		    i++;
		}
		if (i < XLENGTH(v)) { pp(pre+2); Rprintf("...\n"); }
	    }
	    break;
	case STRSXP:
	    {
		R_xlen_t i = 0;
		while (i < XLENGTH(v) && i < pvec) {
		    inspect_tree(pre+2, STRING_ELT(v, i), deep - 1, pvec);
		    i++;
		}
		if (i < XLENGTH(v)) { pp(pre+2); Rprintf("...\n"); }
	    }
	    break;
	case LISTSXP: case LANGSXP:
	    {
		SEXP lc = v;
		while (lc != R_NilValue) {
		    if (TYPEOF(lc) != LISTSXP && TYPEOF(lc) != LANGSXP) {
			/* a dotted pair */
			pp(pre + 2);
			Rprintf(".\n");
			inspect_tree(pre + 2, lc, deep - 1, pvec);
			break;
		    }
		    if (TAG(lc) && TAG(lc) != R_NilValue) {
			pp(pre + 2);
			Rprintf("TAG: "); /* TAG should be a one-liner since it's a symbol so we don't put it on an extra line*/
			inspect_tree(0, TAG(lc), deep - 1, pvec);
		    }
		    if (BNDCELL_TAG(lc)) {
			SEXPTYPE type = BNDCELL_TAG(lc);
			pp(pre + 2);
			Rprintf("immediate %s: ", sexptype2char(type));
			switch(type) {
			case REALSXP:
			    Rprintf("%g\n", BNDCELL_DVAL(lc));
			    break;
			case INTSXP:
			    if (BNDCELL_IVAL(lc) == NA_INTEGER)
				Rprintf("NA\n");
			    else
				Rprintf("%d\n", BNDCELL_IVAL(lc));
			    break;
			case LGLSXP:
			    if (BNDCELL_LVAL(lc) == NA_LOGICAL)
				Rprintf("NA\n");
			    else if (BNDCELL_LVAL(lc))
				Rprintf("TRUE\n");
			    else
				Rprintf("FALSE\n");
			    break;
			default: error("%s", _("unknown immediate binding type"));
			}
		    }
		    else
			inspect_tree(pre + 2, CAR(lc), deep - 1, pvec);
		    lc = CDR(lc);
		}
	    }
	    break;
	case ENVSXP:
	    if (FRAME(v) != R_NilValue) {
		pp(pre); Rprintf("FRAME:\n");
		inspect_tree(pre+2, FRAME(v), deep - 1, pvec);
	    }
	    pp(pre); Rprintf("ENCLOS:\n");
	    inspect_tree(pre+2, ENCLOS(v), 0, pvec);
	    if (HASHTAB(v) != R_NilValue) {
		pp(pre); Rprintf("HASHTAB:\n");
		inspect_tree(pre+2, HASHTAB(v), deep - 1, pvec);
	    }
	    break;

	case CLOSXP:
	    pp(pre); Rprintf("FORMALS:\n");
	    inspect_tree(pre+2, FORMALS(v), deep - 1, pvec);
	    pp(pre); Rprintf("BODY:\n");
	    inspect_tree(pre+2, BODY(v), deep - 1, pvec);
	    pp(pre); Rprintf("CLOENV:\n");
	    inspect_tree(pre+2, CLOENV(v), 0, pvec);
	    break;
	case EXTPTRSXP:
	    {
		SEXP prot = R_ExternalPtrProtected(v);
		SEXP tag = R_ExternalPtrTag(v);
		if (prot != R_NilValue) {
		    pp(pre); Rprintf("PROTECTED:\n");
		    inspect_tree(pre+2, prot, deep - 1, pvec);
		}
		if (tag != R_NilValue) {
		    pp(pre); Rprintf("TAG:\n");
		    inspect_tree(pre+2, tag, deep - 1, pvec);
		}
	    }
	    break;
	default:
	    break;
	}

    if (ATTRIB(v) && ATTRIB(v) != R_NilValue && TYPEOF(v) != CHARSXP) {
	pp(pre); Rprintf("ATTRIB:\n"); inspect_tree(pre+2, ATTRIB(v), deep, pvec);
    }
}

/* internal API - takes one mandatory argument (object to inspect) and
   two optional arguments (deep and pvec - see above), positional argument
   matching only */
attribute_hidden SEXP do_inspect(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    SEXP obj = CAR(args);
    int deep = -1;
    int pvec = 5;
    if (CDR(args) != R_NilValue) {
	deep = asInteger(CADR(args));
	if (CDDR(args) != R_NilValue)
	    pvec = asInteger(CADDR(args));
    }

    inspect_tree(0, CAR(args), deep, pvec);
    return obj;
}

attribute_hidden SEXP do_address(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_MakeExternalPtr((void *) CAR(args), R_NilValue, R_NilValue);
}

attribute_hidden SEXP do_named(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarInteger(NAMED(CAR(args)));
}

attribute_hidden SEXP do_refcnt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarInteger(REFCNT(CAR(args)));
}

/* the following functions can be use internally and for debugging purposes -
   so far they are not used in any actual code */
attribute_hidden SEXP R_inspect(SEXP x) {
    inspect_tree(0, x, -1, 5);
    return x;
}

attribute_hidden SEXP R_inspect3(SEXP x, int deep, int pvec) {
    inspect_tree(0, x, deep, pvec);
    return x;
}
