/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2025  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* This file is installed and available to packages, but only a small
   part of the contents is within the API.  See chapter 6 of 'Writing
   R Extensions'.
 */

/** @file Rinternals.h
 * @brief (As described in 'Writing R Extensions'.)
 */

#ifndef R_INTERNALS_H_
#define R_INTERNALS_H_

#include <CXXR/RTypes.hpp> // for RObject, Rbyte, R_*_t, 

#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>  // includes NORET macro
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h> // for DL_FUNC

/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
#ifndef R_CONFIG_H
# include <Rconfig.h>
#endif

#ifdef LONG_VECTOR_SUPPORT
# define R_PRIdXLEN_T "td"
#else
# define R_PRIdXLEN_T "d"
#endif

#ifndef TESTING_WRITE_BARRIER
# define INLINE_PROTECT
#endif

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.c, and they are serialized.
*/
#include <CXXR/SEXPTYPE.hpp>

/* Define SWITCH_TO_NAMED to use the 'NAMED' mechanism instead of
   reference counting. */
#if ! defined(SWITCH_TO_NAMED) && ! defined(SWITCH_TO_REFCNT)
# define SWITCH_TO_REFCNT
#endif

#if defined(SWITCH_TO_REFCNT) && ! defined(COMPUTE_REFCNT_VALUES)
# define COMPUTE_REFCNT_VALUES
#endif
#if defined(SWITCH_TO_REFCNT) && ! defined(ADJUST_ENVIR_REFCNTS)
# define ADJUST_ENVIR_REFCNTS
#endif

#ifdef __cplusplus
extern "C" {
#endif
#include <R_ext/libextern.h>

#define CHAR(x) R_CHAR(x)
const char *(R_CHAR)(SEXP x);

/* Various tests with macro versions in the internal headers such as Defn.h */
Rboolean (Rf_isNull)(SEXP s);
Rboolean (Rf_isSymbol)(SEXP s);
Rboolean (Rf_isLogical)(SEXP s);
Rboolean (Rf_isReal)(SEXP s);
Rboolean (Rf_isComplex)(SEXP s);
Rboolean (Rf_isRaw)(SEXP s);
Rboolean (Rf_isExpression)(SEXP s);
Rboolean (Rf_isEnvironment)(SEXP s);
Rboolean (Rf_isString)(SEXP s);
Rboolean (Rf_isObject)(SEXP s);

#define IS_SIMPLE_SCALAR(x, type) \
    (IS_SCALAR(x, type) && ATTRIB(x) == R_NilValue)

#ifdef SWITCH_TO_REFCNT
# define INCREMENT_NAMED(x) do { } while (0)
# define DECREMENT_NAMED(x) do { } while (0)
#else
#define NAMEDMAX 7
# define INCREMENT_NAMED(x) do {			\
	SEXP __x__ = (x);				\
	if (NAMED(__x__) != NAMEDMAX)			\
	    SET_NAMED(__x__, NAMED(__x__) + 1);		\
    } while (0)
# define DECREMENT_NAMED(x) do {			    \
	SEXP __x__ = (x);				    \
	int __n__ = NAMED(__x__);			    \
	if (__n__ > 0 && __n__ < NAMEDMAX)		    \
	    SET_NAMED(__x__, __n__ - 1);		    \
    } while (0)
#endif

/* Macros for some common idioms. */
#ifdef USE_RINTERNALS
# ifdef SWITCH_TO_REFCNT
#  define MAYBE_SHARED(x) (REFCNT(x) > 1)
#  define NO_REFERENCES(x) (REFCNT(x) == 0)
# else
#  define MAYBE_SHARED(x) (NAMED(x) > 1)
#  define NO_REFERENCES(x) (NAMED(x) == 0)
# endif
#endif
int (MAYBE_SHARED)(SEXP x);
int (NO_REFERENCES)(SEXP x);

#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define NOT_SHARED(x) (! MAYBE_SHARED(x))

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the internal headers.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (MARK)(SEXP x);
SEXPTYPE (TYPEOF)(SEXP x);
int  (NAMED)(SEXP x);
int  (REFCNT)(SEXP x);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);
void (MARK_NOT_MUTABLE)(SEXP x);
void CLEAR_ATTRIB(SEXP x);
int  (ANY_ATTRIB)(SEXP x);
#define NO_ATTRIB(x) (! ANY_ATTRIB(x))

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x);

/* Vector Access Functions */
int  (LENGTH)(SEXP x);
R_xlen_t (XLENGTH)(SEXP x);
R_xlen_t  (TRUELENGTH)(SEXP x);
int  (IS_LONG_VEC)(SEXP x);
int  (LEVELS)(SEXP x);

int  *(LOGICAL)(SEXP x);
int  *(INTEGER)(SEXP x);
Rbyte *(RAW)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
const int  *(LOGICAL_RO)(SEXP x);
const int  *(INTEGER_RO)(SEXP x);
const Rbyte *(RAW_RO)(SEXP x);
const double *(REAL_RO)(SEXP x);
const Rcomplex *(COMPLEX_RO)(SEXP x);
//SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i);
SEXP (XVECTOR_ELT)(SEXP x, R_xlen_t i);
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_XVECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP *(STRING_PTR)(SEXP x);
const SEXP *(STRING_PTR_RO)(SEXP x);
const SEXP *(VECTOR_PTR_RO)(SEXP x);
NORET SEXP *(VECTOR_PTR)(SEXP x);

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf);
R_xlen_t LOGICAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
R_xlen_t COMPLEX_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf);
R_xlen_t RAW_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf);

/* metadata access */
int INTEGER_IS_SORTED(SEXP x);
int INTEGER_NO_NA(SEXP x);
int REAL_IS_SORTED(SEXP x);
int REAL_NO_NA(SEXP x);
int LOGICAL_IS_SORTED(SEXP x);
int LOGICAL_NO_NA(SEXP x);
int STRING_IS_SORTED(SEXP x);
int STRING_NO_NA(SEXP x);

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	Rf_cons((a), (b))		/* data lists */
#define LCONS(a, b)	Rf_lcons((a), (b))		/* language lists */

SEXP (TAG)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CDDDR)(SEXP e);
SEXP (CD4R)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD3R)(SEXP e);
SEXP (CAD4R)(SEXP e);
SEXP (CAD5R)(SEXP e);
int  (MISSING)(SEXP x);
void SET_TAG(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);

/* AltRep Access Functions */
SEXP (DATA1)(SEXP e);
SEXP (DATA2)(SEXP e);
SEXP (CLASS)(SEXP e);
void SET_DATA1(SEXP x, SEXP y);
void SET_DATA2(SEXP x, SEXP y);
void SET_CLASS(SEXP x, SEXP y);

/* ByteCode Access Functions */
SEXP (CODE0)(SEXP e);
SEXP (CONSTS)(SEXP e);
SEXP (EXPR)(SEXP e);
void SET_CODE(SEXP x, SEXP y);
void SET_CONSTS(SEXP x, SEXP y);
void SET_EXPR(SEXP x, SEXP y);

/* S4Object Access Functions */
SEXP (S4TAG)(SEXP e);
void SET_S4TAG(SEXP x, SEXP y);

/* Closure Access Functions */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int  (RDEBUG)(SEXP x);
int  (RSTEP)(SEXP x);
int  (RTRACE)(SEXP x);
void (SET_RDEBUG)(SEXP x, int v);
void (SET_RSTEP)(SEXP x, int v);
void (SET_RTRACE)(SEXP x, int v);
void SET_FORMALS(SEXP x, SEXP v);
void SET_BODY(SEXP x, SEXP v);
void SET_CLOENV(SEXP x, SEXP v);
SEXP R_mkClosure(SEXP, SEXP, SEXP);
SEXP R_ClosureFormals(SEXP);
SEXP R_ClosureBody(SEXP);
SEXP R_ClosureEnv(SEXP);

/* Symbol Access Functions */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int  (DDVAL)(SEXP x);

/* Environment Access Functions */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int  (ENVFLAGS)(SEXP x);
int  (ENV_RDEBUG)(SEXP x);
void (SET_ENV_RDEBUG)(SEXP x, int v);
SEXP R_ParentEnv(SEXP);

/* Promise Access Functions */
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int  (PRSEEN)(SEXP x);

/* External pointer access macros */
SEXP (EXTPTR_PROT)(SEXP);
SEXP (EXTPTR_TAG)(SEXP);
void *(EXTPTR_PTR)(SEXP);

/* Pointer Protection and Unprotection */
#define PROTECT(s)	Rf_protect(s)
#define UNPROTECT(n)	Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef unsigned int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)

/* Evaluation Environment */
LibExtern SEXP	R_GlobalEnv;	    /* The "global" environment */

LibExtern SEXP  R_EmptyEnv;	    /* An empty environment at the root of the
				    	environment tree */
LibExtern SEXP  R_BaseEnv;	    /* The base environment; formerly R_NilValue */
LibExtern SEXP	R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */

LibExtern SEXP	R_Srcref;           /* Current srcref, for debuggers */

/* Special Values */
// LibExtern SEXP	R_NilValue;	    /* The nil object */
#ifndef R_NilValue
#define R_NilValue NULL
#endif
LibExtern SEXP	R_UnboundValue;	    /* Unbound marker */
LibExtern SEXP	R_MissingArg;	    /* Missing argument marker */
LibExtern SEXP	R_InBCInterpreter;  /* To be found in BC interp. state
				       (marker) */
LibExtern SEXP	R_CurrentExpression; /* Use current expression (marker) */
#ifdef __MAIN__
attribute_hidden
#else
extern
#endif
SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
LibExtern SEXP	R_AsCharacterSymbol;/* "as.character" */
LibExtern SEXP	R_AtsignSymbol;	    /* "@" */
#define R_baseSymbol R_BaseSymbol // <-- backcompatible version of:
LibExtern SEXP	R_BaseSymbol;	// "base"
LibExtern SEXP	R_BraceSymbol;	    /* "{" */
LibExtern SEXP	R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP	R_BracketSymbol;    /* "[" */
LibExtern SEXP	R_ClassSymbol;	    /* "class" */
LibExtern SEXP	R_DeviceSymbol;	    /* ".Device" */
LibExtern SEXP	R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP	R_DimSymbol;	    /* "dim" */
LibExtern SEXP	R_DollarSymbol;	    /* "$" */
LibExtern SEXP	R_DotsSymbol;	    /* "..." */
LibExtern SEXP	R_DoubleColonSymbol;// "::"
LibExtern SEXP	R_DropSymbol;	    /* "drop" */
LibExtern SEXP	R_EvalSymbol;	    /* "eval" */
LibExtern SEXP	R_FunctionSymbol;   /* "function" */
LibExtern SEXP	R_LastvalueSymbol;  /* ".Last.value" */
LibExtern SEXP	R_LevelsSymbol;	    /* "levels" */
LibExtern SEXP	R_ModeSymbol;	    /* "mode" */
LibExtern SEXP	R_NaRmSymbol;	    /* "na.rm" */
LibExtern SEXP	R_NameSymbol;	    /* "name" */
LibExtern SEXP	R_NamesSymbol;	    /* "names" */
LibExtern SEXP	R_NamespaceEnvSymbol;// ".__NAMESPACE__."
LibExtern SEXP	R_PackageSymbol;    /* "package" */
LibExtern SEXP	R_PreviousSymbol;   /* "previous" */
LibExtern SEXP	R_QuoteSymbol;	    /* "quote" */
LibExtern SEXP	R_RowNamesSymbol;   /* "row.names" */
LibExtern SEXP	R_SeedsSymbol;	    /* ".Random.seed" */
LibExtern SEXP	R_SortListSymbol;   /* "sort.list" */
LibExtern SEXP	R_SourceSymbol;	    /* "source" */
LibExtern SEXP	R_SpecSymbol;	// "spec"
LibExtern SEXP	R_TripleColonSymbol;// ":::"
LibExtern SEXP	R_TspSymbol;	    /* "tsp" */

LibExtern SEXP  R_dot_defined;      /* ".defined" */
LibExtern SEXP  R_dot_Method;       /* ".Method" */
LibExtern SEXP	R_dot_packageName;// ".packageName"
LibExtern SEXP  R_dot_target;       /* ".target" */
LibExtern SEXP  R_dot_Generic;      /* ".Generic" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */
LibExtern SEXP	R_BlankScalarString;/* "" as a STRSXP */

/* srcref related functions */
SEXP R_GetCurrentSrcref(int);
SEXP R_GetSrcFilename(SEXP);

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_asChar(SEXP);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);
SEXP Rf_asCharacterFactor(SEXP x);
int Rf_asLogical(SEXP x);
int Rf_asInteger(SEXP x);
double Rf_asReal(SEXP x);
Rcomplex Rf_asComplex(SEXP x);
Rboolean Rf_asRboolean(SEXP x);
bool Rf_asBool(SEXP x);


// also included in R_ext/Rallocators.h
#ifndef R_ALLOCATOR_TYPE
#define R_ALLOCATOR_TYPE
typedef struct R_allocator R_allocator_t;
#endif


/* Other Internally Used Functions, excluding those which are inline-able*/

char *Rf_acopy_string(const char *);
SEXP Rf_alloc3DArray(SEXPTYPE, int, int, int);
SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocLang(int);
SEXP Rf_allocList(int);
SEXP Rf_allocS4Object(void);
SEXP Rf_allocSExp(SEXPTYPE);
// next is not documented but generated by inlined calls to Rf_allocVector
SEXP Rf_allocVector3(SEXPTYPE, R_xlen_t, R_allocator_t *);
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last); // unique.c
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last); // unique.c
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
void Rf_copyMatrix(SEXP, SEXP, Rboolean); // duplicate.c
void Rf_copyListMatrix(SEXP, SEXP, Rboolean); // duplicate.c
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_shallow_duplicate(SEXP);
SEXP R_duplicate_attr(SEXP);
SEXP R_shallow_duplicate_attr(SEXP);
SEXP Rf_lazy_duplicate(SEXP);
/* the next really should not be here and is also in Defn.h */
SEXP Rf_duplicated(SEXP, Rboolean); // duplicate.c
SEXP Rf_eval(SEXP, SEXP);
SEXP Rf_findFun(SEXP, SEXP);
SEXP Rf_findVar(SEXP, SEXP);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, Rboolean); // envir.c
Rboolean R_existsVarInFrame(SEXP, SEXP);
SEXP R_getVar(SEXP, SEXP, Rboolean); // envir.c
SEXP R_getVarEx(SEXP, SEXP, Rboolean, SEXP); // envir.c
void R_removeVarFromFrame(SEXP, SEXP);
SEXP Rf_getAttrib(SEXP, SEXP);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP, SEXP*, SEXP*, const char**, const char**);
SEXP Rf_GetOption(SEXP, SEXP); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP);
int Rf_GetOptionDigits(void);
int Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
SEXP Rf_install(const char *);
SEXP Rf_installChar(SEXP);
SEXP Rf_installNoTrChar(SEXP);
SEXP Rf_installTrChar(SEXP);
Rboolean Rf_isOrdered(SEXP); // util.c
Rboolean Rf_isUnordered(SEXP); // util.c
Rboolean Rf_isUnsorted(SEXP, Rboolean); // sort.c
Rboolean R_isTRUE(SEXP); // util.c
SEXP Rf_lengthgets(SEXP, R_len_t);
SEXP Rf_xlengthgets(SEXP, R_xlen_t);
SEXP R_lsInternal(SEXP, Rboolean); // envir.c
SEXP R_lsInternal3(SEXP, Rboolean, Rboolean); // envir.c
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_namesgets(SEXP, SEXP);
SEXP Rf_mkChar(const char *);
SEXP Rf_mkCharLen(const char *, int);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP); // match.c
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

// ../main/character.c :
typedef enum {Bytes, Chars, Width} nchar_type;
int R_nchar(SEXP string, nchar_type type_,
	    Rboolean allowNA, Rboolean keepNA, const char* msg_name);

SEXP R_ParseEvalString(const char *, SEXP);
SEXP R_ParseString(const char *);
void Rf_PrintValue(SEXP);
SEXP Rf_setAttrib(SEXP, SEXP, SEXP);
void Rf_setVar(SEXP, SEXP, SEXP);
SEXPTYPE Rf_str2type(const char *);
Rboolean Rf_StringBlank(SEXP); // util.c
SEXP Rf_substitute(SEXP,SEXP);
SEXP Rf_topenv(SEXP, SEXP);
const char *Rf_translateChar(SEXP);
const char *Rf_translateCharUTF8(SEXP);
const char *Rf_type2char(SEXPTYPE);
const char *R_typeToChar(SEXP);
#ifdef USE_TYPE2CHAR_2
const char *R_typeToChar2(SEXP, SEXPTYPE);
#endif
SEXP Rf_type2rstr(SEXPTYPE);
SEXP Rf_type2str(SEXPTYPE);
SEXP Rf_type2str_nowarn(SEXPTYPE);

SEXP Rf_protect(SEXP);
void Rf_unprotect(unsigned int);
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
void Rf_unprotect_ptr(SEXP);

SEXP R_tryEval(SEXP, SEXP, int *);
SEXP R_tryEvalSilent(SEXP, SEXP, int *);
SEXP R_GetCurrentEnv(void);

Rboolean Rf_isS4(SEXP); // objects.c
SEXP Rf_asS4(SEXP, Rboolean, int); // objects.c
SEXP Rf_S3Class(SEXP);
int Rf_isBasicClass(const char *);

/* cetype_t is an identifier reseved by POSIX, but it is
   well established as public.  Could remap by a #define though */
typedef enum {
    CE_NATIVE = 0,
    CE_UTF8   = 1,
    CE_LATIN1 = 2,
    CE_BYTES  = 3,
    CE_SYMBOL = 5,
    CE_ANY    =99
} cetype_t;

cetype_t Rf_getCharCE(SEXP);
Rboolean Rf_charIsASCII(SEXP); // sysutils.c
Rboolean Rf_charIsUTF8(SEXP);  // sysutils.c
Rboolean Rf_charIsLatin1(SEXP); // sysutils.c
SEXP Rf_mkCharCE(const char *, cetype_t);
SEXP Rf_mkCharLenCE(const char *, int, cetype_t);
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);

#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

/* Calling a function with arguments evaluated */
SEXP R_forceAndCall(SEXP e, int n, SEXP rho);

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);
// Added in R 3.4.0
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);
DL_FUNC R_ExternalPtrAddrFn(SEXP s);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun); 
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun); // memory,c
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit); // memory,c
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);  // memory,c
void R_RunPendingFinalizers(void);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);  // memory,c
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);  // memory,c
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
SEXP R_BytecodeExpr(SEXP e);

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data); // context.c
SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata);
SEXP R_tryCatch(SEXP (*)(void *), void *,       /* body closure*/
		SEXP,                           /* condition classes (STRSXP) */
		SEXP (*)(SEXP, void *), void *, /* handler closure */
		void (*)(void *), void *);      /* finally closure */
SEXP R_tryCatchError(SEXP (*)(void *), void *,        /* body closure*/
		     SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_withCallingErrorHandler(SEXP (*)(void *), void *, /* body closure*/
			       SEXP (*)(SEXP, void *), void *); /* handler closure */
SEXP R_MakeUnwindCont(void);
NORET void R_ContinueUnwind(SEXP cont);
SEXP R_UnwindProtect(SEXP (*fun)(void *data), void *data,
                     void (*cleanfun)(void *data, Rboolean jump),
                     void *cleandata, SEXP cont); // context.c

/* Environment and Binding Features */
SEXP R_NewEnv(SEXP, int, int);
Rboolean R_IsPackageEnv(SEXP rho); // envir.c
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho); // envir.c
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env); // envir.c
void R_LockBinding(SEXP sym, SEXP env);
void R_unLockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env); // envir.c
Rboolean R_BindingIsActive(SEXP sym, SEXP env); // envir.c
SEXP R_ActiveBindingFunction(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho); // envir.c


/* ../main/errors.c : */
/* needed for R_load/savehistory handling in front ends */
NORET void Rf_errorcall(SEXP, const char *, ...) R_PRINTF_FORMAT(2, 3);
void Rf_warningcall(SEXP, const char *, ...) R_PRINTF_FORMAT(2, 3);
void Rf_warningcall_immediate(SEXP, const char *, ...) R_PRINTF_FORMAT(2, 3);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

typedef void *R_pstream_data_t;

typedef enum {
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format,
    R_pstream_asciihex_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, void *, int);
    SEXP (*OutPersistHookFunc)(SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
#define R_CODESET_MAX 63
struct R_inpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
    char native_encoding[R_CODESET_MAX + 1];
    void *nat2nat_obj;
    void *nat2utf8_obj;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef __cplusplus
void R_InitFileInPStream(R_inpstream_t stream, std::FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, std::FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#else
void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);

/* slot management (in attrib.c) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);
int R_has_slot(SEXP obj, SEXP name);
/* S3-S4 class (inheritance), attrib.c */
SEXP R_S4_extends(SEXP klass, SEXP useTable);

/* class definition, new objects (objects.c) */
SEXP R_do_MAKE_CLASS(const char *what);
SEXP R_getClassDef(const char *what);
SEXP R_getClassDef_R(SEXP what);
Rboolean R_has_methods_attached(void);
Rboolean R_isVirtualClass(SEXP class_def, SEXP env);
Rboolean R_extends(SEXP class1, SEXP class2, SEXP env);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc(SEXP x, const char **valid);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

SEXP R_NewPreciousMSet(int);
void R_PreserveInMSet(SEXP x, SEXP mset);
void R_ReleaseFromMSet(SEXP x, SEXP mset);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.c */
void R_RunExitFinalizers(void);	/* in memory.c */

/* Replacements for popen and system */
#ifdef HAVE_POPEN
# ifdef __cplusplus
std::FILE *R_popen(const char *, const char *);
# else
FILE *R_popen(const char *, const char *);
# endif
#endif
int R_system(const char *);

/* R_compute_identical:  C version of identical() function
   The third arg to R_compute_identical() consists of bitmapped flags for non-default options:
   currently the first 4 default to TRUE, so the flag is set for FALSE values:
   1 = !NUM_EQ              = IDENT_NUM_AS_BITS
   2 = !SINGLE_NA           = IDENT_NA_AS_BITS
   4 = !ATTR_AS_SET         = IDENT_ATTR_BY_ORDER
   8 = !IGNORE_BYTECODE     = IDENT_USE_BYTECODE
  16 = !IGNORE_ENV          = IDENT_USE_CLOENV
  32 = !IGNORE_SRCREF       = IDENT_USE_SRCREF
  64                        = IDENT_EXTPTR_AS_REF
  Default from R's default: 16 = IDENT_USE_CLOENV
*/
#define IDENT_NUM_AS_BITS      1
#define IDENT_NA_AS_BITS       2
#define IDENT_ATTR_BY_ORDER    4
#define IDENT_USE_BYTECODE     8
#define IDENT_USE_CLOENV      16
#define IDENT_USE_SRCREF      32
#define IDENT_EXTPTR_AS_REF   64
Rboolean R_compute_identical(SEXP, SEXP, int); // identical.c

SEXP R_body_no_src(SEXP x); // body(x) without "srcref" etc, ../main/utils.c

/* C version of R's  indx <- order(..., na.last, decreasing) :
   e.g.  arglist = Rf_lang2(x,y)  or  Rf_lang3(x,y,z) 
   In sort.c
*/
void R_orderVector(int *indx, int n, SEXP arglist, Rboolean nalast, Rboolean decreasing);
// C version of R's  indx <- order(x, na.last, decreasing) :
void R_orderVector1(int *indx, int n, SEXP x, Rboolean nalast, Rboolean decreasing);

#ifndef R_NO_REMAP
/* These Rf_ macros are retained for backwards compatibility, but
 * their use is deprecated within rho.  In particular header files
 * should always use the Rf_ prefix explicitly, and not rely on these
 * macros to paste it in.
 */

#define acopy_string		Rf_acopy_string
// #define addMissingVarsToNewEnv	Rf_addMissingVarsToNewEnv
#define alloc3DArray            Rf_alloc3DArray
#define allocArray		Rf_allocArray
// #define allocFormalsList2	Rf_allocFormalsList2
// #define allocFormalsList3	Rf_allocFormalsList3
// #define allocFormalsList4	Rf_allocFormalsList4
// #define allocFormalsList5	Rf_allocFormalsList5
// #define allocFormalsList6	Rf_allocFormalsList6
#define allocLang		Rf_allocLang
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define allocVector3		Rf_allocVector3
#define any_duplicated		Rf_any_duplicated
#define any_duplicated3		Rf_any_duplicated3
// #define applyClosure		Rf_applyClosure
// #define arraySubscript		Rf_arraySubscript
#define asBool			Rf_asBool
#define asChar			Rf_asChar
#define asCharacterFactor	Rf_asCharacterFactor
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
// #define asLogical2		Rf_asLogical2
#define asRboolean		Rf_asRboolean
#define asReal			Rf_asReal
#define asS4			Rf_asS4
#define charIsASCII		Rf_charIsASCII
#define charIsUTF8		Rf_charIsUTF8
#define charIsLatin1		Rf_charIsLatin1
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons			Rf_cons
// #define fixSubset3Args		Rf_fixSubset3Args
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
// #define countContexts		Rf_countContexts
// #define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
// #define DropDims                Rf_DropDims
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define elt			Rf_elt
#define errorcall		Rf_errorcall
#define eval			Rf_eval
// #define ExtractSubset		Rf_ExtractSubset
#define findFun			Rf_findFun
// #define findFun3		Rf_findFun3
// #define findFunctionForBody	Rf_findFunctionForBody
#define findVar			Rf_findVar
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
// #define FixupDigits		Rf_FixupDigits
// #define FixupWidth		Rf_FixupWidth
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getCharCE		Rf_getCharCE
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption1		Rf_GetOption1
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetOption		Rf_GetOption
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define inherits		Rf_inherits
#define install			Rf_install
#define installChar		Rf_installTrChar
#define installNoTrChar		Rf_installNoTrChar
#define installTrChar		Rf_installTrChar
// #define installDDVAL		Rf_installDDVAL
// #define installS3Signature	Rf_installS3Signature
#define isArray			Rf_isArray
#define isBasicClass            Rf_isBasicClass
#define isComplex		Rf_isComplex
#define isDataFrame		Rf_isDataFrame
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
// #define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isSymbol		Rf_isSymbol
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isNumber		Rf_isNumber
#define isObject		Rf_isObject
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isRaw			Rf_isRaw
#define isS4			Rf_isS4
#define isString		Rf_isString
#define isTs			Rf_isTs
// #define isUnmodifiedSpecSym	Rf_isUnmodifiedSpecSym
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lang5			Rf_lang5
#define lang6			Rf_lang6
#define lastElt			Rf_lastElt
#define lazy_duplicate		Rf_lazy_duplicate
#define lcons			Rf_lcons
#ifndef __cplusplus
/* In C++, this macro can play havoc with some standard C++ header
 * files.  Consequently, the alternative approach is taken of defining
 * length as an inline function within the namespace rho (where it can be
 * found via argument-dependent lookup).
 */
#define length(x)		Rf_length(x)
#endif
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define list5			Rf_list5
#define list6			Rf_list6
#define listAppend		Rf_listAppend
#define match			Rf_match
// #define matchE			Rf_matchE
#define mkChar			Rf_mkChar
#define mkCharCE		Rf_mkCharCE
#define mkCharLen		Rf_mkCharLen
#define mkCharLenCE		Rf_mkCharLenCE
#define mkNamed			Rf_mkNamed
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
// #define printwhere		Rf_printwhere
#define protect(x)			Rf_protect(x)
// #define readS3VarsFromFrame	Rf_readS3VarsFromFrame
#define reEnc			Rf_reEnc
// #define reEnc3			Rf_reEnc3
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define ScalarRaw		Rf_ScalarRaw
#define setAttrib		Rf_setAttrib
// #define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define shallow_duplicate	Rf_shallow_duplicate
#define str2type		Rf_str2type
// #define stringSuffix		Rf_stringSuffix
#define stringPositionTr	Rf_stringPositionTr
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define topenv		        Rf_topenv
#define translateChar		Rf_translateChar
// #define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define type2char		Rf_type2char
#define type2rstr		Rf_type2rstr
#define type2str		Rf_type2str
#define type2str_nowarn		Rf_type2str_nowarn
#define unprotect(x)		Rf_unprotect(x)
// #define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets
#endif

/* Defining NO_RINLINEDFUNS disables use to simulate platforms where
   this is not available */
#if ! (defined(CALLED_FROM_DEFN_H) && !defined(__MAIN__) && (defined(COMPILING_R) || ( __GNUC__ && !defined(__INTEL_COMPILER) )) && (defined(COMPILING_R) || !defined(NO_RINLINEDFUNS)))
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the public inlinable functions that are provided in
   Rinlinedfuns.h It is *essential* that these do not appear in any
   other header file, with or without the Rf_ prefix.
*/
SEXP     Rf_allocVector(SEXPTYPE, R_xlen_t);
Rboolean Rf_conformable(SEXP, SEXP);
SEXP	 Rf_elt(SEXP, int);
Rboolean Rf_inherits(SEXP, const char *);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isDataFrame(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNumber(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
Rboolean Rf_isVectorizable(SEXP);
SEXP	 Rf_lang1(SEXP);
SEXP	 Rf_lang2(SEXP, SEXP);
SEXP	 Rf_lang3(SEXP, SEXP, SEXP);
SEXP	 Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lastElt(SEXP);
SEXP	 Rf_lcons(SEXP, SEXP);
R_len_t  Rf_length(SEXP);
SEXP	 Rf_list1(SEXP);
SEXP	 Rf_list2(SEXP, SEXP);
SEXP	 Rf_list3(SEXP, SEXP, SEXP);
SEXP	 Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_listAppend(SEXP, SEXP);
SEXP	 Rf_mkNamed(SEXPTYPE, const char **);
SEXP	 Rf_mkString(const char *);
int	 Rf_nlevels(SEXP);
int	 Rf_stringPositionTr(SEXP, const char *);
SEXP	 Rf_ScalarComplex(Rcomplex);
SEXP	 Rf_ScalarInteger(int);
SEXP	 Rf_ScalarLogical(int);
SEXP	 Rf_ScalarRaw(Rbyte);
SEXP	 Rf_ScalarReal(double);
SEXP	 Rf_ScalarString(SEXP);
R_xlen_t  Rf_xlength(SEXP);
R_xlen_t  (XLENGTH)(SEXP x);
R_xlen_t  (XTRUELENGTH)(SEXP x);
int LENGTH_EX(SEXP x, const char *file, int line);
R_xlen_t XLENGTH_EX(SEXP x);
SEXP (CAR)(SEXP e);
void *(DATAPTR)(SEXP x);
const void *(DATAPTR_RO)(SEXP x);
const void *(DATAPTR_OR_NULL)(SEXP x);
const int *(LOGICAL_OR_NULL)(SEXP x);
const int *(INTEGER_OR_NULL)(SEXP x);
const double *(REAL_OR_NULL)(SEXP x);
const Rcomplex *(COMPLEX_OR_NULL)(SEXP x);
const Rbyte *(RAW_OR_NULL)(SEXP x);
int (INTEGER_ELT)(SEXP x, R_xlen_t i);
double (REAL_ELT)(SEXP x, R_xlen_t i);
int (LOGICAL_ELT)(SEXP x, R_xlen_t i);
Rcomplex (COMPLEX_ELT)(SEXP x, R_xlen_t i);
Rbyte (RAW_ELT)(SEXP x, R_xlen_t i);
SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v);
void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v);
void SET_REAL_ELT(SEXP x, R_xlen_t i, double v);
void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v);
void SET_RAW_ELT(SEXP x, R_xlen_t i, Rbyte v);

/* ALTREP support */
SEXP ALTREP_CLASS(SEXP x);
SEXP R_altrep_data1(SEXP x);
SEXP R_altrep_data2(SEXP x);
void R_set_altrep_data1(SEXP x, SEXP v);
void R_set_altrep_data2(SEXP x, SEXP v);

int *LOGICAL0(SEXP x);
int *INTEGER0(SEXP x);
double *REAL0(SEXP x);
Rcomplex *COMPLEX0(SEXP x);
Rbyte *RAW0(SEXP x);
#endif

int (ALTREP)(SEXP x);

/* ALTREP sorting support */
enum {SORTED_DECR_NA_1ST = -2,
      SORTED_DECR = -1,
      UNKNOWN_SORTEDNESS = INT_MIN, /*INT_MIN is NA_INTEGER! */
      SORTED_INCR = 1,
      SORTED_INCR_NA_1ST = 2,
      KNOWN_UNSORTED = 0};
#define KNOWN_SORTED(sorted) (sorted == SORTED_DECR ||			\
			      sorted == SORTED_INCR ||			\
			      sorted == SORTED_DECR_NA_1ST ||		\
			      sorted == SORTED_INCR_NA_1ST)

#define KNOWN_NA_1ST(sorted) (sorted == SORTED_INCR_NA_1ST ||	\
			      sorted == SORTED_DECR_NA_1ST)

#define KNOWN_INCR(sorted) (sorted == SORTED_INCR ||		\
			    sorted == SORTED_INCR_NA_1ST)

#define KNOWN_DECR(sorted) (sorted == SORTED_DECR ||	\
			    sorted == SORTED_DECR_NA_1ST)


/* ====================== public but non-API entry points =================

   "not documented and subject to change without notice."

   and that includes possible removal.
 */

    
/* Experimental C interface for experimental hash table support

   Not in the API (at least not yet) but declared here to allow some
   experimenting */

/* try to allow some type checking */
typedef struct { SEXP cell; } R_hashtab_type;

/* hash table types */
#define HT_TYPE_IDENTICAL 0
#define HT_TYPE_ADDRESS   1

/* public C interface */
R_hashtab_type R_asHashtable(SEXP h);
SEXP R_HashtabSEXP(R_hashtab_type  h);
int R_isHashtable(SEXP h);
R_hashtab_type R_mkhashtab(int type, int /*K*/);
SEXP R_gethash(R_hashtab_type h, SEXP key, SEXP nomatch);
SEXP R_sethash(R_hashtab_type h, SEXP key, SEXP value);
int R_remhash(R_hashtab_type h, SEXP key);
int R_numhash(R_hashtab_type h);
int R_typhash(R_hashtab_type h);
SEXP R_maphash(R_hashtab_type h, SEXP FUN);
void R_maphashC(R_hashtab_type h, void (*FUN)(SEXP, SEXP, void *), void *data);
void R_clrhash(R_hashtab_type h);


/* Rest of this file
   Stuff that is not API and probably should not be but is getting used.
 */

void (SET_TYPEOF)(SEXP x, SEXPTYPE v); // used by Rcpp and much more
// used by Rcpp (not?), Matrix and more and in an example in R-exts.
void (SET_OBJECT)(SEXP x, int v); // used by Rcpp (not?), Matrix and more
void (SET_S4_OBJECT)(SEXP x); // used by essentials qs redland tibble vectrs
void (UNSET_S4_OBJECT)(SEXP x); // used by essentials vectrs
const char *R_curErrorBuf(void); // used by Rserve
int (IS_SCALAR)(SEXP x, SEXPTYPE type);
Rboolean Rf_psmatch(const char *, const char *, Rboolean); // match.c,  used by rgl and in WRE

/* used in a couple of packages but should probably be dropped 
   error_return: grr rJava rbedrock
   errorcall_return: formerly Runuran(with call=NULL)
*/
				/* match(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error("%s", msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, "%s", msg);   return R_NilValue; }

void (SETLENGTH)(SEXP x, R_xlen_t v); // used by many packages
void (SET_TRUELENGTH)(SEXP x, R_xlen_t v); // used by many packages
void (SETLEVELS)(SEXP x, int v); // used by qs quotedargs

// used by admisc arcpbf b64 box clarabel collapse declared drake fcl rlang this.path
void (SET_ENVFLAGS)(SEXP x, int v);
void SET_FRAME(SEXP x, SEXP v); // used by mmap qs
void SET_ENCLOS(SEXP x, SEXP v); // used by magrittr mmap qs rlang vecrs
void SET_HASHTAB(SEXP x, SEXP v); // used mmap qs

// used by dplyr magrittr quotedargs
void SET_PRENV(SEXP x, SEXP v); 
void SET_PRVALUE(SEXP x, SEXP v);
void SET_PRCODE(SEXP x, SEXP v); 

void *(STDVEC_DATAPTR)(SEXP x); // used by stringfish vctrs vroom

/* Growable vector support */ // formerly used by multbxxc
int (IS_GROWABLE)(SEXP x);
void (SET_GROWABLE_BIT)(SEXP x);

// no longer used
#define BCODE_CONSTS(x) CONSTS(x) // re-enable in Defn.h after removing here
void (SET_NAMED)(SEXP x, int v); // used by fastmatch quotedargs

// R_PromiseExp used in lazyeval precondition rlang tibblify vctrs
#define PREXPR(e) R_PromiseExpr(e)

// formerly used in rlang
#define BODY_EXPR(e) R_ClosureExpr(e)

// used by BioC::matter; might be reasonable to include in API
SEXP R_tryWrap(SEXP);

#ifdef __cplusplus
R_len_t Rf_length(SEXP);
/** @brief Shorthand for Rf_length().
 */
inline R_len_t length(SEXP s)
{
    return Rf_length(s);
}
#endif

#ifdef __cplusplus
} //extern "C"
#endif

#endif /* R_INTERNALS_H_ */
