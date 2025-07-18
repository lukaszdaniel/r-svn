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
 */

/** @file util.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <memory>
#include <vector>
#include <array>
#include <cctype>		/* for isspace */
#include <cfloat>		/* for DBL_MAX */
#include <R_ext/Minmax.h>
#include <CXXR/RContext.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Print.h>
#include <R_ext/Itermacros.h> /* for ITERATE_BY_REGION */
#include <R_ext/GraphicsEngine.h> // for Rf_AdobeSymbol2utf8

#undef COMPILING_R

#include <Print.h>

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h> // for size_t
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <cstdarg>

using namespace R;
using namespace CXXR;


#include <R_ext/RS.h>
#if defined FC_LEN_T
# include <cstddef> // for FC_LEN_T, usually size_t
#ifdef __cplusplus
extern "C" {
#endif
attribute_hidden
void F77_SUB(rwarnc)(const char *msg, int *nchar, FC_LEN_T msg_len);
NORET attribute_hidden
void F77_SUB(rexitc)(const char *msg, int *nchar, FC_LEN_T msg_len);
#ifdef __cplusplus
} // extern "C"
#endif
#else
#ifdef __cplusplus
extern "C" {
#endif
attribute_hidden
void F77_SUB(rwarnc)(const char *msg, int *nchar);
attribute_hidden
NORET void F77_SUB(rexitc)(const char *msg, int *nchar);
#ifdef __cplusplus
} // extern "C"
#endif
#endif

#include <rlocale.h>

/* Many small functions are included from ../include/Rinlinedfuns.h */

int Rf_nrows(SEXP s) // ~== NROW(.)  in R
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (t == R_NilValue) return LENGTH(s);
	return INTEGER(t)[0];
    }
    else if (isDataFrame(s)) {
	return nrows(CAR(s));
    }
    else error("%s", _("object is not a matrix"));
    return -1;
}


int Rf_ncols(SEXP s) // ~== NCOL(.)  in R
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (t == R_NilValue) return 1;
	if (LENGTH(t) >= 2) return INTEGER(t)[1];
	/* This is a 1D (or possibly 0D array) */
	return 1;
    }
    else if (isDataFrame(s)) {
	return length(s);
    }
    else error("%s", _("object is not a matrix"));
    return -1;/*NOTREACHED*/
}

#ifdef UNUSED
const static char type_msg[] = N_("invalid type passed to internal function\n");

void R::internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    errorcall(call, type_msg);
	else
	    error(type_msg);
    }
}
#endif

SEXP Rf_asChar(SEXP x)
{
	if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
	    int w, d, e, wi, di, ei;
	    char buf[MAXELTSIZE];  /* Probably 100 would suffice */

	    switch (TYPEOF(x)) {
	    case LGLSXP:
		if (LOGICAL(x)[0] == NA_LOGICAL)
		    return NA_STRING;
		if (LOGICAL(x)[0])
		    snprintf(buf, MAXELTSIZE, "TRUE");
		else
		    snprintf(buf, MAXELTSIZE, "FALSE");
		return mkChar(buf);
	    case INTSXP:
		if (INTEGER(x)[0] == NA_INTEGER)
		    return NA_STRING;
		snprintf(buf, MAXELTSIZE, "%d", INTEGER(x)[0]);
		return mkChar(buf);
	    case REALSXP:
		PrintDefaults();
		formatReal(REAL(x), 1, &w, &d, &e, 0);
		return mkChar(EncodeReal0(REAL(x)[0], w, d, e, OutDec));
	    case CPLXSXP:
		PrintDefaults();
		formatComplex(COMPLEX(x), 1, &w, &d, &e, &wi, &di, &ei, 0);
		return mkChar(EncodeComplex(COMPLEX(x)[0], w, d, e, wi, di, ei, OutDec));
	    case STRSXP:
		return STRING_ELT(x, 0);
	    default:
		return NA_STRING;
	    }
	} else if(TYPEOF(x) == CHARSXP) {
	    return x;
	} else if(TYPEOF(x) == SYMSXP)
	    return PRINTNAME(x);
    return NA_STRING;
}

// in Rinternals.h
Rboolean Rf_isUnordered(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && !inherits(s, "ordered"));
}

// in Rinternals.h
Rboolean Rf_isOrdered(SEXP s)
{
    return (Rboolean) (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && inherits(s, "ordered"));
}

// In Rinternals.h
Rboolean R_isTRUE(SEXP x)
{
    if (TYPEOF(x) == LGLSXP && XLENGTH(x) == 1) {
	int val = LOGICAL(x)[0];
	return (Rboolean) (val != NA_LOGICAL && val);
    }
    return FALSE;
}


static const struct {
    const char * const str;
    const SEXPTYPE type;
}
TypeTable[] = {
    { "NULL",		NILSXP	   },  /* real types */
    { "symbol",		SYMSXP	   },
    { "pairlist",	LISTSXP	   },
    { "closure",	CLOSXP	   },
    { "environment",	ENVSXP	   },
    { "promise",	PROMSXP	   },
    { "language",	LANGSXP	   },
    { "special",	SPECIALSXP },
    { "builtin",	BUILTINSXP },
    { "char",		CHARSXP	   },
    { "logical",	LGLSXP	   },
    { "integer",	INTSXP	   },
    { "double",		REALSXP	   }, /*-  "real", for R <= 0.61.x */
    { "complex",	CPLXSXP	   },
    { "character",	STRSXP	   },
    { "...",		DOTSXP	   },
    { "any",		ANYSXP	   },
    { "expression",	EXPRSXP	   },
    { "list",		VECSXP	   },
    { "externalptr",	EXTPTRSXP  },
    { "bytecode",	BCODESXP   },
    { "weakref",	WEAKREFSXP },
    { "raw",		RAWSXP },
    { "S4",		S4SXP },
    { "object",		OBJSXP }, /* == S4SXP */
    /* aliases : */
    { "numeric",	REALSXP	   },
    { "name",		SYMSXP	   },

    { (char *)NULL,	(SEXPTYPE) (-1)	   }
};


SEXPTYPE Rf_str2type(const char *s)
{
    if (!s)
	return (SEXPTYPE) (-1);
    for (int i = 0; TypeTable[i].str; i++) {
	if (streql(s, TypeTable[i].str))
	    return (SEXPTYPE) TypeTable[i].type;
    }

    /* SEXPTYPE is an unsigned int, so the compiler warns us w/o the cast. */
    return (SEXPTYPE) (-1);
}

static struct {
    const char *cstrName;
    SEXP rcharName;
    SEXP rstrName;
    SEXP rsymName;
} Type2Table[MAX_NUM_SEXPTYPE];


static int findTypeInTypeTable(SEXPTYPE t)
 {
    for (int i = 0; TypeTable[i].str; i++)
	if (TypeTable[i].type == t) return i;

    return -1;
}

// called from main.c
attribute_hidden
void R::InitTypeTables(void) {

    /* Type2Table */
    for (int type = 0; type < MAX_NUM_SEXPTYPE; type++) {
	int j = findTypeInTypeTable((SEXPTYPE) type);

	if (j != -1) {
	    const char *cstr = TypeTable[j].str;
	    SEXP rchar = PROTECT(mkChar(cstr));
	    SEXP rstr = ScalarString(rchar);
	    MARK_NOT_MUTABLE(rstr);
	    R_PreserveObject(rstr);
	    SEXP rsym = install(cstr);

	    Type2Table[type].cstrName = cstr;
	    Type2Table[type].rcharName = rchar;
	    Type2Table[type].rstrName = rstr;
	    Type2Table[type].rsymName = rsym;
	    UNPROTECT(1); /* rchar */
	} else {
	    Type2Table[type].cstrName = NULL;
	    Type2Table[type].rcharName = NULL;
	    Type2Table[type].rstrName = NULL;
	    Type2Table[type].rsymName = NULL;
	}
    }
}

SEXP Rf_type2str_nowarn(SEXPTYPE t) /* returns a CHARSXP */
{
    // if (t >= 0 && t < MAX_NUM_SEXPTYPE) { /* branch not really needed */
	SEXP res = Type2Table[t].rcharName;
	if (res != NULL) return res;
    // }
    return R_NilValue;
}

SEXP Rf_type2str(SEXPTYPE t) /* returns a CHARSXP */
{
    SEXP s = type2str_nowarn(t);
    if (s != R_NilValue) {
	return s;
    }
    warning(_("type %d is unimplemented in '%s'"), t, "type2str");
    char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return mkChar(buf);
}

SEXP Rf_type2rstr(SEXPTYPE t) /* returns a STRSXP */
{
    // if (t < MAX_NUM_SEXPTYPE) {
	SEXP res = Type2Table[t].rstrName;
	if (res != NULL) return res;
    // }
    error(_("type %d is unimplemented in '%s'"), t,
	  "type2ImmutableScalarString");
    return R_NilValue; /* for -Wall */
}

const char *Rf_type2char(SEXPTYPE t) /* returns a char* */
{
    // if (t >=0 && t < MAX_NUM_SEXPTYPE) { /* branch not really needed */
	const char * res = Type2Table[t].cstrName;
	if (res != NULL) return res;
    // }
    warning(_("type %d is unimplemented in '%s'"), t, "type2char");
    static char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return buf;
}

#ifdef USE_TYPE2CHAR_2
const char *R_typeToChar2(SEXP x, SEXPTYPE t) {
    return (t != OBJSXP)
	? type2char(t)
	: (IS_S4_OBJECT(x) ? "S4" : "object");
}
#endif

const char *R_typeToChar(SEXP x) {
    // = type2char() but distinguishing {S4, object}
    if(TYPEOF(x) == OBJSXP)
	return IS_S4_OBJECT(x) ? "S4" : "object";
    else
	return type2char(TYPEOF(x));
}

#ifdef UNUSED
NORET SEXP R::type2symbol(SEXPTYPE t)
{
    // if (t >= 0 && t < MAX_NUM_SEXPTYPE) { /* branch not really needed */
	SEXP res = Type2Table[t].rsymName;
	if (res != NULL) return res;
    // }
    error(_("type %d is unimplemented in '%s'"), t, "type2symbol");
}
#endif

NORET attribute_hidden
void R::UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t)
{
    for (int i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    error(_("unimplemented type '%s' in '%s'\n"), TypeTable[i].str, s);
    }
    error(_("unimplemented type (%d) in '%s'\n"), t, s);
}

NORET void R::UNIMPLEMENTED_TYPE(const char *s, SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}

# include <R_ext/Riconv.h>
# include <sys/param.h>
# include <cerrno>


/* Previous versions of R (< 2.3.0) assumed wchar_t was in Unicode
   (and it commonly is).  These functions do not. */
# ifdef WORDS_BIGENDIAN
static constexpr char UCS2ENC[] = "UCS-2BE";
# else
static constexpr char UCS2ENC[] = "UCS-2LE";
# endif


/*
 * out=NULL returns the number of the MBCS chars
 */
/* Note: this does not terminate out, as all current uses are to look
 * at 'out' a wchar at a time, and sometimes just one char.
 */
size_t R::mbcsToUcs2(const char *in, R_ucs2_t *out, int nout, int enc)
{
    void   *cd = NULL ;
    const char *i_buf;
    char *o_buf;
    size_t  i_len, o_len, status, wc_len;
    /* out length */
    wc_len = (enc == CE_UTF8)? utf8towcs(NULL, in, 0) : mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open(UCS2ENC, (enc == CE_UTF8) ? "UTF-8": "")))
	return (size_t) -1;

    i_buf = (char *)in;
    i_len = strlen(in); /* not including terminator */
    o_buf = (char *)out;
    o_len = ((size_t) nout) * sizeof(R_ucs2_t);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);
    int serrno = errno;
    Riconv_close(cd);
    if (status == (size_t)-1) {
	switch(serrno){
	case EINVAL:
	    return (size_t) -2;
	case EILSEQ:
	    return (size_t) -1;
	case E2BIG:
	    break;
	default:
	    errno = EILSEQ;
	    return (size_t) -1;
	}
    }
    return wc_len; /* status would be better? */
}


#include <cwctype>

// non-API but used in the internet module and in packages
Rboolean Rf_isBlankString(const char *s)
{
    if(mbcslocale) {
	wchar_t wc; size_t used; mbstate_t mb_st;
	mbs_init(&mb_st);
	// This does not allow for surrogate pairs, but all blanks are in BMP
	while( (used = Mbrtowc(&wc, s, R_MB_CUR_MAX, &mb_st)) ) {
	    if(!iswspace((wint_t) wc)) return FALSE;
	    s += used;
	}
    } else
	while (*s)
	    if (!isspace((int)*s++)) return FALSE;
    return TRUE;
}

// in Rinternals.h
Rboolean Rf_StringBlank(SEXP x)
{
    if (x == R_NilValue) return TRUE;
    else return (Rboolean) (CHAR(x)[0] == '\0');
}

/* Function to test whether a string is a true value */

// non-API but used in packages
Rboolean Rf_StringTrue(const char *name)
{
    static std::vector<std::string> truenames{"T", "True", "TRUE", "true"};
    std::string str(name);
    return Rboolean(std::any_of(truenames.begin(), truenames.end(), [&str](const std::string &s) { return s == str; }));
}

// non-API but used in packages
Rboolean Rf_StringFalse(const char *name)
{
    static std::vector<std::string> falsenames{"F", "False", "FALSE", "false"};
    std::string str(name);
    return Rboolean(std::any_of(falsenames.begin(), falsenames.end(), [&str](const std::string &s) { return s == str; }));
}

/* used in bind.c and options.c */
attribute_hidden SEXP R::EnsureString(SEXP s)
{
    switch(TYPEOF(s)) {
    case SYMSXP:
	s = PRINTNAME(s);
	break;
    case STRSXP:
	s = STRING_ELT(s, 0);
	break;
    case CHARSXP:
	break;
    case NILSXP:
	s = R_BlankString;
	break;
    default:
	error("%s", _("invalid tag in name extraction"));
    }
    return s;
}

// NB: have  checkArity(a,b) :=  Rf_checkArityCall(a,b,call)
void R::Rf_checkArityCall(SEXP op, SEXP args, SEXP call)
{
    if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args)) {
	/* FIXME: ngettext reguires unsigned long, but %u would seem appropriate */
	if (PRIMINTERNAL(op))
	    error(n_("%d argument passed to .Internal(%s) which requires %d",
		     "%d arguments passed to .Internal(%s) which requires %d",
		     (unsigned long) length(args)),
		  length(args), PRIMNAME(op), PRIMARITY(op));
	else
	    errorcall(call,
		      n_("%d argument passed to '%s' which requires %d",
			"%d arguments passed to '%s' which requires %d",
			(unsigned long) length(args)),
		      length(args), PRIMNAME(op), PRIMARITY(op));
    }
}

attribute_hidden void R::check1arg(SEXP arg, SEXP call, const char *formal)
{
    SEXP tag = TAG(arg);
    if (tag == R_NilValue) return;
    const char *supplied = CHAR(PRINTNAME(tag));
    size_t ns = strlen(supplied);
    if (ns > strlen(formal) || !streqln(supplied, formal, ns))
	errorcall(call, _("supplied argument name '%s' does not match '%s'"),
		  supplied, formal);
}


SEXP Rf_nthcdr(SEXP s, int n)
{
    if (isList(s) || isLanguage(s) || isDataFrame(s) || TYPEOF(s) == DOTSXP ) {
	while( n-- > 0 ) {
	    if (s == R_NilValue)
		error(_("'nthcdr' list shorter than %d"), n);
	    s = CDR(s);
	}
	return s;
    }
    else error("%s", _("'nthcdr' needs a list to CDR down"));
    return R_NilValue;/* for -Wall */
}

/* Destructively removes R_NilValue ('NULL') elements from a pairlist. */
attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::R_listCompact(SEXP s, bool keep_initial) {
    if(!keep_initial)
    // skip initial NULL values
	while (s != R_NilValue && CAR(s) == R_NilValue)
	    s = CDR(s);

    SEXP val = s;
    SEXP prev = s;
    while (s != R_NilValue) {
	s = CDR(s);
	if (CAR(s) == R_NilValue) // skip it
	    SETCDR(prev, CDR(s));
	else
	    prev = s;
    }
    return val;
}


/* This is a primitive (with no arguments) */
attribute_hidden SEXP do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nargs = NA_INTEGER;

    checkArity(op, args);
    for (RCNTXT *cptr = R_GlobalContext; cptr != NULL; cptr = cptr->nextcontext) {
	if ((cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == rho) {
	    nargs = length(cptr->promargs);
	    break;
	}
    }
    return ScalarInteger(nargs);
}


/* formerly used in subscript.c, in Utils.h
      Does not know about long vectors ....
      Commented out 2024-02
attribute_hidden void Rf_setIVector(int * vec, int len, int val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
}
*/


/* unused in R, in Utils.h, may have been used in Rcpp at some point,
      but not any more (as per Nov. 2018).
      Does not know about long vectors ....
      RcppClassic has its own version.
      Commented out 2024-02
attribute_hidden void Rf_setRVector(double * vec, int len, double val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
}
*/

/* unused in R, in Defn.h, formerly remapped in Rinternals.h
      Unused in R.
      Does not know about long vectors ....
      Commented out 2024-02
void R::setSVector(SEXP *vec, int len, SEXP val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
}
*/


attribute_hidden bool R::isFree(SEXP val)
{
    for (SEXP t = R_FreeSEXP; t != R_NilValue; t = CAR(t))
	if (val == t)
	    return true;
    return FALSE;
}


/* Debugging functions (hence the d-prefix). */
/* These are intended to be called interactively from */
/* a debugger such as gdb, so you don't have to remember */
/* the names of the data structure components. */

attribute_hidden int dtype(SEXP q)
{
    return((int)TYPEOF(q));
}


attribute_hidden SEXP dcar(SEXP l)
{
    return(CAR(l));
}


attribute_hidden SEXP dcdr(SEXP l)
{
    return(CDR(l));
}


static void isort_with_index(int *x, int *indx, int n)
{
    int i, j, h, iv, v;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && x[j - h] > v)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}


// body(x) without attributes "srcref", "srcfile", "wholeSrcref" :
// NOTE: Callers typically need  PROTECT(R_body_no_src(.))
attribute_hidden SEXP R_body_no_src(SEXP x) {
    SEXP b = PROTECT(duplicate(BODY_EXPR(x)));
    /* R's removeSource() works *recursively* on the body()
       in  ../library/utils/R/sourceutils.R  but that seems unneeded (?) */
    setAttrib(b, R_SrcrefSymbol, R_NilValue);
    setAttrib(b, R_SrcfileSymbol, R_NilValue);
    setAttrib(b, R_WholeSrcrefSymbol, R_NilValue);
    UNPROTECT(1);
    return b;
}

/* merge(xinds, yinds, all.x, all.y) */
/* xinds, yinds are along x and y rows matching into the (numeric)
   common indices, with 0 for non-matches.

   all.x and all.y are boolean.

   The return value is a list with 4 elements (xi, yi, x.alone, y.alone),
   which are index vectors for rows of x or y.
*/
attribute_hidden SEXP do_merge(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP xi, yi, ansx, ansy, ans;
    int nx = 0, ny = 0, i, j, k, nx_lone = 0, ny_lone = 0;
    int ll = 0/* "= 0" : for -Wall */;
    bool all_x = false, all_y = false;
    int nnx, nny;

    checkArity(op, args);
    xi = CAR(args);
    // NB: long vectors are not supported for input
    if ( !isInteger(xi) || !(nx = LENGTH(xi)) )
	error(_("invalid '%s' argument"), "xinds");
    yi = CADR(args);
    if ( !isInteger(yi) || !(ny = LENGTH(yi)) )
	error(_("invalid '%s' argument"), "yinds");
    if(!LENGTH(ans = CADDR(args)))
	error(_("'%s' argument must be TRUE or FALSE"), "all.x");
    all_x = asLogicalNoNA(ans, "all.x");
    if(!LENGTH(ans = CADDDR(args)))
	error(_("'%s' argument must be TRUE or FALSE"), "all.y");
    all_y = asLogicalNoNA(ans, "all.y");

    /* 0. sort the indices */
    int *ix = (int *) R_alloc((size_t) nx, sizeof(int));
    int *iy = (int *) R_alloc((size_t) ny, sizeof(int));
    for(i = 0; i < nx; i++) ix[i] = i+1;
    for(i = 0; i < ny; i++) iy[i] = i+1;
    isort_with_index(INTEGER(xi), ix, nx);
    isort_with_index(INTEGER(yi), iy, ny);

    /* 1. determine result sizes */
    for (i = 0; i < nx; i++)
	if (INTEGER(xi)[i] > 0) break;
    nx_lone = i;
    for (i = 0; i < ny; i++)
	if (INTEGER(yi)[i] > 0) break;
    ny_lone = i;
    double dnans = 0;
    for (i = nx_lone, j = ny_lone; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	/* the next is not in theory necessary,
	   since we have the common values only */
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	/* printf("i %d nnx %d j %d nny %d\n", i, nnx, j, nny); */
	dnans += ((double)(nnx-i))*(nny-j);
    }
    if (dnans > R_XLEN_T_MAX)
	error("%s", _("number of rows in the result exceeds maximum vector length"));
    R_xlen_t nans = (int) dnans;


    /* 2. allocate and store result components */

    const char *nms[] = {"xi", "yi", "x.alone", "y.alone", ""};
    ans = PROTECT(mkNamed(VECSXP, nms));
    ansx = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);

    if(all_x) {
	SEXP x_lone = allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	for (i = 0, ll = 0; i < nx_lone; i++)
	    INTEGER(x_lone)[ll++] = ix[i];
    }

    if(all_y) {
	SEXP y_lone = allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	for (i = 0, ll = 0; i < ny_lone; i++)
	    INTEGER(y_lone)[ll++] = iy[i];
    }

    for (i = nx_lone, j = ny_lone, k = 0; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	for(int i0 = i; i0 < nnx; i0++)
	    for(int j0 = j; j0 < nny; j0++) {
		INTEGER(ansx)[k]   = ix[i0];
		INTEGER(ansy)[k++] = iy[j0];
	    }
    }

    UNPROTECT(1);
    return ans;
}


/* Functions for getting and setting the working directory. */
#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#endif

/* uses R_alloc */
static SEXP intern_getwd(void)
{
    SEXP rval = R_NilValue;

#ifdef Win32
    {
	DWORD res = GetCurrentDirectoryW(0, NULL);
	if (res > 0) {
	    wchar_t *wbuf = (wchar_t*)R_alloc(res, sizeof(wchar_t));
	    DWORD res1 = GetCurrentDirectoryW(res, wbuf);
	    if (res1 <= 0 || res1 >= res)
		return R_NilValue;
	    size_t needed = wcstoutf8(NULL, wbuf, (size_t)INT_MAX + 2);
	    char *buf = R_alloc(needed + 1, 1);
	    wcstoutf8(buf, wbuf, needed + 1);
	    R_UTF8fixslash(buf);
	    PROTECT(rval = allocVector(STRSXP, 1));
	    SET_STRING_ELT(rval, 0, mkCharCE(buf, CE_UTF8));
	    UNPROTECT(1);
	}
    }
#else
    char buf[4*R_PATH_MAX+1];
    char *res = getcwd(buf, R_PATH_MAX); /* can return NULL */
    if(res) rval = mkString(buf);
#endif
    return(rval);
}

attribute_hidden SEXP do_getwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return(intern_getwd());
}


#if defined(Win32) && defined(_MSC_VER)
# include <direct.h> /* for chdir, via io.h */
#endif
#ifdef TRUE
#undef TRUE
#endif
#ifdef FALSE
#undef FALSE
#endif

attribute_hidden SEXP do_setwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue, wd = R_NilValue;	/* -Wall */

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	error("%s", _("character argument expected"));
    if (STRING_ELT(s, 0) == NA_STRING)
	error("%s", _("missing value is invalid"));

    /* get current directory to return */
    PROTECT(wd = intern_getwd());

#ifdef Win32
    {
	const wchar_t *path = filenameToWchar(STRING_ELT(s, 0), TRUE);
	if(_wchdir(path) < 0)
	    error("%s", _("cannot change working directory"));
    }
#else
    {
	const char *path
	    = R_ExpandFileName(translateCharFP(STRING_ELT(s, 0)));
    if(chdir(path) < 0)
	error("%s", _("cannot change working directory"));
    }
#endif
    UNPROTECT(1); /* wd */
    return(wd);
}

/* remove portion of path before file separator if one exists */

#ifdef Win32
attribute_hidden SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    const char *pp;
    int i, n;

    checkArity(op, args);
    s = CAR(args);
    if (TYPEOF(s) != STRSXP)
	error("%s", _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileNameUTF8(trCharUTF8(STRING_ELT(s, i)));
            size_t ll = strlen(pp);
            /* remove trailing file separator(s) */
            while(ll && (pp[ll-1] == '\\' || pp[ll-1] == '/')) ll--;
            size_t ff = ll;
            /* find start of file part */
            while(ff && (pp[ff-1] != '\\' && pp[ff-1] != '/')) ff--;
            SET_STRING_ELT(ans, i, mkCharLenCE(pp+ff, ll-ff, CE_UTF8));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
attribute_hidden SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    const char fsp = FILESEP[0];
    const char *pp;
    int n;

    checkArity(op, args);
    s = CAR(args);
    if (TYPEOF(s) != STRSXP)
	error("%s", _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for (int i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileName(translateCharFP(STRING_ELT(s, i)));
	    size_t ll = strlen(pp);
	    if (ll > R_PATH_MAX - 1)
		error("%s", _("path too long"));
	    /* remove trailing file separator(s) */
	    while(ll && pp[ll-1] == fsp) ll--;
	    size_t ff = ll;
	    /* find start of file part */
	    while(ff && pp[ff-1] != fsp) ff--;
	    SET_STRING_ELT(ans, i, mkCharLenCE(pp+ff, (int)(ll-ff), CE_NATIVE));
	}
    }
    UNPROTECT(1);
    return ans;
}
#endif

/* remove portion of path after last file separator if one exists, else
   return "."
   */

#ifdef Win32
static SEXP root_dir_on_drive(char d)
{
    char buf[3];

    buf[0] = d;
    buf[1] = ':';
    buf[2] = '/';
    return mkCharLenCE(buf, 3, CE_UTF8);
}

attribute_hidden SEXP do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    char *buf;
    const char *pp;
    int n;

    checkArity(op, args);
    s = CAR(args);
    if (TYPEOF(s) != STRSXP)
	error("%s", _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for (int i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileNameUTF8(trCharUTF8(STRING_ELT(s, i)));
	    size_t ll = strlen(pp);
	    if (ll) {
		buf = (char *)R_alloc(ll + 1, sizeof(char));
		memcpy(buf, pp, ll + 1);
		R_UTF8fixslash(buf);
		/* remove trailing file separator(s) */
		while (ll && buf[ll-1] == '/') ll--;
		if (ll == 2 && buf[1] == ':' && buf[2]) {
		    SET_STRING_ELT(ans, i, root_dir_on_drive(buf[0]));
		    continue;
		}
		if (!ll) { /* only separators, not share */
		    SET_STRING_ELT(ans, i, mkCharLenCE("/", 1, CE_UTF8));
		    continue;
		}
		/* remove file part */
		while(ll && buf[ll-1] != '\\' && buf[ll-1] != '/') ll--;
		if (!ll) { /* only file part, not share */
		    /* FIXME: dirname of D: is ., is this good behavior? */
		    SET_STRING_ELT(ans, i, mkChar("."));
		    continue;
		}
		/* remove separator(s) after directory part */
		while(ll && buf[ll-1] == '/') ll--;
		if (ll == 2 && buf[1] == ':' && buf[2]) {
		    SET_STRING_ELT(ans, i, root_dir_on_drive(buf[0]));
		    continue;
		}
		if (!ll) /* only single part, not share */
		    SET_STRING_ELT(ans, i, mkCharLenCE("/", 1, CE_UTF8));
		else if (ll == 2 && buf[0] == '\\' && buf[1] == '\\'
		                 && buf[2] == '/') {
		    /* network share with extra slashes */
		    SET_STRING_ELT(ans, i, mkCharLenCE("/", 1, CE_UTF8));
		} else
		    SET_STRING_ELT(ans, i, mkCharLenCE(buf, ll, CE_UTF8));
	    } else
		/* empty pathname is invalid, but returned */
		SET_STRING_ELT(ans, i, mkChar(""));
	}
    }
    UNPROTECT(1);
    return ans;
}
#else
attribute_hidden SEXP do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    const char fsp = FILESEP[0];
    const char *pp;
    int n;

    checkArity(op, args);
    s = CAR(args);
    if (TYPEOF(s) != STRSXP)
	error("%s", _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for (int i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileName(translateCharFP(STRING_ELT(s, i)));
	    size_t ll = strlen(pp);
	    if (ll > R_PATH_MAX - 1)
		error("%s", _("path too long"));
	    if (ll) { // svMisc calls this with ""
		/* remove trailing file separator(s) */
		while(ll && pp[ll-1] == fsp) ll--;
		if (!ll) { /* only separators */
		    SET_STRING_ELT(ans, i, mkCharLenCE(&fsp, 1, CE_NATIVE));
		    continue;
		}
		/* remove file part */
		while(ll && pp[ll-1] != fsp) ll--;
		if (!ll) { /* only file part */
		    SET_STRING_ELT(ans, i, mkChar("."));
		    continue;
		}
		/* remove separator(s) after directory part */
		while(ll && pp[ll-1] == fsp) ll--;
		if (!ll) { /* only single part */
		    SET_STRING_ELT(ans, i, mkCharLenCE(&fsp, 1, CE_NATIVE));
		    continue;
		}
		SET_STRING_ELT(ans, i, mkCharLenCE(pp, (int)ll, CE_NATIVE));
	    } else
		/* empty pathname is invalid, but returned */
		SET_STRING_ELT(ans, i, mkChar(""));
	}
    }
    UNPROTECT(1);
    return ans;
}
#endif


#ifndef Win32 /* Windows version is in src/gnuwin32/extra.c */
#ifndef HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif

attribute_hidden SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args), elp;
    int i, n = LENGTH(paths);
    const char *path;
    char abspath[R_PATH_MAX+1];

    checkArity(op, args);
    if (!isString(paths))
	error(_("'%s' must be a character vector"), "path");

    int mustWork = asLogical(CADDR(args)); /* 1, NA_LOGICAL or 0 */

/* Does any platform not have this? */
#ifdef HAVE_REALPATH
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	elp = STRING_ELT(paths, i);
	if (elp == NA_STRING) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	    if (mustWork == 1)
		error("path[%d]=NA", i+1);
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=NA", i+1);
	    continue;
	}
	path = translateCharFP2(elp);
	if (path) {
	    char *res = realpath(path, abspath);
	    if (res)
		SET_STRING_ELT(ans, i, mkChar(abspath));
	    else {
		SET_STRING_ELT(ans, i, elp);
		/* and report the problem */
		if (mustWork == 1)
		    error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
		else if (mustWork == NA_LOGICAL)
		    warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    }
	}
	else if (mustWork == 1) error("%s", _("fatal translation error"));
	else SET_STRING_ELT(ans, i, elp);
    }
#else
    bool OK;
    warning("%s", _("this platform does not have realpath so the results may not be canonical"));
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	elp = STRING_ELT(paths, i);
	if (elp == NA_STRING) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	    if (mustWork == 1)
		error("path[%d]=NA", i+1);
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=NA", i+1);
	    continue;
	}
	path = translateCharFP(elp);
	OK = strlen(path) <= R_PATH_MAX;
	if (OK) {
	    if (path[0] == '/') strncpy(abspath, path, R_PATH_MAX);
	    else {
		OK = getcwd(abspath, R_PATH_MAX) != NULL;
		OK = OK && (strlen(path) + strlen(abspath) + 1 <= R_PATH_MAX);
		if (OK) {strcat(abspath, "/"); strcat(abspath, path);}
	    }
	}
	/* we need to check that this exists */
	if (OK) OK = (access(abspath, 0 /* F_OK */) == 0);
	if (OK) SET_STRING_ELT(ans, i, mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, elp);
	    /* and report the problem */
	    if (mustWork == 1)
		error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#endif
    UNPROTECT(1);
    return ans;
}

#ifdef USE_INTERNAL_MKTIME
#ifdef __cplusplus
extern "C"
#endif
const char *getTZinfo(void)
{
    /* FIXME: use filesystem-agnostic limit?*/
    static char def_tz[R_PATH_MAX+1] = "";
    if (def_tz[0]) return def_tz;

    // call Sys.timezone()
    SEXP expr = PROTECT(install("Sys.timezone"));
    SEXP call = PROTECT(lang1(expr));
    SEXP ans = PROTECT(eval(call, R_GlobalEnv));
    if(TYPEOF(ans) == STRSXP && LENGTH(ans) == 1) {
	SEXP el = STRING_ELT(ans, 0);
	if (el != NA_STRING) {
	    if (strlen(CHAR(el)) + 1 > sizeof(def_tz))
		error("%s", _("time zone specification is too long"));
	    strcpy(def_tz, CHAR(el));
	    // printf("tz is %s\n", CHAR(el));
	    UNPROTECT(3);
	    return def_tz;
	}
    }
    UNPROTECT(3);
    warning("%s", _("system timezone name is unknown: set environment variable TZ"));
    strcpy(def_tz, "unknown");  // code will then use TZDEFAULT, which is "UTC"
    return def_tz;
}
#endif

#endif // not Win32


/* encodeString(x, w, quote, justify) */
attribute_hidden SEXP do_encodeString(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> ans;
    SEXP x, s;
    R_xlen_t i, len;
    int w, quote = 0, justify;
    const char *cs;

    checkArity(op, args);
    x = CAR(args);
    if (TYPEOF(x) != STRSXP)
	error("%s", _("a character vector argument expected"));
    if(isNull(CADR(args))) w = NA_INTEGER;
    else {
	w = asInteger(CADR(args));
	if(w != NA_INTEGER && w < 0)
	    error(_("invalid '%s' value"), "width");
    }

    s = CADDR(args);
    if(LENGTH(s) != 1 || TYPEOF(s) != STRSXP)
	error(_("invalid '%s' value"), "quote");
    cs = translateChar(STRING_ELT(s, 0));
    if(strlen(cs) > 0) quote = cs[0];
    if(strlen(cs) > 1)
	warning("%s", _("only the first character of 'quote' will be used"));
    justify = asInteger(CADDDR(args));
    if(justify == NA_INTEGER || justify < 0 || justify > 3)
	error(_("invalid '%s' value"), "justify");
    if(justify == 3) w = 0;
    bool na = asLogicalNoNA(CAD4R(args), "na.encode");

    len = XLENGTH(x);
    bool findWidth = (w == NA_INTEGER);
    if(findWidth && justify < 3) {
	w  = 0;
	for(i = 0; i < len; i++) {
	    s = STRING_ELT(x, i);
	    if(na || s != NA_STRING)
		w = std::max(w, Rstrlen(s, quote));
	}
	if(quote) w +=2; /* for surrounding quotes */
    }
    ans = duplicate(x);
#ifdef Win32
    bool hasUTF8 = false;
    /* do_encodeString is not printing, but returning a string, it therefore
       must not produce Rgui escapes (do_encodeString may get called as part
       of print dispatch with WinUTF8out being already set to TRUE). */
    if (WinUTF8out) {
	hasUTF8 = true;
	WinUTF8out = FALSE;
    }
    try {
#endif
    for(i = 0; i < len; i++) {
	s = STRING_ELT(x, i);
	if(na || s != NA_STRING) {
	    cetype_t ienc = getCharCE(s);
	    if(ienc == CE_UTF8) {
		const char *ss = EncodeString(s, w-1000000, quote,
					      (Rprt_adj) justify);
		SET_STRING_ELT(ans, i, mkCharCE(ss, ienc));
	    } else {
		const char *ss = EncodeString(s, w, quote, (Rprt_adj) justify);
		SET_STRING_ELT(ans, i, mkChar(ss));
	    }
	}
    }
#ifdef Win32
	if (hasUTF8) WinUTF8out = TRUE;
	} catch (...) {
        if (hasUTF8) WinUTF8out = TRUE;
        throw;
	}
#endif

    return ans;
}

attribute_hidden SEXP do_encoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    R_xlen_t i, n;
    const char *tmp;

    checkArity(op, args);
    x = CAR(args);
    if (TYPEOF(x) != STRSXP)
	error("%s", _("a character vector argument expected"));
    n = XLENGTH(x);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if(IS_BYTES(STRING_ELT(x, i))) tmp = "bytes";
	else if(IS_LATIN1(STRING_ELT(x, i))) tmp = "latin1";
	else if(IS_UTF8(STRING_ELT(x, i))) tmp = "UTF-8";
	else tmp = "unknown";
	SET_STRING_ELT(ans, i, mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

attribute_hidden SEXP do_setencoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, enc, tmp;
    int m;
    R_xlen_t i, n;
    const char *this_;

    checkArity(op, args);
    x = CAR(args);
    if (TYPEOF(x) != STRSXP)
	error("%s", _("a character vector argument expected"));
    enc = CADR(args);
    if (TYPEOF(enc) != STRSXP)
	error("%s", _("a character vector 'value' expected"));
    m = LENGTH(enc);
    if (m == 0)
	error("%s", _("'value' must be of positive length"));
    if (MAYBE_REFERENCED(x)) x = duplicate(x);
    PROTECT(x);
    n = XLENGTH(x);
    for (i = 0; i < n; i++) {
	cetype_t ienc = CE_NATIVE;
	this_ = CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if (streql(this_, "latin1")) ienc = CE_LATIN1;
	else if (streql(this_, "UTF-8")) ienc = CE_UTF8;
	else if (streql(this_, "bytes")) ienc = CE_BYTES;
	tmp = STRING_ELT(x, i);
	if (tmp == NA_STRING) continue;
	if (! ((ienc == CE_LATIN1 && IS_LATIN1(tmp)) ||
	       (ienc == CE_UTF8   && IS_UTF8(tmp))   ||
	       (ienc == CE_BYTES  && IS_BYTES(tmp))  ||
	       (ienc == CE_NATIVE && IS_NATIVE(tmp))))
	    SET_STRING_ELT(x, i, mkCharLenCE(CHAR(tmp), LENGTH(tmp), ienc));
    }
    UNPROTECT(1);
    return x;
}

/* `*s` should point to a string derived from `ref` after `ref` has been
   translated to native encoding.  See `?Encoding` */
attribute_hidden SEXP R::markKnown(const char *s, SEXP ref)
{
    cetype_t ienc = CE_NATIVE;
    if (ENC_KNOWN(ref)) {
	if (known_to_be_latin1) ienc = CE_LATIN1;
	if (known_to_be_utf8) ienc = CE_UTF8;
    }
    return mkCharCE(s, ienc);
}

bool R::strIsASCII(const char *str)
{
    for (const char *p = str; *p; p++)
	if ((unsigned int)*p > 0x7F) return false;
    return true;
}

/* Number of additional bytes */
static constexpr unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

int R::utf8clen(const char c)
{
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}

/* These are misnamed: they convert a single char */
static R_wchar_t utf16toucs(wchar_t high, wchar_t low)
{
    return 0x10000 + ((int) (high & 0x3FF) << 10 ) + (int) (low & 0x3FF);
}

/* Return the low UTF-16 surrogate from a UTF-8 string; assumes all testing has been done. */
static wchar_t utf8toutf16low(const char *s)
{
    return (unsigned int) LOW_SURROGATE_START | ((s[2] & 0x0F) << 6) | (s[3] & 0x3F);
}

attribute_hidden R_wchar_t Rf_utf8toucs32(wchar_t high, const char *s)
{
    return utf16toucs(high, utf8toutf16low(s));
}

/* These return the result in wchar_t.  If wchar_t is 16 bit (e.g. UTF-16LE on Windows)
   only the high surrogate is returned; call utf8toutf16low next. */
size_t R::utf8toucs(wchar_t *wc, const char *s)
{
    unsigned int byte;
    wchar_t local, *w;
    byte = *((unsigned char *)s);
    w = wc ? wc: &local;

    if (byte == 0) {
	*w = (wchar_t) 0;
	return 0;
    } else if (byte < 0x80) {
	*w = (wchar_t) byte;
	return 1;
    } else if (byte < 0xC0) {
	return (size_t)-1;
    } else if (byte < 0xE0) {
	if(strlen(s) < 2) return (size_t)-2;
	if ((s[1] & 0xC0) == 0x80) {
	    *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
	    return 2;
	} else return (size_t)-1;
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return (size_t)-2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = (wchar_t) (((byte & 0x0F) << 12)
			    | (unsigned int) ((s[1] & 0x3F) << 6)
			    | (s[2] & 0x3F));
	    byte = (unsigned int) *w;
	    /* Surrogates range */
	    if(byte >= 0xD800 && byte <= 0xDFFF) return (size_t)-1;
	    if(byte == 0xFFFE || byte == 0xFFFF) return (size_t)-1;
	    return 3;
	} else return (size_t)-1;

    } else if (byte < 0xf8) {
	if(strlen(s) < 4) return (size_t)-2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80) && ((s[3] & 0xC0) == 0x80)) {
	    unsigned int cvalue = (((byte & 0x0F) << 18)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 6)
			| (s[3] & 0x3F));
	    if(sizeof(wchar_t) < 4) /* Assume UTF-16 and return high surrogate.  Users need to call utf8toutf16low next. */
		*w = (wchar_t) ((cvalue - 0x10000) >> 10) | 0xD800;
	    else
		*w = (wchar_t) cvalue;
	    return 4;
	} else return (size_t)-1;
    }
    if(sizeof(wchar_t) < 4) return (size_t)-2;
    /* So now handle 5.6 byte sequences with no testing */
    if (byte < 0xFC) {
	if(strlen(s) < 5) return (size_t)-2;
	*w = (wchar_t) (((byte & 0x0F) << 24)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 12)
			| (unsigned int) ((s[3] & 0x3F) << 6)
			| (s[4] & 0x3F));
	return 5;
    } else {
	if(strlen(s) < 6) return (size_t)-2;
	*w = (wchar_t) (((byte & 0x0F) << 30)
			| (unsigned int) ((s[1] & 0x3F) << 24)
			| (unsigned int) ((s[2] & 0x3F) << 18)
			| (unsigned int) ((s[3] & 0x3F) << 12)
			| (unsigned int) ((s[4] & 0x3F) << 6)
			| (s[5] & 0x3F));
	return 6;
    }
}

/* despite its name this translates to UTF-16 if there are (invalid)
 * UTF-8 codings for surrogates in the input */
size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n)
{
    ssize_t m, res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    m  = (ssize_t) utf8toucs(p, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (res >= (ssize_t) n) break;
	    if (IS_HIGH_SURROGATE(*p)) {
		*(++p) = utf8toutf16low(t);
		res ++;
		if (res >= (ssize_t) n) break;
	    }
	}
    else
	for(t = s; ; t += m) {
	    m  = (ssize_t) utf8toucs(&local, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (IS_HIGH_SURROGATE(local))
		res ++;
	}
    return (size_t) res;
}

attribute_hidden /* would need to be in an installed header if not hidden */
size_t Rf_utf8towcs4(R_wchar_t *wc, const char *s, size_t n)
{
    ssize_t m, res = 0;
    const char *t;
    R_wchar_t *p;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    wchar_t local = L'\0';
	    m  = (ssize_t) utf8toucs(&local, t);
	    /* Needed to write all of R_wchar_t even on Windows where it is bigger
	       than wchar_t */
	    *p = (R_wchar_t) local;
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs32'"), s);
	    if (m == 0) break;
	    if (IS_HIGH_SURROGATE(*p)) *p = utf8toucs32(*p, s);
	    res++;
	    if (res >= (ssize_t) n) break;
	}
    else
	for(t = s; ; t += m) {
	    wchar_t local;
	    m  = (ssize_t) utf8toucs(&local, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs32'"), s);
	    if (m == 0) break;
	    res++;
	}
    return (size_t) res;
}

/* based on pcre.c */
static constexpr std::array<R_wchar_t, 6> utf8_table1 =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static constexpr std::array<unsigned int, 6> utf8_table2 = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

/* s is NULL, or it contains at least n bytes.  Just write a
   terminator if it's not big enough.

   Strangely named: converts from UCS-4 to UTF-8.
*/

static size_t Rwcrtomb32(char *s, R_wchar_t cvalue, size_t n)
{
    size_t i, j;
    if (!n) return 0;
    if (s) *s = 0;    /* Simplifies exit later */
    if(cvalue == 0) return 0;
    for (i = 0; i < utf8_table1.size(); i++)
	if (cvalue <= utf8_table1[i]) break;
    if (i >= n - 1) return 0;  /* need space for terminal null */
    if (s) {
	s += i;
	for (j = i; j > 0; j--) {
	    *s-- = (char) (0x80 | (cvalue & 0x3f));
	    cvalue >>= 6;
	}
	*s = (char) (utf8_table2[i] | cvalue);
    }
    return i + 1;
}

/* On input, wc is a wide string encoded in UTF-16 or UCS-2 or UCS-4.

   s can be a buffer of size n >= 0 chars, or NULL.  If n = 0 or s =
   NULL, nothing is written.

   The return value is the number of chars including the terminating
   null.  If the buffer is not big enough, the result is truncated but
   still null-terminated
*/
attribute_hidden // but used in windlgs
size_t R::wcstoutf8(char *s, const wchar_t *wc, size_t n)
{
    size_t m, res = 0;
    char *t;
    const wchar_t *p;
    if (!n) return 0;
    for(p = wc, t = s; ; p++) {
	if (IS_SURROGATE_PAIR(*p, *(p+1))) {
	    R_wchar_t cvalue =  ((*p & 0x3FF) << 10) + (*(p+1) & 0x3FF) + 0x010000;
	    m = Rwcrtomb32(t, cvalue, n - res);
	    p++;
	} else {
	    if (IS_HIGH_SURROGATE(*p) || IS_LOW_SURROGATE(*p))
		warning(_("unpaired surrogate Unicode point %x"), (unsigned int)*p);
	    m = Rwcrtomb32(t, (R_wchar_t)(*p), n - res);
	}
	if (!m) break;
	res += m;
	if (t)
	    t += m;
    }
    return res + 1;
}

/* convert from R_wchar_t * (UCS-4) */
attribute_hidden
size_t Rf_wcs4toutf8(char *s, const R_wchar_t *wc, size_t n)
{
    size_t m, res=0;
    char *t;
    const R_wchar_t *p;
    if (!n) return 0;
    for(p = wc, t = s; ; p++) {
	m = Rwcrtomb32(t, (*p), n - res);
	if (!m) break;
	res += m;
	if (t)
	    t += m;
    }
    return res + 1;
}

/* A version that reports failure as an error 
 * Exported as Rf_mbrtowc
 */
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return (size_t)0;
    used = mbrtowc(wc, s, n, ps);
    if((int) used < 0) {
	/* This gets called from the menu setup in RGui */
	if (!R_Is_Running) return (size_t)-1;
	/* let's try to print out a readable version */
	R_CheckStack2(4*strlen(s) + 10);
	size_t sz = 4*strlen(s) + 1;
	std::unique_ptr<char[]> tmp = std::make_unique<char[]>(sz);
	char *err = tmp.get();
	char *q;
	const char *p;
	for(p = s, q = err; *p; ) {
	    /* don't do the first to keep ps state straight */
	    if(p > s) used = mbrtowc(NULL, p, n, ps);
	    if(used == 0) break;
	    else if((int) used > 0) {
		memcpy(q, p, used);
		p += used;
		q += used; sz -= used;
		n -= used;
	    } else {
		snprintf(q, sz, "<%02x>", (unsigned char) *p++);
		q += 4; sz -= 4;
		n--;
	    }
	}
	*q = '\0';
	error(_("invalid multibyte string at '%s'"), err);
    }
    return used;
}

/* Truncate a string in place (in native encoding) so that it only contains
   valid multi-byte characters. Has no effect in non-mbcs locales.

   This function may be invoked by the error handler via
   REvprintf->Rvsnprintf_mbcs.  Do not change it unless you are SURE that
   your changes are compatible with the error handling mechanism.

   REvprintf is also used in R_Suicide on Unix.
   */
attribute_hidden
char* R::mbcsTruncateToValid(char *s)
{
    if (!mbcslocale || *s == '\0')
	return s;

    mbstate_t mb_st;
    size_t slen = strlen(s); /* at least 1 */
    size_t goodlen = 0;

    mbs_init(&mb_st);

    if (utf8locale) {
	/* UTF-8 is self-synchronizing so we can look back from the end
	   for the first non-continuation byte */
	goodlen = slen - 1; /* at least 0 */
	/* for char == signed char we assume 2's complement representation */
	while (goodlen && ((s[goodlen] & '\xC0') == '\x80'))
	    --goodlen;
    }
    while(goodlen < slen) {
	size_t res;
	res = mbrtowc(NULL, s + goodlen, slen - goodlen, &mb_st);
	if (res == (size_t) -1 || res == (size_t) -2) {
	    /* strip off all remaining characters */
	    for(;goodlen < slen; goodlen++)
		s[goodlen] = '\0';
	    return s;
	}
	goodlen += res;
    }
    return s;
}

bool R::mbcsValid(const char *str)
{
    return ((int)mbstowcs(NULL, str, 0) >= 0);
}


/* used in src/library/grDevices/src/cairo/cairoFns.c */
#include "valid_utf8.h"
bool R::utf8Valid(const char *str)
{
    return (valid_utf8(str, strlen(str)) == 0);
}

attribute_hidden SEXP do_validUTF8(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    if (!isString(x))
	error(_("invalid '%s' argument"), "x");
    R_xlen_t n = XLENGTH(x);
    SEXP ans = allocVector(LGLSXP, n); // no allocation below
    int *lans = LOGICAL(ans);
    for (R_xlen_t i = 0; i < n; i++)
	lans[i] = utf8Valid(CHAR(STRING_ELT(x, i)));
    return ans;
}

attribute_hidden SEXP do_validEnc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    if (!isString(x))
	error(_("invalid '%s' argument"), "x");
    R_xlen_t n = XLENGTH(x);
    SEXP ans = allocVector(LGLSXP, n); // no allocation below
    int *lans = LOGICAL(ans);
    for (R_xlen_t i = 0; i < n; i++) {
	SEXP p = STRING_ELT(x, i);
	if (IS_BYTES(p) || IS_LATIN1(p)) lans[i] = 1;
	else if (IS_UTF8(p) || utf8locale) lans[i] = utf8Valid(CHAR(p));
	else if(mbcslocale) lans[i] = mbcsValid(CHAR(p));
	else lans[i] = 1;
    }
    return ans;
}


/* MBCS-aware versions of common comparisons.  Only used for ASCII c */
char *Rf_strchr(const char *s, int c)
{
    char *p = (char *)s;
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return (char *) strchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) return p;
	p += used;
    }
    return (char *)NULL;
}

attribute_hidden char *R::Rf_strrchr(const char *s, int c)
{
    char *p = (char *)s, *plast = NULL;
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return (char *) strrchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) plast = p;
	p += used;
    }
    return plast;
}

#ifdef Win32
void R::R_fixslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *p = '/';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '\\') *p = '/';
    /* preserve network shares */
    if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

void R::R_UTF8fixslash(char *s)
{
    char *p = s;

    for (; *p; p++) if (*p == '\\') *p = '/';
    /* preserve network shares */
    if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

void R::R_wfixslash(wchar_t *s)
{
    wchar_t *p = s;

    for (; *p; p++) if (*p == L'\\') *p = L'/';
    /* preserve network shares */
    if(s[0] == L'/' && s[1] == L'/') s[0] = s[1] = L'\\';
}

void R::R_fixbackslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st))) {
	    if(*p == '/') *p = '\\';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '/') *p = '\\';
}

void R::R_wfixbackslash(wchar_t *s)
{
    for (wchar_t *p = s; *p; p++) if (*p == L'/') *p = L'\\';
}

#endif

#ifdef __cplusplus
extern "C"
#endif
#if defined FC_LEN_T
NORET void F77_SUB(rexitc)(const char *msg, int *nchar, FC_LEN_T msg_len)
#else
NORET void F77_SUB(rexitc)(const char *msg, int *nchar)
#endif
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	warning("%s", _("error message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, (size_t) nc);
    buf[nc] = '\0';
    mbcsTruncateToValid(buf);
    error("%s", buf);
}

#ifdef __cplusplus
extern "C"
#endif
#if defined FC_LEN_T
void F77_SUB(rwarnc)(const char *msg, int *nchar, FC_LEN_T msg_len)
#else
void F77_SUB(rwarnc)(const char *msg, int *nchar)
#endif
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	warning("%s", _("warning message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, (size_t) nc);
    buf[nc] = '\0';
    mbcsTruncateToValid(buf);
    warning("%s", buf);
}

#ifdef __cplusplus
extern "C"
#endif
void F77_SUB(rchkusr)(void)
{
    R_CheckUserInterrupt();
}

/* Return a copy of a string using memory from R_alloc.
   NB: caller has to manage R_alloc stack.  Used in platform.c
*/
char *Rf_acopy_string(const char *in)
{
    char *out = NULL;
    size_t len = strlen(in);
    if (len > 0) {
	out = (char *) R_alloc(1 + len, sizeof(char));
	strcpy(out, in);
    } else
	out = (char *) "";
    return out;
}




/* Table from
https://unicode.org/Public/MAPPINGS/VENDORS/ADOBE/symbol.txt
*/

/* Conversion table that DOES use Private Usage Area
 * (should work better with specialised "symbol" fonts)
 */
static constexpr int s2u[224] = {
    0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
    0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
    0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
    0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
    0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
    0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
    0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
    0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
    0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
    0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
    0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
    0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5,
    0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
    0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
    0x2220, 0x2207, 0xF6DA, 0xF6D9, 0xF6DB, 0x220F, 0x221A, 0x22C5,
    0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
    0x25CA, 0x2329, 0xF8E8, 0xF8E9, 0xF8EA, 0x2211, 0xF8EB, 0xF8EC,
    0xF8ED, 0xF8EE, 0xF8EF, 0xF8F0, 0xF8F1, 0xF8F2, 0xF8F3, 0xF8F4,
    0x0020, 0x232A, 0x222B, 0x2320, 0xF8F5, 0x2321, 0xF8F6, 0xF8F7,
    0xF8F8, 0xF8F9, 0xF8FA, 0xF8FB, 0xF8FC, 0xF8FD, 0xF8FE, 0x0020
};

/* Conversion table that does NOT use Private Usage Area (0xF8*)
 * (should work better with fonts that have good Unicode coverage)
 *
 * NOTE that ...
 *   23D0 VERTICAL LINE EXTENTION is used for VERTICAL ARROW EXTENDER
 *   23AF HORIZONTAL LINE EXTENSION is used for HORIZONTAL ARROW EXTENDER
 * ... neither of which may be very good AND ...
 *   23AF HORIZONTAL LINE EXTENSION is also used for RADICAL EXTENDER
 * ... and that is unlikely to be right for BOTH this use AND
 * HORIZONTAL ARROW EXTENDER (if either)
 */
static constexpr int s2unicode[224] = {
    0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
    0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
    0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
    0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
    0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
    0x23AF, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
    0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
    0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
    0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
    0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
    0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
    0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0x23D0, 0x23AF, 0x21B5,
    0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
    0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
    0x2220, 0x2207, 0x00AE, 0x00A9, 0x2122, 0x220F, 0x221A, 0x22C5,
    0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
    0x25CA, 0x2329, 0x00AE, 0x00A9, 0x2122, 0x2211, 0x239B, 0x239C,
    0x239D, 0x23A1, 0x23A2, 0x23A3, 0x23A7, 0x23A8, 0x23A9, 0x23AA,
    0x0020, 0x232A, 0x222B, 0x2320, 0x23AE, 0x2321, 0x239E, 0x239F,
    0x23A0, 0x23A4, 0x23A5, 0x23A6, 0x23AB, 0x23AC, 0x23AD, 0x0020
};

void *Rf_AdobeSymbol2utf8(char *work, const char *c0, size_t nwork,
			  Rboolean usePUA)
{
    const unsigned char *c = (unsigned char *) c0;
    unsigned char *t = (unsigned char *) work;
    while (*c) {
	if (*c < 32) *t++ = ' ';
	else {
	    unsigned int u;
	    if (usePUA) {
		u = (unsigned int) s2u[*c - 32];
	    } else {
		u = (unsigned int) s2unicode[*c - 32];
	    }
	    if (u < 128) *t++ = (unsigned char) u;
	    else if (u < 0x800) {
		*t++ = (unsigned char) (0xc0 | (u >> 6));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
	    } else {
		*t++ = (unsigned char) (0xe0 | (u >> 12));
		*t++ = (unsigned char) (0x80 | ((u >> 6) & 0x3f));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
	    }
	}
	if (t+6 > (unsigned char *)(work + nwork)) break;
	c++;
    }
    *t = '\0';
    return (char*) work;
}

/* Convert UTF8 symbol back to single-byte symbol
 * ASSUME fontface == 5 and 'str' is UTF8, i.e., we are dealing with
 * a UTF8 string that has been through Rf_AdobeSymbol2utf8(usePUA=TRUE)
 * (or through Rf_AdobeSymbol2ucs2() then Rf_ucstoutf8())
 * i.e., we are dealing with CE_UTF8 string that has come from CE_SYMBOL string.
*/
int Rf_utf8toAdobeSymbol(char *out, const char *in) {
    int i, j, k, used, tmp, nc = 0, found;
    int *symbolint;
    const char *s = in;
    const char *p = in;
    for ( ; *p; p += utf8clen(*p)) nc++;
    symbolint = (int *) R_alloc(nc, sizeof(int));
    for (i = 0, j = 0; i < nc; i++, j++) {
	/* Convert UTF8 to int */
	used = mbrtoint(&tmp, s);
	if (used < 0)
	    error("%s", _("invalid UTF-8 string"));
	symbolint[j] = tmp;
	found = 0;
	/* Convert int to CE_SYMBOL char */
	for (k = 0; k < 224; k++) {
	    if (symbolint[j] == s2u[k]) {
		out[j] = (char)(k + 32);
		found = 1;
	    }
	    if (found) break;
	}
	if (!found)
	    error("%s", _("Conversion failed"));
	s += used;
    }
    out[nc] = '\0';
    return nc;
}

const char* Rf_utf8Toutf8NoPUA(const char *in)
{
    int i, j, used, tmp;
    /* At least enough because assumes each incoming char only one byte */
    int nChar = 3*(int)strlen(in) + 1;
    char *result = R_alloc(nChar, sizeof(char));
    const char *s = in;
    char *p = result;
    for (i = 0; i < nChar; i++) {
	/* Convert UTF8 char to int */
	used = mbrtoint(&tmp, s);
	/* Only re-encode if necessary
	 * This is more efficient AND protects against input that is
	 * NOT from Rf_AdobeSymbol2utf8(), e.g., plotmath on Windows
	 * (which is from reEnc(CE_LATIN1, CE_UTF8))
	 */
	if (tmp > 0xF600) {
	    char inChar[4], symbolChar[2], utf8Char[4];
	    char *q;
	    for (j = 0; j < used; j++) {
		inChar[j] = *s++;
	    }
	    inChar[used] = '\0';
	    Rf_utf8toAdobeSymbol(symbolChar, inChar);
	    Rf_AdobeSymbol2utf8(utf8Char, symbolChar, 4, FALSE);
	    q = utf8Char;
	    while (*q) {
		*p++ = *q++;
	    }
	} else {
	    for (j = 0; j < used; j++) {
		*p++ = *s++;
	    }
	}
    }
    *p = '\0';
    return result;
}

const char *Rf_utf8ToLatin1AdobeSymbol2utf8(const char *in, Rboolean usePUA)
{
  const char *latinStr;
  char *utf8str;
  latinStr = reEnc(in, CE_UTF8, CE_LATIN1, 2);
  int nc = 3*(int)strlen(latinStr) + 1;
  utf8str = R_alloc(nc, sizeof(char));
  Rf_AdobeSymbol2utf8(utf8str, latinStr, nc, usePUA);
  return utf8str;
}

attribute_hidden int R::Rf_AdobeSymbol2ucs2(int n)
{
    if(n >= 32 && n < 256) return s2u[n-32];
    else return 0;
}

/* Introduced on 2008-03-21 with comment

       use our own strtod/atof to mitigate effects of setting LC_NUMERIC

   Also allows complete control of which non-numeric strings are
   accepted; e.g. glibc allows NANxxxx, macOS NAN(s), this accepts "NA".

   Exported and in Utils.h (but only in R-exts as of 4.4.1).

   Variants:
   R_strtod4 is used by scan(), allows the decimal point (byte) to be
   specified and whether "NA" is accepted.

   R_strtod5 is used by type_convert(numerals=) (utils/src/io.c)

   The parser uses R_atof (and handles non-numeric strings itself).
   That is the same as R_strtod but ignores endptr.  Also used by
   gnuwin32/windlgs/src/ttest.c, exported and in Utils.h (and
   documeented in R-exts only since R 4.4.1 )
*/

double R::R_strtod5(const char *str, char **endptr, char dec,
		 bool NA, int exact)
{
    LDOUBLE ans = 0.0;
    LDOUBLE p10 = 10., fac = 1.0;
    int sign = 1;
    int n, expn = 0;
    int ndigits = 0;
    const char *p = str;

    /* optional whitespace */
    while (isspace(*p)) p++;

    if (NA && streqln(p, "NA", 2)) {
	ans = NA_REAL;
	p += 2;
	goto done;
    }

   /* optional sign */
    switch (*p) {
    case '-': sign = -1;
    case '+': p++;
    default: ;
    }

    if (strncasecmp(p, "NaN", 3) == 0) {
	ans = R_NaN;
	p += 3;
	goto done;
    /* C99 specifies this: must come first to avoid 'inf' match */
    } else if (strncasecmp(p, "infinity", 8) == 0) {
	ans = R_PosInf;
	p += 8;
	goto done;
    } else if (strncasecmp(p, "Inf", 3) == 0) {
	ans = R_PosInf;
	p += 3;
	goto done;
    }

    if(strlen(p) > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) { // Hexadecimal "0x....."
	/* Prior to 4.5.0 this did not allow forms such as 0x1.234
	   without an exponent.: C99 allow this and implicitly
	   appends "p0"".

	   Changed following PR#18805
	 */
	int exph = -1;

	/* This will overflow to Inf if appropriate */
	for(p += 2; p; p++) {
	    if('0' <= *p && *p <= '9') ans = 16*ans + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ans = 16*ans + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ans = 16*ans + (*p -'A' + 10);
	    else if(*p == dec) {exph = 0; continue;}
	    else break;
	    if (exph >= 0) exph += 4;
	}
#define strtod_EXACT_CLAUSE						\
	if(exact && ans > 0x1.fffffffffffffp52) {			\
	    if(exact == NA_LOGICAL)					\
		warning(_(						\
		"accuracy loss in conversion from \"%s\" to numeric"),	\
			str);						\
	    else {							\
		ans = NA_REAL;						\
		p = str; /* back out */					\
		goto done;						\
	    }								\
	}
	strtod_EXACT_CLAUSE;
	/* Binary exponent, if any */
	if (*p == 'p' || *p == 'P') {
	    int expsign = 1;
	    switch(*++p) {
	    case '-': expsign = -1;
	    case '+': p++;
	    default: ;
	    }
#define MAX_EXPONENT_PREFIX 9999
	    /* exponents beyond ca +1024/-1076 over/underflow 
	       Limit exponent from PR#16358.
	     */
	    int ndig = 0;
	    for (n = 0; *p >= '0' && *p <= '9'; p++, ndig++)
		n = (n < MAX_EXPONENT_PREFIX) ? n * 10 + (*p - '0') : n;
	    if (ndig == 0) {
		ans = NA_REAL;
		p = str; /* back out */
		goto done;
	    }
	    expn += expsign * n;
	}
	if (ans != 0.0) { /* PR#15976:  allow big exponents on 0 */
	    LDOUBLE fac = 1.0;
	    double p2 = 2.0;
	    if(exph > 0) {
		if (expn - exph < -122) {	/* PR#17199:  fac may overflow below if expn - exph is too small.
					       2^-122 is a bit bigger than 1E-37, so should be fine on all systems */
		    for (n = exph, fac = 1.0; n; n >>= 1, p2 *= p2)
			if (n & 1) fac *= p2;
		    ans /= fac;
		    p2 = 2.0;
		} else
		    expn -= exph;
	    }
	    if (expn < 0) {
		for (n = -expn, fac = 1.0; n; n >>= 1, p2 *= p2)
		    if (n & 1) fac *= p2;
		ans /= fac;
	    } else {
		for (n = expn, fac = 1.0; n; n >>= 1, p2 *= p2)
		    if (n & 1) fac *= p2;
		ans *= fac;
	    }
	}
	goto done;
    } // end {hexadecimal case}

    for ( ; *p >= '0' && *p <= '9'; p++, ndigits++) ans = 10*ans + (*p - '0');
    if (*p == dec)
	for (p++; *p >= '0' && *p <= '9'; p++, ndigits++, expn--)
	    ans = 10*ans + (*p - '0');
    if (ndigits == 0) {
	ans = NA_REAL;
	p = str; /* back out */
	goto done;
    }
    strtod_EXACT_CLAUSE;

    if (*p == 'e' || *p == 'E') {
	int ndigits = 0;
	int expsign = 1;
	switch(*++p) {
	case '-': expsign = -1;
	case '+': p++;
	default: ;
	}
	/* The test for n is in response to PR#16358; which was
	   parsing 1e999999999999.
	   It's not right if the exponent is very large, but the
	   overflow or underflow below will handle it.
	   1e308 is already Inf, but negative exponents can go down to -323
	   before undeflowing to zero.  And people could do perverse things
	   like 0.00000001e312.
	*/
	// C17 §6.4.4.2 requires a non-empty 'digit sequence'
	for (n = 0; *p >= '0' && *p <= '9'; p++, ndigits++)
	    n = (n < MAX_EXPONENT_PREFIX) ? n * 10 + (*p - '0') : n;
	if (ndigits == 0) {
	    ans = NA_REAL;
	    p = str; /* back out */
	    goto done;
	}
	expn += expsign * n;
    }

    /* avoid unnecessary underflow for large negative exponents */
    if (expn + ndigits < -300) {
	for (n = 0; n < ndigits; n++) ans /= 10.0;
	expn += ndigits;
    }
    if (expn < -307) { /* use underflow, not overflow */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac /= p10;
	ans *= fac;
    } else if (expn < 0) { /* positive powers are exact */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac *= p10;
	ans /= fac;
    } else if (ans != 0.0) { /* PR#15976:  allow big exponents on 0, e.g. 0E4933 */
	for (n = expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac *= p10;
	ans *= fac;
    }

    /* explicit overflow to infinity */
    if (ans > DBL_MAX) {
	if (endptr) *endptr = (char *) p;
	return (sign > 0) ? R_PosInf : R_NegInf;
    }

done:
    if (endptr) *endptr = (char *) p;
    return sign * (double) ans;
}


attribute_hidden double R::R_strtod4(const char *str, char **endptr, char dec, bool NA)
{
    return R_strtod5(str, endptr, dec, NA, FALSE);
}

double R_strtod(const char *str, char **endptr)
{
    return R_strtod5(str, endptr, '.', FALSE, FALSE);
}

double R_atof(const char *str)
{
    return R_strtod5(str, NULL, '.', FALSE, FALSE);
}

/* enc2native and enc2utf8, but they are the same in a UTF-8 locale */
/* primitive */
attribute_hidden SEXP do_enc2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, el;
    R_xlen_t i;
    bool duped = false;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (!isString(CAR(args)))
	errorcall(call, _("'%s' is not a character vector"), "x");
    ans = CAR(args);
    for (i = 0; i < XLENGTH(ans); i++) {
	el = STRING_ELT(ans, i);
	if (el == NA_STRING) continue;
	if (PRIMVAL(op) || known_to_be_utf8) { /* enc2utf8 */
	    if (IS_UTF8(el) || IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (!duped) { ans = PROTECT(duplicate(ans)); duped = true; }
	    SET_STRING_ELT(ans, i,
			   mkCharCE(translateCharUTF8(el), CE_UTF8));
	} else if (ENC_KNOWN(el)) { /* enc2native */
	    if (IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (known_to_be_latin1 && IS_LATIN1(el)) continue;
	    if (!duped) { PROTECT(ans = duplicate(ans)); duped = true; }
	    if (known_to_be_latin1)
		SET_STRING_ELT(ans, i, mkCharCE(translateChar(el), CE_LATIN1));
	    else
		SET_STRING_ELT(ans, i, mkChar(translateChar(el)));
	}
    }
    if(duped) UNPROTECT(1);
    return ans;
}

#undef isNull
#ifdef USE_ICU
# include <clocale>
#ifdef USE_ICU_APPLE
/* macOS is missing the headers */
typedef int UErrorCode; /* really an enum these days */
struct UCollator;
typedef struct UCollator UCollator;

typedef enum {
  UCOL_EQUAL    = 0,
  UCOL_GREATER    = 1,
  UCOL_LESS    = -1
} UCollationResult ;

typedef enum {
  UCOL_DEFAULT = -1,
  UCOL_PRIMARY = 0,
  UCOL_SECONDARY = 1,
  UCOL_TERTIARY = 2,
  UCOL_DEFAULT_STRENGTH = UCOL_TERTIARY,
  UCOL_CE_STRENGTH_LIMIT,
  UCOL_QUATERNARY=3,
  UCOL_IDENTICAL=15,
  UCOL_STRENGTH_LIMIT,
  UCOL_OFF = 16,
  UCOL_ON = 17,
  UCOL_SHIFTED = 20,
  UCOL_NON_IGNORABLE = 21,
  UCOL_LOWER_FIRST = 24,
  UCOL_UPPER_FIRST = 25,
  UCOL_ATTRIBUTE_VALUE_COUNT
} UColAttributeValue;

typedef UColAttributeValue UCollationStrength;

typedef enum {
      UCOL_FRENCH_COLLATION,
      UCOL_ALTERNATE_HANDLING,
      UCOL_CASE_FIRST,
      UCOL_CASE_LEVEL,
      UCOL_NORMALIZATION_MODE,
      UCOL_DECOMPOSITION_MODE = UCOL_NORMALIZATION_MODE,
      UCOL_STRENGTH,
      UCOL_HIRAGANA_QUATERNARY_MODE,
      UCOL_NUMERIC_COLLATION,
      UCOL_ATTRIBUTE_COUNT
} UColAttribute;

/* UCharIterator struct has to be defined since we use its instances as
   local variables, but we don't actually use any of its members. */
typedef struct UCharIterator {
  const void *context;
  int32_t length, start, index, limit, reservedField;
  void *fns[16]; /* we overshoot here (there is just 10 fns in ICU 3.6),
		    but we have to make sure that enough stack space
		    is allocated when used as a local var in future
		    versions */
} UCharIterator;

#ifdef __cplusplus
extern "C" {
#endif
UCollator* ucol_open(const char *loc, UErrorCode *status);
void ucol_close(UCollator *coll);
void ucol_setAttribute(UCollator *coll, UColAttribute attr,
		       UColAttributeValue value, UErrorCode *status);
void ucol_setStrength(UCollator *coll, UCollationStrength strength);
UCollationResult ucol_strcollIter(const UCollator *coll,
				  UCharIterator *sIter,
				  UCharIterator *tIter,
				  UErrorCode *status);
void uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length);

void uloc_setDefault(const char* localeID, UErrorCode* status);

typedef enum {
    ULOC_ACTUAL_LOCALE = 0,
    ULOC_VALID_LOCALE = 1,
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
} ULocDataLocaleType ;


const char* ucol_getLocaleByType(const UCollator *coll,
				 ULocDataLocaleType type,
				 UErrorCode *status);
#ifdef __cplusplus
} // extern "C"
#endif

#define U_ZERO_ERROR 0
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)
#define ULOC_ACTUAL_LOCALE 0

#else
#include <unicode/utypes.h>
#include <unicode/ucol.h>
#include <unicode/uloc.h>
#include <unicode/uiter.h>
#endif

static UCollator *collator = NULL;
static int collationLocaleSet = 0;

/* called from platform.c */
attribute_hidden void R::resetICUcollator(bool disable)
{
    if (collator) ucol_close(collator);
    collator = NULL;
    collationLocaleSet = disable ? 1 : 0;
}

static constexpr struct {
    const char * const str;
    int val;
} ATtable[] = {
    { "case_first", UCOL_CASE_FIRST },
    { "upper", UCOL_UPPER_FIRST },
    { "lower", UCOL_LOWER_FIRST },
    { "default ", UCOL_DEFAULT },
    { "strength", 999 },
    { "primary ", UCOL_PRIMARY },
    { "secondary ", UCOL_SECONDARY },
    { "tertiary ", UCOL_TERTIARY },
    { "quaternary ", UCOL_QUATERNARY },
    { "identical ", UCOL_IDENTICAL },
    { "french_collation", UCOL_FRENCH_COLLATION },
    { "on", UCOL_ON },
    { "off", UCOL_OFF },
    { "normalization", UCOL_NORMALIZATION_MODE },
    { "alternate_handling", UCOL_ALTERNATE_HANDLING },
    { "non_ignorable", UCOL_NON_IGNORABLE },
    { "shifted", UCOL_SHIFTED },
    { "case_level", UCOL_CASE_LEVEL },
    { "hiragana_quaternary", UCOL_HIRAGANA_QUATERNARY_MODE },
    { NULL,  0 }
};

#ifdef Win32
#define BUFFER_SIZE 512
typedef int (WINAPI *PGSDLN)(LPWSTR, int);

static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    if (p && p[0]) return p;

    // FIXME: ideally, we would use a locale name corresponding to the current
    //        C runtime locale, so as reported by setlocale(); since 4.2 this
    //        means a UCRT locale name. As this is only approximated, we don't
    //        use ICU by default on Windows yet for collation, even though
    //        already having UTF-8 as the native encoding.

    // ICU should accept almost all of these, e.g. en-US and uz-Latn-UZ
    WCHAR wcBuffer[BUFFER_SIZE];
    GetSystemDefaultLocaleName(wcBuffer, BUFFER_SIZE);
    static char locale[BUFFER_SIZE];
    WideCharToMultiByte(CP_ACP, 0, wcBuffer, -1,
			locale, BUFFER_SIZE, NULL, NULL);
    return locale;
}
#else
static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    return (p && p[0]) ? p : setlocale(LC_COLLATE, NULL);
}
#endif

attribute_hidden SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    UErrorCode  status = U_ZERO_ERROR;

    for (; args != R_NilValue; args = CDR(args)) {
	if (Rf_isNull(TAG(args))) error("%s", _("all arguments must be named"));
	const char *this_ = CHAR(PRINTNAME(TAG(args)));
	const char *s;

	x = CAR(args);
	if (!isString(x) || LENGTH(x) != 1)
	    error(_("invalid '%s' argument"), this_);
	s = CHAR(STRING_ELT(x, 0));
	if (streql(this_, "locale")) {
	    if (collator) {
		ucol_close(collator);
		collator = NULL;
	    }
	    if(streql(s, "ASCII")) {
		collationLocaleSet = 2;
	    } else {
		int usable_icu = 1;
#ifdef Win32
		/* ICU 72 requires this function (and other from Windows 7) */
		if (!GetProcAddress(GetModuleHandle(TEXT("kernel32")),
		                    "ResolveLocaleName")) {
		    usable_icu = 0;
		    warning("%s", _("cannot use ICU on this system"));
		}
#endif
		if(usable_icu && !streql(s, "none")) {
		    if(streql(s, "default"))
			uloc_setDefault(getLocale(), &status);
		    else uloc_setDefault(s, &status);
		    if(U_FAILURE(status))
			error(_("failed to set ICU locale %s (%d)"), s, status);
		    collator = ucol_open(NULL, &status);
		    if (U_FAILURE(status)) {
			collator = NULL;
			error(_("failed to open ICU collator (%d)"), status);
		    }
		}
		collationLocaleSet = 1;
	    }
	} else {
	    int i, at = -1, val = -1;
	    for (i = 0; ATtable[i].str; i++)
		if (streql(this_, ATtable[i].str)) {
		    at = ATtable[i].val;
		    break;
		}
	    for (i = 0; ATtable[i].str; i++)
		if (streql(s, ATtable[i].str)) {
		    val = ATtable[i].val;
		    break;
		}
	    if (collator && at == 999 && val >= 0) {
		ucol_setStrength(collator, (UCollationStrength) val);
	    } else if (collator && at >= 0 && val >= 0) {
		ucol_setAttribute(collator, (UColAttribute) at, (UColAttributeValue) val, &status);
		if (U_FAILURE(status))
		    error("%s", _("failed to set ICU collator attribute"));
	    }
	}
    }

    return R_NilValue;
}

attribute_hidden SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *ans = "unknown", *res;
    checkArity(op, args);

    if (collationLocaleSet == 2) {
	ans = "ASCII";
    } else if(collator) {
	UErrorCode  status = U_ZERO_ERROR;
	int type = asInteger(CAR(args));
	if (type < 1 || type > 2)
	    error(_("invalid '%s' value"), "type");

	res = ucol_getLocaleByType(collator,
				   (ULocDataLocaleType) (type == 1 ? ULOC_ACTUAL_LOCALE : ULOC_VALID_LOCALE),
				   &status);
	if(!U_FAILURE(status) && res) ans = res;
    } else ans = "ICU not in use";
    return mkString(ans);
}

/* Caller has to manage the R_alloc stack */
/* NB: strings can have equal collation weight without being identical */
attribute_hidden
int R::Scollate(SEXP a, SEXP b)
{
    if (!collationLocaleSet) {
	int errsv = errno;      /* OSX may set errno in the operations below. */
	collationLocaleSet = 1;

	/* A lot of code depends on that setting LC_ALL or LC_COLLATE to "C"
	   via environment variables or Sys.setlocale ensures the "C" collation
	   order. Originally, R_ICU_LOCALE always took precedence over LC_ALL
	   and LC_COLLATE variables and over Sys.setlocale (except on Unix when
	   R_ICU_LOCALE=C). This now adds an exception: when LC_ALL is set to "C"
	   (or unset and LC_COLLATE is set to "C"), the "C" collation order will
	   be used. */
	const char *envl = getenv("LC_ALL");
	if (!envl || !envl[0])
	    envl = getenv("LC_COLLATE");
	bool useC = envl && streql(envl, "C");

#ifndef Win32
	if (!useC && !streql("C", getLocale()) ) {
#else
	/* On Windows, ICU is used for R_ICU_LOCALE=C, on Unix, it is not. */
	/* FIXME: as ICU does not support C as locale, could we use the Unix
	   behavior on all systems? */
	const char *p = getenv("R_ICU_LOCALE");
	int use_icu = p && p[0] && (!useC || streql(p, "C"));

	/* ICU 72 requires this function (and other from Windows 7) */
	if (use_icu &&
	    !GetProcAddress(GetModuleHandle(TEXT("kernel32")),
			    "ResolveLocaleName")) {
	    use_icu = 0;
	    warning("%s", _("cannot use ICU on this system"));
	}
	if(use_icu) {
#endif
	    UErrorCode status = U_ZERO_ERROR;
	    uloc_setDefault(getLocale(), &status);
	    if(U_FAILURE(status))
		error(_("failed to set ICU locale (%d)"), status);
	    collator = ucol_open(NULL, &status);
	    if (U_FAILURE(status)) {
		collator = NULL;
		error(_("failed to open ICU collator (%d)"), status);
	    }
	}
	errno = errsv;
    }
    // translation may use escapes, but that is OK here
    if (collator == NULL)
	return collationLocaleSet == 2 ?
	    !streql(translateChar(a), translateChar(b)) :
	    strcoll(translateChar(a), translateChar(b));

    UCharIterator aIter, bIter;
    const char *as = translateCharUTF8(a), *bs = translateCharUTF8(b);
    int len1 = (int) strlen(as), len2 = (int) strlen(bs);
    uiter_setUTF8(&aIter, as, len1);
    uiter_setUTF8(&bIter, bs, len2);
    UErrorCode status = U_ZERO_ERROR;
    int result = ucol_strcollIter(collator, &aIter, &bIter, &status);
    if (U_FAILURE(status)) error("%s", _("could not collate using ICU"));
    return result;
}

#else /* not USE_ICU */

attribute_hidden SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning("%s", _("ICU is not supported on this build"));
    return R_NilValue;
}

attribute_hidden SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString("ICU not in use");
}

attribute_hidden void R::resetICUcollator(bool disable) {}

# ifdef Win32

static int Rstrcoll(const char *s1, const char *s2)
{
    R_CheckStack2(sizeof(wchar_t) * (2 + strlen(s1) + strlen(s2)));
    wchar_t w1[strlen(s1)+1], w2[strlen(s2)+1];
    utf8towcs(w1, s1, strlen(s1));
    utf8towcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

int R::Scollate(SEXP a, SEXP b)
{
    if(getCharCE(a) == CE_UTF8 || getCharCE(b) == CE_UTF8)
	return Rstrcoll(translateCharUTF8(a), translateCharUTF8(b));
    else
	return strcoll(translateChar(a), translateChar(b));
}

# else
attribute_hidden
int R::Scollate(SEXP a, SEXP b)
{
    return strcoll(translateChar(a), translateChar(b));
}

# endif
#endif

#include <lzma.h>

attribute_hidden SEXP do_crc64(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP in = CAR(args);
    uint64_t crc = 0;
    char ans[17];
    if (!isString(in)) error("%s", _("input must be a character string"));
    const char *str = CHAR(STRING_ELT(in, 0));

    /* Seems this is really 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return mkString(ans);
}

static void bincode(double *x, R_xlen_t n, double *breaks, int nb,
	int *code, bool right, bool include_border)
{
    int lo, hi, nb1 = nb - 1, new_;
    bool lft = !right;

    /* This relies on breaks being sorted, so wise to check that */
    for(int i = 1; i < nb; i++)
	if(breaks[i-1] > breaks[i]) error("%s", _("'breaks' is not sorted"));

    for(R_xlen_t i = 0; i < n; i++) {
	code[i] = NA_INTEGER;
	if(!ISNAN(x[i])) {
	    lo = 0;
	    hi = nb1;
	    if(x[i] <  breaks[lo] || breaks[hi] < x[i] ||
	       (x[i] == breaks[lft ? hi : lo] && ! include_border)) ;
	    else {
		while(hi - lo >= 2) {
		    new_ = (hi + lo)/2;
		    if(x[i] > breaks[new_] || (lft && x[i] == breaks[new_]))
			lo = new_;
		    else
			hi = new_;
		}
		code[i] = lo + 1;
	    }
	}
    }
}

/* 'breaks' cannot be a long vector as the return codes are integer. */
attribute_hidden SEXP do_bincode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x, breaks, right, lowest;
    x = CAR(args); args = CDR(args);
    breaks = CAR(args); args = CDR(args);
    right = CAR(args); args = CDR(args);
    lowest = CAR(args);
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(breaks))
	error(_("long vector '%s' is not supported"), "breaks");
#endif
    PROTECT(x = coerceVector(x, REALSXP));
    PROTECT(breaks = coerceVector(breaks, REALSXP));
    R_xlen_t n = XLENGTH(x);
    int nB = LENGTH(breaks);
    bool sr = asLogicalNoNA(right, "right"), sl = asLogicalNoNA(lowest, "include.lowest");
    if (nB == NA_INTEGER) error(_("invalid '%s' argument"), "breaks");

    SEXP codes;
    PROTECT(codes = allocVector(INTSXP, n));
    bincode(REAL(x), n, REAL(breaks), nB, INTEGER(codes), sr, sl);
    UNPROTECT(3);
    return codes;
}

attribute_hidden SEXP do_tabulate(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP in = CAR(args), nbin = CADR(args);
    if (TYPEOF(in) != INTSXP)  error("%s", _("invalid input"));
    R_xlen_t n = XLENGTH(in);
    int nb = asInteger(nbin);
    if (nb == NA_INTEGER || nb < 0)
	error(_("invalid '%s' argument"), "nbin");
    int *x = INTEGER(in);
    SEXP ans;
#ifdef LONG_VECTOR_SUPPORT
    if (n > INT_MAX) {
	ans = allocVector(REALSXP, nb);
	double *y = REAL(ans);
	if (nb) memset(y, 0, nb * sizeof(double));
	for(R_xlen_t i = 0 ; i < n ; i++)
	    if (x[i] != NA_INTEGER && x[i] > 0 && x[i] <= nb) y[x[i] - 1]++;
    } else
#endif
    {
	ans = allocVector(INTSXP, nb);
	int *y = INTEGER(ans);
	if (nb) memset(y, 0, nb * sizeof(int));
	for(R_xlen_t i = 0 ; i < n ; i++)
	    if (x[i] != NA_INTEGER && x[i] > 0 && x[i] <= nb) y[x[i] - 1]++;
    }
    return ans;
}

/* Note: R's findInterval( x , vec, ...)  has first two arguments swapped !
 * .Internal(findInterval(vec, x, rightmost.closed, all.inside,  left.open))
 *                         xt  x    right             inside       leftOp
 * x can be a long vector but xt cannot since the result is integer
*/
attribute_hidden SEXP do_findinterval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP xt, x, right, inside, leftOp, chkNA;
    xt = CAR(args); args = CDR(args);
    x = CAR(args); args = CDR(args);
    right = CAR(args); args = CDR(args);
    inside = CAR(args);args = CDR(args);
    leftOp = CAR(args);args = CDR(args);
    chkNA  = CAR(args);
    if(TYPEOF(xt) != REALSXP || TYPEOF(x) != REALSXP) error("%s", _("invalid input"));
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(xt))
	error(_("long vector '%s' is not supported"), "vec");
#endif
    int n = LENGTH(xt);
    if (n == NA_INTEGER) error(_("invalid '%s' argument"), "vec");
    R_xlen_t nx = XLENGTH(x);
    bool sr = asBool2(right, call),
	si = asBool2(inside, call),
	lO = asBool2(leftOp, call);
    /*   if (sr == NA_INTEGER)
	error(_("invalid '%s' argument"), "rightmost.closed");
    if (si == NA_INTEGER)
    error(_("invalid '%s' argument"), "all.inside"); */
    SEXP ans = allocVector(INTSXP, nx);
    double *rxt = REAL(xt), *rx = REAL(x);
    int ii = 1, mfl;
    if (chkNA)
      for(int i = 0; i < nx; i++) {
	if (ISNAN(rx[i]))
	    ii = NA_INTEGER;
	else
#define FIND_INT ii = findInterval2(rxt, n, rx[i], (Rboolean) sr, (Rboolean) si, (Rboolean) lO, ii, &mfl) /* -> ../appl/interv.c */
	    FIND_INT;
	INTEGER(ans)[i] = ii;
      }
    else { // do *not* check ISNAN(rx[i])
	for(int i = 0; i < nx; i++) {
	    FIND_INT;
	    INTEGER(ans)[i] = ii;
	}
    }
    return ans;
}

#ifdef Win32
// this includes RS.h
# undef ERROR
#endif
#include <R_ext/Applic.h>
/* .Internal(pretty(min(x), max(x), n, min.n, shrink.sml,
 *                  c(high.u.bias, u5.bias), eps.correct))
 */
attribute_hidden SEXP do_pretty(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    double l = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(l)) error(_("invalid '%s' argument"), "l");
    double u = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(u)) error(_("invalid '%s' argument"), "u");
    int n = asInteger(CAR(args)); args = CDR(args);
    if (n == NA_INTEGER || n < 0) error(_("invalid '%s' argument"), "n");
    int min_n = asInteger(CAR(args)); args = CDR(args);
    if (min_n == NA_INTEGER || min_n < 0 || min_n > n)
	error(_("invalid '%s' argument"), "min.n");
    double shrink = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(shrink) || shrink <= 0.)
	error(_("invalid '%s' argument"), "shrink.sml");
    SEXP hi = PROTECT(coerceVector(CAR(args), REALSXP)); args = CDR(args); // (h, h5, f.min)
    double *z = REAL(hi);
    if (!R_FINITE(z[0]) || z[0] < 0.)
	error(_("invalid '%s' argument"), "high.u.bias");
    if (!R_FINITE(z[1]) || z[1] < 0.)
	error(_("invalid '%s' argument"), "u5.bias");
    if (!R_FINITE(z[2]) || z[2] <= 0.)
	error(_("invalid '%s' argument"), "f.min");
    int eps = asInteger(CAR(args)); args = CDR(args); /* eps.correct */
    if (eps == NA_INTEGER || eps < 0 || eps > 2)
	error("%s", _("'eps.correct' must be 0, 1, or 2"));
    bool return_bounds = asLogicalNoNA(CAR(args), "bounds"); args = CDR(args); /* bounds */
    double unit;
    if(return_bounds)
	       R_pretty(&l, &u, &n, min_n, shrink, REAL(hi), eps, 1);
    else // unit  and (ns,nu)
	unit = R_pretty(&l, &u, &n, min_n, shrink, REAL(hi), eps, 0);
    int l_ans = return_bounds ? 3 : 4;
    SEXP ans = PROTECT(allocVector(VECSXP, l_ans)),
	nm = allocVector(STRSXP, l_ans);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_VECTOR_ELT(ans, 0, ScalarReal(l));
    SET_VECTOR_ELT(ans, 1, ScalarReal(u));
    SET_VECTOR_ELT(ans, 2, ScalarInteger(n));
    SET_STRING_ELT(nm, 2, mkChar("n"));
    if(return_bounds) {
	SET_STRING_ELT(nm, 0, mkChar("l"));
	SET_STRING_ELT(nm, 1, mkChar("u"));
    } else {
	SET_STRING_ELT(nm, 0, mkChar("ns"));
	SET_STRING_ELT(nm, 1, mkChar("nu"));
	SET_STRING_ELT(nm, 3, mkChar("unit"));
	SET_VECTOR_ELT(ans,3, ScalarReal(unit));
    }
    UNPROTECT(2);
    return ans;
}

/*
    r <- .Internal(formatC(x, as.character(mode), width, digits,
		   as.character(format), as.character(flag), i.strlen))
*/

static void str_signif_sexp(SEXP x, const char *type, int width, int digits,
	   const char *format, const char *flag, char **result);

attribute_hidden SEXP do_formatC(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args); args = CDR(args);
    if (!isVector(x)) error("%s", _("'x' must be a vector"));
    R_xlen_t n = XLENGTH(x);
    const char *type = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    int width = asInteger(CAR(args)); args = CDR(args);
    int digits = asInteger(CAR(args)); args = CDR(args);
    const char *fmt = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    const char *flag = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    SEXP i_strlen = PROTECT(coerceVector(CAR(args), INTSXP));
    char **cptr = (char **) R_alloc(n, sizeof(char*));
    for (R_xlen_t i = 0; i < n; i++) {
	int ix = INTEGER(i_strlen)[i] + 2;
	cptr[i] = (char *) R_alloc(ix + 1, sizeof(char));
	memset(cptr[i], ' ', ix);
	cptr[i][ix] = 0;
    }
    str_signif_sexp(x, type, width, digits, fmt, flag, cptr);
    SEXP ans = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++) SET_STRING_ELT(ans, i, mkChar(cptr[i]));
    UNPROTECT(2);
    return ans;
}

/* Former src/appl/strsignif.c
 *
 *  Copyright (C) Martin Maechler, 1994, 1998
 *  Copyright (C) 2001-2013 the R Core Team
 *
 *  I want you to preserve the copyright of the original author(s),
 *  and encourage you to send me any improvements by e-mail. (MM).
 *
 *  Originally from Bill Dunlap
 *  bill@stat.washington.edu
 *  Wed Feb 21, 1990
 *
 *  Much improved by Martin Maechler, including the "fg" format.
 *
 *  Patched by Friedrich.Leisch@ci.tuwien.ac.at
 *  Fri Nov 22, 1996
 *
 *  Some fixes by Ross Ihaka
 *  ihaka@stat.auckland.ac.nz
 *  Sat Dec 21, 1996
 *  Integer arguments changed from "long" to "int"
 *  Bus error due to non-writable strings fixed
 *
 *  BDR 2001-10-30 use R_alloc not Calloc as memory was not
 *  reclaimed on error (and there are many error exits).
 *
 *	type	"double" or "integer" (R - numeric 'mode').
 *
 *	width	The total field width; width < 0 means to left justify
 *		the number in this field (equivalent to flag = "-").
 *		It is possible that the result will be longer than this,
 *		but that should only happen in reasonable cases.
 *
 *	digits	The desired number of digits after the decimal point.
 *		digits < 0 uses the default for C, namely 6 digits.
 *
 *	format	"d" (for integers) or "f", "e","E", "g", "G" (for 'real')
 *		"f" gives numbers in the usual "xxx.xxx" format;
 *		"e" and "E" give n.ddde<nn> or n.dddE<nn> (scientific format);
 *		"g" and "G" puts them into scientific format if it saves
 *		space to do so.
 *	    NEW: "fg" gives numbers in "xxx.xxx" format as "f",
 *		  ~~  however, digits are *significant* digits and,
 *		      if digits > 0, no trailing zeros are produced, as in "g".
 *
 *	flag	Format modifier as in K&R "C", 2nd ed., p.243;
 *		e.g., "0" pads leading zeros; "-" does left adjustment
 *		the other possible flags are  "+", " ", and "#".
 *	  New (Feb.98): if flag has more than one character, all are passed..
 *
 *  Gabe Becker (2019-05-21): Added str_signif_sexp which wraps
 *  original DATAPTR based str_signif to support ALTREPs.
 *
 *     Any future calls to str_signif on SEXP data should be via
 *     str_signif_sexp to ensure ALTREP support.
 *
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef Win32
/* avoid latest MinGW's redefinition in stdio.h */
#include <trioremap.h>
#endif
#include <Rmath.h>		/* fround */

static void str_signif(void *x, R_xlen_t n, const char *type, int width, int digits,
		const char *format, const char *flag, char **result)
{
    int dig = abs(digits);
    bool do_fg = streql("fg", format); /* TRUE  iff  format == "fg" */
    double xx;
    int iex;
    size_t j, len_flag = strlen(flag);
    CXXR::RAllocStack::Scope rscope;

    char *f0  =	 R_alloc((size_t) do_fg ? 1+1+len_flag+3 : 1, sizeof(char));
    char *form = R_alloc((size_t) 1+1+len_flag+3 + strlen(format),
			 sizeof(char));

    if (width == 0)
	error("%s", _("width cannot be zero"));

    if (streql("d", format)) {
	if (len_flag == 0)
	    strcpy(form, "%*d");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*d");
	}
	if (streql("integer", type))
	    for (R_xlen_t i = 0; i < n; i++)
		snprintf(result[i], strlen(result[i]) + 1,
			 form, width, ((int *)x)[i]);
	else
	    error("%s", _("'type' must be \"integer\" for \"d\"-format"));
    }
    else { /* --- floating point --- */
	if (len_flag == 0)
	    strcpy(form, "%*.*");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*.*");
	}

	if(do_fg) {
	    strcpy(f0, "%");
	    strcat(f0, flag);
	    strcat(f0, ".*f");
	    strcat(form, "g");
	}
	else
	    strcat(form, format);
#ifdef DEBUG
	fprintf(stderr, "strsignif.c: form='%s', width=%d, dig=%d\n",
		form, width, dig);
	if(do_fg) fprintf(stderr, "\t\"fg\": f0='%s'.", f0);
#endif
	if (streql("double", type)) {
	    if(do_fg) /* do smart "f" : */
		for (R_xlen_t i = 0; i < n; i++) {
		    xx = ((double *)x)[i];
		    if(xx == 0.)
			strcpy(result[i], "0");
		    else {
			/* This was iex= (int)floor(log10(fabs(xx)))
			   That's wrong, as xx might get rounded up,
			   and we do need some fuzz or 99.5 is correct.
			*/
			double xxx = fabs(xx), X;
			iex = (int)floor(log10(xxx) + 1e-12);
			X = fround(xxx/Rexp10((double)iex) + 1e-12,
				   (double)(dig-1));
			if(iex > 0 &&  X >= 10) {
			    xx = X * Rexp10((double)iex);
			    iex++;
			}
			if(iex == -4 && fabs(xx)< 1e-4) {/* VERY rare case */
			    iex = -5;
			}
			if(iex < -4) {
				/* "g" would result in 'e-' representation:*/
			    snprintf(result[i], strlen(result[i]) + 1,
				     f0, dig-1 + -iex, xx);
#ifdef DEBUG
			    fprintf(stderr, " x[%d]=%g, iex=%d\n", i, xx, iex);
			    fprintf(stderr, "\tres. = '%s'; ", result[i]);
#endif
			    /* Remove trailing  "0"s __ IFF flag has no '#': */
			    bool rm_trailing_0 = (digits >= 0);
			    if(rm_trailing_0) {
				j = strlen(result[i])-1;
#ifdef DEBUG
				int jL = j;
#endif
				while(result[i][j] == '0') j--;
				result[i][j+1] = '\0';
#ifdef DEBUG
				fprintf(stderr, "\t>>> jL=%d, j=%d; new res= '%s'\n",
					jL, j, result[i]);
#endif
			    }

			} else { /* iex >= -4:	NOT "e-" */
				/* if iex >= dig, would have "e+" representation */
#ifdef DEBUG
			    fprintf(stderr, "\t  iex >= -4; using %d for 'dig'\n",
				    (iex >= dig) ? (iex+1) : dig);
#endif
			    snprintf(result[i], strlen(result[i]) + 1,
				     form, width, (iex >= dig) ? (iex+1) : dig, xx);
			}
		    } /* xx != 0 */
		} /* if(do_fg) for(i..) */
	    else
		for (R_xlen_t i = 0; i < n; i++)
		    snprintf(result[i], strlen(result[i]) + 1,
			     form, width, dig, ((double *)x)[i]);
	} else
	    error("%s", _("'type' must be \"real\" for this format"));
    }
}


/* wrap original DATAPTR based str_signif in ITERATE_BY_REGION calls to
   support ALTREPs

   We still accept type because it is part of the defined API and only defaults
   to matching the SEXP type.
*/
static void str_signif_sexp(SEXP x, const char *type, int width, int digits,
		     const char *format, const char *flag, char **result)
{
    /* result + idx is the overall position of the chunk we're populating */
    if(TYPEOF(x) == INTSXP) {
	ITERATE_BY_REGION(x, px, idx, nb, int, INTEGER,
			  {
			      str_signif((void *) px, nb, type, width, digits,
					 format, flag, result + idx);
			  });
    } else if (TYPEOF(x) == REALSXP) {
	ITERATE_BY_REGION(x, px, idx, nb, double, REAL,
			  {
			      str_signif((void *) px, nb, type, width, digits,
					 format, flag, result + idx);
			  });
    } else {
	error("%s", _("unsupported type"));
    }
}

/* added in R 4.1.0.
   This checks if it succeeds.
   FIXME: is this worth inlining?
 */
char *Rstrdup(const char *s)
{
    size_t nb = strlen(s) + 1;
    void *cpy = malloc(nb);
    if (cpy == NULL) error(_("allocation error in '%s'"), "Rstrdup");
    memcpy(cpy, s, nb);
    return (char *) cpy;
}

static int compareNumericVersion(SEXP x, SEXP y)
{
    int i, nx, ny, nc, *ix, *iy;
    if(!isInteger(x))
	error(_("invalid '%s' argument"), "x");
    if(!isInteger(y))
	error(_("invalid '%s' argument"), "y");
    nx = LENGTH(x);
    ny = LENGTH(y);
    nc =  nx > ny ? ny : nx;
    if(nc == 0)
	return NA_INTEGER;
    ix = INTEGER(x);
    iy = INTEGER(y);
    for(i = 0; i < nc; i++) {
	if(ix[i] > iy[i])
	    return 1;
	if(ix[i] < iy[i])
	    return -1;
    }
    if(nc < nx) {
	for(i = nc; i < nx; i++) {
	    if(ix[i] > 0)
		return 1;
	}
    } else if(nc < ny) {
	for(i = nc; i < ny; i++) {
	    if(iy[i] > 0)
		return -1;
	}
    }
    return 0;
}

attribute_hidden
SEXP do_compareNumericVersion(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, y;
    int i, nx, ny, na;

    checkArity(op, args);

    x = CAR(args); args = CDR(args);
    y = CAR(args);

    if(!isVector(x))
	error(_("invalid '%s' argument"), "x");
    if(!isVector(y))
	error(_("invalid '%s' argument"), "y");
    nx = LENGTH(x);
    ny = LENGTH(y);
    if(nx > 0 && ny > 0)
	na = nx > ny ? nx : ny;
    else
	na = 0;
    PROTECT(ans = allocVector(INTSXP, na));
    for(i = 0; i < na; i++) {
	INTEGER(ans)[i] =
	    compareNumericVersion(VECTOR_ELT(x, i % nx),
				  VECTOR_ELT(y, i % ny));
    }
    UNPROTECT(1);
    return ans;
}

attribute_hidden int R::Rasprintf_malloc(char **str, const char *fmt, ...)
{
    va_list ap;
    int ret;
    char dummy[1];

    *str = NULL;

    va_start(ap, fmt);
    /* could optimize by using non-zero initial size, large
       enough so that most prints with fill */
    /* trio does not accept NULL as str */
    ret = vsnprintf(dummy, 0, fmt, ap);
    va_end(ap);

    if (ret <= 0)
	/* error or empty print */
	return ret;

    size_t needed = ret + 1;
    char *buf = (char *) malloc(needed);
    if (!buf) {
	errno = ENOMEM;
	return -1;
    }

    va_start(ap, fmt);
    ret = vsnprintf(buf, needed, fmt, ap);
    va_end(ap);

    if (ret < 0 || (size_t)ret >= needed)
	/* error */
	free(buf);
    else
	*str = buf;
    return ret;
}


