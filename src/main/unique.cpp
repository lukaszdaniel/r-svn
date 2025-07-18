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

/* This is currently restricted to vectors of length < 2^30 */

/** @file unique.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <memory>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Expression.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Altrep.h>
#include <R_ext/Itermacros.h>

using namespace R;
using namespace CXXR;

/* inline version of function R_NaN_is_R_NA defined in arithmetic.c */
/* may not be needed if LTO is enabled */
#define R_NaN_is_R_NA R_NaN_is_R_NA_inline
static R_INLINE int R_NaN_is_R_NA_inline(double x)
{
#ifdef WORDS_BIGENDIAN
    static const int lw = 1;
#else  /* !WORDS_BIGENDIAN */
    static const int lw = 0;
#endif /* WORDS_BIGENDIAN */
    union {
	double value;
	unsigned int word[2];
    } y;
    y.value = x;
    return y.word[lw] == 1954;
}

#define NIL -1
#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)

/* interval at which to check interrupts */
#define NINTERRUPT 1000000

typedef size_t hlen;

/* Hash function and equality test for keys */

struct HashData {
    int K;
    hlen M;
    R_xlen_t nmax;
#ifdef LONG_VECTOR_SUPPORT
    bool isLong;
#endif
    hlen (*hash)(SEXP, R_xlen_t, HashData *);
    bool (*equal)(SEXP, R_xlen_t, SEXP, R_xlen_t);
    SEXP HashTable;

    int nomatch;
    bool useUTF8;
    bool useCache;
    bool useCloEnv;
    bool extptrAsRef;
    bool inHashtab;

    HashData(int k = 0, bool useUTF8 = false, bool useCache = false) : K(k), useUTF8(useUTF8), useCache(useCache)
    {
    }
};

#define HTDATA_INT(d) (INTEGER0((d)->HashTable))
#define HTDATA_DBL(d) (REAL0((d)->HashTable))


/*
   Integer keys are hashed via a random number generator
   based on Knuth's recommendations.  The high order K bits
   are used as the hash code.

   NB: lots of this code relies on M being a power of two and
   on silent integer overflow mod 2^32.

   <FIXME> Integer keys are wasteful for logical and raw vectors, but
   the tables are small in that case.  It would be much easier to
   implement long vectors, though.
*/

/*  Currently the hash table is implemented as a (signed) integer
    array.  So there are two 31-bit restrictions, the length of the
    array and the values.  The values are initially NIL (-1).  O-based
    indices are inserted by isDuplicated, and invalidated by setting
    to NA_INTEGER.
*/

static hlen scatter(unsigned int key, HashData *d)
{
    return 3141592653U * key >> (32 - d->K);
}

static hlen lhash(SEXP x, R_xlen_t indx, HashData *d)
{
    int xi = LOGICAL_ELT(x, indx);
    if (xi == NA_LOGICAL) return 2U;
    return (hlen) xi;
}

static R_INLINE hlen ihash(SEXP x, R_xlen_t indx, HashData *d)
{
    int xi = INTEGER_ELT(x, indx);
    if (xi == NA_INTEGER) return 0;
    return scatter((unsigned int) xi, d);
}

/* We use unions here because Solaris gcc -O2 has trouble with
   casting + incrementing pointers.  We use tests here, but R currently
   assumes int is 4 bytes and double is 8 bytes.
 */
union foo {
    double d;
    unsigned int u[2];
};

static R_INLINE hlen rhash(SEXP x, R_xlen_t indx, HashData *d)
{
    /* There is a problem with signed 0s under IEC60559 */
    double xi = REAL_ELT(x, indx);
    double tmp = (xi == 0.0) ? 0.0 : xi;
    /* need to use both 32-byte chunks or endianness is an issue */
    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp)) tmp = NA_REAL;
    else if (R_IsNaN(tmp)) tmp = R_NaN;
#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	union foo tmpu;
	tmpu.d = tmp;
	return scatter(tmpu.u[0] + tmpu.u[1], d);
    }
#else
    return scatter(*((unsigned int *) (&tmp)), d);
#endif
}

static Rcomplex unify_complex_na(Rcomplex z) {
    Rcomplex ans;
    ans.r = (z.r == 0.0) ? 0.0 : z.r;
    ans.i = (z.i == 0.0) ? 0.0 : z.i;
    if (R_IsNA(ans.r) || R_IsNA(ans.i))
	ans.r = ans.i = NA_REAL;
    else if (R_IsNaN(ans.r) || R_IsNaN(ans.i))
	ans.r = ans.i = R_NaN;
    return ans;
}

static hlen chash(SEXP x, R_xlen_t indx, HashData *d)
{
    Rcomplex tmp = unify_complex_na(COMPLEX_ELT(x, indx));

#if 2*SIZEOF_INT == SIZEOF_DOUBLE
    {
	unsigned int u;
	union foo tmpu;
	tmpu.d = tmp.r;
	u = tmpu.u[0] ^ tmpu.u[1];
	tmpu.d = tmp.i;
	u ^= tmpu.u[0] ^ tmpu.u[1];
	return scatter(u, d);
    }
#else
	return scatter((*((unsigned int *)(&tmp.r)) ^
			(*((unsigned int *)(&tmp.i)))), d);
#endif
}

/* Pointer hashing as used here isn't entirely portable (we do it in
   several other places, sometimes in slightly different ways) but it
   could be made so by computing a unique value based on the
   allocation page and position in the page.

   Pointer hashes will not be valid if serialized and unserialized in
   another process.

   Hash values are int, For 64bit pointers, we do (upper ^ lower) */
static R_INLINE unsigned int PTRHASH(void *x)
{
    intptr_t z = (intptr_t) x;
    unsigned int z1 = (unsigned int)(z & 0xffffffff), z2 = 0;
#if SIZEOF_LONG == 8
    z2 = (unsigned int)(z/0x100000000L);
#endif
    return z1 ^ z2;
}

/* Hash CHARSXP by address. */
static R_INLINE hlen cshash(SEXP x, R_xlen_t indx, HashData *d)
{
    return scatter(PTRHASH(STRING_ELT(x, indx)), d);
}

static R_INLINE hlen shash(SEXP x, R_xlen_t indx, HashData *d)
{
    unsigned int k;
    const char *p;
    if (d->inHashtab) {
	SEXP xi = STRING_ELT(x, indx);
	bool noTrans = (IS_BYTES(xi) || IS_ASCII(xi));
	if (d->useCache && noTrans)
	    return scatter(PTRHASH(xi), d);
	CXXR::RAllocStack::Scope rscope;
	p = noTrans ? CHAR(xi) : translateCharUTF8(xi);
	k = 0;
	while (*p++)
	    /* multiplier was 8 but 11 isn't a power of 2 */
	    k = 11 * k + (unsigned int) *p;
	return scatter(k, d);
    }
    if(!d->useUTF8 && d->useCache) return cshash(x, indx, d);
    CXXR::RAllocStack::Scope rscope;
    /* Not having d->useCache really should not happen anymore. */
    p = translateCharUTF8(STRING_ELT(x, indx));
    k = 0;
    while (*p++)
	k = 11 * k + (unsigned int) *p; /* was 8 but 11 isn't a power of 2 */
    return scatter(k, d);
}

static bool lequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (LOGICAL_ELT(x, i) == LOGICAL_ELT(y, j));
}


static R_INLINE bool iequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (INTEGER_ELT(x, i) == INTEGER_ELT(y, j));
}

/* BDR 2002-1-17  We don't want NA and other NaNs to be equal */
static R_INLINE bool requal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    double xi = REAL_ELT(x, i);
    double yj = REAL_ELT(y, j);
    if (!ISNAN(xi) && !ISNAN(yj))
	return (xi == yj);
    else if (R_IsNA(xi) && R_IsNA(yj)) return true;
    else if (R_IsNaN(xi) && R_IsNaN(yj)) return true;
    else return false;
}

/* This is differentiating {NA,1}, {NA,0}, {NA, NaN}, {NA, NA},
 * but R's print() and format()  render all as "NA" */
inline static bool cplx_eq(const Rcomplex &x, const Rcomplex &y)
{
    if (!ISNAN(x.r) && !ISNAN(x.i) && !ISNAN(y.r) && !ISNAN(y.i))
	return x.r == y.r && x.i == y.i;
    else if (R_IsNA(x.r) || R_IsNA(x.i)) // x is NA
	return (R_IsNA(y.r) || R_IsNA(y.i)) ? 1 : 0;
    else if (R_IsNA(y.r) || R_IsNA(y.i)) // y is NA but x is not
	return 0;
    // else : none is NA but there's at least one NaN;  hence  ISNAN(.) == R_IsNaN(.)
    return
	(((ISNAN(x.r) && ISNAN(y.r)) || (!ISNAN(x.r) && !ISNAN(y.r) && x.r == y.r)) && // Re
	 ((ISNAN(x.i) && ISNAN(y.i)) || (!ISNAN(x.i) && !ISNAN(y.i) && x.i == y.i))    // Im
	    ) ? 1 : 0;
}

static bool cequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return cplx_eq(COMPLEX_ELT(x, i), COMPLEX_ELT(y, j));
}

static R_INLINE bool sequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    SEXP xi = STRING_ELT(x, i);
    SEXP yj = STRING_ELT(y, j);
    /* Two strings which have the same address must be the same,
       so avoid looking at the contents */
    if (xi == yj) return true;
    /* Then if either is NA the other cannot be */
    /* Once all CHARSXPs are cached, Seql will handle this */
    if (xi == NA_STRING || yj == NA_STRING)
	return false;
    /* another pre-test to avoid the call to Seql */
    if (IS_CACHED(xi) && IS_CACHED(yj) && ENC_KNOWN(xi) == ENC_KNOWN(yj))
	return false;
    return Seql(xi, yj);
}

static hlen rawhash(SEXP x, R_xlen_t indx, HashData *d)
{
    return (hlen) RAW_ELT(x, indx);
}

static bool rawequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return (RAW_ELT(x, i) == RAW_ELT(y, j));
}

static hlen vhash_one(SEXP _this, HashData *d);
static hlen vhash(SEXP x, R_xlen_t indx, HashData *d)
{
    SEXP _this = VECTOR_ELT(x, indx);
    return vhash_one(_this, d);
}

static hlen vhash_one(SEXP _this, HashData *d)
{
    /* Handle environments by pointer hashing. Previously,
       environments were hashed based only on length, which is not
       very effective and could be expensive to compute. */
    if (TYPEOF(_this) == ENVSXP)
	return scatter(PTRHASH(_this), d);

    unsigned int key = OBJECT(_this) + 2*TYPEOF(_this) + 100U*(unsigned int) length(_this);
    /* maybe we should also look at attributes, but that slows us down */
    switch (TYPEOF(_this)) {
    case LGLSXP:
	/* This is not too clever: pack into 32-bits and then scatter? */
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= lhash(_this, i, d);
	    key *= 97;
	}
	break;
    case INTSXP:
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= ihash(_this, i, d);
	    key *= 97;
	}
	break;
    case REALSXP:
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= rhash(_this, i, d);
	    key *= 97;
	}
	break;
    case CPLXSXP:
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= chash(_this, i, d);
	    key *= 97;
	}
	break;
    case STRSXP:
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= shash(_this, i, d);
	    key *= 97;
	}
	break;
    case RAWSXP:
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= scatter((unsigned int)rawhash(_this, i, d), d);
	    key *= 97;
	}
	break;
    case EXPRSXP:
    case VECSXP:
	R_CheckStack();
	for (int i = 0; i < LENGTH(_this); i++) {
	    key ^= vhash(_this, i, d);
	    key *= 97;
	}
	break;
    case LANGSXP:
    case LISTSXP:
	R_CheckStack();
	/* all attributes are ignored */
	/* might be good to consider environments on formulas */
	for (SEXP next = _this; next != R_NilValue; next = CDR(next)) {
	    key ^= vhash_one(CAR(next), d);
	    key *= 97;
	}
	break;
    case CLOSXP:
	/* all attributes are ignored */
	key ^= vhash_one(BODY_EXPR(_this), d);
	key *= 97;
	if (d->useCloEnv) {
	    key ^= vhash_one(CLOENV(_this), d);
	    key *= 97;
	}
	break;
    case SYMSXP:
	key *= PTRHASH(_this);
	key *= 97;
	break;
    case CHARSXP:
	if(!d->useUTF8 && d->useCache) key *= PTRHASH(_this);
	/**** otherwise, do nothing for now */
	/* this should only happen in C-level hash tables */
	/* eventually this should do what shash does */
	key *= 97;
	break;
    case EXTPTRSXP:
	if (d->extptrAsRef)
	    key ^= PTRHASH(_this);
	else
	    /* identical() considers only the EXTPTR_PTR values ... */
	    key ^= PTRHASH(EXTPTR_PTR(_this));
	key *= 97;
	break;
    default:
	break;
    }
    return scatter(key, d);
}

static bool vequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    if (i < 0 || j < 0) return false;
    return R_compute_identical(VECTOR_ELT(x, i), VECTOR_ELT(y, j), 0);
}

/*
  Choose M to be the smallest power of 2
  not less than 2*n and set K = log2(M).
  Need K >= 1 and hence M >= 2, and 2^M < 2^31-1, hence n <= 2^29.

  Dec 2004: modified from 4*n to 2*n, since in the worst case we have
  a 50% full table, and that is still rather efficient -- see
  R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606.
*/
static void MKsetup(R_xlen_t n, HashData *d, R_xlen_t nmax)
{
#ifdef LONG_VECTOR_SUPPORT
    /* M = 2^32 is safe, hence n <= 2^31 -1 */
    if(n < 0) /* protect against overflow to -ve */
	error(_("length %lld is too large for hashing"), (long long)n);
#else
    if(n < 0 || n >= 1073741824) /* protect against overflow to -ve */
	error(_("length %lld is too large for hashing"), (long long)n);
#endif

    if (nmax != NA_INTEGER && nmax != 1) n = nmax;
    size_t n2 = 2U * (size_t) n;
    d->M = 2;
    d->K = 1;
    while (d->M < n2) {
	d->M *= 2;
	d->K++;
    }
    d->nmax = n;
}

#define IMAX 4294967296L
static void HashTableSetup(SEXP x, HashData *d, R_xlen_t nmax)
{
    d->useUTF8 = FALSE;
    d->useCache = TRUE;
    switch (TYPEOF(x)) {
    case LGLSXP:
	d->hash = lhash;
	d->equal = lequal;
	d->nmax = d->M = 4;
	d->K = 2; /* unused */
	break;
    case INTSXP:
    {
	d->hash = ihash;
	d->equal = iequal;
#ifdef LONG_VECTOR_SUPPORT
	R_xlen_t nn = XLENGTH(x);
	if (nn > IMAX) nn = IMAX;
	MKsetup(nn, d, nmax);
#else
	MKsetup(LENGTH(x), d, nmax);
#endif
    }
	break;
    case REALSXP:
	d->hash = rhash;
	d->equal = requal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case CPLXSXP:
	d->hash = chash;
	d->equal = cequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case STRSXP:
	d->hash = shash;
	d->equal = sequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    case RAWSXP:
	d->hash = rawhash;
	d->equal = rawequal;
	d->nmax = d->M = 256;
	d->K = 8; /* unused */
	break;
    case EXPRSXP:
    case VECSXP:
	d->hash = vhash;
	d->equal = vequal;
	MKsetup(XLENGTH(x), d, nmax);
	break;
    default:
	UNIMPLEMENTED_TYPE("HashTableSetup", x);
    }
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = IS_LONG_VEC(x);
    if (d->isLong) {
	d->HashTable = allocVector(REALSXP, (R_xlen_t) d->M);
	for (hlen i = 0; i < d->M; i++) HTDATA_DBL(d)[i] = NIL;
    } else
#endif
    {
	d->HashTable = allocVector(INTSXP, (R_xlen_t) d->M);
	for (hlen i = 0; i < d->M; i++) HTDATA_INT(d)[i] = NIL;
    }
}

/* Open address hashing */
/* Collision resolution is by linear probing */
/* The table is guaranteed large so this is sufficient */

static int isDuplicated(SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = HTDATA_DBL(d);
	hlen i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, (R_xlen_t) h[i], x, indx))
		return (h[i] >= 0);
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) error("%s", _("hash table is full"));
	h[i] = (double) indx;
    } else
#endif
    {
	int *h = HTDATA_INT(d);
	hlen i = d->hash(x, indx, d);
	while (h[i] != NIL) {
	    if (d->equal(x, h[i], x, indx))
		return (h[i] >= 0);
	    i = (i + 1) % d->M;
	}
	if (d->nmax-- < 0) error("%s", _("hash table is full"));
	h[i] = (int) indx;
    }
    return 0;
}

static bool duplicatedInit(SEXP x, HashData *d)
{
    bool stop = FALSE;

    if(TYPEOF(x) == STRSXP) {
	R_xlen_t i, n = XLENGTH(x);
	for(i = 0; i < n; i++) {
	    if(IS_BYTES(STRING_ELT(x, i))) {
		d->useUTF8 = FALSE;
		stop = TRUE;
		break;
	    }
	    if(ENC_KNOWN(STRING_ELT(x, i))) {
		d->useUTF8 = TRUE;
	    }
	    /* uncached strings are currently not properly supported */
	    if(!IS_CACHED(STRING_ELT(x, i))) {
		d->useCache = FALSE;
		stop = TRUE;
		break;
	    }
	}
    } else if (TYPEOF(x) == VECSXP) {
	R_xlen_t i, n = XLENGTH(x);
	for(i = 0; i < n; i++)
	    if (duplicatedInit(VECTOR_ELT(x, i), d)) {
		stop = TRUE;
		break;
	    }
    } else if (TYPEOF(x) == EXPRSXP) {
	R_xlen_t i, n = XLENGTH(x);
	for(i = 0; i < n; i++)
	    if (duplicatedInit(XVECTOR_ELT(x, i), d)) {
		stop = TRUE;
		break;
	    }
    } else if (TYPEOF(x) == LANGSXP || TYPEOF(x) == LISTSXP) {
	for(SEXP head = x; head != R_NilValue; head = CDR(head))
	    if (duplicatedInit(CAR(head), d)) {
		stop = TRUE;
		break;
	    }

    } else if (TYPEOF(x) == CLOSXP) {
	if (duplicatedInit(BODY_EXPR(x), d))
	    stop = TRUE;
    }
    return stop;
}

static void removeEntry(SEXP table, SEXP x, R_xlen_t indx, HashData *d)
{
#ifdef LONG_VECTOR_SUPPORT
    if (d->isLong) {
	double *h = HTDATA_DBL(d);
	hlen i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, (R_xlen_t) h[i], x, indx)) {
		h[i] = NA_INTEGER;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    } else
#endif
    {
	int *h = HTDATA_INT(d);
	hlen i = d->hash(x, indx, d);
	while (h[i] >= 0) {
	    if (d->equal(table, h[i], x, indx)) {
		h[i] = NA_INTEGER;  /* < 0, only index values are inserted */
		return;
	    }
	    i = (i + 1) % d->M;
	}
    }
}

#define DUPLICATED_INIT				\
    HashData data = { 0 };			\
    HashTableSetup(x, &data, nmax);		\
    data.useUTF8 = FALSE; data.useCache = TRUE;	\
    duplicatedInit(x, &data);

/* used in scan() */
SEXP Rf_duplicated(SEXP x, Rboolean from_last)
{
    SEXP ans;
    int nmax = NA_INTEGER;

    if (!isVector(x)) error("%s", _("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    int *v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

attribute_hidden R_xlen_t sorted_real_count_NANs(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    if(n == 0)
	return 0;
    if(n == 1)
	return ISNAN(REAL_ELT(x, 0));

    int sorted = REAL_IS_SORTED(x);
    if(!KNOWN_SORTED(sorted)) /* this should never happen! */
	error("%s", _("sorted_real_count_NANs got unsorted vector: this should not happen"));

    double rtmp;
    R_xlen_t ret, nanpos, lowedge, highedge;
    int nas1st = KNOWN_NA_1ST(sorted);
    rtmp = nas1st ? REAL_ELT(x, 0) : REAL_ELT(x, n - 1);
    if(!ISNAN(rtmp))
	return 0;
    if((nas1st && ISNAN(REAL_ELT(x, n - 1))) ||
       (!nas1st && ISNAN(REAL_ELT(x, 0))))
	return n;

    nanpos = n / 2;
    lowedge = 0;
    highedge = n - 1;

#define FIND_NAN_EDGE(yes, no, final) do {	\
	while(highedge > lowedge + 1) {		\
	    rtmp = REAL_ELT(x, nanpos);		\
	    if(ISNAN(rtmp))			\
		yes;				\
	    else				\
		no;				\
	    nanpos = (highedge + lowedge) / 2;	\
	}					\
	ret = final;				\
    } while(0)

    if(nas1st) {
	FIND_NAN_EDGE(lowedge = nanpos, highedge = nanpos, lowedge + 1);
    } else {
	FIND_NAN_EDGE(highedge = nanpos, lowedge = nanpos, n - highedge);
    }
    return ret;
}

#undef FIND_NAN_EDGE

#define DUP_DO_ONE(x, y, ind) do {	\
    if(x == y)				\
	v[ind] = TRUE;			\
    else				\
	v[ind] = FALSE;			\
    } while(0)

static SEXP sorted_Duplicated(SEXP x, bool from_last, int nmax)
{
    //  n guaranteed >= 2 from calling function (Duplicated)
    R_xlen_t n = XLENGTH(x), numnas, na_left = -1, startpos;
    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    int *v = LOGICAL(ans), itmp, sorted;
    double rtmp;
    bool seen_na = FALSE, seen_nan = FALSE,
	nas1st= TRUE;

#define SORTED_DUP_NONNANS(start, niter, tmpvar, eetype, vvtype) do {	\
	if(from_last) {							\
	    v[start + niter] = FALSE;					\
	    tmpvar = vvtype##_ELT(x, start + niter);			\
	    ITERATE_BY_REGION_PARTIAL_REV(x, xptr, idx, nb, eetype,	\
					  vvtype, start, niter, {	\
					      DUP_DO_ONE(xptr[nb - 1],	\
							 tmpvar,	\
							 idx + nb - 1);	\
					      for(R_xlen_t k = nb - 2; k >= 0; k--) { \
						  DUP_DO_ONE(xptr[k + 1], \
							     xptr[k],	\
							     idx + k);	\
					      }				\
					      tmpvar = xptr[0];		\
					  });				\
	} else { /* !from_last */					\
	    v[start] = FALSE;						\
	    tmpvar = vvtype##_ELT(x, start);				\
	    ITERATE_BY_REGION_PARTIAL(x, xptr, idx, nb, eetype,		\
				      vvtype, start + 1, niter, {	\
					  DUP_DO_ONE(xptr[0], tmpvar,	\
						     idx);		\
					  for(R_xlen_t k = 1; k < nb; k++) { \
					      DUP_DO_ONE(xptr[k],	\
							 xptr[k - 1],	\
							 idx + k);	\
					  }				\
					  tmpvar = xptr[nb - 1];	\
				      });				\
	}								\
    } while(0)

#define SORTED_DUP_NANS(itype, istart, icond, iter) do {		\
	ITERATE_BY_REGION_##itype(x, xptr, idx, nb,			\
				  double, REAL, na_left, numnas, {	\
				      for(R_xlen_t i = istart; icond; iter) { \
					  if(R_NaN_is_R_NA(xptr[i])) {	\
					      v[idx + i] = seen_na;	\
					      seen_na = TRUE;		\
					  } else {			\
					      v[idx + i] = seen_nan;	\
					      seen_nan = TRUE;		\
					  }				\
				      }					\
				  });					\
    } while(0)

    switch(TYPEOF(x)) {
    case INTSXP:
	sorted = INTEGER_IS_SORTED(x);
	if(!KNOWN_SORTED(sorted))
	    error("%s", _("sorted_Duplicated got unsorted vector: this should not happen"));
	SORTED_DUP_NONNANS(0, n - 1, itmp, int, INTEGER);
	break;
    case REALSXP:
	sorted = REAL_IS_SORTED(x);
	if(!KNOWN_SORTED(sorted))
	    error("%s", _("sorted_Duplicated got unsorted vector: this should not happen"));
	numnas = sorted_real_count_NANs(x);
	nas1st = KNOWN_NA_1ST(REAL_IS_SORTED(x));

	if(numnas > 0) {
	    na_left = nas1st ? 0 : n - numnas;
	    if(from_last) {
		SORTED_DUP_NANS(PARTIAL_REV, nb - 1, i >= 0, i--);
	    } else { // !from_last
		SORTED_DUP_NANS(PARTIAL, 0, i < nb, i++);
	    } // from_last
	} // numnas > 0

	if(numnas < n) {
	    startpos = nas1st ? numnas : 0;
	    SORTED_DUP_NONNANS(startpos, n - numnas - 1, rtmp, double, REAL);
	}
	break;
    default:
	error(_("sorted_Duplicated got unsupported type %d: this should not happen"), TYPEOF(x));
    }
    UNPROTECT(1); //ans
    return ans;
}

#undef SORTED_DUP_NONNANS
#undef SORTED_DUP_NANS
#undef DUP_DO_ONE

/* to add sorted fastpass support for new SEXP types modify sorted_Duplicated
   and sorted_any_Duplicated then add them here */
#define DUP_KNOWN_SORTED(x)						\
    ((TYPEOF(x) == INTSXP && KNOWN_SORTED(INTEGER_IS_SORTED(x))) ||	\
     (TYPEOF(x) == REALSXP && KNOWN_SORTED(REAL_IS_SORTED(x))))

static SEXP Duplicated(SEXP x, bool from_last, int nmax)
{
    SEXP ans;

    if (!isVector(x)) error("%s", _("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    if(n == 0)
	return allocVector(LGLSXP, 0);
    else if (n == 1)
	return ScalarLogical(FALSE);

    if(DUP_KNOWN_SORTED(x)) {
    	return sorted_Duplicated(x, from_last, nmax);
    }
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    int *v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    UNPROTECT(2);
    return ans;
}

// In Rinternals.h
attribute_hidden R_xlen_t sorted_any_duplicated(SEXP x, bool from_last) {
    int itmp, sorted;
    double rtmp;
    bool seen_na = FALSE, seen_nan = FALSE, na1st = FALSE;

#define SORTED_ANYDUP_NONNANS_FROM_LAST(start, count, tmpvar, eetype, vvtype) do { \
	if (count > 1) {							\
	    tmpvar = vvtype##_ELT(x, start + count - 1);			\
	    ITERATE_BY_REGION_PARTIAL_REV(x, xptr, idx, nb, eetype, vvtype,	\
					  start, count - 1, {			\
					      if(xptr[nb - 1] == tmpvar) {	\
						  return idx + nb;		\
					      }					\
					      for(R_xlen_t k = nb - 2; k >= 0; k--) { \
						  if(xptr[k + 1] == xptr[k]) {	\
						      return idx + k + 1;	\
						  }				\
					      }					\
					      tmpvar = xptr[0];			\
					  });					\
	}									\
    } while(0)

#define SORTED_ANYDUP_NONNANS_FROM_FIRST(start, count, tmpvar, eetype, vvtype) do { \
	if (count > 1) {							\
	    tmpvar = vvtype##_ELT(x, start);					\
	    ITERATE_BY_REGION_PARTIAL(x, xptr, idx, nb, eetype,			\
				      vvtype, start + 1, count - 1, {		\
					  if(xptr[0] == tmpvar) {		\
					      return idx + 1;			\
					  }					\
					  for(R_xlen_t k = 1; k < nb; k++) {	\
					      if(xptr[k] == xptr[k - 1]) {	\
						  return idx + k + 1;		\
					      }					\
					  }					\
					  tmpvar = xptr[nb - 1];		\
				      });					\
	}									\
    } while(0)

#define SORTED_ANYDUP_NANS(start, count, itype, istart, icond, iter) do { \
	if (count > 1) {							\
	    ITERATE_BY_REGION_##itype(x, xptr, idx, nb, double, REAL,		\
				      start, count, {				\
					  for(R_xlen_t i = istart; icond; iter) { \
					      if(R_NaN_is_R_NA(xptr[i])) {	\
						  if(seen_na) {			\
						      return idx + i + 1;	\
						  } else {			\
						      seen_na = TRUE;		\
						  }				\
					      } else {				\
						  if(seen_nan) {		\
						      return idx + i + 1;	\
						  } else {			\
						      seen_nan = TRUE;		\
						  }				\
					      }					\
					  }					\
				      });					\
	}									\
    } while(0)

    switch(TYPEOF(x)) {
    case INTSXP:
	sorted = INTEGER_IS_SORTED(x);
	if(!KNOWN_SORTED(sorted))
	    error("%s", _("sorted_any_duplicated got unsorted vector: this should not happen"));
	if(from_last) {
	    SORTED_ANYDUP_NONNANS_FROM_LAST(0, XLENGTH(x), itmp, int, INTEGER);
	} else {
	    SORTED_ANYDUP_NONNANS_FROM_FIRST(0, XLENGTH(x), itmp, int, INTEGER);
	}
	break;
    case REALSXP:
	{
	sorted = REAL_IS_SORTED(x);
	if(!KNOWN_SORTED(sorted))
	    error("%s", _("sorted_any_duplicated got unsorted vector: this should not happen"));
	R_xlen_t numnas = sorted_real_count_NANs(x), napivot;
	napivot = XLENGTH(x) - numnas;
	na1st = KNOWN_NA_1ST(sorted);

	if(from_last) {
	    if(na1st) {
		SORTED_ANYDUP_NONNANS_FROM_LAST(numnas, napivot, rtmp, double,
						REAL);
		SORTED_ANYDUP_NANS(0, numnas, PARTIAL_REV, nb - 1, i >=0, i--);
	    } else {
		SORTED_ANYDUP_NANS(napivot, numnas, PARTIAL_REV, nb - 1, i >= 0,
				   i--);
		SORTED_ANYDUP_NONNANS_FROM_LAST(0, napivot, rtmp, double, REAL);
	    }
	} else { // !from_last
	    if(na1st) {
		SORTED_ANYDUP_NANS(0, numnas, PARTIAL, 0, i < nb, i++);
		SORTED_ANYDUP_NONNANS_FROM_FIRST(numnas, napivot, rtmp, double,
						 REAL);
	    } else {
		SORTED_ANYDUP_NONNANS_FROM_FIRST(0, napivot, rtmp, double,
						 REAL);
		SORTED_ANYDUP_NANS(napivot, numnas, PARTIAL, 0, i < nb, i++);
	    }
	} //from_last
	}
	break;
    default:
	error(_("sorted_Duplicated got unsupported type %d: this should not happen"), TYPEOF(x));
    }
    return 0;
}

#undef SORTED_ANYDUP_NONNANS_FROM_LAST
#undef SORTED_ANYDUP_NONNANS_FRO_FIRST
#undef SORTED_ANYDUP_NANS

/* simpler version of the above : return 1-based index of first, or 0 : */
// In Rinternals.h
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last)
{
    R_xlen_t result = 0;
    int nmax = NA_INTEGER;

    if (!isVector(x)) error("%s", _("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);

    if(DUP_KNOWN_SORTED(x)) {
    	return sorted_any_duplicated(x, from_last);
    }

    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(from_last) {
	for (i = n-1; i >= 0; i--) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    } else {
	for (i = 0; i < n; i++) {
	    if(isDuplicated(x, i, &data)) { result = ++i; break; }
	}
    }
    UNPROTECT(1);
    return result;
}

#undef DUP_KNOWN_SORTED

static SEXP duplicated3(SEXP x, SEXP incomp, bool from_last, int nmax)
{
    SEXP ans;
    int j, m;

    if (!isVector(x)) error("%s", _("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;

    PROTECT(data.HashTable);
    PROTECT(ans = allocVector(LGLSXP, n));

    int *v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}
    else
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    v[i] = isDuplicated(x, i, &data);
	}

    if(length(incomp)) {
	PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
	m = length(incomp);
	for (i = 0; i < n; i++)
	    if(v[i]) {
		for(j = 0; j < m; j++)
		    if(data.equal(x, i, incomp, j)) {v[i] = 0; break;}
	    }
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return ans;
}

/* return (1-based) index of first duplication, or 0 : */
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last)
{
    int j, m = length(incomp), nmax = NA_INTEGER;

    if (!isVector(x)) error("%s", _("'duplicated' applies only to vectors"));
    R_xlen_t i, n = XLENGTH(x);
    DUPLICATED_INIT;
    PROTECT(data.HashTable);

    if(!m) error("%s", _("any_duplicated3(., <0-length incomp>)"));

    PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
    m = length(incomp);

    if(from_last)
	for (i = n-1; i >= 0; i--) {
#define IS_DUPLICATED_CHECK				\
	    if(isDuplicated(x, i, &data)) {		\
		bool isDup = TRUE;			\
		for(j = 0; j < m; j++)			\
		    if(data.equal(x, i, incomp, j)) {	\
			isDup = FALSE; break;		\
		    }					\
		if(isDup) {				\
		    UNPROTECT(2);			\
		    return ++i;				\
		}					\
		/* else continue */			\
	    }
	    IS_DUPLICATED_CHECK;
	}
    else {
	for (i = 0; i < n; i++) {
	    IS_DUPLICATED_CHECK;
	}
    }

    UNPROTECT(2);
    return 0;
}

#undef IS_DUPLICATED_CHECK
#undef DUPLICATED_INIT


/* .Internal(   duplicated(x, incomparables, fromLast, nmax))  [op=0]
   .Internal(       unique(x, incomparables, fromLast, nmax))  [op=1]
   .Internal(anyDuplicated(x, incomparables, fromLast      ))  [op=2]
*/
attribute_hidden SEXP do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    SEXP incomp = CADR(args);
    if (length(CADDR(args)) < 1)
	error(_("'%s' must be length 1"), "fromLast");
    bool fL = asLogicalNoNA(CADDR(args), "fromLast");

    /* handle zero length vectors, and NULL */
    R_xlen_t n = xlength(x);
    if (n == 0)
	return(PRIMVAL(op) <= 1
	       ? allocVector(PRIMVAL(op) != 1 ? LGLSXP : TYPEOF(x), 0)
	       : ScalarInteger(0));

    if (!isVector(x)) {
	error(_("%s() applies only to vectors"),
	      (PRIMVAL(op) == 0 ? "duplicated" :
	       (PRIMVAL(op) == 1 ? "unique" : /* 2 */ "anyDuplicated")));
    }
    int nmax = NA_INTEGER;
    if (PRIMVAL(op) <= 1) {
	nmax = asInteger(CADDDR(args));
	if (nmax != NA_INTEGER && nmax <= 0)
	    error("%s", _("'nmax' must be positive"));
    }

    SEXP dup;
    if(length(incomp) && /* S has FALSE to mean empty */
       !(isLogical(incomp) && length(incomp) == 1 &&
	 LOGICAL_ELT(incomp, 0) == 0)) {
	if(PRIMVAL(op) == 2) {
	    /* return R's 1-based index :*/
	    R_xlen_t ind  = any_duplicated3(x, incomp, (Rboolean) fL);
	    if(ind > INT_MAX) return ScalarReal((double) ind);
	    else return ScalarInteger((int)ind);
	} else
	    dup = duplicated3(x, incomp, fL, nmax);
    }
    else {
	if(PRIMVAL(op) == 2) {
	    R_xlen_t ind  = any_duplicated(x, (Rboolean) fL);
	    if(ind > INT_MAX) return ScalarReal((double) ind);
	    else return ScalarInteger((int)ind);
	} else
	    dup = Duplicated(x, fL, nmax);
    }
    if (PRIMVAL(op) == 0) /* "duplicated()" */
	return dup;
    /*	ELSE
	use the results of "duplicated" to get "unique" */

    /* count unique entries */
    R_xlen_t i, k = 0;
    PROTECT(dup);
    ITERATE_BY_REGION(dup, duptr, idx, nb, int, LOGICAL, {
	    for(R_xlen_t j=0; j < nb; j++)
		if(duptr[j] == 0) k++;
	});

    SEXP ans = PROTECT(allocVector(TYPEOF(x), k));

    k = 0;
    switch (TYPEOF(x)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		LOGICAL0(ans)[k++] = LOGICAL_ELT(x, i);
	break;
    case INTSXP:
	ITERATE_BY_REGION(dup, duptr, idx, nb, int, LOGICAL, {
		for(R_xlen_t j = 0; j < nb; j++) {
		    if(duptr[j] == 0)
			INTEGER0(ans)[k++] = INTEGER_ELT(x, idx + j);
		}
	    });
	break;
    case REALSXP:
	ITERATE_BY_REGION(dup, duptr, idx, nb, int, LOGICAL, {
		for(R_xlen_t j = 0; j < nb; j++) {
		    if(duptr[j] == 0)
			REAL0(ans)[k++] = REAL_ELT(x, idx + j);
		}
	    });
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		COMPLEX0(ans)[k++] = COMPLEX_ELT(x, i);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		SET_STRING_ELT(ans, k++, STRING_ELT(x, i));
	break;
    case EXPRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		SET_XVECTOR_ELT(ans, k++, XVECTOR_ELT(x, i));
	break;
    case VECSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		SET_VECTOR_ELT(ans, k++, VECTOR_ELT(x, i));
	break;
    case RAWSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(dup, i) == 0)
		RAW0(ans)[k++] = RAW_ELT(x, i);
	break;
    default:
	UNIMPLEMENTED_TYPE("duplicated", x);
    }
    UNPROTECT(2);
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table, HashData *d)
{
    R_xlen_t n = XLENGTH(table);
    for (R_xlen_t i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	(void) isDuplicated(table, i, d);
    }
}

/* invalidate entries: normally few */
static void UndoHashing(SEXP x, SEXP table, HashData *d)
{
    for (R_xlen_t i = 0; i < XLENGTH(x); i++) removeEntry(table, x, i, d);
}

#define DEFLOOKUP(NAME, HASHFUN, EQLFUN)			\
    static R_INLINE int						\
    NAME(SEXP table, SEXP x, R_xlen_t indx, HashData *d)	\
    {								\
	int *h = HTDATA_INT(d);					\
	hlen i = HASHFUN(x, indx, d);				\
	while (h[i] != NIL) {					\
	    if (EQLFUN(table, h[i], x, indx))			\
		return h[i] >= 0 ? h[i] + 1 : d->nomatch;	\
	    i = (i + 1) % d->M;					\
	}							\
	return d->nomatch;					\
    }

/* definitions to help the C compiler to inline of most important cases */
DEFLOOKUP(iLookup, ihash, iequal)
DEFLOOKUP(rLookup, rhash, requal)
DEFLOOKUP(sLookup, shash, sequal)

/* definition for the general case */
DEFLOOKUP(Lookup, d->hash, d->equal)

/* Now do the table lookup */
static SEXP HashLookup(SEXP table, SEXP x, HashData *d)
{
    SEXP ans;
    R_xlen_t i, n;

    n = XLENGTH(x);
    PROTECT(ans = allocVector(INTSXP, n));
    int *pa = INTEGER0(ans);

    switch (TYPEOF(x)) {
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = iLookup(table, x, i, d);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = rLookup(table, x, i, d);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    pa[i] = sLookup(table, x, i, d);
	break;
    default:
	for (i = 0; i < n; i++)
	    pa[i] = Lookup(table, x, i, d);
    }

    UNPROTECT(1);
    return ans;
}

static SEXP match_transform(SEXP s, SEXP env)
{
    if(OBJECT(s)) {
	if(inherits(s, "factor")) return asCharacterFactor(s);
	/*
	else if(inherits(s, "POSIXlt")) { // and maybe more classes in the future:
					  // Call R's (generic) as.character(s):
	    SEXP call, r;
	    PROTECT(call = lang2(R_AsCharacterSymbol, s));
	    r = eval(call, env);
	    UNPROTECT(1);
	    return r;
	}
	*/
	else {
	    SEXP call, r;
	    PROTECT(call = lang2(install("mtfrm"), s));
	    r = eval(call, env);
	    UNPROTECT(1);
	    return r;
	}
    }
    /* else */
    return duplicate(s);
}

/* assumes that x does not have any element in bytes encoding */
static SEXP asUTF8(SEXP x)
{
    R_xlen_t nx = xlength(x);
    SEXP ux = NULL;
    for(R_xlen_t i = 0; i < nx; i++) {
	SEXP xi = STRING_ELT(x, i);
	if ((xi != NA_STRING) && !IS_ASCII(xi) && !IS_UTF8(xi)) {
	    if (!ux) {
		ux = PROTECT(allocVector(STRSXP, nx));
		for(R_xlen_t j = 0; j < i; j++)
		    SET_STRING_ELT(ux, j, STRING_ELT(x, j));
	    }
	    SET_STRING_ELT(ux, i, mkCharCE(translateCharUTF8(xi), CE_UTF8));
	} else if (ux)
	    SET_STRING_ELT(ux, i, xi);
    }
    if (ux) {
	UNPROTECT(1);
	return ux;
    } else
	return x;
}

// workhorse of R's match() and hence also  " ix %in% itable "
static /* or attribute_hidden? */
SEXP match5(SEXP itable, SEXP ix, int nmatch, SEXP incomp, SEXP env)
{
    R_xlen_t n = xlength(ix);
    /* handle zero length arguments */
    if (n == 0) return allocVector(INTSXP, 0);

    SEXP ans;
    if (length(itable) == 0) {
	ans = allocVector(INTSXP, n);
	int *pa = INTEGER0(ans);
	for (R_xlen_t i = 0; i < n; i++) pa[i] = nmatch;
	return ans;
    }

    GCStackRoot<> x, table;
    int nprot = 0;

    bool D1; /* special case  <Date> o <character> */
    if ((D1 = isObject(ix)     && inherits(ix,     "Date") && isValidString(itable)) ||
	(     isObject(itable) && inherits(itable, "Date") && isValidString(ix))) {
	/* Do *not* translate the <Date> to integer below (which later would be coerced
	 * to character: e.g, as.character(as.vector(as.Date("2025-06-26"))) |--> "20265"
	 * but rather *do*  as.Date(<character>) for the other, and then compare (the numbers of)
	 * as.vector(<Date>).
	*/
	GCStackRoot<> call, form_Ymd;
	form_Ymd = mkString("%Y-%m-%d");
	if(D1) { // table := as.Date.character(itable, "%Y-%m-%d")
	    call = lang3(install("as.Date.character"), itable, form_Ymd);
	    table = eval(call, env);

	    table = match_transform(table, env);
	    x     = match_transform(ix,    env);
	} else { // x := as.Date.character(ix, "%Y-%m-%d")
	    call = lang3(install("as.Date.character"), ix, form_Ymd);
	    x = eval(call, env);

	    x     = match_transform(x,      env);
	    table = match_transform(itable, env);
	}
    } else { /* regular cases */
	x     = match_transform(ix,     env);
    table = match_transform(itable, env);
    }

    SEXPTYPE type;
    /* Coerce to a common type; type == NILSXP is ok here.
     * Note that above we coerce factors and "POSIXlt", only to character.
     * Hence, coerce to character or to `higher' type
     * (given that we have "Vector" or NULL) */
    if(TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP) type = STRSXP;
    else type = TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
    x	    = coerceVector(x,	  type);
    table = coerceVector(table, type);

    // special case scalar x -- for speed only :
    if(XLENGTH(x) == 1 && !incomp) {
      int val = nmatch;
      int ntable = LENGTH(table);
      switch (type) {
      case STRSXP: {
	  SEXP x_val = STRING_ELT(x,0);
	  for (int i=0; i < ntable; i++) if (Seql(STRING_ELT(table,i), x_val)) {
		  val = i + 1; break;
	      }
	  break; }
      case LGLSXP: {
	  int x_val = LOGICAL_ELT(x, 0),
	      *table_p = LOGICAL(table);
	  for (int i=0; i < ntable; i++) if (table_p[i] == x_val) {
		  val = i + 1; break;
	      }
	  break; }
      case INTSXP: {
	  int x_val = INTEGER_ELT(x, 0),
	      *table_p = INTEGER(table);
	  for (int i=0; i < ntable; i++) if (table_p[i] == x_val) {
		  val = i + 1; break;
	      }
	  break; }
      case REALSXP: {
	  double xv = REAL_ELT(x, 0);
	  // pblm with signed 0s under IEC60559
	  double x_val = (xv == 0.) ? 0. : xv;
	  double *table_p = REAL(table);
	  /* we want all NaNs except NA equal, and all NAs equal */
	  if (R_IsNA(x_val)) {
	      for (int i=0; i < ntable; i++) if (R_IsNA(table_p[i])) {
		      val = i + 1; break;
		  }
	  }
	  else if (R_IsNaN(x_val)) {
	      for (int i=0; i < ntable; i++) if (R_IsNaN(table_p[i])) {
		      val = i + 1; break;
		  }
	  }
	  else {
	      for (int i=0; i < ntable; i++) if (table_p[i] == x_val) {
		      val = i + 1; break;
	      }
	  }
	  break; }
      case CPLXSXP: {
	  Rcomplex x_val = COMPLEX_ELT(x, 0),
	      *table_p = COMPLEX(table);
	  for (int i=0; i < ntable; i++)
	      if (cplx_eq(table_p[i], x_val)) {
		  val = i + 1; break;
	      }
	  break; }
      case RAWSXP: {
	  Rbyte x_val = RAW_ELT(x, 0),
	      *table_p = RAW(table);
	  for (int i=0; i < ntable; i++) if (table_p[i] == x_val) {
		  val = i + 1; break;
	      }
	  break; }
	default: error("%s", _("invalid type")); break;
      }
      PROTECT(ans = ScalarInteger(val)); nprot++;
    }
    else { // regular case
	HashData data = { 0 };
	if (incomp) { PROTECT(incomp = coerceVector(incomp, type)); nprot++; }
	data.nomatch = nmatch;
	HashTableSetup(table, &data, NA_INTEGER);
	PROTECT(data.HashTable); nprot++;
	if(type == STRSXP) {
	    bool useBytes = FALSE;
	    bool useUTF8 = FALSE;
	    bool useCache = TRUE;
	    for(R_xlen_t i = 0; i < xlength(x); i++) {
		SEXP s = STRING_ELT(x, i);
		if(IS_BYTES(s)) {
		    useBytes = TRUE;
		    useUTF8 = FALSE;
		    break;
		}
		if(ENC_KNOWN(s)) {
		    useUTF8 = TRUE;
		}
		if(!IS_CACHED(s)) {
		    useCache = FALSE;
		    break;
		}
	    }
	    if(!useBytes || useCache) {
		for(int i = 0; i < LENGTH(table); i++) {
		    SEXP s = STRING_ELT(table, i);
		    if(IS_BYTES(s)) {
			useBytes = TRUE;
			useUTF8 = FALSE;
			break;
		    }
		    if(ENC_KNOWN(s)) {
			useUTF8 = TRUE;
		    }
		    if(!IS_CACHED(s)) {
			useCache = FALSE;
			break;
		    }
		}
	    }
	    if(useUTF8) {
		x = asUTF8(x);
		table = asUTF8(table);
	    }
	    data.useUTF8 = useUTF8;
	    data.useCache = useCache;
	}
	DoHashing(table, &data);
	if (incomp) UndoHashing(incomp, table, &data);
	ans = HashLookup(table, x, &data);
    }
    UNPROTECT(nprot);
    return ans;
} // end{ match5 }

SEXP R::matchE(SEXP itable, SEXP ix, int nmatch, SEXP env)
{
    return match5(itable, ix, nmatch, NULL, env);
}

/* used from other code, not here: */
SEXP Rf_match(SEXP itable, SEXP ix, int nmatch)
{
    return match5(itable, ix, nmatch, NULL, R_BaseEnv);
}


// .Internal(match(x, table, nomatch, incomparables)) :
attribute_hidden SEXP do_match(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if ((!isVector(CAR(args)) && !isNull(CAR(args))) ||
	(!isVector(CADR(args)) && !isNull(CADR(args))))
	error("%s", _("'match' requires vector arguments"));

    int nomatch = asInteger(CADDR(args));
    SEXP incomp = CADDDR(args);

    if (isNull(incomp) || /* S has FALSE to mean empty */
	(length(incomp) == 1 && isLogical(incomp) &&
	 LOGICAL_ELT(incomp, 0) == 0))
	return match5(CADR(args), CAR(args), nomatch, NULL, env);
    else
	return match5(CADR(args), CAR(args), nomatch, incomp, env);
}

/* pmatch and charmatch return integer positions, so cannot be used
   for long vector tables */

/* Partial Matching of Strings */
/* Fully S Compatible version. */

/* Hmm, this was not all S compatible!	The desired behaviour is:
 * First do exact matches, and mark elements as used as they are matched
 *   unless dup_ok is true.
 * Then do partial matching, from left to right, using up the table
 *   unless dup_ok is true.  Multiple partial matches are ignored.
 * Empty strings are unmatched			      BDR 2000/2/16
 */

// .Internal(pmatch(x, table, nomatch, duplicates.ok))
attribute_hidden SEXP do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP input  = CAR(args), // = x     in R
	target = CADR(args), // = table  "
	ans;
    R_xlen_t n_input = XLENGTH(input);

    int n_target = LENGTH(target), // not allowed to be long
	no_match = asInteger(CADDR(args));
    bool no_dups = !asLogicalNoNA(CADDDR(args), "duplicates.ok");

    if (!isString(input) || !isString(target))
	error("%s", _("argument is not of mode character"));

    int *used = NULL;
    if(no_dups) {
	used = (int *) R_alloc((size_t) n_target, sizeof(int));
	for (int j = 0; j < n_target; j++) used[j] = 0;
    }

    bool useBytes = FALSE, useUTF8 = FALSE;
    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(R_xlen_t i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    const char **in = (const char **) R_alloc((size_t) n_input,  sizeof(char *));
    const char **tar = (const char **) R_alloc((size_t) n_target, sizeof(char *));
    PROTECT(ans = allocVector(INTSXP, n_input));
    int *ians = INTEGER0(ans);
    if(useBytes) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = CHAR(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = CHAR(STRING_ELT(target, j));
    }
    else if(useUTF8) {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = translateCharUTF8(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = translateCharUTF8(STRING_ELT(target, j));
    } else {
	for(R_xlen_t i = 0; i < n_input; i++) {
	    in[i] = translateChar(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(int j = 0; j < n_target; j++)
	    tar[j] = translateChar(STRING_ELT(target, j));
    }
    /* First pass, exact matching */
    R_xlen_t nexact = 0;
    /* Compromise when hashing used changed in 3.2.0 (PR#15697) */
    if (n_input <= 100 || n_target <= 100) {
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss = in[i];
	    if (strlen(ss) == 0) continue;
	    for (int j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (streql(ss, tar[j])) {
		    ians[i] = j + 1;
		    if (no_dups) used[j] = 1;
		    nexact++;
		    break;
		}
	    }
	}
    } else {
	HashData data = { 0 };
	HashTableSetup(target, &data, NA_INTEGER);
	data.useUTF8 = useUTF8;
	data.nomatch = 0;
	DoHashing(target, &data);
	for (R_xlen_t i = 0; i < n_input; i++) {
	    if (strlen(in[i]) == 0) /* don't look up "" */
		continue;
	    int j = Lookup(target, input, i, &data);
	    if ((j == 0) || (no_dups && used[j - 1])) continue;
	    if (no_dups) used[j - 1] = 1;
	    ians[i] = j;
	    nexact++;
	}
    }

    if(nexact < n_input) {
	/* Second pass, partial matching */
	for (R_xlen_t i = 0; i < n_input; i++) {
	    const char *ss;
	    if (ians[i]) continue;
	    ss = in[i];
	    size_t temp = strlen(ss);
	    if (temp == 0) continue;
	    int mtch = 0,
		mtch_count = 0;
	    for (int j = 0; j < n_target; j++) {
		if (!(((size_t)i * n_target + j) & 0x1fff))
		    R_CheckUserInterrupt();
		if (no_dups && used[j]) continue;
		if (streqln(ss, tar[j], temp)) {
		    mtch = j + 1;
		    mtch_count++;
		}
	    }
	    if (mtch > 0 && mtch_count == 1) {
		if(no_dups) used[mtch - 1] = 1;
		ians[i] = mtch;
	    }
	}
	/* Third pass, set no matches */
	for (R_xlen_t i = 0; i < n_input; i++)
	    if(ians[i] == 0) ians[i] = no_match;

    }
    UNPROTECT(1);
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

attribute_hidden SEXP do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    const char *ss, *st;
    bool useBytes = FALSE, useUTF8 = FALSE;

    checkArity(op, args);

    input = CAR(args);
    R_xlen_t n_input = LENGTH(input);
    target = CADR(args);
    int n_target = LENGTH(target);

    if (!isString(input) || !isString(target))
	error("%s", _("argument is not of mode character"));
    int no_match = asInteger(CADDR(args));

    for(R_xlen_t i = 0; i < n_input; i++) {
	if(IS_BYTES(STRING_ELT(input, i))) {
	    useBytes = TRUE;
	    useUTF8 = FALSE;
	    break;
	} else if(ENC_KNOWN(STRING_ELT(input, i))) {
	    useUTF8 = TRUE;
	}
    }
    if(!useBytes) {
	for(int i = 0; i < n_target; i++) {
	    if(IS_BYTES(STRING_ELT(target, i))) {
		useBytes = TRUE;
		useUTF8 = FALSE;
		break;
	    } else if(ENC_KNOWN(STRING_ELT(target, i))) {
		useUTF8 = TRUE;
	    }
	}
    }

    PROTECT(ans = allocVector(INTSXP, n_input));
    int *ians = INTEGER0(ans);

    CXXR::RAllocStack::Scope rscope;  // prudence: .Internal does this too.
    for(R_xlen_t i = 0; i < n_input; i++) {
	if(useBytes)
	    ss = CHAR(STRING_ELT(input, i));
	else if(useUTF8)
	    ss = translateCharUTF8(STRING_ELT(input, i));
	else
	    ss = translateChar(STRING_ELT(input, i));
	size_t temp = strlen(ss);
	int imatch = NA_INTEGER;
	bool perfect = FALSE;
	/* we could reset vmax here too: worth it? */
	for(int j = 0; j < n_target; j++) {
	    if(useBytes)
		st = CHAR(STRING_ELT(target, j));
	    else if(useUTF8)
		st = translateCharUTF8(STRING_ELT(target, j));
	    else
		st = translateChar(STRING_ELT(target, j));
	    if (streqln(ss, st, temp)) {
		if (strlen(st) == temp) {
		    if (perfect)
			imatch = 0;
		    else {
			perfect = TRUE;
			imatch = j + 1;
		    }
		}
		else if (!perfect) {
		    if (imatch == NA_INTEGER)
			imatch = j + 1;
		    else
			imatch = 0;
		}
	    }
	}
	ians[i] = (imatch == NA_INTEGER) ? no_match : imatch;
    }
    UNPROTECT(1);
    return ans;
}


/* Functions for matching the supplied arguments to the */
/* formal arguments of functions.  The returned value */
/* is a list with all components named. */

static SEXP StripUnmatched(SEXP s)
{
    if (s == R_NilValue) return s;

    if (CAR(s) == R_MissingArg && !ARGUSED(s) ) {
	return StripUnmatched(CDR(s));
    }
    else if (CAR(s) == R_DotsSymbol ) {
	return StripUnmatched(CDR(s));
    }
    else {
	SETCDR(s, StripUnmatched(CDR(s)));
	return s;
    }
}

static SEXP ExpandDots(SEXP s, int expdots)
{
    SEXP r;
    if (s == R_NilValue)
	return s;
    if (TYPEOF(CAR(s)) == DOTSXP ) {
	SET_TYPEOF(CAR(s), LISTSXP);	/* a safe mutation */
	if (expdots) {
	    r = CAR(s);
	    while (CDR(r) != R_NilValue ) {
		SET_ARGUSED(r, 1);
		r = CDR(r);
	    }
	    SET_ARGUSED(r, 1);
	    SETCDR(r, ExpandDots(CDR(s), expdots));
	    return CAR(s);
	}
    }
    else
	SET_ARGUSED(s, 0);
    SETCDR(s, ExpandDots(CDR(s), expdots));
    return s;
}

static SEXP subDots(SEXP rho)
{
    SEXP rval, dots, a, b, t;
    int len,i;

    dots = R_findVar(R_DotsSymbol, rho);

    if (dots == R_UnboundValue)
	error("%s", _("... used in a situation where it does not exist"));

    if (dots == R_MissingArg)
	return dots;

    if (!isPairList(dots))
	error("%s", _("... is not a pairlist"));

    len = length(dots);
    PROTECT(dots);
    PROTECT(rval=allocList(len));
    for(a = dots, b = rval, i = 1; i <= len; a = CDR(a), b = CDR(b), i++) {
	SET_TAG(b, TAG(a));
	t = CAR(a);
	while (Promise::isA(t))
	    t = PREXPR(t);
	if( isSymbol(t) || isLanguage(t) )
	    SETCAR(b, installDDVAL(i));
	else
	    SETCAR(b, t);
    }
    UNPROTECT(2);
    return rval;
}


attribute_hidden SEXP do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, f, b, rval, sysp, t1, t2, tail;

    checkArity(op,args);

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP)
	funcall = VECTOR_ELT(funcall, 0);

    if (TYPEOF(funcall) != LANGSXP)
	error(_("invalid '%s' argument"), "call");

    b = CAR(args);
    if (TYPEOF(b) != CLOSXP)
	error(_("invalid '%s' argument"), "definition");

    sysp = CAR(CDDDR(args));
    if (!isEnvironment(sysp))
	error(_("'%s' must be an environment"), "envir");

    /* Do we expand ... ? */

    bool expdots = asLogicalNoNA(CAR(CDDR(args)), "expand.dots");

    /* Get the formals and match the actual args */

    formals = FORMALS(b);
    PROTECT(actuals = shallow_duplicate(CDR(funcall)));

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

    t2 = R_MissingArg;
    for (t1=actuals ; t1!=R_NilValue ; t1 = CDR(t1) ) {
	if (CAR(t1) == R_DotsSymbol) {
	    t2 = subDots(sysp);
	    break;
	}
    }
    /* now to splice t2 into the correct spot in actuals */
    if (t2 != R_MissingArg ) {	/* so we did something above */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = listAppend(t2, CDR(actuals));
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, t2);
		    listAppend(actuals,tail);
		    break;
		}
	    }
	}
    } else { /* get rid of it */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = CDR(actuals);
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, tail);
		    break;
		}
	    }
	}
    }
    rlist = matchArgs_RC(formals, actuals, call);

    /* Attach the argument names as tags */

    for (f = formals, b = rlist; b != R_NilValue; b = CDR(b), f = CDR(f)) {
	SET_TAG(b, TAG(f));
    }


    /* Handle the dots */

    PROTECT(rlist = ExpandDots(rlist, expdots));

    /* Eliminate any unmatched formals and any that match R_DotSymbol */
    /* This needs to be after ExpandDots as the DOTSXP might match ... */

    rlist = StripUnmatched(rlist);

    PROTECT(rval = CXXR_cons<Expression>(lazy_duplicate(CAR(funcall)), rlist));

    UNPROTECT(3);
    return rval;
}


#include <R_ext/RS.h> /* for Memzero */

#ifdef _AIX  /*some people just have to be different: is this still needed? */
#    include <memory.h>
#endif


static SEXP rowsum(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans;
    int n, p, ng;
    R_xlen_t offset = 0, offsetg = 0;
    HashData data = { 0 };
    data.nomatch = 0;

    n = LENGTH(g);
    ng = length(uniqueg);
    bool narm = asLogicalNoNA(snarm, "na.rm");
    if(isMatrix(x)) p = ncols(x); else p = 1;

    HashTableSetup(uniqueg, &data, NA_INTEGER);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));
    int *pmatches = INTEGER(matches);

    PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

    switch(TYPEOF(x)){
    case REALSXP:
	Memzero(REAL0(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    double *pa = REAL0(ans);
	    for(int j = 0; j < n; j++) {
		double xjpo = REAL_ELT(x, j + offset);
		if(!narm || !ISNAN(xjpo))
		    pa[pmatches[j] - 1 + offsetg] += xjpo;
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    case INTSXP:
	Memzero(INTEGER0(ans), ng*p);
	for(int i = 0; i < p; i++) {
	    int *pa = INTEGER0(ans);
	    for(int j = 0; j < n; j++) {
		int xjpo = INTEGER_ELT(x, j + offset);
		if (xjpo == NA_INTEGER) {
		    if(!narm)
			pa[pmatches[j] - 1 + offsetg] = NA_INTEGER;
		} else if (pa[pmatches[j] - 1 + offsetg] != NA_INTEGER) {
		    /* check for integer overflows */
		    int itmp = pa[pmatches[j] - 1 + offsetg];
		    double dtmp = itmp;
		    dtmp += xjpo;
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += xjpo;
		    pa[pmatches[j] - 1 + offsetg] = itmp;
		}
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    default:
	error("%s", _("non-numeric matrix in rowsum(): this should not happen"));
    }
    if (TYPEOF(rn) != STRSXP) error("%s", _("row names are not character"));
    SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
    setAttrib(ans, R_DimNamesSymbol, dn);
    SET_VECTOR_ELT(dn, 0, rn);
    dn2 = getAttrib(x, R_DimNamesSymbol);
    if(length(dn2) >= 2 &&
       !isNull(dn3 = VECTOR_ELT(dn2, 1))) SET_VECTOR_ELT(dn, 1, dn3);

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

static SEXP rowsum_df(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn)
{
    SEXP matches,ans,col,xcol;
    int p;
    HashData data = { 0 };
    data.nomatch = 0;

    R_xlen_t n = XLENGTH(g);
    p = LENGTH(x);
    R_xlen_t ng = XLENGTH(uniqueg);
    bool narm = asLogicalNoNA(snarm, "na.rm");

    HashTableSetup(uniqueg, &data, NA_INTEGER);
    PROTECT(data.HashTable);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup(uniqueg, g, &data));
    int *pmatches = INTEGER(matches);

    PROTECT(ans = allocVector(VECSXP, p));

    for(int i = 0; i < p; i++) {
	xcol = VECTOR_ELT(x,i);
	if (!isNumeric(xcol))
	    error("%s", _("non-numeric data frame in rowsum"));
	switch(TYPEOF(xcol)){
	case REALSXP:
	    PROTECT(col = allocVector(REALSXP,ng));
	    Memzero(REAL0(col), ng);
	    for(R_xlen_t j = 0; j < n; j++) {
		double xj = REAL_ELT(xcol, j);
		if(!narm || !ISNAN(xj))
		    REAL0(col)[pmatches[j] - 1] += xj;
	    }
	    SET_VECTOR_ELT(ans,i,col);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(col = allocVector(INTSXP, ng));
	    Memzero(INTEGER0(col), ng);
	    for(R_xlen_t j = 0; j < n; j++) {
		int xj = INTEGER_ELT(xcol, j);
		if (xj == NA_INTEGER) {
		    if(!narm)
			INTEGER0(col)[pmatches[j] - 1] = NA_INTEGER;
		} else if (INTEGER0(col)[pmatches[j] - 1] != NA_INTEGER) {
		    int itmp = INTEGER0(col)[pmatches[j] - 1];
		    double dtmp = itmp;
		    dtmp += xj;
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += xj;
		    INTEGER0(col)[pmatches[j] - 1] = itmp;
		}
	    }
	    SET_VECTOR_ELT(ans, i, col);
	    UNPROTECT(1);
	    break;

	default:
	    error("%s", _("this cannot happen"));
	}
    }
    namesgets(ans, getAttrib(x, R_NamesSymbol));
    if (TYPEOF(rn) != STRSXP) error("%s", _("row names are not character"));
    setAttrib(ans, R_RowNamesSymbol, rn);
    classgets(ans, mkString("data.frame"));

    UNPROTECT(3); /* HashTable, matches, ans */
    return ans;
}

attribute_hidden SEXP do_rowsum(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if(PRIMVAL(op) == 1)
	return rowsum_df(CAR(args), CADR(args), CADDR(args), CADDDR(args),
			 CAD4R(args));
    else
	return rowsum(CAR(args), CADR(args), CADDR(args), CADDDR(args),
		      CAD4R(args));
}


/* returns 1-based duplicate no */
static int isDuplicated2(SEXP x, int indx, HashData *d)
{
    int *h = HTDATA_INT(d);
    hlen i = d->hash(x, indx, d);
    while (h[i] != NIL) {
	if (d->equal(x, h[i], x, indx))
	    return h[i] + 1;
	i = (i + 1) % d->M;
    }
    h[i] = indx;
    return 0;
}

static SEXP duplicated2(SEXP x, HashData *d)
{
    SEXP ans;
    int n;

    n = LENGTH(x);
    HashTableSetup(x, d, NA_INTEGER);
    PROTECT(d->HashTable);
    PROTECT(ans = allocVector(INTSXP, n));

    int *h = HTDATA_INT(d);
    int *v = INTEGER0(ans);
    for (hlen i = 0; i < d->M; i++) h[i] = NIL;
    for (int i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	v[i] = isDuplicated2(x, i, d);
    }
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_makeunique(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP names, sep, ans, dup, newx;
    int i, cnt, *cnts, dp;
    int n, len, maxlen = 0;
    HashData data = { 0 };
    const char *csep, *ss;
    const void *vmax;

    checkArity(op, args);
    names = CAR(args);
    if(!isString(names))
	error(_("'%s' must be a character vector"), "names");
    n = LENGTH(names);
    sep = CADR(args);
    if(!isString(sep) || LENGTH(sep) != 1)
	error(_("'%s' must be a character string"), "sep");
    csep = translateChar(STRING_ELT(sep, 0));
    PROTECT(ans = allocVector(STRSXP, n));
    vmax = vmaxget();
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(ans, i, STRING_ELT(names, i));
	len = (int) strlen(translateChar(STRING_ELT(names, i)));
	if(len > maxlen) maxlen = len;
	vmaxset(vmax);
    }
    if(n > 1) {
	/* +2 for terminator and rounding error */
	size_t sz = maxlen + (int) strlen(csep)
	    + (int) (log((double)n)/log(10.0)) + 2;
	std::unique_ptr<char[]> tmp = std::make_unique<char[]>(sz);
	char *buf = tmp.get();
	if(n < 10000) {
	    R_CheckStack2((size_t)n * sizeof(int));
	    cnts = (int *) alloca(((size_t) n) * sizeof(int));
	} else {
	    /* This is going to be slow so use expensive allocation
	       that will be recovered if interrupted. */
	    cnts = (int *) R_alloc((size_t) n,	sizeof(int));
	}
	for(i = 0; i < n; i++) cnts[i] = 1;
	data.nomatch = 0;
	PROTECT(newx = allocVector(STRSXP, 1));
	PROTECT(dup = duplicated2(names, &data));
	PROTECT(data.HashTable);
	for(i = 1; i < n; i++) { /* first cannot be a duplicate */
	    CXXR::RAllocStack::Scope rscope;
	    dp = INTEGER_ELT(dup, i); /* 1-based number of first occurrence */
	    if(dp == 0) continue;
	    ss = translateChar(STRING_ELT(names, i));
	    /* Try appending 1,2,3, ..., n-1 until it is not already in use */
	    for(cnt = cnts[dp - 1]; cnt < n; cnt++) {
		snprintf(buf, sz, "%s%s%d", ss, csep, cnt);
		SET_STRING_ELT(newx, 0, mkChar(buf));
		if(Lookup(ans, newx, 0, &data) == data.nomatch) break;
	    }
	    SET_STRING_ELT(ans, i, STRING_ELT(newx, 0));
	    /* insert it */ (void) isDuplicated(ans, i, &data);
	    cnts[dp - 1] = cnt+1; /* cache the first unused cnt value */
	}
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return ans;
}

/* Use hashing to improve object.size. Here we want equal CHARSXPs,
   not equal contents. */

inline static bool csequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
{
    return STRING_ELT(x, i) == STRING_ELT(y, j);
}

static void HashTableSetup1(SEXP x, HashData *d)
{
    d->hash = cshash;
    d->equal = csequal;
#ifdef LONG_VECTOR_SUPPORT
    d->isLong = FALSE;
#endif
    MKsetup(XLENGTH(x), d, NA_INTEGER);
    d->HashTable = allocVector(INTSXP, (R_xlen_t) d->M);
    for (hlen i = 0; i < d->M; i++) HTDATA_INT(d)[i] = NIL;
}

/* used in utils */
SEXP R::csduplicated(SEXP x)
{
    if(TYPEOF(x) != STRSXP)
	error("%s", _("C function 'csduplicated' not called on a STRSXP"));
    R_xlen_t n = XLENGTH(x);
    HashData data = { 0 };
    HashTableSetup1(x, &data);
    PROTECT(data.HashTable);
    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    int *v = LOGICAL(ans);

    for (R_xlen_t i = 0; i < n; i++) v[i] = isDuplicated(x, i, &data);

    UNPROTECT(2);
    return ans;
}

#include <R_ext/Random.h>

// sample.int(.) --> .Internal(sample2(n, size)) :
attribute_hidden SEXP do_sample2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans;
    double dn = asReal(CAR(args));
    SEXP sk = CADR(args);
    if (length(sk) != 1)
	error(_("invalid '%s' argument"), "size");
    int k = asInteger(sk);
    if  (length(CAR(args)) != 1 || !R_FINITE(dn) || dn < 0
		|| dn > 4.5e15 || (k > 0 && dn == 0))
	error("%s", _("invalid first argument"));
    if (k < 0) error(_("invalid '%s' argument"), "size"); // includes NA
    if (k > dn/2) error("%s", _("This algorithm is for size <= n/2"));
    HashData data = { 0 };
    GetRNGstate();
    if (dn > INT_MAX) {
	ans = PROTECT(allocVector(REALSXP, k));
	double *ry = REAL0(ans);
	HashTableSetup(ans, &data, NA_INTEGER);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		ry[i] = R_unif_index(dn) + 1;
		if(!isDuplicated(ans, i, &data)) break;
	    }
   } else {
	ans = PROTECT(allocVector(INTSXP, k));
	int *iy = INTEGER0(ans);
	HashTableSetup(ans, &data, NA_INTEGER);
	PROTECT(data.HashTable);
	for (int i = 0; i < k; i++)
	    for(int j = 0; j < 100; j++) { // average < 2
		iy[i] = (int)(R_unif_index(dn) + 1);
		if(!isDuplicated(ans, i, &data)) break;
	    }
    }
    PutRNGstate();
    UNPROTECT(2);
    return ans;
}

/***
 *** Experimental Hash Table Implementation
 ***
 *** The interface is based loosely on the design of the Common Lisp
 *** hash table support. Two equality tests are supported: identical()
 *** and pointer equality.
 ***/

/**
 ** C Level Interface
 **/

/*
 * Low Level Functions
 */

static int hash_identical(SEXP x, int K, bool useCloEnv)
{
    /* using 31 seems to work reasonably */
    if (K == 0 || K > 31) K = 31;

    HashData d = HashData(K, FALSE, TRUE);
    d.useCloEnv = useCloEnv;
    d.extptrAsRef = TRUE;
    d.inHashtab = TRUE;

    int val = (int) vhash_one(x, &d);
    if (val == NA_INTEGER) val = 0;
    if (val < 0) val = -val;
    return val;
}

static int hash_address(SEXP x, int K)
{
    if (K == 0 || K > 31) K = 31;

    HashData d = HashData(K);

    int val = (int) scatter(PTRHASH(x), &d);
    if (val == NA_INTEGER) val = 0;
    if (val < 0) val = -val;
    return val;
}

/* allow for compiling with NAMED */
static R_INLINE SEXP INC_NMD(SEXP x) {
    INCREMENT_NAMED(x);
    return x;
}

#define HT_SEXP(h) (h).cell
#define HT_META_SIZE 3
#define HT_META(h) R_ExternalPtrTag(HT_SEXP(h))

#define HT_TABLE(h) R_ExternalPtrProtected(HT_SEXP(h))
#define SET_HT_TABLE(h, table) R_SetExternalPtrProtected(HT_SEXP(h), table)
#define HT_TABLE_SIZE(h) LENGTH(HT_TABLE(h))

#define HT_COUNT(h) (INTEGER(HT_META(h))[0])
#define HT_TYPE(h) (INTEGER(HT_META(h))[1])
#define HT_TABLE_K(h) (INTEGER(HT_META(h))[2])

#define HT_IS_VALID(h) (R_ExternalPtrAddr(HT_SEXP(h)) != NULL)
#define HT_VALIDATE(h) R_SetExternalPtrAddr(HT_SEXP(h), HT_SEXP(h))

static R_INLINE int HT_HASH(R_hashtab_type h, SEXP key)
{
    switch(HT_TYPE(h)) {
    case HT_TYPE_IDENTICAL:
	return hash_identical(key, HT_TABLE_K(h), TRUE);
    case HT_TYPE_ADDRESS:
	return hash_address(key, HT_TABLE_K(h));
    default:
	error("%s", _("bad hash table type"));
    }
}

static R_INLINE int HT_EQUAL(R_hashtab_type h, SEXP x, SEXP y)
{
    switch(HT_TYPE(h)) {
    case HT_TYPE_IDENTICAL:
	{
	    /* IDENT_USE_CLOENV corresponds to the default in identical().
	       IDENT_EXTPTR_AS_REF ensures that EXTPTRSXP objects in
	       keys unserialize sensibly. */
	    int flags = IDENT_USE_CLOENV | IDENT_EXTPTR_AS_REF;
	    return R_compute_identical(x, y, flags);
	}
    case HT_TYPE_ADDRESS:
	return x == y;
    default:
	error("%s", _("bad hash table type"));
    }
}

static void rehash(R_hashtab_type h, int resize)
{
    /* If the meta-data structure is changed then a saved hash table
       will be invalid. For now, just check and reject incompatible
       oned. For future changes would be better to try to
       rewrite/repair the meta data. */
    if (TYPEOF(HT_META(h)) != INTSXP || LENGTH(HT_META(h)) != HT_META_SIZE)
	error("%s", _("invalid hash table meta data"));

    SEXP old_table = PROTECT(HT_TABLE(h));
    int old_size = LENGTH(old_table);

    R_xlen_t new_size = resize ? 2 * old_size : old_size;
    if (new_size > INT_MAX)
	error("%s", _("hash size would exceed limit"));

    HT_COUNT(h) = 0;
    HT_VALIDATE(h);

    SET_HT_TABLE(h, allocVector(VECSXP, new_size));
    if (resize) HT_TABLE_K(h)++;

    for (int i = 0; i < old_size; i++)
	for (SEXP cell = VECTOR_ELT(old_table, i);
	     cell != R_NilValue;
	     cell = CDR(cell))
	    R_sethash(h, TAG(cell), CAR(cell));

    UNPROTECT(1); /* old_table */
}

static SEXP getcell(R_hashtab_type h, SEXP key, int *pidx)
{
    SEXP table = HT_TABLE(h);

    if (! HT_IS_VALID(h))
	rehash(h, FALSE);

    int idx = HT_HASH(h, key);
    *pidx = idx;

    SEXP chain = VECTOR_ELT(table, idx);
    while (chain != R_NilValue) {
	if (HT_EQUAL(h, TAG(chain), key))
	    return chain;
	chain = CDR(chain);
    }
    return R_NilValue;
}


/*
 * Higer Level Public Functions
 */

/* initial size = 2 ^ HT_INIT_K */
#define HT_INIT_K 3

R_hashtab_type R_mkhashtab(int type, int K)
{
    if (K < 3 || K > 30) K = 3;
    int size = 1;
    for (int i = 0; i < K; i++) size *= 2;

    switch(type) {
    case HT_TYPE_IDENTICAL:
    case HT_TYPE_ADDRESS: break;
    default: error("%s", _("bad hash table type"));
    }
    SEXP table = PROTECT(allocVector(VECSXP, size));
    SEXP meta = PROTECT(allocVector(INTSXP, HT_META_SIZE));
    R_hashtab_type val;
    val.cell = R_MakeExternalPtr(NULL, meta, table);
    HT_VALIDATE(val);
    HT_COUNT(val) = 0;
    HT_TYPE(val) = type;
    HT_TABLE_K(val) = K;
    UNPROTECT(2); /* table, meta */
    return val;
}

SEXP R_gethash(R_hashtab_type h, SEXP key, SEXP nomatch)
{
    int idx;
    PROTECT(HT_SEXP(h));
    PROTECT(key);
    PROTECT(nomatch);
    SEXP cell = getcell(h, key, &idx);
    UNPROTECT(3); /* h, key, nomatch */
    if (cell == R_NilValue)
	return nomatch;
    else
	return CAR(cell);
}

SEXP R_sethash(R_hashtab_type h, SEXP key, SEXP value)
{
    int idx;
    PROTECT(HT_SEXP(h));
    PROTECT(key);
    PROTECT(value);
    SEXP cell = getcell(h, key, &idx);
    if (cell == R_NilValue) {
	int new_count = HT_COUNT(h) + 1;
	if (new_count > 0.5 * HT_TABLE_SIZE(h)) {
	    /* rehash() will fail if new_count would be too large */
	    rehash(h, TRUE);
	    /* recalculate the hash bin, cell will still be R_NilValue */
	    (void) getcell(h, key, &idx);
	}
	SEXP table = HT_TABLE(h);
	SEXP chain = CONS(INC_NMD(value), VECTOR_ELT(table, idx));
	SET_TAG(chain, INC_NMD(key));
	SET_VECTOR_ELT(table, idx, chain);
	HT_COUNT(h) = new_count;
    }
    else {
	SETCAR(cell, value);
	INCREMENT_NAMED(value);
    }
    UNPROTECT(3); /* h, key, value */
    return value;
}

int R_remhash(R_hashtab_type h, SEXP key)
{
    int idx;
    PROTECT(HT_SEXP(h));
    PROTECT(key);
    SEXP cell = getcell(h, key, &idx);
    UNPROTECT(2); /* h, key */

    if (cell == R_NilValue)
	return FALSE;
    else {
	SEXP table = HT_TABLE(h);
	if (cell == VECTOR_ELT(table, idx))
	    SET_VECTOR_ELT(table, idx, CDR(cell));
	else {
	    SEXP prev = VECTOR_ELT(table, idx);
	    while (CDR(prev) !=  cell)
		prev = CDR(prev);
	    SETCDR(prev, CDR(cell));
	}
	HT_COUNT(h)--;
	SETCAR(cell, R_NilValue);  // drop REFCNT on old value
	SET_TAG(cell, R_NilValue); // drop REFCNT on old key
	return TRUE;
    }
}

int R_numhash(R_hashtab_type h) { return HT_COUNT(h); }
int R_typhash(R_hashtab_type h) { return HT_TYPE(h); }

static R_INLINE void defvar(SEXP sym, SEXP val, SEXP env)
{
    defineVar(sym, INC_NMD(val), env);
}

SEXP R_maphash(R_hashtab_type h, SEXP FUN)
{
    PROTECT(HT_SEXP(h));
    PROTECT(FUN);
    SEXP FUN_sym = install("FUN");
    SEXP key_sym = install("key");
    SEXP val_sym = install("value");

    SEXP env = PROTECT(R_NewEnv(R_GlobalEnv, FALSE, 0));
    SEXP call = PROTECT(lang3(FUN_sym, key_sym, val_sym));
    defvar(FUN_sym, FUN, env);

    SEXP table = PROTECT(HT_TABLE(h)); // PROTECT in case FUN causes a rehash
    int size = LENGTH(table);
    for (int i = 0; i < size; i++) {
	SEXP cell = VECTOR_ELT(table, i);
	while (cell != R_NilValue) {
	    SEXP next = PROTECT(CDR(cell));
	    defvar(key_sym, TAG(cell), env); // key is PROTECTed by env
	    defvar(val_sym, CAR(cell), env); // val is PROTECTed by env
	    eval(call, env);
	    cell = next;
	    UNPROTECT(1); /* next */
	}
    }
    UNPROTECT(5); /* h, FUN, env, call, table */
    return R_NilValue;
}

void R_maphashC(R_hashtab_type h, void (*FUN)(SEXP, SEXP, void *), void *data)
{
    PROTECT(HT_SEXP(h));
    SEXP table = PROTECT(HT_TABLE(h)); // PROTECT in case FUN causes a rehash
    int size = LENGTH(table);
    for (int i = 0; i < size; i++) {
	SEXP cell = VECTOR_ELT(table, i);
	while (cell != R_NilValue) {
	    SEXP next = PROTECT(CDR(cell));
	    SEXP key = PROTECT(TAG(cell));
	    SEXP val = PROTECT(CAR(cell));
	    FUN(key, val, data);
	    cell = next;
	    UNPROTECT(3); /* next, key, val */
	}
    }
    UNPROTECT(2); /* h, table */
}

void R_clrhash(R_hashtab_type h)
{
    SEXP table = HT_TABLE(h);
    int size = LENGTH(table);
    for (int i = 0; i < size; i++) {
	for (SEXP cell = VECTOR_ELT(table, i);
	     cell != R_NilValue;
	     cell = CDR(cell)) {
	    SETCAR(cell, R_NilValue);  // drop REFCNT on old value
	    SET_TAG(cell, R_NilValue); // drop REFCNT on old key
	}
	SET_VECTOR_ELT(table, i, R_NilValue);
    }
    HT_COUNT(h) = 0;
    /* could also drop table size back down to HT_INIT_SIZE */
}


/**
 ** R Level Interface Support
 **/

R_hashtab_type R_asHashtable(SEXP h)
{
    if (TYPEOF(h) != VECSXP || LENGTH(h) != 1 || ! inherits(h, "hashtab"))
	error("%s", _("not a proper hash table object"));
    SEXP p = VECTOR_ELT(h, 0);
    if (TYPEOF(p) != EXTPTRSXP)
	error("%s", _("hash table object is corrupted"));
    R_hashtab_type val;
    val.cell = p;
    return val;
}

SEXP R_HashtabSEXP(R_hashtab_type  h)
{
    return HT_SEXP(h);
}

int R_isHashtable(SEXP h)
{
    if (TYPEOF(h) != VECSXP ||
	LENGTH(h) != 1 ||
	! inherits(h, "hashtab") ||
	TYPEOF(VECTOR_ELT(h, 0)) != EXTPTRSXP)
	return FALSE;
    else
	return TRUE;
}

/* This allows experimenting with creating hash tables at the R level. */
attribute_hidden SEXP do_vhash(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x = CAR(args);
    SEXP sK = CADR(args);
    SEXP sUseCloEnv = CADDR(args);

    int K = sK == R_NilValue ? 31 : asInteger(sK);
    bool useCloEnv =
	(sUseCloEnv == R_NilValue) ? true : asBool2(sUseCloEnv,call);

    int val = hash_identical(x, K, useCloEnv);
    return ScalarInteger(val);
}
