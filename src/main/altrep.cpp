/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016--2023   The R Core Team
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
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

/** @file altrep.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/Complex.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Symbol.hpp>
#include <Localization.h>
#include <Defn.h>
#include <R_ext/Altrep.h>

using namespace R;
using namespace CXXR;

/***
 *** ALTREP Abstract Class Framework 
 ***/

/**
 **  ALTREP Class Registry for Serialization
 **/

/* Use ATTRIB field to hold class info. OK since not visible outside. */
#define ALTREP_CLASS_SERIALIZED_CLASS(x) ATTRIB(x)
inline void SET_ALTREP_CLASS_SERIALIZED_CLASS(SEXP x, SEXP csym, SEXP psym, SEXP stype)
{
    GCStackRoot<> pl;
    pl = Rf_list3(csym, psym, stype);
    SET_TAG(pl, Symbol::obtain("Altrep class"));
    SET_TAG(CDR(pl), Symbol::obtain("Package"));
    SET_TAG(CDR(CDR(pl)), Symbol::obtain("Underlying type"));
    SET_ATTRIB(x, pl);
}
#define ALTREP_SERIALIZED_CLASS_CLSSYM(x) CAR(x)
#define ALTREP_SERIALIZED_CLASS_PKGSYM(x) CADR(x)
#define ALTREP_SERIALIZED_CLASS_TYPE(x) (SEXPTYPE) INTEGER0(CADDR(x))[0]
#define ALTREP_OBJECT_CLSSYM(x) ALTREP_SERIALIZED_CLASS_CLSSYM( \
	ALTREP_SERIALIZED_CLASS(x))
#define ALTREP_OBJECT_PKGSYM(x) ALTREP_SERIALIZED_CLASS_PKGSYM( \
	ALTREP_SERIALIZED_CLASS(x))

#define ALTREP_CLASS_BASE_TYPE(x) \
    ALTREP_SERIALIZED_CLASS_TYPE(ALTREP_CLASS_SERIALIZED_CLASS(x))

static SEXP Registry = NULL;

static SEXP LookupClassEntry(SEXP csym, SEXP psym)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain))
	if (TAG(CAR(chain)) == csym && CADR(CAR(chain)) == psym)
	    return CAR(chain);
    return NULL;
}

static void RegisterClass(SEXP class_, SEXPTYPE type, const char *cname, const char *pname,
	      DllInfo *dll)
{
    PROTECT(class_);
    if (Registry == NULL) {
	Registry = CXXR_cons(R_NilValue, R_NilValue);
	R_PreserveObject(Registry);
    }

    Symbol *csym = Symbol::obtain(cname);
    Symbol *psym = Symbol::obtain(pname);
    SEXP stype = PROTECT(ScalarInteger(type));
    SEXP iptr = R_MakeExternalPtr(dll, R_NilValue, R_NilValue);
    SEXP entry = LookupClassEntry(csym, psym);
    if (entry == NULL) {
	entry = list4(class_, psym, stype, iptr);
	SET_TAG(entry, csym);
	SETCDR(Registry, CONS(entry, CDR(Registry)));
    }
    else {
	SETCAR(entry, class_);
	SETCAR(CDR(CDR(entry)), stype);
	SETCAR(CDR(CDR(CDR(entry))), iptr);
    }
    SET_ALTREP_CLASS_SERIALIZED_CLASS(class_, csym, psym, stype);
    UNPROTECT(2); /* class_, stype */
}

static SEXP LookupClass(SEXP csym, SEXP psym)
{
    SEXP entry = LookupClassEntry(csym, psym);
    return entry != NULL ? CAR(entry) : NULL;
}

static void reinit_altrep_class(SEXP sclass);
attribute_hidden void R::R_reinit_altrep_classes(DllInfo *dll)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain)) {
	SEXP entry = CAR(chain);
	SEXP iptr = CAR(CDR(CDR(CDR(entry))));
	if (R_ExternalPtrAddr(iptr) == dll)
	    reinit_altrep_class(CAR(entry));
    }
}


/**
 **  ALTREP Method Tables and Class Objects
 **/

#define ALTREP_ERROR_IN_CLASS(msg, x) do {			\
	error("%s [class: %s, pkg: %s]",			\
	      msg,						\
	      CHAR(PRINTNAME(ALTREP_OBJECT_CLSSYM(x))),		\
	      CHAR(PRINTNAME(ALTREP_OBJECT_PKGSYM(x))));	\
    } while(0)

#define CLASS_METHODS_TABLE(class) STDVEC_DATAPTR(class)
#define GENERIC_METHODS_TABLE(x, class) \
    ((class##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))

#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)
#define ALTINTEGER_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altinteger)
#define ALTREAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altreal)
#define ALTLOGICAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altlogical)
#define ALTRAW_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altraw)
#define ALTCOMPLEX_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altcomplex)
#define ALTSTRING_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altstring)
#define ALTLIST_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altlist)

static SEXP altrep_UnserializeEX_default(SEXP class_, SEXP state, SEXP attr,
                                         int objf, int levs);
static SEXP altrep_Unserialize_default(SEXP class_, SEXP state);
static SEXP altrep_Serialized_state_default(SEXP x);
static SEXP altrep_DuplicateEX_default(SEXP x, Rboolean deep);
static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep);
static SEXP altrep_Coerce_default(SEXP x, int type);
static Rboolean altrep_Inspect_default(SEXP x, int pre, int deep, int pvec,
                                       void (*inspect_subtree)(SEXP, int, int, int));
static R_xlen_t altrep_Length_default(SEXP x);

struct altrep_methods_t
{
    altrep_methods_t();
    R_altrep_UnserializeEX_method_t UnserializeEX;
    R_altrep_Unserialize_method_t Unserialize;
    R_altrep_Serialized_state_method_t Serialized_state;
    R_altrep_DuplicateEX_method_t DuplicateEX;
    R_altrep_Duplicate_method_t Duplicate;
    R_altrep_Coerce_method_t Coerce;
    R_altrep_Inspect_method_t Inspect;
    R_altrep_Length_method_t Length;

    // Virtual destructor for proper cleanup in derived classes
    virtual ~altrep_methods_t() = default;
};

static void *altvec_Dataptr_default(SEXP x, Rboolean writeable);
static const void *altvec_Dataptr_or_null_default(SEXP x);
static SEXP altvec_Extract_subset_default(SEXP x, SEXP indx, SEXP call);

struct altvec_methods_t : public altrep_methods_t
{
    altvec_methods_t();
    R_altvec_Dataptr_method_t Dataptr;
    R_altvec_Dataptr_or_null_method_t Dataptr_or_null;
    R_altvec_Extract_subset_method_t Extract_subset;
};

struct altinteger_methods_t : public altvec_methods_t
{
    altinteger_methods_t();
    R_altinteger_Elt_method_t Elt;
    R_altinteger_Get_region_method_t Get_region;
    R_altinteger_Is_sorted_method_t Is_sorted;
    R_altinteger_No_NA_method_t No_NA;
    R_altinteger_Sum_method_t Sum;
    R_altinteger_Min_method_t Min;
    R_altinteger_Max_method_t Max;
};

struct altreal_methods_t : public altvec_methods_t
{
    altreal_methods_t();
    R_altreal_Elt_method_t Elt;
    R_altreal_Get_region_method_t Get_region;
    R_altreal_Is_sorted_method_t Is_sorted;
    R_altreal_No_NA_method_t No_NA;
    R_altreal_Sum_method_t Sum;
    R_altreal_Min_method_t Min;
    R_altreal_Max_method_t Max;
};

struct altlogical_methods_t : public altvec_methods_t
{
    altlogical_methods_t();
    R_altlogical_Elt_method_t Elt;
    R_altlogical_Get_region_method_t Get_region;
    R_altlogical_Is_sorted_method_t Is_sorted;
    R_altlogical_No_NA_method_t No_NA;
    R_altlogical_Sum_method_t Sum;
};

struct altraw_methods_t : public altvec_methods_t
{
    altraw_methods_t();
    R_altraw_Elt_method_t Elt;
    R_altraw_Get_region_method_t Get_region;
};

struct altcomplex_methods_t : public altvec_methods_t
{
    altcomplex_methods_t();
    R_altcomplex_Elt_method_t Elt;
    R_altcomplex_Get_region_method_t Get_region;
};

struct altstring_methods_t : public altvec_methods_t
{
    altstring_methods_t();
    R_altstring_Elt_method_t Elt;
    R_altstring_Set_elt_method_t Set_elt;
    R_altstring_Is_sorted_method_t Is_sorted;
    R_altstring_No_NA_method_t No_NA;
};

struct altlist_methods_t : public altvec_methods_t
{
    altlist_methods_t();
    R_altstring_Elt_method_t Elt;
    R_altstring_Set_elt_method_t Set_elt;
};

/* Macro to extract first element from ... macro argument.
   From Richard Hansen's answer in
   https://stackoverflow.com/questions/5588855/standard-alternative-to-gccs-va-args-trick
*/
#define DISPATCH_TARGET(...) DISPATCH_TARGET_HELPER(__VA_ARGS__, dummy)
#define DISPATCH_TARGET_HELPER(x, ...) x

#define DO_DISPATCH(type, fun, ...)					\
    type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun(__VA_ARGS__)

#define ALTREP_DISPATCH(fun, ...) DO_DISPATCH(ALTREP, fun, __VA_ARGS__)
#define ALTVEC_DISPATCH(fun, ...) DO_DISPATCH(ALTVEC, fun, __VA_ARGS__)
#define ALTINTEGER_DISPATCH(fun, ...) DO_DISPATCH(ALTINTEGER, fun, __VA_ARGS__)
#define ALTREAL_DISPATCH(fun, ...) DO_DISPATCH(ALTREAL, fun, __VA_ARGS__)
#define ALTLOGICAL_DISPATCH(fun, ...) DO_DISPATCH(ALTLOGICAL, fun, __VA_ARGS__)
#define ALTRAW_DISPATCH(fun, ...) DO_DISPATCH(ALTRAW, fun, __VA_ARGS__)
#define ALTCOMPLEX_DISPATCH(fun, ...) DO_DISPATCH(ALTCOMPLEX, fun, __VA_ARGS__)
#define ALTSTRING_DISPATCH(fun, ...) DO_DISPATCH(ALTSTRING, fun, __VA_ARGS__)
#define ALTLIST_DISPATCH(fun, ...) DO_DISPATCH(ALTLIST, fun, __VA_ARGS__)


/*
 * Generic ALTREP support
 */

attribute_hidden SEXP R::ALTREP_COERCE(SEXP x, int type)
{
    return ALTREP_DISPATCH(Coerce, x, type);
}

static SEXP ALTREP_DUPLICATE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate, x, deep);
}

attribute_hidden SEXP R::ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(DuplicateEX, x, deep);
}

attribute_hidden Rboolean R::ALTREP_INSPECT(SEXP x, int pre, int deep, int pvec,
	       void (*inspect_subtree)(SEXP, int, int, int))
{
    return ALTREP_DISPATCH(Inspect, x, pre, deep, pvec, inspect_subtree);
}


attribute_hidden SEXP R::ALTREP_SERIALIZED_STATE(SEXP x)
{
    return ALTREP_DISPATCH(Serialized_state, x);
}

attribute_hidden SEXP R::ALTREP_SERIALIZED_CLASS(SEXP x)
{
    SEXP val = ALTREP_CLASS_SERIALIZED_CLASS(ALTREP_CLASS(x));
    return val != R_NilValue ? val : NULL;
}

static SEXP find_namespace(void *data) { return R_FindNamespace((SEXP) data); }
static SEXP handle_namespace_error(SEXP cond, void *data) { return R_NilValue; }

static SEXP ALTREP_UNSERIALIZE_CLASS(SEXP info)
{
    if (TYPEOF(info) == LISTSXP) {
	SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
	SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
	SEXP class_ = LookupClass(csym, psym);
	if (class_ == NULL) {
	    GCStackRoot<> pname;
	    pname = ScalarString(PRINTNAME(psym));
	    R_tryCatchError(find_namespace, pname,
			    handle_namespace_error, NULL);
	    class_ = LookupClass(csym, psym);
	}
	return class_;
    }
    return NULL;
}

attribute_hidden SEXP R::ALTREP_UNSERIALIZE_EX(SEXP info, SEXP state, SEXP attr, int objf, int levs)
{
    SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
    SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
    SEXPTYPE type = ALTREP_SERIALIZED_CLASS_TYPE(info);

    /* look up the class in the registry and handle failure */
    SEXP class_ = ALTREP_UNSERIALIZE_CLASS(info);
    if (class_ == NULL) {
	switch(type) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case VECSXP:
	case EXPRSXP:
	    warning(_("cannot unserialize ALTVEC object of class '%s' from package '%s'; returning length zero vector"),
		    CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)));
	    return allocVector(type, 0);
	default:
	    error("%s", _("cannot unserialize this ALTREP object"));
	}
    }

    /* check the registered and unserialized types match */
    SEXPTYPE rtype = ALTREP_CLASS_BASE_TYPE(class_);
    if (type != rtype)
	warning(_("serialized class '%s' from package '%s' has type %s; registered class has type %s"),
		CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)),
		type2char(type), type2char(rtype));

    /* dispatch to a class method */
    altrep_methods_t *m = (altrep_methods_t *) CLASS_METHODS_TABLE(class_);
    SEXP val = m->UnserializeEX(class_, state, attr, objf, levs);
    return val;
}

/*attribute_hidden*/ R_xlen_t R::ALTREP_LENGTH(SEXP x)
{
    return ALTREP_DISPATCH(Length, x);
}

attribute_hidden R_xlen_t R::ALTREP_TRUELENGTH(SEXP x) { return 0; }


/*
 * Generic ALTVEC support
 */

static R_INLINE void *ALTVEC_DATAPTR_EX(SEXP x, Rboolean writable)
{
    /* Disallow taking the writable `DATAPTR()` of an ALTLIST. This
       check could be moved to `DATAPTR()` to catch more faulty
       usages. */
    if (TYPEOF(x) == VECSXP && writable)
        ALTREP_ERROR_IN_CLASS("cannot take a writable DATAPTR of an ALTLIST",
			      x);

    /**** move GC disabling into methods? */
    if (GCManager::gcIsRunning())
	error(_("cannot get '%s' during GC"), "ALTVEC DATAPTR");
    R_CHECK_THREAD;
    GCManager::GCInhibitor no_gc;

    return ALTVEC_DISPATCH(Dataptr, x, writable);
}

/*attribute_hidden*/ void *R::ALTVEC_DATAPTR(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, TRUE);
}

/*attribute_hidden*/ const void *R::ALTVEC_DATAPTR_RO(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, FALSE);
}

attribute_hidden const void *R::ALTVEC_DATAPTR_OR_NULL(SEXP x)
{
    return ALTVEC_DISPATCH(Dataptr_or_null, x);
}

attribute_hidden SEXP R::ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call)
{
    return ALTVEC_DISPATCH(Extract_subset, x, indx, call);
}


/*
 * Typed ALTVEC support
 */

attribute_hidden int R::ALTINTEGER_ELT(SEXP x, R_xlen_t i)
{
    return ALTINTEGER_DISPATCH(Elt, x, i);
}

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = INTEGER_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTINTEGER_DISPATCH(Get_region, sx, i, n, buf);
}

int INTEGER_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int INTEGER_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_DISPATCH(No_NA, x) : 0;
}

attribute_hidden double R::ALTREAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTREAL_DISPATCH(Elt, x, i);
}

R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    const double *x = REAL_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(double));
	return ncopy;
    }
    else
	return ALTREAL_DISPATCH(Get_region, sx, i, n, buf);
}

int REAL_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int REAL_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTREAL_DISPATCH(No_NA, x) : 0;
}

R_xlen_t LOGICAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = (const int *) DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTLOGICAL_DISPATCH(Get_region, sx, i, n, buf);
}

attribute_hidden int LOGICAL_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTLOGICAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}


int LOGICAL_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTLOGICAL_DISPATCH(No_NA, x) : 0;
}


R_xlen_t RAW_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf)
{
    const Rbyte *x = (const Rbyte *) DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTRAW_DISPATCH(Get_region, sx, i, n, buf);
}


R_xlen_t COMPLEX_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf)
{
    const Rcomplex *x = (const Rcomplex *) DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTCOMPLEX_DISPATCH(Get_region, sx, i, n, buf);
}


/*attribute_hidden*/ SEXP R::ALTSTRING_ELT(SEXP x, R_xlen_t i)
{
    /**** move GC disabling into method? */
    if (GCManager::gcIsRunning())
	error(_("cannot get '%s' during GC"), "ALTSTRING_ELT");
    R_CHECK_THREAD;
    GCManager::GCInhibitor no_gc;

    return ALTSTRING_DISPATCH(Elt, x, i);
}

attribute_hidden void R::ALTSTRING_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
    if (GCManager::gcIsRunning())
	error(_("cannot set '%s' during GC"), "ALTSTRING_ELT");
    R_CHECK_THREAD;
    GCManager::GCInhibitor no_gc;

    ALTSTRING_DISPATCH(Set_elt, x, i, v);
}

int STRING_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int STRING_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(No_NA, x) : 0;
}

attribute_hidden SEXP R::ALTLIST_ELT(SEXP x, R_xlen_t i)
{
    /**** move GC disabling into method? */
    if (GCManager::gcIsRunning())
	error(_("cannot get '%s' during GC"), "ALTLIST_ELT");
    R_CHECK_THREAD;
    GCManager::GCInhibitor no_gc;

    return ALTLIST_DISPATCH(Elt, x, i);
}

attribute_hidden void R::ALTLIST_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
    if (GCManager::gcIsRunning())
	error(_("cannot set '%s' during GC"), "ALTLIST_ELT");
    R_CHECK_THREAD;
    GCManager::GCInhibitor no_gc;

    ALTLIST_DISPATCH(Set_elt, x, i, v);
}

attribute_hidden SEXP R::ALTINTEGER_SUM(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Sum, x, narm);
}

attribute_hidden SEXP R::ALTINTEGER_MIN(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Min, x, narm);
}

attribute_hidden SEXP R::ALTINTEGER_MAX(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Max, x, narm);

}

attribute_hidden SEXP R::ALTREAL_SUM(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Sum, x, narm);
}

attribute_hidden SEXP R::ALTREAL_MIN(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Min, x, narm);
}

attribute_hidden SEXP R::ALTREAL_MAX(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Max, x, narm);

}

attribute_hidden SEXP R::ALTLOGICAL_SUM(SEXP x, Rboolean narm)
{
    return ALTLOGICAL_DISPATCH(Sum, x, narm);
}

attribute_hidden int R::ALTLOGICAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTLOGICAL_DISPATCH(Elt, x, i);
}

attribute_hidden Rcomplex R::ALTCOMPLEX_ELT(SEXP x, R_xlen_t i)
{
    return ALTCOMPLEX_DISPATCH(Elt, x, i);
}

attribute_hidden Rbyte R::ALTRAW_ELT(SEXP x, R_xlen_t i)
{
    return ALTRAW_DISPATCH(Elt, x, i);
}


/*
 * Not yet implemented
 */

attribute_hidden void R::ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    INTEGER(x)[i] = v; /* dispatch here */
}

attribute_hidden void R::ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    LOGICAL(x)[i] = v; /* dispatch here */
}

attribute_hidden void R::ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v)
{
    REAL(x)[i] = v; /* dispatch here */
}

attribute_hidden void R::ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    COMPLEX(x)[i] = Complex(v); /* dispatch here */
}

attribute_hidden void R::ALTRAW_SET_ELT(SEXP x, R_xlen_t i, Rbyte v)
{
    RAW(x)[i] = v; /* dispatch here */
}


/**
 ** ALTREP Default Methods
 **/

static SEXP altrep_UnserializeEX_default(SEXP class_, SEXP state, SEXP attr,
					 int objf, int levs)
{
    altrep_methods_t *m = (altrep_methods_t *) CLASS_METHODS_TABLE(class_);
    SEXP val = m->Unserialize(class_, state);
    SET_ATTRIB(val, attr);
    SET_OBJECT(val, objf);
    SETLEVELS(val, levs);
    return val;
}

static SEXP altrep_Serialized_state_default(SEXP x) { return NULL; }

static SEXP altrep_Unserialize_default(SEXP class_, SEXP state)
{
    error("%s", _("cannot unserialize this ALTREP object yet"));
    return R_NilValue;
}

static SEXP altrep_Coerce_default(SEXP x, int type) { return NULL; }

static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep)
{
    return NULL;
}

static SEXP altrep_DuplicateEX_default(SEXP x, Rboolean deep)
{
    GCStackRoot<> ans;
    ans = ALTREP_DUPLICATE(x, deep);

    if (ans != NULL &&
	ans != x) { /* leave attributes alone if returning original */
	/* handle attributes generically */
	SEXP attr = ATTRIB(x);
	if (attr != R_NilValue) {
	    SET_ATTRIB(ans, deep ? duplicate(attr) : shallow_duplicate(attr));
	    SET_OBJECT(ans, OBJECT(x));
	    ans->setS4Object(IS_S4_OBJECT(x));
	}
	else if (ATTRIB(ans) != R_NilValue) {
	    ans->clearAttributes();
	    UNSET_S4_OBJECT(ans);
	}
    }
    return ans;
}

static Rboolean altrep_Inspect_default(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    return FALSE;
}

static R_xlen_t altrep_Length_default(SEXP x)
{
    ALTREP_ERROR_IN_CLASS("no ALTREP Length method defined", x);
    return 0;
}

static void *altvec_Dataptr_default(SEXP x, Rboolean writable)
{
    ALTREP_ERROR_IN_CLASS("cannot access data pointer for this ALTVEC object", x);
    return NULL;
}

static const void *altvec_Dataptr_or_null_default(SEXP x)
{
    return NULL;
}

static SEXP altvec_Extract_subset_default(SEXP x, SEXP indx, SEXP call)
{
    return NULL;
}

static int altinteger_Elt_default(SEXP x, R_xlen_t i) { return INTEGER(x)[i]; }

static R_xlen_t altinteger_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = INTEGER_ELT(sx, k + i);
    return ncopy;
}

static int altinteger_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altinteger_No_NA_default(SEXP x) { return 0; }

static SEXP altinteger_Sum_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altinteger_Min_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altinteger_Max_default(SEXP x, Rboolean narm) { return NULL; }

static double altreal_Elt_default(SEXP x, R_xlen_t i) { return REAL(x)[i]; }

static R_xlen_t altreal_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = REAL_ELT(sx, k + i);
    return ncopy;
}

static int altreal_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altreal_No_NA_default(SEXP x) { return 0; }

static SEXP altreal_Sum_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altreal_Min_default(SEXP x, Rboolean narm) { return NULL; }
static SEXP altreal_Max_default(SEXP x, Rboolean narm) { return NULL; }

static int altlogical_Elt_default(SEXP x, R_xlen_t i) { return LOGICAL(x)[i]; }

static R_xlen_t altlogical_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = LOGICAL_ELT(sx, k + i);
    return ncopy;
}

static int altlogical_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altlogical_No_NA_default(SEXP x) { return 0; }

static SEXP altlogical_Sum_default(SEXP x, Rboolean narm) { return NULL; }


static Rbyte altraw_Elt_default(SEXP x, R_xlen_t i) { return RAW(x)[i]; }

static R_xlen_t altraw_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = RAW_ELT(sx, k + i);
    return ncopy;
}


static Rcomplex altcomplex_Elt_default(SEXP x, R_xlen_t i)
{
    return COMPLEX(x)[i];
}

static R_xlen_t altcomplex_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = COMPLEX_ELT(sx, k + i);
    return ncopy;
}

static SEXP altstring_Elt_default(SEXP x, R_xlen_t i)
{
    ALTREP_ERROR_IN_CLASS("No Elt method found for ALTSTRING class", x);
    return R_NilValue;
}

static void altstring_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    ALTREP_ERROR_IN_CLASS("No Set_elt found for ALTSTRING class", x);
}

static int altstring_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altstring_No_NA_default(SEXP x) { return 0; }

static SEXP altlist_Elt_default(SEXP x, R_xlen_t i)
{
    ALTREP_ERROR_IN_CLASS("ALTLIST classes must provide an Elt method", x);
}

static void altlist_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    ALTREP_ERROR_IN_CLASS("ALTLIST classes must provide a Set_elt method", x);
}

static void *altlist_Dataptr_default(SEXP x, Rboolean writable)
{
    ALTREP_ERROR_IN_CLASS("No Dataptr method found for ALTLIST class", x);
}

static const void *altlist_Dataptr_or_null_default(SEXP x)
{
    return NULL;
}

/**
 ** ALTREP Initial Method Tables
 **/
altrep_methods_t::altrep_methods_t()
{
    UnserializeEX = altrep_UnserializeEX_default;
    Unserialize = altrep_Unserialize_default;
    Serialized_state = altrep_Serialized_state_default;
    DuplicateEX = altrep_DuplicateEX_default;
    Duplicate = altrep_Duplicate_default;
    Coerce = altrep_Coerce_default;
    Inspect = altrep_Inspect_default;
    Length = altrep_Length_default;
};

altvec_methods_t::altvec_methods_t() : altrep_methods_t()
{
    Dataptr = altvec_Dataptr_default;
    Dataptr_or_null = altvec_Dataptr_or_null_default;
    Extract_subset = altvec_Extract_subset_default;
};

altinteger_methods_t::altinteger_methods_t() : altvec_methods_t()
{
    Elt = altinteger_Elt_default;
    Get_region = altinteger_Get_region_default;
    Is_sorted = altinteger_Is_sorted_default;
    No_NA = altinteger_No_NA_default;
    Sum = altinteger_Sum_default;
    Min = altinteger_Min_default;
    Max = altinteger_Max_default;
}
static altinteger_methods_t altinteger_default_methods;

altreal_methods_t::altreal_methods_t() : altvec_methods_t()
{
    Elt = altreal_Elt_default;
    Get_region = altreal_Get_region_default;
    Is_sorted = altreal_Is_sorted_default;
    No_NA = altreal_No_NA_default;
    Sum = altreal_Sum_default;
    Min = altreal_Min_default;
    Max = altreal_Max_default;
}
static altreal_methods_t altreal_default_methods;

altlogical_methods_t::altlogical_methods_t() : altvec_methods_t()
{
    Elt = altlogical_Elt_default;
    Get_region = altlogical_Get_region_default;
    Is_sorted = altlogical_Is_sorted_default;
    No_NA = altlogical_No_NA_default;
    Sum = altlogical_Sum_default;
}
static altlogical_methods_t altlogical_default_methods;

altraw_methods_t::altraw_methods_t() : altvec_methods_t()
{
    Elt = altraw_Elt_default;
    Get_region = altraw_Get_region_default;
}
static altraw_methods_t altraw_default_methods;

altcomplex_methods_t::altcomplex_methods_t() : altvec_methods_t()
{
    Elt = altcomplex_Elt_default;
    Get_region = altcomplex_Get_region_default;
}
static altcomplex_methods_t altcomplex_default_methods;

altstring_methods_t::altstring_methods_t() : altvec_methods_t()
{
    Elt = altstring_Elt_default;
    Set_elt = altstring_Set_elt_default;
    Is_sorted = altstring_Is_sorted_default;
    No_NA = altstring_No_NA_default;
}
static altstring_methods_t altstring_default_methods;

altlist_methods_t::altlist_methods_t() : altvec_methods_t()
{
    Dataptr = altlist_Dataptr_default;
    Dataptr_or_null = altlist_Dataptr_or_null_default;
    Extract_subset = altvec_Extract_subset_default;
    Elt = altlist_Elt_default;
    Set_elt = altlist_Set_elt_default;
}
static altlist_methods_t altlist_default_methods;


/**
 ** Class Constructors
 **/

#define INIT_CLASS(cls, type) do {				\
	*((type##_methods_t *) (CLASS_METHODS_TABLE(cls))) =	\
	    type##_default_methods;				\
    } while (FALSE)

#define MAKE_CLASS(var, type) do {				\
	var = allocVector(RAWSXP, sizeof(type##_methods_t));	\
	R_PreserveObject(var);					\
	INIT_CLASS(var, type);					\
    } while (FALSE)

static R_INLINE R_altrep_class_t R_cast_altrep_class(SEXP x)
{
    /**** some king of optional check? */
    R_altrep_class_t val = R_SUBTYPE_INIT(x);
    return val;
}

static R_altrep_class_t make_altrep_class(SEXPTYPE type, const char *cname, const char *pname, DllInfo *dll)
{
    SEXP class_;
    switch(type) {
    case INTSXP:  MAKE_CLASS(class_, altinteger); break;
    case REALSXP: MAKE_CLASS(class_, altreal);    break;
    case LGLSXP:  MAKE_CLASS(class_, altlogical); break;
    case RAWSXP:  MAKE_CLASS(class_, altraw);     break;
    case CPLXSXP: MAKE_CLASS(class_, altcomplex); break;
    case STRSXP:  MAKE_CLASS(class_, altstring);  break;
    case VECSXP:  MAKE_CLASS(class_, altlist);    break;
    default: error("%s", _("unsupported ALTREP class"));
    }
    RegisterClass(class_, type, cname, pname, dll);
    return R_cast_altrep_class(class_);
}

/*  Using macros like this makes it easier to add new methods, but
    makes searching for source harder. Probably a good idea on
    balance though. */
#define DEFINE_CLASS_CONSTRUCTOR(cls, sexptype)			\
    R_altrep_class_t R_make_##cls##_class(const char *cname,	\
					  const char *pname,	\
					  DllInfo *dll)		\
    {								\
	return  make_altrep_class(sexptype, cname, pname, dll);	\
    }

DEFINE_CLASS_CONSTRUCTOR(altstring, STRSXP)
DEFINE_CLASS_CONSTRUCTOR(altlist, VECSXP)
DEFINE_CLASS_CONSTRUCTOR(altinteger, INTSXP)
DEFINE_CLASS_CONSTRUCTOR(altreal, REALSXP)
DEFINE_CLASS_CONSTRUCTOR(altlogical, LGLSXP)
DEFINE_CLASS_CONSTRUCTOR(altraw, RAWSXP)
DEFINE_CLASS_CONSTRUCTOR(altcomplex, CPLXSXP)

static void reinit_altrep_class(SEXP class_)
{
    switch (ALTREP_CLASS_BASE_TYPE(class_)) {
    case INTSXP: INIT_CLASS(class_, altinteger); break;
    case REALSXP: INIT_CLASS(class_, altreal); break;
    case STRSXP: INIT_CLASS(class_, altstring); break;
    case LGLSXP: INIT_CLASS(class_, altlogical); break;
    case RAWSXP: INIT_CLASS(class_, altraw); break;
    case CPLXSXP: INIT_CLASS(class_, altcomplex); break;
    case VECSXP: INIT_CLASS(class_, altlist); break;
    default: error("%s", _("unsupported ALTREP class"));
    }
}


/**
 ** ALTREP Method Setters
 **/

#define DEFINE_METHOD_SETTER(CNAME, MNAME)				\
    void R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
					  R_##CNAME##_##MNAME##_method_t fun) \
    {									\
	CNAME##_methods_t *m = (CNAME##_methods_t *)CLASS_METHODS_TABLE(R_SEXP(cls));	\
	m->MNAME = fun;							\
    }

DEFINE_METHOD_SETTER(altrep, UnserializeEX)
DEFINE_METHOD_SETTER(altrep, Unserialize)
DEFINE_METHOD_SETTER(altrep, Serialized_state)
DEFINE_METHOD_SETTER(altrep, DuplicateEX)
DEFINE_METHOD_SETTER(altrep, Duplicate)
DEFINE_METHOD_SETTER(altrep, Coerce)
DEFINE_METHOD_SETTER(altrep, Inspect)
DEFINE_METHOD_SETTER(altrep, Length)

DEFINE_METHOD_SETTER(altvec, Dataptr)
DEFINE_METHOD_SETTER(altvec, Dataptr_or_null)
DEFINE_METHOD_SETTER(altvec, Extract_subset)

DEFINE_METHOD_SETTER(altinteger, Elt)
DEFINE_METHOD_SETTER(altinteger, Get_region)
DEFINE_METHOD_SETTER(altinteger, Is_sorted)
DEFINE_METHOD_SETTER(altinteger, No_NA)
DEFINE_METHOD_SETTER(altinteger, Sum)
DEFINE_METHOD_SETTER(altinteger, Min)
DEFINE_METHOD_SETTER(altinteger, Max)

DEFINE_METHOD_SETTER(altreal, Elt)
DEFINE_METHOD_SETTER(altreal, Get_region)
DEFINE_METHOD_SETTER(altreal, Is_sorted)
DEFINE_METHOD_SETTER(altreal, No_NA)
DEFINE_METHOD_SETTER(altreal, Sum)
DEFINE_METHOD_SETTER(altreal, Min)
DEFINE_METHOD_SETTER(altreal, Max)

DEFINE_METHOD_SETTER(altlogical, Elt)
DEFINE_METHOD_SETTER(altlogical, Get_region)
DEFINE_METHOD_SETTER(altlogical, Is_sorted)
DEFINE_METHOD_SETTER(altlogical, No_NA)
DEFINE_METHOD_SETTER(altlogical, Sum)

DEFINE_METHOD_SETTER(altraw, Elt)
DEFINE_METHOD_SETTER(altraw, Get_region)

DEFINE_METHOD_SETTER(altcomplex, Elt)
DEFINE_METHOD_SETTER(altcomplex, Get_region)

DEFINE_METHOD_SETTER(altstring, Elt)
DEFINE_METHOD_SETTER(altstring, Set_elt)
DEFINE_METHOD_SETTER(altstring, Is_sorted)
DEFINE_METHOD_SETTER(altstring, No_NA)

DEFINE_METHOD_SETTER(altlist, Elt)
DEFINE_METHOD_SETTER(altlist, Set_elt)

/**
 ** ALTREP Object Constructor and Utility Functions
 **/

SEXP R_new_altrep(R_altrep_class_t aclass, SEXP data1, SEXP data2)
{
    SEXP sclass = R_SEXP(aclass);
    SEXPTYPE type = ALTREP_CLASS_BASE_TYPE(sclass);
    SEXP ans = CXXR_cons(data1, data2, sclass);
    ALTREP_SET_TYPEOF(ans, type);
    // SET_ALTREP_CLASS
    SETALTREP(ans, 1);

    return ans;
}

Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t class_)
{
    return (Rboolean) (ALTREP(x) && ALTREP_CLASS(x) == R_SEXP(class_));
}

attribute_hidden SEXP do_altrep_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    if (ALTREP(x)) {
	SEXP info = ALTREP_SERIALIZED_CLASS(x);
	SEXP val = allocVector(STRSXP, 2);
	SET_STRING_ELT(val, 0, PRINTNAME(ALTREP_SERIALIZED_CLASS_CLSSYM(info)));
	SET_STRING_ELT(val, 1, PRINTNAME(ALTREP_SERIALIZED_CLASS_PKGSYM(info)));
	return val;
    }
    else
	return R_NilValue;
}

