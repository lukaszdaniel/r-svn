/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025  The R Core Team.
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

/* Internal header, not installed, used in some standard packages */

/** @file Defn.h
 *
 * @brief A ragbag.
 *
 * As CXXR development proceeds, the type definitions, many function
 * prototypes etc. defined in this header file will disappear, because
 * the relevant functionality will have been absorbed into the rho
 * core, and declared within the appropriate header file in the
 * <tt>src/include/CXXR</tt> directory.
 *
 * In a few cases, a declaration within this file is repeated in a
 * header file under <tt>src/include/CXXR</tt>; this is because source
 * files within the rho core never <tt>\#include</tt>s
 * <tt>Defn.h</tt> itself (nor <tt>Rinternals.h</tt>).  In such a case
 * the relevant rho header file is <tt>\#include</tt>d back into
 * <tt>Defn.h</tt>, so that the compiler can detect any inconsistency
 * between the two declarations.
 */

#ifndef DEFN_H_
#define DEFN_H_

#ifndef __cplusplus
#error Defn.h can only be included in C++ files
#endif

#include <CXXR/config.hpp>

#include <vector>
#include <cwchar>
/* some commonly needed headers */
#include <cmath>
#include <cstdlib>
#include <cstring>

/* To test the write barrier used by the generational collector,
   define TESTING_WRITE_BARRIER.  This makes the internal structure of
   SEXPRECs visible only inside of files that explicitly define
   USE_RINTERNALS, and all uses of RObject fields that do not go
   through the appropriate functions or macros will become compilation
   errors.  Since this does impose a small but noticable performance
   penalty, code that includes Defn.h (or code that explicitly defines
   USE_RINTERNALS) can access a RObject's fields directly. */

#ifndef TESTING_WRITE_BARRIER
# define USE_RINTERNALS
#endif

#include <R_ext/Visibility.h>
#include <R_ext/Complex.h>
#include <R_ext/Print.h>
#include <R_ext/RStartup.h> // for otype_t
#include <Errormsg.h>
#include <CXXR/Complex.hpp>
#include <CXXR/NodeStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCNode.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/VectorBase.hpp>

#ifdef __MAIN__
# define extern0 attribute_hidden
#else
# define extern0 extern
#endif

#define attribute_no_sanitizer_instrumentation
#ifdef __has_attribute
# if __has_attribute(disable_sanitizer_instrumentation)
#  undef attribute_no_sanitizer_instrumentation
#  define attribute_no_sanitizer_instrumentation \
     __attribute__((disable_sanitizer_instrumentation))
# elif __has_attribute(no_sanitize)
#  undef attribute_no_sanitizer_instrumentation
#  define attribute_no_sanitizer_instrumentation \
     __attribute__ ((no_sanitize ("address", "thread", "leak", "undefined")))
# endif
#endif

#define MAXELTSIZE 8192 /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */

namespace R {
void CoercionWarning(int);/* warning code */
int LogicalFromInteger(int, int*);
int LogicalFromReal(double, int*);
int LogicalFromComplex(Rcomplex, int*);
int IntegerFromLogical(int, int*);
int IntegerFromReal(double, int*);
int IntegerFromComplex(Rcomplex, int*);
double RealFromLogical(int, int*);
double RealFromInteger(int, int*);
double RealFromComplex(Rcomplex, int*);
Rcomplex ComplexFromLogical(int, int*);
Rcomplex ComplexFromInteger(int, int*);
Rcomplex ComplexFromReal(double, int*);
} // namespace R 

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef CALLED_FROM_DEFN_H

/* UUID identifying the internals version -- packages using compiled
   code should be re-installed when this changes */
#define R_INTERNALS_UUID "2fdf6c18-697a-4ba7-b8ef-11c0d92f1327"

// ======================= USE_RINTERNALS section

namespace R {
#ifdef USE_RINTERNALS
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */
#define CXXR_EXPAND(x, val_1) (((x) == R_NilValue) ? R_NilValue : val_1.get())
#define CXXR_EXPAND2(x, val_1, val_2) (((x) == R_NilValue) ? (val_1) : (val_2))
#define CXXR_EXPAND3(x, val_1, val_2) ((val_2))

/* General GCNode Attributes */
#define NODE_GENERATION(s) ((s)->sxpinfo.m_gcgen)
#define SET_NODE_GENERATION(s,g) ((s)->sxpinfo.m_gcgen=(g))
#define NODE_CLASS(s) ((s)->sxpinfo.gccls)
#define SET_NODE_CLASS(s,v) (((s)->sxpinfo.gccls) = (v))

/* General Cons Cell Attributes */
#define ATTRIB(x)	CXXR_EXPAND((x), (x)->m_attrib)
#define OBJECT(x)	CXXR_EXPAND2((x), 0, (x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.m_mark)
#define TYPEOF(x)	CXXR_EXPAND2((x), NILSXP, (x)->sxpinfo.type)
#define NAMED(x)	CXXR_EXPAND2((x), NAMEDMAX, (x)->sxpinfo.m_refcnt)
#define RTRACE(x)	CXXR_EXPAND2((x), 0, (x)->sxpinfo.trace)
#define LEVELS(x)	((x)->sxpinfo.gp)
#define SET_OBJECT(x,v)	(((x)->sxpinfo.obj)=(v))
#define SET_TYPEOF(x,v)	(((x)->sxpinfo.type)=(v))
#define SET_NAMED(x,v)	(((x)->sxpinfo.m_refcnt)=(v))
#define SET_RTRACE(x,v)	(((x)->sxpinfo.trace)=(v))
#define SETLEVELS(x,v)	(((x)->sxpinfo.gp)=((unsigned short)v))
#define ALTREP(x)       CXXR_EXPAND2((x), 0, (x)->sxpinfo.alt)
#define SETALTREP(x, v) (((x)->sxpinfo.alt) = (v))
#define SETSCALAR(x, v) (((x)->sxpinfo.scalar) = (v))
#define ANY_ATTRIB(x) (ATTRIB(x) != R_NilValue)

#if defined(COMPUTE_REFCNT_VALUES)
# define REFCNT(x) CXXR_EXPAND2((x), REFCNTMAX, (x)->sxpinfo.m_refcnt)
# define REFCNT_ENABLED(x) ((x)->sxpinfo.m_refcnt_enabled)
// # define TRACKREFS(x) REFCNT_ENABLED(x)
#else
# define REFCNT(x) 0
# define REFCNT_ENABLED(x) FALSE
// # define TRACKREFS(x) REFCNT_ENABLED(x)
#endif

#if defined(COMPUTE_REFCNT_VALUES)
# define SET_REFCNT(x,v) ((x)->sxpinfo.m_refcnt = (v))
# if defined(EXTRA_REFCNT_FIELDS)
#  define SET_TRACKREFS(x,v) (REFCNT_ENABLED(x) = (v))
# else
#  define SET_TRACKREFS(x,v) ((x)->sxpinfo.m_refcnt_enabled = (v))
# endif
# define DECREMENT_REFCNT(x) CXXR::GCNode::decRefCount(x)
# define INCREMENT_REFCNT(x) CXXR::GCNode::incRefCount(x)
#else
# define SET_REFCNT(x,v) do {} while(0)
# define SET_TRACKREFS(x,v) do {} while(0)
# define DECREMENT_REFCNT(x) do {} while(0)
# define INCREMENT_REFCNT(x) do {} while(0)
#endif

#define ENABLE_REFCNT(x) SET_TRACKREFS(x, TRUE)
#define DISABLE_REFCNT(x) do { if (TYPEOF(x) != CLOSXP) { SET_TRACKREFS(x, FALSE); } } while (0)

#ifdef SWITCH_TO_REFCNT
# define MARK_NOT_MUTABLE(x) do { if ((x) != R_NilValue) { SET_REFCNT(x, REFCNTMAX); } } while (0)
#else
# define MARK_NOT_MUTABLE(x) do { if ((x) != R_NilValue) { SET_NAMED(x, NAMEDMAX); } } while (0)
#endif

/* To make complex assignments a bit safer, in particular with
   reference counting, a bit is set on the LHS binding cell or symbol
   at the beginning of the complex assignment process and unset at the
   end.

   - When the assignment bit is set and a new value is assigned to the
     binding then the reference count on the old value is not
     decremented. This prevents moving a single binding from the LHS
     variable of the assignment to another variable during the
     assignment process.

  - If a complex assignment tries to update a binding that already has
    its bit set then, the value of the binding is shallow-duplicated
    before proceeding. This ensures that the structure involved in the
    original complex assignment will not be mutated by further R level
    assignments during the original assignment process.

  For now, no attempt is made to unset the bit if the end of an
  assignment is not reached because of a jump. This may result in some
  unnecessary duplications. This could be prevented by maintaining a
  stack of pending assignments to resent the bits on jump, but that
  seems like overkill.

  It might also be useful to use this bit to communicate to functions
  when they are used in a getter/setter context.

  The bit used is bit 11 in the 'gp' field. An alternative would be to
  take a bit from the 'extra' field.

  LT
*/

#define ASSIGNMENT_PENDING_MASK (1 << 11)
#define ASSIGNMENT_PENDING(x) ((x)->sxpinfo.gp & ASSIGNMENT_PENDING_MASK)
#define SET_ASSIGNMENT_PENDING(x, v) do {			\
	if (v) (((x)->sxpinfo.gp) |= ASSIGNMENT_PENDING_MASK);	\
	else (((x)->sxpinfo.gp) &= ~ASSIGNMENT_PENDING_MASK);	\
    } while (0)

/* The same bit can be used to mark calls used in complex assignments
   to allow replacement functions to determine when they are being
   called in an assignment context and can modify an object with one
   reference */
#define MARK_ASSIGNMENT_CALL(call) SET_ASSIGNMENT_PENDING(call, TRUE)
#define IS_ASSIGNMENT_CALL(call) ASSIGNMENT_PENDING(call)

#ifdef SWITCH_TO_REFCNT
# undef NAMED
# undef SET_NAMED
# define NAMED(x) REFCNT(x)
/* no definition for SET_NAMED; any calls will use the one in memory.c */
# define ENSURE_NAMEDMAX(v) do { } while (0)
# define ENSURE_NAMED(v) do { } while (0)
#else
# define ENSURE_NAMEDMAX(v) do {		\
	SEXP __enm_v__ = (v);			\
	if (NAMED(__enm_v__) < NAMEDMAX)	\
	    SET_NAMED( __enm_v__, NAMEDMAX);	\
    } while (0)
# define ENSURE_NAMED(v) do { if (NAMED(v) == 0) SET_NAMED(v, 1); } while (0)
#endif

#ifdef SWITCH_TO_REFCNT
# define SETTER_CLEAR_NAMED(x) do { } while (0)
# define RAISE_NAMED(x, n) do { } while (0)
#else
# define SETTER_CLEAR_NAMED(x) do {			\
	SEXP __x__ = (x);				\
	if (NAMED(__x__) == 1) SET_NAMED(__x__, 0);	\
    } while (0)
# define RAISE_NAMED(x, n) do {			\
	SEXP __x__ = (x);			\
	int __n__ = (n);			\
	if (NAMED(__x__) < __n__)		\
	    SET_NAMED(__x__, __n__);		\
    } while (0)
#endif

/* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK ((unsigned short)(1<<4))
#define S4TAG(e) ((e)->u.s4ptr.m_tag)
#define IS_S4_OBJECT(x) CXXR_EXPAND2((x), 0, (x)->sxpinfo.gp & S4_OBJECT_MASK)
#define SET_S4_OBJECT(x) (((x)->sxpinfo.gp) |= S4_OBJECT_MASK)
#define UNSET_S4_OBJECT(x) (((x)->sxpinfo.gp) &= ~S4_OBJECT_MASK)

/* JIT optimization support */
#define NOJIT_MASK ((unsigned short)(1<<5))
#define NOJIT(x) ((x)->sxpinfo.gp & NOJIT_MASK)
#define SET_NOJIT(x) (((x)->sxpinfo.gp) |= NOJIT_MASK)
#define MAYBEJIT_MASK ((unsigned short)(1<<6))
#define MAYBEJIT(x) ((x)->sxpinfo.gp & MAYBEJIT_MASK)
#define SET_MAYBEJIT(x) (((x)->sxpinfo.gp) |= MAYBEJIT_MASK)
#define UNSET_MAYBEJIT(x) (((x)->sxpinfo.gp) &= ~MAYBEJIT_MASK)

/* Growable vector support */
#define GROWABLE_MASK ((unsigned short)(1<<5))
#define GROWABLE_BIT_SET(x) ((x)->sxpinfo.gp & GROWABLE_MASK)
#define SET_GROWABLE_BIT(x) (((x)->sxpinfo.gp) |= GROWABLE_MASK)
#define IS_GROWABLE(x) (GROWABLE_BIT_SET(x) && XLENGTH(x) < XTRUELENGTH(x))

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
# define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
# define IS_LONG_VEC(x) 0
#endif
#define STDVEC_LENGTH(x) (((CXXR::VectorBase *) (x))->u.vecsxp.m_length)
#define STDVEC_TRUELENGTH(x) (((CXXR::VectorBase *) (x))->u.vecsxp.m_truelength)
#define SET_STDVEC_TRUELENGTH(x, v) (STDVEC_TRUELENGTH(x)=(v))
#define SET_TRUELENGTH(x,v) do {				\
	SEXP sl__x__ = (x);					\
	R_xlen_t sl__v__ = (v);					\
	if (ALTREP(x)) error("%s", _("can't set ALTREP truelength"));	\
	SET_STDVEC_TRUELENGTH(sl__x__, sl__v__);	\
    } while (0)

#define IS_SCALAR(x, t) ((x) != R_NilValue && ((x)->sxpinfo.type == (t)) && (x)->sxpinfo.scalar)
#define LENGTH(x) LENGTH_EX(x, __FILE__, __LINE__)
#define TRUELENGTH(x) XTRUELENGTH(x)

/* defined as a macro since fastmatch packages tests for it */
#define XLENGTH(x) XLENGTH_EX(x)

/* THIS ABSOLUTELY MUST NOT BE USED IN PACKAGES !!! */
#define SET_STDVEC_LENGTH(x,v) do {		\
	SEXP __x__ = (x);			\
	R_xlen_t __v__ = (v);			\
	STDVEC_LENGTH(__x__) = __v__;		\
	SETSCALAR(__x__, __v__ == 1 ? 1 : 0);	\
    } while (0)

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define STDVEC_DATAPTR(x) ((void *) (((CXXR::VectorBase *) (x))->u.vecsxp.m_data)) // data part
// #define STDVEC_DATAPTR(x) ((void *) (((CXXR::VectorBase *) (x)) + 1)) // data part
#undef CHAR
#define CHAR(x)		((const char *) STDVEC_DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((CXXR::Complex *) DATAPTR(x))
#define CXXR_COMPLEX(x)	((CXXR::Complex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))
#define LOGICAL_RO(x)	((const int *) DATAPTR_RO(x))
#define INTEGER_RO(x)	((const int *) DATAPTR_RO(x))
#define RAW_RO(x)	((const Rbyte *) DATAPTR_RO(x))
#define COMPLEX_RO(x)	((const CXXR::Complex *) DATAPTR_RO(x))
#define REAL_RO(x)	((const double *) DATAPTR_RO(x))
#define STRING_PTR_RO(x)((const SEXP *) DATAPTR_RO(x))
#define VECTOR_PTR_RO(x)((const SEXP *) DATAPTR_RO(x))

/* External Pointer Access Macros */
#define EXTPTR_PROT(x)	((x)->u.extptr.m_protege)
#define EXTPTR_TAG(x)	((x)->u.extptr.m_tag)
#define EXTPTR_PTR(e)	((e)->u.extptr.m_ptr)

/* Weak Reference Access Macros */
#define WEAKREF_KEY(w) ((w)->u.weakrrefptr.m_key)
#define WEAKREF_VALUE(w) ((w)->u.weakrrefptr.m_value)
#define WEAKREF_FINALIZER(w) ((w)->u.weakrrefptr.m_finalizer)
void SET_WEAKREF_KEY(SEXP x, SEXP v);
void SET_WEAKREF_VALUE(SEXP x, SEXP v);
void SET_WEAKREF_FINALIZER(SEXP x, SEXP v);

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		CXXR_EXPAND((e), (e)->u.listsxp.m_tag)
#define CAR0(e)		CXXR_EXPAND((e), (e)->u.listsxp.m_car)
#define CDR(e)		CXXR_EXPAND((e), (e)->u.listsxp.m_tail)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CDDDR(e)	CDR(CDR(CDR(e)))
#define CD4R(e)		CDR(CDR(CDR(CDR(e))))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD3R(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)
#define BNDCELL_TAG(e)	CXXR_EXPAND2((e), NILSXP, (e)->sxpinfo.m_binding_tag)
#define SET_BNDCELL_TAG(e, v) ((e)->sxpinfo.m_binding_tag = (v))
#define SET_PROMISE_TAG(x, v) SET_BNDCELL_TAG(x, v)

// #if ( SIZEOF_SIZE_T < SIZEOF_DOUBLE )
# define BOXED_BINDING_CELLS 1
// #else
// # define BOXED_BINDING_CELLS 0
// # define IMMEDIATE_PROMISE_VALUES 1
// #endif

#if BOXED_BINDING_CELLS
/* Use allocated scalars to hold immediate binding values. A little
   less efficient but does not change memory layout or use. These
   allocated scalars must not escape their bindings. */
#define BNDCELL_DVAL(v) SCALAR_DVAL(CAR0(v))
#define BNDCELL_IVAL(v) SCALAR_IVAL(CAR0(v))
#define BNDCELL_LVAL(v) SCALAR_LVAL(CAR0(v))

#define SET_BNDCELL_DVAL(cell, dval) SET_SCALAR_DVAL(CAR0(cell), dval)
#define SET_BNDCELL_IVAL(cell, ival) SET_SCALAR_IVAL(CAR0(cell), ival)
#define SET_BNDCELL_LVAL(cell, lval) SET_SCALAR_LVAL(CAR0(cell), lval)

#define INIT_BNDCELL(cell, type) do {		\
	SEXP val = allocVector(type, 1);	\
	SETCAR(cell, val);			\
	INCREMENT_NAMED(val);			\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)
#else
/* Use a union in the CAR field to represent an SEXP or an immediate
   value.  More efficient, but changes the menory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
union R_bndval_t {
    SEXP sxpval;
    double dval;
    int ival;
    int lval;
};

#define BNDCELL_DVAL(v) ((R::R_bndval_t *) &CAR0(v))->dval
#define BNDCELL_IVAL(v) ((R::R_bndval_t *) &CAR0(v))->ival
#define BNDCELL_LVAL(v) ((R::R_bndval_t *) &CAR0(v))->lval

#define SET_BNDCELL_DVAL(cell, dval) (BNDCELL_DVAL(cell) = (dval))
#define SET_BNDCELL_IVAL(cell, ival) (BNDCELL_IVAL(cell) = (ival))
#define SET_BNDCELL_LVAL(cell, lval) (BNDCELL_LVAL(cell) = (lval))

#define INIT_BNDCELL(cell, type) do {		\
	if (BNDCELL_TAG(cell) == NILSXP)	\
	    SETCAR(cell, R_NilValue);		\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)
#endif

/* ByteCode Access Macros */
#define CODE0(x)	((x)->u.bytecode.m_code)
#define CONSTS(x)	((x)->u.bytecode.m_constants)
#define EXPR(x)	((x)->u.bytecode.m_expression)

/* AltRep Access Macros */
#define DATA1(x)	((x)->u.altrep.m_data1)
#define DATA2(x)	((x)->u.altrep.m_data2)
#define CLASS(x)	((x)->u.altrep.m_altclass)

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.m_formals)
#define BODY(x)		((x)->u.closxp.m_body)
#define CLOENV(x)	((x)->u.closxp.m_env)
#define RDEBUG(x)	((x)->sxpinfo.debug)
#define SET_RDEBUG(x,v)	(((x)->sxpinfo.debug)=(v))
#define RSTEP(x)	((x)->sxpinfo.m_rstep)
#define SET_RSTEP(x,v)	(((x)->sxpinfo.m_rstep)=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	CXXR_EXPAND((x), (x)->u.symsxp.m_pname)
#define SYMVALUE(x)	((x)->u.symsxp.m_value)
#define INTERNAL(x)	((x)->u.symsxp.m_internal)
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) if (v) { SET_DDVAL_BIT(x); } else { UNSET_DDVAL_BIT(x); } /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	CXXR_EXPAND((x), (x)->u.envsxp.m_frame)
#define ENCLOS(x)	((x)->u.envsxp.m_enclos)
#define HASHTAB(x)	CXXR_EXPAND((x), (x)->u.envsxp.m_hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))
#define ENV_RDEBUG(x)	((x)->sxpinfo.debug)
#define SET_ENV_RDEBUG(x,v)	(((x)->sxpinfo.debug)=(v))

/* Test macros with function versions above */
#undef isNull
#define isNull(s)	(TYPEOF(s) == NILSXP)
#undef isSymbol
#define isSymbol(s)	(TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s)	(TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s)	(TYPEOF(s) == REALSXP)
#undef isComplex
#define isComplex(s)	(TYPEOF(s) == CPLXSXP)
#undef isRaw
#define isRaw(s)	(TYPEOF(s) == RAWSXP)
#undef isExpression
#define isExpression(s) (TYPEOF(s) == EXPRSXP)
#undef isEnvironment
#define isEnvironment(s) (TYPEOF(s) == ENVSXP)
#undef isString
#define isString(s)	(TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s)	(OBJECT(s) != 0)

/* macro version of R_CheckStack */
#define R_CheckStack() do {						\
	int dummy;							\
	intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy); \
	if (R_CStackLimit != (uintptr_t)(-1) && usage > ((intptr_t) R_CStackLimit)) \
	    R_SignalCStackOverflow(usage);				\
    } while (FALSE)

#ifdef __has_feature
# if __has_feature(address_sanitizer)
#  undef R_CheckStack
# endif
#endif

#ifdef R_CheckStack
# if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__)
#  undef R_CheckStack
# endif
#endif

#endif /* USE_RINTERNALS */

#define INCREMENT_LINKS(x) do {			\
	SEXP il__x__ = (x);			\
	INCREMENT_NAMED(il__x__);		\
	INCREMENT_REFCNT(il__x__);		\
    } while (0)
#define DECREMENT_LINKS(x) do {			\
	SEXP dl__x__ = (x);			\
	DECREMENT_NAMED(dl__x__);		\
	DECREMENT_REFCNT(dl__x__);		\
    } while (0)

/* Complex assignment support */
/* temporary definition that will need to be refined to distinguish
   getter from setter calls */
#define IS_GETTER_CALL(call) (CADR(call) == R_TmpvalSymbol)

#ifdef LONG_VECTOR_SUPPORT
NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif

/* checking for mis-use of multi-threading */
#ifdef TESTING_WRITE_BARRIER
# define THREADCHECK
#endif
#ifdef THREADCHECK
void R_check_thread(const char *s);
# define R_CHECK_THREAD R_check_thread(__func__)
#else
# define R_CHECK_THREAD do {} while (0)
#endif

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the internal headers.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
bool (REFCNT_ENABLED)(SEXP x);
// void (SET_OBJECT)(SEXP x, int v); // declared in Rinternals.h
// void (SET_TYPEOF)(SEXP x, SEXPTYPE v); // declared in Rinternals.h
// void (SET_NAMED)(SEXP x, int v); // declared in Rinternals.h
void (ENSURE_NAMEDMAX)(SEXP x);
void (ENSURE_NAMED)(SEXP x);
void (SETTER_CLEAR_NAMED)(SEXP x);
void (RAISE_NAMED)(SEXP x, int n);
void (DECREMENT_REFCNT)(SEXP x);
void (INCREMENT_REFCNT)(SEXP x);
void (DISABLE_REFCNT)(SEXP x);
void (ENABLE_REFCNT)(SEXP x);

/* S4 object setting */
// void (SET_S4_OBJECT)(SEXP x); // declared in Rinternals.h
// void (UNSET_S4_OBJECT)(SEXP x); // declared in Rinternals.h

bool (ASSIGNMENT_PENDING)(SEXP x);
void (SET_ASSIGNMENT_PENDING)(SEXP x, int v);
bool (IS_ASSIGNMENT_CALL)(SEXP x);
void (MARK_ASSIGNMENT_CALL)(SEXP x);

/* JIT optimization support */
bool (NOJIT)(SEXP x);
bool (MAYBEJIT)(SEXP x);
void (SET_NOJIT)(SEXP x);
void (SET_MAYBEJIT)(SEXP x);
void (UNSET_MAYBEJIT)(SEXP x);

/* Growable vector support */
// int (IS_GROWABLE)(SEXP x); // declared in Rinternals.h
// void (SET_GROWABLE_BIT)(SEXP x); // declared in Rinternals.h

/* Vector Access Functions */
// void (SETLENGTH)(SEXP x, R_xlen_t v); // declared in Rinternals.h
// void (SET_TRUELENGTH)(SEXP x, R_xlen_t v); // declared in Rinternals.h
// int  (SETLEVELS)(SEXP x, int v); // declared in Rinternals.h
R_xlen_t (STDVEC_LENGTH)(SEXP);
R_xlen_t (STDVEC_TRUELENGTH)(SEXP);
void (SETALTREP)(SEXP, int);

/* Binding Cell Access Functions */
SEXPTYPE (BNDCELL_TAG)(SEXP e);
void (SET_BNDCELL_TAG)(SEXP e, SEXPTYPE v);
double (BNDCELL_DVAL)(SEXP cell);
int (BNDCELL_IVAL)(SEXP cell);
int (BNDCELL_LVAL)(SEXP cell);
void (SET_BNDCELL_DVAL)(SEXP cell, double v);
void (SET_BNDCELL_IVAL)(SEXP cell, int v);
void (SET_BNDCELL_LVAL)(SEXP cell, int v);
void (INIT_BNDCELL)(SEXP cell, SEXPTYPE type);
void SET_BNDCELL(SEXP cell, SEXP val);
SEXPTYPE (PROMISE_TAG)(SEXP e);
void (SET_PROMISE_TAG)(SEXP e, SEXPTYPE v);

/* List Access Functions */
SEXP (CAR0)(SEXP e);
void (SET_MISSING)(SEXP x, unsigned int v);
SEXP CONS_NR(SEXP a, SEXP b);

/* Symbol Access Functions */
void (SET_DDVAL)(SEXP x, int v);
void SET_PRINTNAME(SEXP x, SEXP v);
void SET_SYMVALUE(SEXP x, SEXP v);
void SET_INTERNAL(SEXP x, SEXP v);

/* Environment Access Functions */
// void (SET_ENVFLAGS)(SEXP x, int v); // declared in Rinternals.h
// void SET_FRAME(SEXP x, SEXP v); // declared in Rinternals.h
// void SET_ENCLOS(SEXP x, SEXP v); // declared in Rinternals.h
// void SET_HASHTAB(SEXP x, SEXP v); // declared in Rinternals.h

/* Promise Access Functions */
void (SET_PRSEEN)(SEXP x, int v);
// void SET_PRENV(SEXP x, SEXP v); // declared in Rinternals.h
// void SET_PRVALUE(SEXP x, SEXP v); // declared in Rinternals.h
// void SET_PRCODE(SEXP x, SEXP v); // declared in Rinternals.h
void IF_PROMSXP_SET_PRVALUE(SEXP x, SEXP v);
bool (PROMISE_IS_EVALUATED)(SEXP x);

/* Hashing Functions */
bool (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);

/* Bytecode access macros */
#define BCODE_CODE(x)	CODE0(x)
//#define BCODE_CONSTS(x) CONSTS(x)
#define BCODE_EXPR(x)	EXPR(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* ALTREP internal support */
// int (IS_SCALAR)(SEXP x, SEXPTYPE type); // declared in Rinternals.h
void ALTREP_SET_TYPEOF(SEXP, SEXPTYPE); /* in memory.c */
SEXP ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep);
SEXP ALTREP_COERCE(SEXP x, int type);
Rboolean ALTREP_INSPECT(SEXP, int, int, int, void (*)(SEXP, int, int, int));
SEXP ALTREP_SERIALIZED_CLASS(SEXP);
SEXP ALTREP_SERIALIZED_STATE(SEXP);
SEXP ALTREP_UNSERIALIZE_EX(SEXP, SEXP, SEXP, int, int);
R_xlen_t ALTREP_LENGTH(SEXP x);
R_xlen_t ALTREP_TRUELENGTH(SEXP x);
void *ALTVEC_DATAPTR(SEXP x);
const void *ALTVEC_DATAPTR_RO(SEXP x);
const void *ALTVEC_DATAPTR_OR_NULL(SEXP x);
SEXP ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call);

/* data access */
int ALTINTEGER_ELT(SEXP x, R_xlen_t i);
void ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v);
int ALTLOGICAL_ELT(SEXP x, R_xlen_t i);
void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v);
double ALTREAL_ELT(SEXP x, R_xlen_t i);
void ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v);
SEXP ALTSTRING_ELT(SEXP, R_xlen_t);
void ALTSTRING_SET_ELT(SEXP, R_xlen_t, SEXP);
Rcomplex ALTCOMPLEX_ELT(SEXP x, R_xlen_t i);
void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v);
Rbyte ALTRAW_ELT(SEXP x, R_xlen_t i);
void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, Rbyte v);
SEXP ALTLIST_ELT(SEXP, R_xlen_t);
void ALTLIST_SET_ELT(SEXP, R_xlen_t, SEXP);

/* invoking ALTREP class methods */
SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MIN(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MAX(SEXP x, Rboolean narm);
// SEXP INTEGER_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
// SEXP INTEGER_IS_NA(SEXP x);
SEXP ALTREAL_SUM(SEXP x, Rboolean narm);
SEXP ALTREAL_MIN(SEXP x, Rboolean narm);
SEXP ALTREAL_MAX(SEXP x, Rboolean narm);
// SEXP REAL_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
// SEXP REAL_IS_NA(SEXP x);
SEXP ALTLOGICAL_SUM(SEXP x, Rboolean narm);

/* constructors for internal ALTREP classes */
SEXP R_compact_intrange(R_xlen_t n1, R_xlen_t n2);
SEXP R_deferred_coerceToString(SEXP v, SEXP info);
// SEXP R_virtrep_vec(SEXP, SEXP);
// SEXP R_tryWrap(SEXP); // declared in Rinternals.h
SEXP R_tryUnwrap(SEXP);

bool Rf_pmatch(SEXP, SEXP, bool);
// Rboolean Rf_psmatch(const char *, const char *, Rboolean); // declared in Rinternals.h
void printwhere(void);
void readS3VarsFromFrame(SEXP, SEXP*, SEXP*, SEXP*, SEXP*, SEXP*, SEXP*);

NORET void R_signal_protect_error(void);
NORET void R_signal_unprotect_error(void);
NORET void R_signal_reprotect_error(PROTECT_INDEX i);

// const char *R_curErrorBuf(void); // declared in Rinternals.h
bool R_cycle_detected(SEXP s, SEXP child);

void R_init_altrep(void);
void R_reinit_altrep_classes(DllInfo *);

const char *translateCharFP(SEXP);
const char *translateCharFP2(SEXP);
const char *trCharUTF8(SEXP);
const char *trCharUTF82(SEXP);
const wchar_t *wtransChar2(SEXP);

extern0 SEXP	R_CommentSymbol;    /* "comment" */
extern0 SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern0 SEXP	R_ExactSymbol;	    /* "exact" */
extern0 SEXP	R_RecursiveSymbol;  /* "recursive" */
extern0 SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern0 SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern0 SEXP	R_UseNamesSymbol;   /* "use.names" */
extern0 SEXP	R_ColonSymbol;         /* ":" */
//extern0 SEXP	R_DoubleColonSymbol;   /* "::" */
//extern0 SEXP	R_TripleColonSymbol;   /* ":::" */
extern0 SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern0 SEXP    R_DevicesSymbol;  /* ".Devices" */

extern0 SEXP    R_dot_Methods;  /* ".Methods" */
extern0 SEXP    R_dot_Group;  /* ".Group" */
extern0 SEXP    R_dot_Class;  /* ".Class" */
extern0 SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern0 SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */


 /* writable char access for R internal use only */
#define CHAR_RW(x)	((char *) CHAR(x))

/* CHARSXP charset bits */
#define BYTES_MASK (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK (1<<3)
/* (1<<4) is taken by S4_OBJECT_MASK */
#define CACHED_MASK (1<<5)
#define ASCII_MASK (1<<6)
#define HASHASH_MASK 1
/**** HASHASH uses the first bit -- see HASHASH_MASK defined below */

#ifdef USE_RINTERNALS
# define IS_BYTES(x) ((x)->sxpinfo.gp & BYTES_MASK)
# define SET_BYTES(x) (((x)->sxpinfo.gp) |= BYTES_MASK)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define SET_LATIN1(x) (((x)->sxpinfo.gp) |= LATIN1_MASK)
# define IS_ASCII(x) ((x)->sxpinfo.gp & ASCII_MASK)
# define SET_ASCII(x) (((x)->sxpinfo.gp) |= ASCII_MASK)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define SET_UTF8(x) (((x)->sxpinfo.gp) |= UTF8_MASK)
# define IS_NATIVE(x) ((!IS_LATIN1(x) && !IS_UTF8(x) && !IS_BYTES(x)))
# define ENC_KNOWN(x) ((x)->sxpinfo.gp & (LATIN1_MASK | UTF8_MASK))
# define SET_CACHED(x) (((x)->sxpinfo.gp) |= CACHED_MASK)
# define IS_CACHED(x) (((x)->sxpinfo.gp) & CACHED_MASK)
#else /* USE_RINTERNALS */
/* Needed only for write-barrier testing */
bool IS_NATIVE(SEXP x);
bool IS_BYTES(SEXP x);
void SET_BYTES(SEXP x);
bool IS_LATIN1(SEXP x);
void SET_LATIN1(SEXP x);
bool IS_ASCII(SEXP x);
void SET_ASCII(SEXP x);
bool IS_UTF8(SEXP x);
void SET_UTF8(SEXP x);
int ENC_KNOWN(SEXP x);
void SET_CACHED(SEXP x);
bool IS_CACHED(SEXP x);
#endif /* USE_RINTERNALS */

/* macros and declarations for managing CHARSXP cache */
# define CXHEAD(x) (x)
# define CXTAIL(x) ATTRIB(x)
SEXP (SET_CXTAIL)(SEXP x, SEXP y);
} // namespace R



#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

/* F77_SYMBOL was a minimal version of F77_SUB from RS.h, 
   used in main/util.c and main/registration.c
   F77_QSYMBOL is unused
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif
*/

/*  Heap and Pointer Protection Stack Sizes.  */

/* These headers are all required by C99.
   However, we use types below such as uintptr_t which are optional in C11.
   And on some older systems they were in inttypes.h but not stdint.h.

   Up to 2.11.1 (r52035, May 2010) we had

#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
 typedef long intptr_t;
#endif
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif
    but size_t might be better.

 */
#ifdef HAVE_INTTYPES_H
# include <cinttypes>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <cstdint>
#endif
#ifdef HAVE_LIMITS_H
# include <climits>
#endif

#if defined HAVE_DECL_SIZE_MAX && HAVE_DECL_SIZE_MAX
  typedef size_t R_size_t;
# define R_SIZE_T_MAX SIZE_MAX
#else
# error SIZE_MAX is required for C99
#endif

#define Kilo 1024. /* 1 Kilo Byte := 2^10 Bytes */
#define Mega 1048576.    /* Misnomer !! 1 MiB (mebibyte) := 2^20 (= 1048576) Bytes */
#define Giga 1073741824. /* Misnomer !! 1 GiB (gibibyte) := 2^30 = (2^10)^3 Bytes */

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in ../main/startup.c */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	50000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#ifndef R_VSIZE
#define	R_VSIZE		67108864L // 64 MB
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif
#ifdef __cplusplus
} // extern "C"
#endif

/* PATH_MAX has historically been understood to be the maximal length in
   bytes of an entire path name that may exist.  In current POSIX, when
   defined, it is the maximum number of bytes that will be stored to a
   user-supplied buffer by file-system operations which do not allow the
   caller to provide actual size of the buffer (and such calls are rare).
   If the system limited path names to a certain value, it shall not be less
   than this value, if defined.

   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris, macOS, *BSD have 1024, Linux glibc has 4192 (but glibc does not
   enforce the limit).  File names are limited to FILENAME_MAX bytes
   (usually the same as PATH_MAX) or NAME_MAX (often 255/256).

   POSIX requires PATH_MAX to be defined in limits.h (included above) if
   independent of the file path (file system).  However, if it can vary by
   filepath, it is required to be undefined there. */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* MinGW-W64 defines PATH_MAX to MAX_PATH, which is 260 in Windows. It used to
   be the maximal length of an entire path name (depending on the API used,
   in bytes or in UCS-2 units, including the terminator).  This is no longer
   the case and like PATH_MAX on Unix, MAX_PATH is only used as the limit in
   several old API calls for the user buffer when the size cannot be passed
   by user.  Some additional old components of Windows and old APIs seem not
   to support longer paths.

   But longer paths can and do exist, they can be created using some API,
   usually UCS-2, sometimes using the extended syntax
   (\\?\D:\very_long_path) but sometimes using the common syntax.
   Applications that haven't opted for "long paths" (and those also have to
   be enabled system-wide) are shielded from seeing the long paths in some
   API calls to prevent buffer-overflow and other errors in user code that
   did not check the lengths.

   Like on Unix, user/library code should not depend on that there is a
   maximum to the path length.  The actual real maximum on Windows is not
   even precisely specified.
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

/* R_PATH_MAX is an R-defined limit on the length of entire path name,
   including the terminator.  It may be used in some older code that has not
   yet been rewritten to support arbitrary path lengths.  Such code still
   has to detect longer paths, but may not support them, e.g.  may throw an
   error. R_PATH_MAX shall be at least PATH_MAX, when PATH_MAX is defined. */
#ifdef Unix
# define R_PATH_MAX PATH_MAX
#else
  /* On Windows, 260 is too limiting */
# define R_PATH_MAX 5000
#endif

namespace R {
#define HSIZE	  49157	/* The size of the hash table for symbols */
#define MAXIDSIZE 10000	/* Largest symbol size,
			   in bytes excluding terminator.
			   Was 256 prior to 2.13.0, now just a sanity check.
			*/

/** @brief Current evaluation status.
 */
enum EvaluationStatus
{
    DEFAULT = 0,      /**< Default status of this Promise. */
    UNDER_EVALUATION, /**< This Promise is currently under evaluation. */
    INTERRUPTED       /**< Evaluation of this Promise has been interrupted by a jump */
};

#ifdef USE_RINTERNALS
/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Primitive Access Macros */
#define PRIMOFFSET(x)	((x)->u.primsxp.m_offset)
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.m_offset)=(v))
#define PRIMFUN(x)	(R_FunTab[(x)->u.primsxp.m_offset].cfun)
#define PRIMNAME(x)	(R_FunTab[(x)->u.primsxp.m_offset].name)
#define PRIMVAL(x)	(R_FunTab[(x)->u.primsxp.m_offset].code)
#define PRIMARITY(x)	(R_FunTab[(x)->u.primsxp.m_offset].arity)
#define PPINFO(x)	(R_FunTab[(x)->u.primsxp.m_offset].gram)
#define PRIMPRINT(x)	(((R_FunTab[(x)->u.primsxp.m_offset].eval)/100)%10)
#define PRIMINTERNAL(x)	(((R_FunTab[(x)->u.primsxp.m_offset].eval)%100)/10)

/* Promise Access Macros */
#define PRCODE(x)	((x)->u.promsxp.m_expr)
#define PRENV(x)	((x)->u.promsxp.m_env)
#define PRVALUE0(x) ((x)->u.promsxp.m_value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))
#ifdef IMMEDIATE_PROMISE_VALUES
# define PRVALUE(x) \
    (PROMISE_TAG(x) ? R_expand_promise_value(x) : PRVALUE0(x).get())
# define PROMISE_IS_EVALUATED(x) \
    (PROMISE_TAG(x) || PRVALUE0(x) != R_UnboundValue)
# define PROMISE_TAG(x)  BNDCELL_TAG(x)
#else
# define PRVALUE(x) PRVALUE0(x).get()
# define PROMISE_IS_EVALUATED(x) (PRVALUE(x) != R_UnboundValue)
# define PROMISE_TAG(x) NILSXP
#endif

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp & HASHASH_MASK)
#define HASHVALUE(x)    ((int) TRUELENGTH(x))
#define SET_HASHASH(x,v) ((v) ? (((x)->sxpinfo.gp) |= HASHASH_MASK) : \
			  (((x)->sxpinfo.gp) &= (~HASHASH_MASK)))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, ((int) (v)))

/* Vector Heap Structure */
#if !(SIZEOF_DOUBLE == 8)
# error SIZEOF_DOUBLE has to be equal to 8
#endif
#define VECREC double

/* Vector Heap Macros */
template <typename T>
R_size_t to_doubles(R_xlen_t n_elem)
{
    if (n_elem <= 0) return 0;

    return 1 + (n_elem * sizeof(T) - 1) / sizeof(VECREC);
}
#define BYTE2VEC(n) to_doubles<Rbyte>(n)
#define INT2VEC(n) to_doubles<int>(n)
#define FLOAT2VEC(n) to_doubles<double>(n)
#define COMPLEX2VEC(n) to_doubles<Rcomplex>(n)
#define PTR2VEC(n) to_doubles<SEXP>(n)

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) CXXR_EXPAND2((b), FALSE, (b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) do {						\
	SEXP lb__b__ = b;						\
	if (! IS_ACTIVE_BINDING(lb__b__)) {				\
	    if (TYPEOF(lb__b__) == SYMSXP)				\
		MARK_NOT_MUTABLE(SYMVALUE(lb__b__));			\
	    else							\
		MARK_NOT_MUTABLE(CAR(lb__b__));				\
	}								\
	((lb__b__))->sxpinfo.gp |= BINDING_LOCK_MASK;			\
    } while (0)
#define UNLOCK_BINDING(b) ((b)->sxpinfo.gp &= (~BINDING_LOCK_MASK))

#define BASE_SYM_CACHED_MASK (1<<13)
#define SET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp |= BASE_SYM_CACHED_MASK)
#define UNSET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp &= (~BASE_SYM_CACHED_MASK))
#define BASE_SYM_CACHED(b) ((b)->sxpinfo.gp & BASE_SYM_CACHED_MASK)

#define SPECIAL_SYMBOL_MASK (1<<12)
#define SET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define IS_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)
#define SET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)

#else /* USE_RINTERNALS */

int (PRIMOFFSET)(SEXP x);
void (SET_PRIMOFFSET)(SEXP x, int v);

#define PRIMFUN(x)	(R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x)	(R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x)	(R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x)	(R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x)	(R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x)	(((R_FunTab[PRIMOFFSET(x)].eval)/100)%10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval)%100)/10)


bool (IS_ACTIVE_BINDING)(SEXP b);
bool (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

void (SET_BASE_SYM_CACHED)(SEXP b);
void (UNSET_BASE_SYM_CACHED)(SEXP b);
bool (BASE_SYM_CACHED)(SEXP b);

void (SET_SPECIAL_SYMBOL)(SEXP b);
void (UNSET_SPECIAL_SYMBOL)(SEXP b);
bool (IS_SPECIAL_SYMBOL)(SEXP b);
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b);
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b);
bool (NO_SPECIAL_SYMBOLS)(SEXP b);

#endif /* USE_RINTERNALS */

/* saved bcEval() state for implementing recursion using goto */
// typedef struct R_bcFrame R_bcFrame_type;

/* Miscellaneous Definitions */
// #define streql(s, t)	(!strcmp((s), (t)))
// #define streqln(s, t, n)	(!strncmp((s), (t), (n)))

/* Arithmetic and Relation Operators */
typedef enum {
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
} ARITHOP_TYPE;

typedef enum {
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
} RELOP_TYPE;

typedef enum {
    MATPROD_DEFAULT = 1,
    MATPROD_INTERNAL,
    MATPROD_BLAS,
    MATPROD_DEFAULT_SIMD  /* experimental */
} MATPROD_TYPE;

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1


/*--- Global Variables ---------------------------------------------------- */

/* Defined and initialized in names.c (not main.c) :*/
// #ifndef __R_Names__
// extern
// std::vector<FUNTAB> R_FunTab;	    /* Built in functions */
// #endif
} // namespace R

#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif
namespace R {
LibExtern SEXP  R_SrcfileSymbol;    /* "srcfile" */
LibExtern SEXP  R_SrcrefSymbol;     /* "srcref" */


// LibExtern bool R_interrupts_suspended INI_as(FALSE); // declared in GraphicsDevice.h
// LibExtern bool R_interrupts_pending INI_as(FALSE); // declared in GraphicsDevice.h

/* R Home Directory */
// LibExtern char *R_Home;		    /* Root of the R tree */ // declared in Rinterface.h

/* Memory Management */
extern0 R_size_t R_NSize  INI_as(R_NSIZE);/* Size of cons cell heap */
extern0 R_size_t R_VSize  INI_as(R_VSIZE);/* Size of the vector heap */
// extern0 bool	R_GCEnabled INI_as(1);
// extern0 int	R_in_gc INI_as(0);
// extern0 bool	R_BCIntActive INI_as(0); /* bcEval called more recently than eval */
// extern0 void*	R_BCpc INI_as(NULL);/* current byte code instruction */
// extern0 SEXP	R_BCbody INI_as(NULL); /* current byte code object */
// extern0 R_bcFrame_type *R_BCFrame INI_as(NULL); /* bcEval() frame */
// extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
// extern0 R_size_t R_Collected;	    /* Number of free cons cells (after gc) */
extern0 int	R_Is_Running;	    /* for Windows memory manager */

/* The Pointer Protection Stack */
// LibExtern size_t	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
// LibExtern size_t	R_PPStackTop;	    /* The top of the stack */
// LibExtern SEXP*	R_PPStack;	    /* The pointer protection stack */

void R_ReleaseMSet(SEXP mset, int keepSize);

/* Evaluation Environment */
// extern0 SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
// extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */

// extern bool R_Visible;	    /* Value visibility flag */
// extern0 int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser :
						 * options(deparse.max.lines) */
// extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
// extern0 int	R_Expressions_keep INI_as(5000);/* options(expressions) */
extern0 bool R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 bool R_CBoundsCheck	INI_as(FALSE);	/* options(CBoundsCheck) */
extern0 MATPROD_TYPE R_Matprod	INI_as(MATPROD_DEFAULT);  /* options(matprod) */
extern0 int	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);

/* C stack checking */
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_OldCStackLimit INI_as((uintptr_t)0); /* Old value while
							   handling overflow */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
/* Default here is for Windows: set from configure in src/unix/system.c */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

/* File Input/Output */
// LibExtern bool R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/ // declared in Rinterface.h
extern0 bool R_Quiet	INI_as(FALSE);	/* Be as quiet as possible */
// extern bool  R_NoEcho	INI_as(FALSE);	/* do not echo R code */ // declared in Rinterface.h
extern0 bool R_Verbose	INI_as(FALSE);	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
// extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */ // declared in Rinterface.h
// extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */ // declared in Rinterface.h
extern0 int	R_ErrorCon	INI_as(2);	/* Error connection */
// LibExtern char *R_TempDir	INI_as(NULL);	/* Name of per-session dir */ // declared in Rembedded.h
extern0 char   *Sys_TempDir	INI_as(NULL);	/* Name of per-session dir
						   if set by R itself */
extern0 char	R_StdinEnc[31]  INI_as("");	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
LibExtern int	R_ParseError	INI_as(0); /* Line where parse error occurred */
extern0 int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern0 SEXP	R_ParseErrorFile;   /* Source file where parse error was seen.  Either a
				       STRSXP or (when keeping srcrefs) a SrcFile ENVSXP */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
LibExtern char	R_ParseErrorMsg[PARSE_ERROR_SIZE] INI_as("");
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
LibExtern char	R_ParseContext[PARSE_CONTEXT_SIZE] INI_as("");
LibExtern int	R_ParseContextLast INI_as(0); /* last character in context buffer */
LibExtern int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
// extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */ // declared in Rembedded.h

/* History */
// LibExtern char *R_HistoryFile;	/* Name of the history file */ // declared in Rinterface.h
// LibExtern int	R_HistorySize;	/* Size of the history file */ // declared in Rinterface.h
// LibExtern int	R_RestoreHistory;	/* restore the history file? */ // declared in Rinterface.h
// extern void 	R_setupHistory(void); // declared in Rinterface.h

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
extern0 SEXP	R_Warnings;	    /* the warnings and their calls */
extern0 bool	R_ShowErrorMessages INI_as(1);	/* show error messages? */
extern0 SEXP	R_HandlerStack;	/* Condition handler stack */
extern0 SEXP	R_RestartStack;	/* Stack of available restarts */
extern0 bool R_warn_partial_match_args   INI_as(FALSE);
extern0 bool R_warn_partial_match_dollar INI_as(FALSE);
extern0 bool R_warn_partial_match_attr INI_as(FALSE);
extern0 bool R_ShowWarnCalls INI_as(FALSE);
extern0 bool R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

LibExtern bool utf8locale  INI_as(FALSE);  /* is this a UTF-8 locale? */
// LibExtern bool mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */ // declared in GraphicsDevice.h
extern0   bool latin1locale INI_as(FALSE); /* is this a Latin-1 locale? */
LibExtern int      R_MB_CUR_MAX INI_as(FALSE); /* corrected variant of MB_CUR_MAX */
#ifdef Win32
// LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */ // declared in internal.h
LibExtern unsigned int systemCP  INI_as(437);  /* the ANSI codepage, GetACP */
extern0   bool WinUTF8out  INI_as(FALSE);  /* Use UTF-8 for output */
extern0   void WinCheckUTF8(void);
#endif

extern const char* OutDec	INI_as(".");  /* decimal point used for output */
extern0 bool R_DisableNLinBrowser	INI_as(FALSE);
extern0 char R_BrowserLastCommand	INI_as('n');

/* Initialization of the R environment when it is embedded */
// extern int Rf_initEmbeddedR(int argc, char **argv); // declared in Rembedded.h

/* GUI type */

// extern const char	*R_GUIType	INI_as("unknown"); // declared in Rinterface.h
// extern bool R_isForkedChild		INI_as(FALSE); /* was this forked? */

extern0 double cpuLimit			INI_as(-1.0);
extern0 double cpuLimit2	       	INI_as(-1.0);
extern0 double cpuLimitValue		INI_as(-1.0);
extern0 double elapsedLimit		INI_as(-1.0);
extern0 double elapsedLimit2		INI_as(-1.0);
extern0 double elapsedLimitValue       	INI_as(-1.0);

void resetTimeLimits(void);
void R_CheckTimeLimits(void);

extern0 int R_jit_enabled INI_as(0); /* has to be 0 during R startup */
extern0 int R_compile_pkgs INI_as(0);
extern0 int R_check_constants INI_as(0);
// extern0 int R_disable_bytecode INI_as(0);
extern SEXP R_cmpfun1(SEXP); /* unconditional fresh compilation */
extern void R_init_jit_enabled(void);
extern void R_initEvalSymbols(void);
extern SEXP R_getCurrentSrcref(void);
extern SEXP R_getBCInterpreterExpression(void);
extern ptrdiff_t R_BCRelPC(SEXP, void *);

LibExtern int R_num_math_threads INI_as(1);
LibExtern int R_max_num_math_threads INI_as(1);

/* Pointer  type and utilities for dispatch in the methods package */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP); /* typedef */
//R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef, SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef, SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.c, set in Init_R_Machine */
extern0 int R_dec_min_exponent		INI_as(-308);

/* structure for caching machine accuracy values */
typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;

LibExtern AccuracyInfo R_AccuracyInfo;

extern unsigned int max_contour_segments INI_as(25000);

/* used in package utils */
// extern bool known_to_be_latin1 INI_as(FALSE);
// extern0 bool known_to_be_utf8 INI_as(FALSE);

/* pre-allocated boolean values */
// LibExtern SEXP R_TrueValue INI_as(NULL);
// LibExtern SEXP R_FalseValue INI_as(NULL);
// LibExtern SEXP R_LogicalNAValue INI_as(NULL);

/* for PCRE as from R 3.4.0 */
extern0 bool R_PCRE_use_JIT INI_as(TRUE);
#ifdef HAVE_PCRE2
extern0 int R_PCRE_study INI_as(-2);
#else
extern0 int R_PCRE_study INI_as(10);
#endif
extern0 int R_PCRE_limit_recursion;


#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

#define checkArity(a,b) Rf_checkArityCall(a,b,call)

/*--- FUNCTIONS ------------------------------------------------------ */

/* Internal type coercions */
bool asRbool(SEXP x, SEXP call);
bool asBool2(SEXP x, SEXP call);
int asLogical2(SEXP x, bool checking, SEXP call);
bool asLogicalNoNA(SEXP x, const char *str);
bool asLogicalNAFalse(SEXP x);


typedef enum { iSILENT, iWARN, iERROR } warn_type;

/* Other Internally Used Functions, excluding those which are inline-able*/
SEXP applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP, bool);
void addMissingVarsToNewEnv(SEXP, SEXP);
SEXP allocFormalsList2(SEXP sym1, SEXP sym2);
SEXP allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3);
SEXP allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4);
SEXP allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5);
SEXP allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6);
SEXP R_allocObject(void);
SEXP arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP fixSubset3Args(SEXP, SEXP, SEXP, SEXP*);
int countContexts(unsigned int, bool);
SEXP CreateTag(SEXP);
SEXP DropDims(SEXP);
bool R_envHasNoSpecialSymbols(SEXP);
SEXP ExtractSubset(SEXP, SEXP, SEXP);
SEXP findFun3(SEXP, SEXP, SEXP);
void findFunctionForBody(SEXP);
int FixupDigits(SEXP, warn_type);
int FixupWidth(SEXP, warn_type);
SEXP installDDVAL(int i);
SEXP installS3Signature(const char *, const char *);
bool isFree(SEXP);
bool isUnmodifiedSpecSym(SEXP sym, SEXP env);
SEXP matchE(SEXP, SEXP, int, SEXP);
// void setSVector(SEXP*, int, SEXP);
SEXP stringSuffix(SEXP, int);
const char *translateChar0(SEXP);

void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP);
SEXP R_bcDecode(SEXP);
void R_registerBC(SEXP, SEXP);
bool R_checkConstants(bool);
bool R_BCVersionOK(SEXP);
int R_NaN_is_R_NA(double);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);
#if 0
# define allocCharsxp		Rf_allocCharsxp
# define asBool2	       	Rf_asBool2
# define asRbool		Rf_asRbool
# define asVecSize		Rf_asVecSize
# define asXLength		Rf_asXLength
# define begincontext		Rf_begincontext
# define BindDomain		Rf_BindDomain
# define check_stack_balance	Rf_check_stack_balance
# define check1arg		Rf_check1arg
# define CheckFormals		Rf_CheckFormals
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define createS3Vars		Rf_createS3Vars
# define currentTime		Rf_currentTime
# define CustomPrintValue	Rf_CustomPrintValue
# define DataFrameClass		Rf_DataFrameClass
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1m		Rf_deparse1m
# define deparse1w		Rf_deparse1w
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define DispatchAnyOrEval      Rf_DispatchAnyOrEval
# define dynamicfindVar		Rf_dynamicfindVar
# define EncodeChar             Rf_EncodeChar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeReal2            Rf_EncodeReal2
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define endcontext		Rf_endcontext
# define errorcall_cpy		Rf_errorcall_cpy
# define ErrorMessage		Rf_ErrorMessage
# define evalList		Rf_evalList
# define evalListKeepMissing	Rf_evalListKeepMissing
# define factorsConform		Rf_factorsConform
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define FrameClassFix		Rf_FrameClassFix
# define framedepth		Rf_framedepth
# define frameSubscript		Rf_frameSubscript
# define get1index		Rf_get1index
# define GetOptionCutoff       	Rf_GetOptionCutoff
# define getVar			Rf_getVar
# define getVarInFrame		Rf_getVarInFrame
# define InitArithmetic		Rf_InitArithmetic
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitBaseEnv		Rf_InitBaseEnv
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitS3DefaultTypes	Rf_InitS3DefaultTypes
# define InitTempDir		Rf_InitTempDir
# define InitTypeTables		Rf_InitTypeTables
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
//# define installTrChar		Rf_installTrChar
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define levelsgets		Rf_levelsgets
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs_NR		Rf_matchArgs_NR
# define matchArgs_RC		Rf_matchArgs_RC
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mbcsToUcs2		Rf_mbcsToUcs2
# define memtrace_report	Rf_memtrace_report
# define mkCharWUTF8		Rf_mkCharWUTF8
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkQUOTE		Rf_mkQUOTE
# define mkSYMSXP		Rf_mkSYMSXP
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onintrNoResume		Rf_onintrNoResume
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define patchArgsByActuals	Rf_patchArgsByActuals
# define PrintInit              Rf_PrintInit
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define Seql			Rf_Seql
# define sexptype2char		Rf_sexptype2char
# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define StrToInternal		Rf_StrToInternal
# define strmat2intmat		Rf_strmat2intmat
# define substituteList		Rf_substituteList
# define TimeToSeed		Rf_TimeToSeed
# define translateCharFP	Rf_translateCharFP
# define translateCharFP2	Rf_translateCharFP2
# define trCharUTF8      	Rf_trCharUTF8
# define trCharUTF82      	Rf_trCharUTF82
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
# define wtransChar2		Rf_wtransChar2
# define yychar			Rf_yychar
# define yylval			Rf_yylval
# define yynerrs		Rf_yynerrs
# define yyparse		Rf_yyparse
#endif
/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

/* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
#define CONSOLE_BUFFER_SIZE 4096
int	R_ReadConsole(const char *, unsigned char *, int, int);
void	R_WriteConsole(const char *, int); /* equivalent to R_WriteConsoleEx(a, b, otype_t(0)) */
void	R_WriteConsoleEx(const char *, int, otype_t);
void	R_ResetConsole(void);
// void R_FlushConsole(void); // declared in R.h and Rinterface.h
// void R_ClearerrConsole(void); // declared in Rinterface.h
void	R_Busy(int);
int	R_ShowFiles(int, const char **, const char **, const char *,
		    Rboolean, const char *);
int     R_EditFiles(int, const char **, const char **, const char *);
int	R_ChooseFile(int, char *, int);
// char	*R_HomeDir(void); // declared in Rinterface.h
bool R_FileExists(const char *);
bool R_HiddenFile(const char *);
double	R_FileMtime(const char *);
int	R_GetFDLimit(void);
int	R_EnsureFDLimit(int);
bool R_IsDirPath(const char *);

/* environment cell access */
typedef struct { SEXP cell; } R_varloc_t; /* use struct to prevent casting */
#define R_VARLOC_IS_NULL(loc) ((loc).cell == NULL)
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
R_varloc_t R_findVarLoc(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
bool R_GetVarLocMISSING(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);
SEXP R_findVar(SEXP, SEXP);
SEXP R_findVarInFrame(SEXP, SEXP);

/* deparse option bits: change do_dump if more are added */
enum DeparseOptionBits
{
    KEEPINTEGER = 1,
    QUOTEEXPRESSIONS = 2,
    SHOWATTRIBUTES = 4,
    USESOURCE = 8,
    WARNINCOMPLETE = 16,
    DELAYPROMISES = 32,
    KEEPNA = 64,
    S_COMPAT = 128,
    HEXNUMERIC = 256,
    DIGITS17 = 512,
    NICE_NAMES = 1024,
    /* common combinations of the above */
    SIMPLEDEPARSE = 0,
    DEFAULTDEPARSE = 1089, /* KEEPINTEGER | KEEPNA | NICE_NAMES, used for calls */
    FORSOURCING = 95       /* not DELAYPROMISES, used in edit.c */
};

/* Coercion functions */
int LogicalFromString(SEXP, int*);
int IntegerFromString(SEXP, int*);
double RealFromString(SEXP, int*);
Rcomplex ComplexFromString(SEXP, int*);
SEXP StringFromLogical(int);
SEXP StringFromInteger(int, int*);
SEXP StringFromReal(double, int*);
SEXP StringFromComplex(Rcomplex, int*);
SEXP EnsureString(SEXP);

/* ../../main/printutils.c : */
enum Rprt_adj {
    Rprt_adj_left = 0,
    Rprt_adj_right = 1,
    Rprt_adj_centre = 2,
    Rprt_adj_none = 3
};

/* ../../main/print.c : */
struct R_PrintData {
    int width;
    int na_width;
    int na_width_noquote;
    int digits;
    int scipen;
    int gap;
    bool quote;
    Rprt_adj right;
    int max;
    SEXP na_string;
    SEXP na_string_noquote;
    DeparseOptionBits useSource;
    size_t cutoff; // for deparsed language objects
    SEXP env;
    SEXP callArgs;
};

/* Dirent wrappers/implementation */

struct R_dirent {
    char *d_name; /* null-terminated filename */
};

typedef struct R_DIR_INTERNAL R_DIR;

R_DIR *R_opendir(const char *name);
struct R_dirent *R_readdir(R_DIR *rdir);
int R_closedir(R_DIR *rdir);

#ifdef Win32
struct R_wdirent {
    wchar_t *d_name; /* null-terminated filename */
};

typedef struct R_WDIR_INTERNAL R_WDIR;

R_WDIR *R_wopendir(const wchar_t *name);
struct R_wdirent *R_wreaddir(R_WDIR *rdir);
int R_wclosedir(R_WDIR *rdir);
#endif

/* Other Internally Used Functions */

SEXP allocCharsxp(R_xlen_t);
// SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
R_xlen_t asVecSize(SEXP x);
R_xlen_t asXLength(SEXP x);
void check1arg(SEXP, SEXP, const char *);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
void CheckFormals(SEXP, const char*);
void R_check_locale(void);
void check_stack_balance(SEXP op, size_t save);
// void CleanEd(void); // declared in Rembedded.h
void copyMostAttribNoTs(SEXP, SEXP);
SEXP createS3Vars(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
void CustomPrintValue(SEXP, SEXP);
double currentTime(void);
void DataFrameClass(SEXP);
SEXP ddfindVar(SEXP, SEXP);
SEXP deparse1(SEXP,bool,int);
SEXP deparse1m(SEXP call, bool abbrev, int opts);
SEXP deparse1w(SEXP,bool,int);
SEXP deparse1line(SEXP, bool);
SEXP deparse1line_ex(SEXP, bool, int);
SEXP deparse1s(SEXP call);
std::pair<bool, SEXP> DispatchAnyOrEval(SEXP, SEXP, const char *, SEXP, SEXP, bool, bool);
bool DispatchAnyOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, bool, bool);
std::pair<bool, SEXP> DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, bool, bool);
bool DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, bool, bool);
std::pair<bool, SEXP> DispatchGroup(const char *, SEXP, SEXP, SEXP, SEXP);
bool DispatchGroup(const char *, SEXP,SEXP,SEXP,SEXP,SEXP*);
R_xlen_t dispatch_xlength(SEXP, SEXP, SEXP);
R_len_t dispatch_length(SEXP, SEXP, SEXP);
SEXP dispatch_subset2(SEXP, R_xlen_t, SEXP, SEXP);
// SEXP Rf_duplicated(SEXP, Rboolean); // declared in Rinternals.h
// R_xlen_t Rf_any_duplicated(SEXP, Rboolean); // declared in Rinternals.h
// R_xlen_t Rf_any_duplicated3(SEXP, SEXP, Rboolean); // declared in Rinternals.h
SEXP evalList(SEXP, SEXP, SEXP, int);
SEXP evalListKeepMissing(SEXP, SEXP);
int factorsConform(SEXP, SEXP);
NORET void findcontext(int, SEXP, SEXP);
SEXP findVar1(SEXP, SEXP, SEXPTYPE, bool);
void FrameClassFix(SEXP);
// SEXP frameSubscript(int, SEXP, SEXP); // unused
R_xlen_t get1index(SEXP, SEXP, R_xlen_t, int, int, SEXP);
int GetOptionCutoff(void);
SEXP getVar(SEXP, SEXP);
SEXP getVarInFrame(SEXP, SEXP);
bool Rf_GetOptionDeviceAsk(void);
void R_SignalCStackOverflow(intptr_t usage);
void InitArithmetic(void);
void InitConnections(void);
void InitEd(void);
void InitFunctionHashing(void);
void InitBaseEnv(void);
void InitGlobalEnv(void);
bool R_current_trace_state(void);
bool R_current_debug_state(void);
bool R_has_methods(SEXP);
void R_InitialData(void);
std::pair<bool, SEXP> R_possible_dispatch(SEXP, SEXP, SEXP, SEXP, bool);
bool inherits2(SEXP, const char *);
void InitGraphics(void);
void InitMemory(void);
void InitNames(void);
void InitOptions(void);
void InitStringHash(void);
void Init_R_Variables(SEXP);
void InitTempDir(void);
void R_reInitTempDir(int);
void InitTypeTables(void);
void initStack(void);
void InitS3DefaultTypes(void);
void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
bool isMethodsDispatchOn(void);
bool isValidName(const char *);
// NORET void Rf_jump_to_toplevel(void); // declared in Rinterface.h
// void Rf_KillAllDevices(void); // declared in Rembedded.h
SEXP levelsgets(SEXP, SEXP);
// void Rf_mainloop(void); // declared in Rinterface.h
SEXP makeSubscript(SEXP, SEXP, R_xlen_t *, SEXP);
SEXP markKnown(const char *, SEXP);
SEXP mat2indsub(SEXP, SEXP, SEXP, SEXP);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgExact(SEXP, SEXP*);
SEXP matchArgs_NR(SEXP, SEXP, SEXP);
SEXP matchArgs_RC(SEXP, SEXP, SEXP);
SEXP matchPar(const char *, SEXP*);
void memtrace_report(void *, void *);
SEXP mkCharWUTF8(const wchar_t *);
SEXP mkCLOSXP(SEXP, SEXP, SEXP);
SEXP mkFalse(void);
SEXP mkPRIMSXP(unsigned int, bool);
SEXP mkPROMISE(SEXP, SEXP);
SEXP R_mkEVPROMISE(SEXP, SEXP);
SEXP R_mkEVPROMISE_NR(SEXP, SEXP);
SEXP mkQUOTE(SEXP);
SEXP mkSYMSXP(SEXP, SEXP);
SEXP mkTrue(void);
const char *R_nativeEncoding(void);
SEXP NewEnvironment(SEXP, SEXP, SEXP);
// void Rf_onintr(void); // declared in Rinterface.h
// void Rf_onintrNoResume(void); // declared in Rinterface.h
void onsigusr1(int);
void onsigusr2(int);
R_xlen_t OneIndex(SEXP, SEXP, R_xlen_t, int, SEXP*, int, SEXP);
SEXP parse(FILE*, int);
SEXP patchArgsByActuals(SEXP, SEXP, SEXP);
void PrintInit(R_PrintData *, SEXP);
void PrintDefaults(void);
void PrintGreeting(void);
void PrintValueEnv(SEXP, SEXP);
void PrintValueRec(SEXP, R_PrintData *);
void PrintVersion(char *, size_t len);
void PrintVersion_part_1(char *, size_t len);
void PrintVersionString(char *, size_t len);
void PrintWarnings(const char *hdr = nullptr);
// void process_site_Renviron(void); // declared in Rinterface.h
// void process_system_Renviron(void); // declared in Rinterface.h
// void process_user_Renviron(void); // declared in Rinterface.h
SEXP promiseArgs(SEXP, SEXP);
int Rcons_vprintf(const char *, va_list);
int REvprintf_internal(otype_t otype, const char *, va_list);
SEXP R_data_class(SEXP , bool);
SEXP R_data_class2(SEXP);
char *R_LibraryFileName(const char *, char *, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, int);
int R_Newhashpjw(const char *);
FILE* R_OpenLibraryFile(const char *);
SEXP R_Primitive(const char *);
// void R_RestoreGlobalEnv(void); // declared in Rinterface.h
// void R_RestoreGlobalEnvFromFile(const char *, bool); // declared in Rinterface.h
// void R_SaveGlobalEnv(void); // declared in Rembedded.h and Rinterface.h
// void R_SaveGlobalEnvToFile(const char *); // declared in Rinterface.h
void R_SaveToFile(SEXP, FILE*, int);
void R_SaveToFileV(SEXP, FILE*, int, int);
bool R_seemsOldStyleS4Object(SEXP object);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
SEXP R_SetOption(SEXP, SEXP);
// NORET void R_Suicide(const char *); // defined in Rinterface.h
SEXP R_flexiblas_info(void);
void R_getProcTime(double *data);
bool R_isMissing(SEXP symbol, SEXP rho);
bool R_missing(SEXP symbol, SEXP rho);
const char *sexptype2char(SEXPTYPE type);
void sortVector(SEXP, bool);
void SrcrefPrompt(const char *, SEXP);
void ssort(SEXP*,int);
int StrToInternal(const char *);
SEXP strmat2intmat(SEXP, SEXP, SEXP, SEXP);
SEXP substituteList(SEXP, SEXP);
unsigned int TimeToSeed(void);
SEXP tspgets(SEXP, SEXP);
SEXP type2symbol(SEXPTYPE);
void unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
// void unmarkPhase(void); // unused
#endif
SEXP R_LookupMethod(SEXP, SEXP, SEXP, SEXP);
std::pair<bool, SEXP> usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
int usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP*);
SEXP vectorIndex(SEXP, SEXP, int, int, int, SEXP, bool);

/* ../main/bind.c */
SEXP ItemName(SEXP, R_xlen_t);

/* ../main/errors.c : */
NORET void errorcall_cpy(SEXP, const char *, ...) R_PRINTF_FORMAT(2,3);
NORET void ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, int, ...); // R_WARNING replaced by int
SEXP R_GetTraceback(int);    // including deparse()ing
SEXP R_GetTracebackOnly(int);// no        deparse()ing
NORET void R_signalErrorCondition(SEXP cond, SEXP call);
NORET void R_signalErrorConditionEx(SEXP cond, SEXP call, int exitOnly);
void R_signalWarningCondition(SEXP cond);
SEXP R_vmakeErrorCondition(SEXP call,
			   const char *classname, const char *subclassname,
			   int nextra, const char *format, va_list ap)
     R_PRINTF_FORMAT(5,0);

SEXP R_makeErrorCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
     R_PRINTF_FORMAT(5,0);
SEXP R_makeWarningCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
     R_PRINTF_FORMAT(5,0);

NORET void R_MissingArgError     (SEXP symbol,     SEXP call, const char* subclass);
NORET void R_MissingArgError_c   (const char *arg, SEXP call, const char* subclass);

SEXP R_makePartialMatchWarningCondition(SEXP call, SEXP argument, SEXP formal);

void R_setConditionField(SEXP cond, R_xlen_t idx, const char *name, SEXP val);
SEXP R_makeNotSubsettableError(SEXP x, SEXP call);
SEXP R_makeMissingSubscriptError(SEXP x, SEXP call);
SEXP R_makeMissingSubscriptError1(SEXP call);
SEXP R_makeOutOfBoundsError(SEXP x, int subscript, SEXP sindex,
			    SEXP call, const char *prefix);
SEXP R_makeCStackOverflowError(SEXP call, intptr_t usage);
SEXP R_getProtectStackOverflowError(void);
SEXP R_getExpressionStackOverflowError(void);
SEXP R_getNodeStackOverflowError(void);
void R_InitConditions(void);

R_size_t R_GetMaxVSize(void);
bool R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
bool R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char *p, int *ierr);
void R_SetPPSize(R_size_t);
void R_SetNconn(int);

void R_expand_binding_value(SEXP);
#ifdef IMMEDIATE_PROMISE_VALUES
SEXP R_expand_promise_value(SEXP);
#endif

void R_args_enable_refcnt(SEXP);
void R_try_clear_args_refcnt(SEXP);

/* ../main/devices.c, used in memory.c, gnuwin32/extra.c */
#define R_MaxDevices 64

/* gnuwin32/extra.c */
wchar_t *R_getFullPathNameW(const wchar_t *);
char *R_getFullPathName(const char *);

int Rstrwid(const char *str, int slen, cetype_t ienc, int quote);
int Rstrlen(SEXP, int);
const char *EncodeRaw(Rbyte, const char *);
const char *EncodeString(SEXP, int, int, Rprt_adj);
const char *EncodeReal2(double, int, int, int);
const char *EncodeChar(SEXP);

/* main/raw.c */
int mbrtoint(int *w, const char *s);

/* main/sort.c */
void orderVector1(int *indx, int n, SEXP key, bool nalast,
		  bool decreasing, SEXP rho);

/* main/subset.c */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);

/* main/subassign.c */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);


/* main/util.c */
NORET void UNIMPLEMENTED_TYPE(const char *s, SEXP x);
NORET void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
bool strIsASCII(const char *str);
int utf8clen(const char c);
int Rf_AdobeSymbol2ucs2(int n);
double R_strtod5(const char *str, char **endptr, char dec, bool NA, int exact);
SEXP R_listCompact(SEXP s, bool keep_initial);

typedef unsigned short R_ucs2_t;
size_t mbcsToUcs2(const char *in, R_ucs2_t *out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(R_ucs2_t *in, char *out);
size_t ucs2Mblen(R_ucs2_t *in); */
size_t utf8toucs(wchar_t *wc, const char *s);
// size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n); // used in graphapp
size_t ucstomb(char *s, const unsigned int wc);
// size_t Rf_ucstoutf8(char *s, const unsigned int wc); // declared in GraphicsDevice.h
size_t mbtoucs(unsigned int *wc, const char *s, size_t n);
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n);

// SEXP Rf_installTrChar(SEXP); // declared in Rinternals.h

const wchar_t *wtransChar(SEXP x); /* from sysutils.c */
const char *reEnc3(const char *x, const char *fromcode, const char *tocode, int subst);

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
// size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps); // used in graphapp
bool mbcsValid(const char *str);
char *mbcsTruncateToValid(char *s);
bool utf8Valid(const char *str);
// char *Rf_strchr(const char *s, int c); // used in graphapp
char *Rf_strrchr(const char *s, int c);
int Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
    R_PRINTF_FORMAT(3,0);

int Rsnprintf_mbcs(char *str, size_t size, const char *format, ...)
    R_PRINTF_FORMAT(3,4);

int Rasprintf_malloc(char **str, const char *fmt, ...)
    R_PRINTF_FORMAT(2,3);

SEXP fixup_NaRm(SEXP args); /* summary.c */
void invalidate_cached_recodings(void);  /* from sysutils.c */
void resetICUcollator(bool disable); /* from util.c */
void dt_invalidate_locale(void); /* from Rstrptime.h */
extern int R_OutputCon; /* from connections.c */

extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.c */
SEXP R_SerializeInfo(R_inpstream_t ips);

void get_current_mem(size_t *,size_t *,size_t *); /* from memory.c */
unsigned long get_duplicate_counter(void);  /* from duplicate.c */
void reset_duplicate_counter(void);  /* from duplicate.c */
void BindDomain(char *); /* from main.c */
extern bool LoadInitFile;  /* from startup.c, uses in sys-*.c */

// Unix and Windows versions
double R_getClockIncrement(void);
// void R_getProcTime(double *data); //already declared in this file
void InitDynload(void);
// void R_CleanTempDir(void); // declared in Rembedded.h

#ifdef Win32
void R_fixslash(char *s);
void R_fixbackslash(char *s);
void R_wfixbackslash(wchar_t *s);
void R_wfixslash(wchar_t *s);
void R_UTF8fixslash(char *s);
wchar_t *filenameToWchar(const SEXP fn, const bool expand);
#endif

FILE *RC_fopen(const SEXP fn, const char *mode, const bool expand);
bool Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char *str, char **endptr, char dec, bool NA);
// double R_strtod(const char *str, char **endptr); // declared in Utils.h
// double R_atof(const char *str); // declared in Utils.h

/* unix/sys-std.c, main/options.c */
void set_rl_word_breaks(const char *str);

/* unix/sys-unix.c, main/connections.c */
FILE *R_popen_pg(const char *cmd, const char *type);
int R_pclose_pg(FILE *fp);
#ifdef Unix
bool R_access_X11(void); /* from src/unix/X11.c */
#endif
SEXP R_execMethod(SEXP op, SEXP rho);
SEXP csduplicated(SEXP x); /* from unique.c */

/* from connections.c */
SEXP R_compress1(SEXP in);
SEXP R_decompress1(SEXP in, bool *err);
SEXP R_compress2(SEXP in);
SEXP R_decompress2(SEXP in, bool *err);
SEXP R_compress3(SEXP in);
SEXP R_decompress3(SEXP in, bool *err);

SEXP R_FixupExitingHandlerResult(SEXP); /* defined in error.c */
SEXP R_UnwindHandlerStack(SEXP); /* defined in error.c */

void savePalette(bool save); /* from colors.c */
const char *get_workspace_name(void);  /* from startup.c */
void R_init_base(DllInfo *); /* In registration.c */

#ifdef _WIN32
void reEnc2(const char *x, char *y, int ny,
	    cetype_t ce_in, cetype_t ce_out, int subst); /* from sysutils.c */
#endif

    /** @brief Print expression.
     *
     * @note Formely defined in main.cpp and eval.cpp.
     */
    void PrintCall(SEXP call, SEXP rho);

int Rf_envlength(SEXP rho);
R_xlen_t Rf_envxlength(SEXP rho);
int R_isWriteableDir(const char *path); // from sysutils.c
FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode);
const char *typeName(SEXP v);
bool RunFinalizers(void);
bool R_cairoCdynload(int local, int now);
const char *getPRIMNAME(SEXP object);
} // namespace R
/* From localecharset.c */
// extern const char *locale2charset(const char *); // used in extra/intl/localecharset.c

/* Defining NO_RINLINEDFUNS disables use to simulate platforms where
   this is not available */
#if !defined(__MAIN__) && (defined(COMPILING_R) || ( __GNUC__ && !defined(__INTEL_COMPILER) )) && (defined(COMPILING_R) || !defined(NO_RINLINEDFUNS))
#include "Rinlinedfuns.h"
#else
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the private inlinable functions that are provided in
   Rinlinedfuns.h It is *essential* that these do not appear in any
   other header file, with or without the Rf_ prefix.
*/
namespace R {
SEXP R_FixupRHS(SEXP x, SEXP y);
double SCALAR_DVAL(SEXP x);
int SCALAR_LVAL(SEXP x);
int SCALAR_IVAL(SEXP x);
Rbyte SCALAR_BVAL(SEXP x);
Rcomplex SCALAR_CVAL(SEXP x);
void SET_SCALAR_DVAL(SEXP x, double v);
void SET_SCALAR_LVAL(SEXP x, int v);
void SET_SCALAR_IVAL(SEXP x, int v);
void SET_SCALAR_CVAL(SEXP x, Rcomplex v);
void SET_SCALAR_BVAL(SEXP x, Rbyte v);
} // namespace R
#endif

/* Macros for suspending interrupts: also in GraphicsDevice.h */
#define BEGIN_SUSPEND_INTERRUPTS do { \
    bool __oldsusp__ = R_interrupts_suspended; \
    R_interrupts_suspended = TRUE;
#define END_SUSPEND_INTERRUPTS R_interrupts_suspended = __oldsusp__; \
    if (R_interrupts_pending && ! R_interrupts_suspended) \
        Rf_onintr(); \
} while(0)

// Functions and variables declared also in other headers
#ifdef __cplusplus
extern "C" {
#endif
#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif

LibExtern bool R_interrupts_suspended INI_as(FALSE); // declared in GraphicsDevice.h
LibExtern bool R_interrupts_pending INI_as(FALSE); // declared in GraphicsDevice.h

 /* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */ // declared in Rinterface.h

/* The Pointer Protection Stack */
LibExtern size_t	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
// #define R_PPStackTop R_PPStack.size()	    /* The top of the stack */
// LibExtern std::vector<SEXP>	R_PPStack;	    /* The pointer protection stack */

/* File Input/Output */
// Next two are duplicated in Rinterface.h
// R_Interactive is accessed in parallel's fork.c and on Windows in util's stubs.c
LibExtern bool R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/ // declared in Rinterface.h
extern bool  R_NoEcho	INI_as(FALSE);	/* do not echo R code */ // declared in Rinterface.h
extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */ // declared in Rinterface.h
extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */ // declared in Rinterface.h
LibExtern char *R_TempDir	INI_as(NULL);	/* Name of per-session dir */ // declared in Rembedded.h

LibExtern bool mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */ // declared in GraphicsDevice.h

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */ // declared in Rembedded.h

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */ // declared in Rinterface.h
LibExtern int	R_HistorySize;	/* Size of the history file */ // declared in Rinterface.h
LibExtern int	R_RestoreHistory;	/* restore the history file? */ // declared in Rinterface.h

/* GUI type */

extern const char	*R_GUIType	INI_as("unknown"); // declared in Rinterface.h
extern bool R_isForkedChild		INI_as(FALSE); /* was this forked? */

#ifdef Win32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */ // declared in internal.h
#endif

/* pre-allocated boolean values */
LibExtern SEXP R_TrueValue INI_as(NULL);
LibExtern SEXP R_FalseValue INI_as(NULL);
LibExtern SEXP R_LogicalNAValue INI_as(NULL);

#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

/* safer alternative */
char *Rstrdup(const char *s);
void R_ProcessEvents(void); // declared in R.h
#ifdef Win32
void R_WaitEvent(void); // declared in R.h
#endif
void R_setupHistory(void); // declared in Rinterface.h
#define utf8towcs Rf_utf8towcs
size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n); // used in graphapp/gdraw.c
#define Mbrtowc Rf_mbrtowc
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps); // used in graphapp/menus.c
char *Rf_strchr(const char *s, int c); // used in graphapp/menus.c
const char *locale2charset(const char *); // used in extra/intl/localecharset.c
size_t Rf_ucstoutf8(char *s, const unsigned int wc); // declared in GraphicsDevice.h
// below are declared in Rinterface.h and/or R.h/Rembedded.h
void R_FlushConsole(void);
void R_ClearerrConsole(void);
char *R_HomeDir(void);
NORET void Rf_jump_to_toplevel(void);
void Rf_KillAllDevices(void);
void Rf_mainloop(void);
void Rf_onintr(void);
void Rf_onintrNoResume(void);
void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);
void R_RestoreGlobalEnv(void);
void R_RestoreGlobalEnvFromFile(const char *, bool);
void R_SaveGlobalEnv(void);
void R_SaveGlobalEnvToFile(const char *);
NORET void R_Suicide(const char *);
#ifdef __cplusplus
} // extern "C"
#endif

/*
   alloca is neither C99 nor POSIX.

   It might be better to try alloca.h first, see
   https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Particular-Functions.html
*/
#ifdef __GNUC__
// This covers GNU, Clang and Intel compilers
// The undef is needed in case some other header, e.g. malloc.h, already did this
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
// Needed for native compilers on Solaris and AIX
#  include <alloca.h>
# endif
// it might have been defined via some other standard header, e.g. stdlib.h
# if !HAVE_DECL_ALLOCA
#  include <cstddef> // for size_t
extern void *alloca(size_t);
# endif
#endif

/* Required by C99, but might be slow */
#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

/* int_fast64_t is required by C99/C11
   Alternative would be to use intmax_t.
   Used in summary.c
 */
#ifdef HAVE_INT64_T
# define LONG_INT int64_t
# define LONG_INT_MAX INT64_MAX
#elif defined(HAVE_INT_FAST64_T)
# define LONG_INT int_fast64_t
# define LONG_INT_MAX INT_FAST64_MAX
#endif

// for reproducibility for now: use exp10 or pown later if accurate enough.
#define Rexp10(x) pow(10.0, x)

#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
