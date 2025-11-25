/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2024  The R Core Team.
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

/*
 *	This code implements a non-moving generational collector
 *      with two or three generations.
 *
 *	Memory allocated by R_alloc is maintained in a stack.  Code
 *	that R_allocs memory must use vmaxget and vmaxset to obtain
 *	and reset the stack pointer.
 */

/** @file memory.cpp
 *
 * Memory management, garbage collection, and memory profiling.
 */

#define USE_RINTERNALS
#define COMPILING_MEMORY_C

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_STDALIGN_H
# include <stdalign.h>
#endif

#include <cstdint>
#include <memory>
#include <forward_list>
#include <iostream>
#include <list>
#include <cstdarg>
#include <map>
#include <unordered_map>
#include <utility>
#include <array>
#include <R_ext/Minmax.h>
#include <CXXR/Complex.hpp>
#include <CXXR/Logical.hpp>
#include <R_ext/Altrep.h>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/JMPException.hpp>
#include <CXXR/CommandTerminated.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/String.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Expression.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/WeakRef.hpp>
#include <CXXR/ExternalPointer.hpp>
#include <CXXR/S4Object.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/ComplexVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/BadObject.hpp>

#include <R_ext/RS.h> /* for S4 allocation */
#include <R_ext/Print.h>

/* Declarations for Valgrind.

   These are controlled by the
     --with-valgrind-instrumentation=
   option to configure, which sets VALGRIND_LEVEL to the
   supplied value (default 0) and defines NVALGRIND if
   the value is 0.

   level 0 is no additional instrumentation
   level 1 marks uninitialized numeric, logical, integer, raw,
	   complex vectors and R_alloc memory
   level 2 marks the data section of vector nodes as inaccessible
	   when they are freed.

   level 3 was withdrawn in R 3.2.0.

   It may be necessary to define NVALGRIND for a non-gcc
   compiler on a supported architecture if it has different
   syntax for inline assembly language from gcc.

   For Win32, Valgrind is useful only if running under Wine.
*/
#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif


#ifndef VALGRIND_LEVEL
# define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
# include "valgrind/memcheck.h"
#endif

/* For speed in cases when the argument is known to not be an ALTREP list. */
#define VECTOR_ELT_0(x,i)        ((SEXP *) STDVEC_DATAPTR(x))[i]
#define SET_VECTOR_ELT_0(x,i, v) (((SEXP *) STDVEC_DATAPTR(x))[i] = (v))

#include <Localization.h>
#include <Defn.h>
#include <Internal.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>
#include <R_ext/Rallocators.h> /* for R_allocator_t structure */
#include <Rmath.h> // R_pow_di
#include <Print.h> // R_print
#include "RBufferUtils.h"

using namespace R;
using namespace CXXR;

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

/* These are used in profiling to separate out time in GC */
attribute_hidden int R_gc_running(void) { return GCManager::gcIsRunning(); }

#ifdef TESTING_WRITE_BARRIER
#define CR_ASSERT(x) (assert(((x) != R_NilValue) && ((x) != NULL)))
#define CR_TYPE_ASSERT(x) (assert((x)))
#define CR_CONSCELL_ASSERT(e) assert((TYPEOF(e) == LISTSXP) || (TYPEOF(e) == DOTSXP) || (TYPEOF(e) == LANGSXP) || (TYPEOF(e) == NILSXP))
#define CR_CLOSURE_ASSERT(e) assert((TYPEOF(e) == CLOSXP) || (TYPEOF(e) == NILSXP))
#define CR_SYMBOL_ASSERT(e) assert((TYPEOF(e) == SYMSXP) || (TYPEOF(e) == NILSXP))
#define CR_ENVIRONMENT_ASSERT(e) assert((TYPEOF(e) == ENVSXP) || (TYPEOF(e) == NILSXP))
#define CR_PROMISE_ASSERT(e) assert((TYPEOF(e) == PROMSXP) || (TYPEOF(e) == NILSXP))
#define CR_WEAKREF_ASSERT(e) assert((TYPEOF(e) == WEAKREFSXP) || (TYPEOF(e) == NILSXP))
#define CR_S4OBJECT_ASSERT(e) assert((TYPEOF(e) == OBJSXP) || (TYPEOF(e) == NILSXP))
#define CR_ALTREP_ASSERT(e) assert(ALTREP(e) || (TYPEOF(e) == NILSXP))
#define CR_BYTECODE_ASSERT(e) assert((TYPEOF(e) == BCODESXP) || (TYPEOF(e) == NILSXP))
#define CR_BUILTIN_ASSERT(e) assert((TYPEOF(e) == SPECIALSXP) || (TYPEOF(e) == BUILTINSXP) || (TYPEOF(e) == NILSXP))
#else
#define CR_ASSERT(x)
#define CR_TYPE_ASSERT(x)
#define CR_CONSCELL_ASSERT(e)
#define CR_CLOSURE_ASSERT(e)
#define CR_SYMBOL_ASSERT(e)
#define CR_ENVIRONMENT_ASSERT(e)
#define CR_PROMISE_ASSERT(e)
#define CR_WEAKREF_ASSERT(e)
#define CR_S4OBJECT_ASSERT(e)
#define CR_ALTREP_ASSERT(e)
#define CR_BYTECODE_ASSERT(e)
#define CR_BUILTIN_ASSERT(e)
#endif

#ifdef PROTECTCHECK
/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and
       SEXP outputs with CHK() to see if a reachable node is a
       FREESXP and signal an error if a FREESXP is found.

   Combined with GC torture this can help locate where an unprotected
   SEXP is being used.

   This approach will miss cases where an unprotected node has been
   re-allocated.  For these cases it is possible to set
   s_gc_inhibit_release to TRUE.  FREESXP nodes will not be reallocated,
   or large ones released, until s_gc_inhibit_release is set to FALSE
   again.  This will of course result in memory growth and should be
   used with care and typically in combination with OS mechanisms to
   limit process memory usage.  LT */

/* Before a node is marked as a FREESXP by the collector the previous
   type is recorded.  For now using the LEVELS field seems
   reasonable.  */
#define OLDTYPE(s) (SEXPTYPE) LEVELS(s)
#define SETOLDTYPE(s, t) SETLEVELS(s, t)

static R_INLINE void VOID_CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x != NULL && TYPEOF(x) == FREESXP)
	error(_("unprotected object (%p) encountered (was %s)"),
	      (void *)x, sexptype2char(OLDTYPE(x)));
}

static R_INLINE SEXP CHK(SEXP x)
{
    VOID_CHK(x);
    return x;
}
#else
#define CHK(x) x
#define VOID_CHK(x)
#endif

/** @brief Translate SEXPTYPE enum to a character string
 * 
 * @param type SEXP object's type
 * 
 * @return name of the type
 * 
 * @note also called from typeName() in inspect.cpp
 */
attribute_hidden
const char *R::sexptype2char(SEXPTYPE type) {
    switch (type) {
    case NILSXP:	return "NILSXP";
    case SYMSXP:	return "SYMSXP";
    case LISTSXP:	return "LISTSXP";
    case CLOSXP:	return "CLOSXP";
    case ENVSXP:	return "ENVSXP";
    case PROMSXP:	return "PROMSXP";
    case LANGSXP:	return "LANGSXP";
    case SPECIALSXP:	return "SPECIALSXP";
    case BUILTINSXP:	return "BUILTINSXP";
    case CHARSXP:	return "CHARSXP";
    case LGLSXP:	return "LGLSXP";
    case INTSXP:	return "INTSXP";
    case REALSXP:	return "REALSXP";
    case CPLXSXP:	return "CPLXSXP";
    case STRSXP:	return "STRSXP";
    case DOTSXP:	return "DOTSXP";
    case ANYSXP:	return "ANYSXP";
    case VECSXP:	return "VECSXP";
    case EXPRSXP:	return "EXPRSXP";
    case BCODESXP:	return "BCODESXP";
    case EXTPTRSXP:	return "EXTPTRSXP";
    case WEAKREFSXP:	return "WEAKREFSXP";
    case OBJSXP:	return "OBJSXP"; /* was S4SXP */
    case RAWSXP:	return "RAWSXP";
    case NEWSXP:	return "NEWSXP"; /* should never happen */
    case FREESXP:	return "FREESXP";
    default:		return "<unknown>";
    }
}

#ifdef R_MEMORY_PROFILING
static void R_ReportAllocation(R_size_t);
#endif

#define GC_PROT(X) do { \
    unsigned int __wait__ = GCManager::gc_force_wait();	\
    unsigned int __gap__ = GCManager::gc_force_gap();	\
    bool __release__ = GCManager::gc_inhibit_release();	\
    X;						\
    GCManager::setTortureParameters(__gap__, __wait__, __release__);			\
}  while(0)

// static void R_gc_no_finalizers(R_size_t size_needed);
static void R_gc_full(bool full = false);
// static void mem_err_heap();

#define NODE_IS_MARKED(s) (MARK(s)==1)
#define MARK_NODE(s) (MARK(s)=1)
#define UNMARK_NODE(s) (MARK(s)=0)


/* Tuning Constants. Most of these could be made settable from R,
   within some reasonable constraints at least.  Since there are quite
   a lot of constants it would probably make sense to put together
   several "packages" representing different space/speed tradeoffs
   (e.g. very aggressive freeing and small increments to conserve
   memory; much less frequent releasing and larger increments to
   increase speed). */

/* When a level N collection fails to produce at least MinFreeFrac *
   R_NSize free nodes and MinFreeFrac * R_VSize free vector space, the
   next collection will be a level N + 1 collection.

   This constant is also used in heap size adjustment as a minimal
   fraction of the minimal heap size levels that should be available
   for allocation. */
static double R_MinFreeFrac = 0.2;

/* When pages are released, a number of free nodes equal to
   R_MaxKeepFrac times the number of allocated nodes for each class is
   retained.  Pages not needed to meet this requirement are released.
   An attempt to release pages is made every R_PageReleaseFreq level 1
   or level 2 collections. */
// static double R_MaxKeepFrac = 0.5;
// static unsigned int R_PageReleaseFreq = 1;

/* The heap size constants R_NSize and R_VSize are used for triggering
   collections.  The initial values set by defaults or command line
   arguments are used as minimal values.  After full collections these
   levels are adjusted up or down, though not below the minimal values
   or above the maximum values, towards maintain heap occupancy within
   a specified range.  When the number of nodes in use reaches
   R_NGrowFrac * R_NSize, the value of R_NSize is incremented by
   R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize.  When the number of
   nodes in use falls below R_NShrinkFrac, R_NSize is decremented by
   R_NShrinkIncrMin + R_NShrinkFrac * R_NSize.  Analogous adjustments
   are made to R_VSize.

   This mechanism for adjusting the heap size constants is very
   primitive but hopefully adequate for now.  Some modeling and
   experimentation would be useful.  We want the heap sizes to get set
   at levels adequate for the current computations.  The present
   mechanism uses only the size of the current live heap to provide
   information about the current needs; since the current live heap
   size can be very volatile, the adjustment mechanism only makes
   gradual adjustments.  A more sophisticated strategy would use more
   of the live heap history.

   Some of the settings can now be adjusted by environment variables.
*/
static double R_NGrowFrac = 0.70;
static double R_NShrinkFrac = 0.30;

static double R_VGrowFrac = 0.70;
static double R_VShrinkFrac = 0.30;

#ifdef SMALL_MEMORY
/* On machines with only 32M of memory (or on a classic Mac OS port)
   it might be a good idea to use settings like these that are more
   aggressive at keeping memory usage down. */
static double R_NGrowIncrFrac = 0.0, R_NShrinkIncrFrac = 0.2;
static unsigned int R_NGrowIncrMin = 50000, R_NShrinkIncrMin = 0;
static double R_VGrowIncrFrac = 0.0, R_VShrinkIncrFrac = 0.2;
static unsigned int R_VGrowIncrMin = 100000, R_VShrinkIncrMin = 0;
#else
static double R_NGrowIncrFrac = 0.2, R_NShrinkIncrFrac = 0.2;
static unsigned int R_NGrowIncrMin = 40000, R_NShrinkIncrMin = 0;
static double R_VGrowIncrFrac = 0.2, R_VShrinkIncrFrac = 0.2;
static unsigned int R_VGrowIncrMin = 80000, R_VShrinkIncrMin = 0;
#endif

static void init_gc_grow_settings(void)
{
    char *arg = getenv("R_GC_MEM_GROW");
    if (arg != NULL) {
	int which = (int) atof(arg);
	switch (which) {
	case 0: /* very conservative -- the SMALL_MEMORY settings */
	    R_NGrowIncrFrac = 0.0;
	    R_VGrowIncrFrac = 0.0;
	    break;
	case 1: /* default */
	    break;
	case 2: /* somewhat aggressive */
	    R_NGrowIncrFrac = 0.3;
	    R_VGrowIncrFrac = 0.3;
	    break;
	case 3: /* more aggressive */
	    R_NGrowIncrFrac = 0.4;
	    R_VGrowIncrFrac = 0.4;
	    R_NGrowFrac = 0.5;
	    R_VGrowFrac = 0.5;
	    break;
	}
    }
    arg = getenv("R_GC_GROWFRAC");
    if (arg != NULL) {
	double frac = atof(arg);
	if (0.35 <= frac && frac <= 0.75) {
	    R_NGrowFrac = frac;
	    R_VGrowFrac = frac;
	}
    }
    arg = getenv("R_GC_GROWINCRFRAC");
    if (arg != NULL) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80) {
	    R_NGrowIncrFrac = frac;
	    R_VGrowIncrFrac = frac;
	}
    }
    arg = getenv("R_GC_NGROWINCRFRAC");
    if (arg != NULL) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80)
	    R_NGrowIncrFrac = frac;
    }
    arg = getenv("R_GC_VGROWINCRFRAC");
    if (arg != NULL) {
	double frac = atof(arg);
	if (0.05 <= frac && frac <= 0.80)
	    R_VGrowIncrFrac = frac;
    }
}

/* Maximal Heap Limits.  These variables contain upper limits on the
   heap sizes.  They could be made adjustable from the R level,
   perhaps by a handler for a recoverable error.

   Access to these values is provided with reader and writer
   functions; the writer function insures that the maximal values are
   never set below the current ones. */
// static R_size_t R_MaxVSize = R_SIZE_T_MAX;
// static R_size_t R_MaxNSize = R_SIZE_T_MAX;
// static unsigned int vsfac = 1; /* current units for vsize: changes at initialization */

attribute_hidden void R::R_SetPPSize(R_size_t size)
{
    R_PPStackSize = size;
}

attribute_hidden SEXP do_maxVSize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double newval = asReal(CAR(args));

    if (newval > 0) {
	if (newval == R_PosInf)
	    R_MaxVSize = R_SIZE_T_MAX;
	else {
	    double newbytes = newval * Mega;
	    if (newbytes >= (double) R_SIZE_T_MAX)
		R_MaxVSize = R_SIZE_T_MAX;
	    else if (!GCManager::R_SetMaxVSize((R_size_t) newbytes))
		warning("%s", _("a limit lower than current usage, so ignored"));
	}
    }

    if (R_MaxVSize == R_SIZE_T_MAX)
	return ScalarReal(R_PosInf);
    else
	return ScalarReal(GCManager::R_GetMaxVSize() / Mega);
}

attribute_hidden SEXP do_maxNSize(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double newval = asReal(CAR(args));

    if (newval > 0) {
	if (newval == R_PosInf)
	    R_MaxNSize = R_SIZE_T_MAX;
	else {
	    if (newval >= (double) R_SIZE_T_MAX) 
		R_MaxNSize = R_SIZE_T_MAX;
	    else if (!GCManager::R_SetMaxNSize((R_size_t) newval))
		warning("%s", _("a limit lower than current usage, so ignored"));
	}
    }

    if (R_MaxNSize == R_SIZE_T_MAX)
	return ScalarReal(R_PosInf);
    else
	return ScalarReal(GCManager::R_GetMaxNSize());
}


/* Miscellaneous Globals. */

// static SEXP R_VStack = NULL;		/* R_alloc stack pointer */
// static R_size_t orig_R_NSize;
// static R_size_t orig_R_VSize;

static R_size_t R_N_maxused=0;
static R_size_t R_V_maxused=0;

/* Node Generations. */

#define NODE_GEN_IS_YOUNGER(s,g) \
  (!NODE_IS_MARKED(s) || NODE_GENERATION(s) < (g))
#define NODE_IS_OLDER(x, y) ((x) && (y) && (NODE_GENERATION(x) > NODE_GENERATION(y)))


#define VHEAP_FREE() (R_VSize - MemoryBank::doublesAllocated())

#define NEXT_NODE(s) (s)->m_next
#define PREV_NODE(s) (s)->m_prev
#define SET_NEXT_NODE(s,t) (NEXT_NODE(s) = (t))
#define SET_PREV_NODE(s,t) (PREV_NODE(s) = (t))


/* Node List Manipulation */

/* link node s to t */
#define LINK_NODE(s,t) do { \
  SET_NEXT_NODE(s, t); \
  SET_PREV_NODE(t, s); \
} while (0)

/* unsnap node s from its list */
#define UNSNAP_NODE(s) do { \
  const GCNode *un__n__ = (s); \
  LINK_NODE(PREV_NODE(un__n__), NEXT_NODE(un__n__)); \
} while(0)

/* snap in node s before node t */
#define SNAP_NODE(s,t) do { \
  const GCNode *sn__n__ = (s); \
  const GCNode *tn__n__ = (t); \
  LINK_NODE(PREV_NODE(tn__n__), sn__n__); \
  LINK_NODE(sn__n__, tn__n__); \
} while (0)

/* move all nodes on from_peg to to_peg */
#define BULK_MOVE(from_peg,to_peg) do { \
  const GCNode *__from__ = (from_peg); \
  const GCNode *__to__ = (to_peg); \
  const GCNode *first_old = NEXT_NODE(__from__); \
  const GCNode *last_old = PREV_NODE(__from__); \
  const GCNode *first_new = NEXT_NODE(__to__); \
  LINK_NODE(__to__, first_old); \
  LINK_NODE(last_old, first_new); \
  LINK_NODE(__from__, __from__); \
} while (0);

/* Node Allocation. */

/* Debugging Routines. */

#ifdef DEBUG_GC
static void DEBUG_CHECK_NODE_COUNTS(const char *where)
{
    REprintf("Node counts %s:\n", where);
    unsigned int NewCount = 0;
    unsigned int OldCount = 0;
    unsigned int OldToNewCount = 0;
    for (const GCNode *s = NEXT_NODE(GCNode::s_New);
        s != GCNode::s_New.get();
        s = NEXT_NODE(s)) {
        NewCount++;
    }
    for (unsigned int gen = 1; gen < GCNode::numGenerations(); gen++) {
        for (const GCNode *s = NEXT_NODE(GCNode::s_Old[gen]);
            s != GCNode::s_Old[gen].get();
            s = NEXT_NODE(s)) {
            OldCount++;
            if (gen != NODE_GENERATION(s))
                GCManager::gc_error(_("Inconsistent node generation in s_Old\n"));
            GCNode::OldToNewChecker o2n(gen);
            s->visitReferents(&o2n);
        }
        for (const GCNode *s = NEXT_NODE(GCNode::s_OldToNew[gen]);
            s != GCNode::s_OldToNew[gen].get();
            s = NEXT_NODE(s)) {
            OldToNewCount++;
            if (gen != NODE_GENERATION(s))
                GCManager::gc_error(_("Inconsistent node generation in s_OldToNew\n"));
        }
    }
    REprintf("New = %d, Old = %d, OldToNew = %d, Total = %d\n",
        NewCount, OldCount, OldToNewCount,
        NewCount + OldCount + OldToNewCount);
}

static void DEBUG_GC_SUMMARY(int full_gc)
{
    REprintf("\n%s, VSize = %lu", full_gc ? "Full" : "Minor",
        MemoryBank::doublesAllocated());
    unsigned int OldCount = 0;
    for (unsigned int gen = 1; gen < GCNode::numGenerations(); gen++)
        OldCount += GCNode::s_gencount[gen];
    REprintf(": %d", OldCount);
}
#else
#define DEBUG_CHECK_NODE_COUNTS(s)
#define DEBUG_GC_SUMMARY(x)
#endif /* DEBUG_GC */

#ifdef DEBUG_ADJUST_HEAP
static void DEBUG_ADJUST_HEAP_PRINT(double node_occup, double vect_occup)
{
    REprintf("Node occupancy: %.0f%%\nVector occupancy: %.0f%%\n",
	     100.0 * node_occup, 100.0 * vect_occup);
    R_size_t alloc = MemoryBank::doublesAllocated() +
	sizeof(VectorBase) * MemoryBank::blocksAllocated();
    REprintf("Total allocation: %lu\n", alloc);
    REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
}
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

/* Page Allocation and Release. */

/* Heap Size Adjustment. */
namespace
{
    R_size_t applyAggressiveGrow(R_size_t change)
    {
        static R_size_t last_in_use = 0;
        static unsigned int adjust_count = 1;

        if (adjust_count < 50) {
            ++adjust_count;

            /* estimate next in-use count by assuming linear growth */
            R_size_t current_in_use = GCNode::numNodes();
            R_size_t next_in_use = current_in_use + (current_in_use - last_in_use);
            last_in_use = current_in_use;

            /* try to achieve an occupancy rate of R_NGrowFrac */
            R_size_t next_nsize = (R_size_t)(next_in_use / R_NGrowFrac);
            if (next_nsize > R_NSize + change)
                change = next_nsize - R_NSize;
        }
        return change;
    }
} // anonymous namespace

static void AdjustHeapSize(R_size_t size_needed)
{
    // Calculate minimum required free nodes and vectors
    R_size_t R_MinNFree = (R_size_t)(orig_R_NSize * R_MinFreeFrac);
    R_size_t R_MinVFree = (R_size_t)(orig_R_VSize * R_MinFreeFrac);

    // Calculate required node and vector sizes
    R_size_t NNeeded = GCNode::numNodes() + R_MinNFree;
    R_size_t VNeeded = MemoryBank::doublesAllocated() + size_needed + R_MinVFree;

    // Calculate occupancy rates
    double node_occup = ((double)NNeeded) / R_NSize;
    double vect_occup = ((double)VNeeded) / R_VSize;

    // Adjust node heap size
    if (node_occup > R_NGrowFrac) {
        R_size_t change = (R_size_t)(R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize);

        /* for early adjustments grow more aggressively */
        change = applyAggressiveGrow(change);

        if (R_MaxNSize >= R_NSize + change)
            R_NSize += change;
    }
    else if (node_occup < R_NShrinkFrac) {
        R_NSize -= (R_size_t)(R_NShrinkIncrMin + R_NShrinkIncrFrac * R_NSize);

        if (R_NSize < NNeeded)
            R_NSize = (NNeeded < R_MaxNSize) ? NNeeded : R_MaxNSize;

        if (R_NSize < orig_R_NSize)
            R_NSize = orig_R_NSize;
    }

    // Adjust vector heap size
    if (vect_occup > 1.0 && VNeeded < R_MaxVSize)
        R_VSize = VNeeded;

    if (vect_occup > R_VGrowFrac) {
        R_size_t change = (R_size_t)(R_VGrowIncrMin + R_VGrowIncrFrac * R_VSize);

        if (R_MaxVSize - R_VSize >= change)
            R_VSize += change;
    }
    else if (vect_occup < R_VShrinkFrac) {
        R_VSize -= (R_size_t)(R_VShrinkIncrMin + R_VShrinkIncrFrac * R_VSize);

        if (R_VSize < VNeeded)
            R_VSize = VNeeded;

        if (R_VSize < orig_R_VSize)
            R_VSize = orig_R_VSize;
    }

    DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup);
}


/* Managing Old-to-New References. */
static void old_to_new(SEXP x, SEXP y)
{
#ifdef EXPEL_OLD_TO_NEW
    GCNode::Ager age(NODE_GENERATION(x));
    age(y);
#else
    GCNode::s_OldToNew[NODE_GENERATION(x)]->splice(x);
#endif
}

#ifdef COMPUTE_REFCNT_VALUES
#define FIX_REFCNT_EX(x, old, new_, chkpnd) do {			\
	SEXP __x__ = (x);						\
	if (REFCNT_ENABLED(__x__)) {					\
	    SEXP __old__ = (old);					\
	    SEXP __new__ = (new_);					\
	    if (__old__ != __new__) {					\
		if (__old__) {						\
		    if ((chkpnd) && ASSIGNMENT_PENDING(__x__))		\
			SET_ASSIGNMENT_PENDING(__x__, FALSE);		\
		    else						\
			DECREMENT_REFCNT(__old__);			\
		}							\
		if (__new__) INCREMENT_REFCNT(__new__);			\
	    }								\
	}								\
    } while (0)
#define FIX_REFCNT(x, old, new_) FIX_REFCNT_EX(x, old, new_, FALSE)
#define FIX_BINDING_REFCNT(x, old, new_)		\
    FIX_REFCNT_EX(x, old, new_, TRUE)
#else
#define FIX_REFCNT(x, old, new_) do {} while (0)
#define FIX_BINDING_REFCNT(x, old, new_) do {				\
	SEXP __x__ = (x);						\
	SEXP __old__ = (old);						\
	SEXP __new__ = (new_);						\
	if (ASSIGNMENT_PENDING(__x__) && __old__ &&			\
	    __old__ != __new__)						\
	    SET_ASSIGNMENT_PENDING(__x__, FALSE);			\
    } while (0)
#endif

#define CHECK_OLD_TO_NEW(x,y) do { \
  if (NODE_IS_OLDER(CHK(x), CHK(y))) old_to_new(x,y);  } while (0)


/* Node Sorting.  SortNodes attempts to improve locality of reference
   by rearranging the free list to place nodes on the same place page
   together and order nodes within pages.  This involves a sweep of the
   heap, so it should not be done too often, but doing it at least
   occasionally does seem essential.  Sorting on each full colllection is
   probably sufficient.
*/

#define SORT_NODES
#ifdef SORT_NODES
static void SortNodes(void)
{
    MemoryBank::defragment();
}
#endif


/* Finalization and Weak References */

/* The design of this mechanism is very close to the one described in
   "Stretching the storage manager: weak pointers and stable names in
   Haskell" by Peyton Jones, Marlow, and Elliott (at
   https://www.microsoft.com/en-us/research/wp-content/uploads/1999/09/stretching.pdf). --LT */

static std::list<SEXP> s_R_weak_refs;

#define READY_TO_FINALIZE_MASK 1

#define SET_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp |= READY_TO_FINALIZE_MASK)
#define CLEAR_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp &= ~READY_TO_FINALIZE_MASK)
#define IS_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp & READY_TO_FINALIZE_MASK)

#define FINALIZE_ON_EXIT_MASK 2

#define SET_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp |= FINALIZE_ON_EXIT_MASK)
#define CLEAR_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp &= ~FINALIZE_ON_EXIT_MASK)
#define FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp & FINALIZE_ON_EXIT_MASK)

static SEXP MakeCFinalizer(R_CFinalizer_t cfun);

static SEXP NewWeakRef(SEXP key, SEXP val, SEXP fin, bool onexit)
{
    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case BCODESXP:
	break;
    default: error("%s", _("can only weakly reference/finalize reference objects"));
    }

    PROTECT(key);
    PROTECT(val = MAYBE_REFERENCED(val) ? duplicate(val) : val);
    PROTECT(fin);
    WeakRef *w = WeakRef::create();
    if (key != R_NilValue) {
	/* If the key is R_NilValue we don't register the weak reference.
	   This is used in loading saved images. */
	SET_WEAKREF_KEY(w, key);
	SET_WEAKREF_VALUE(w, val);
	SET_WEAKREF_FINALIZER(w, fin);
	CLEAR_READY_TO_FINALIZE(w);
	if (onexit)
	    SET_FINALIZE_ON_EXIT(w);
	else
	    CLEAR_FINALIZE_ON_EXIT(w);
	s_R_weak_refs.push_back(w);
    }
    UNPROTECT(3);
    return w;
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    switch (TYPEOF(fin)) {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	break;
    default: error("%s", _("finalizer must be a function or NULL"));
    }
    return NewWeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    PROTECT(key);
    PROTECT(val);
    SEXP w = NewWeakRef(key, val, MakeCFinalizer(fin), onexit);
    UNPROTECT(2);
    return w;
}

static bool s_R_finalizers_pending = false;
static void CheckFinalizers(void)
{
    s_R_finalizers_pending = false;
    for (auto &s : s_R_weak_refs) {
	if (s && WEAKREF_KEY(s) && !NODE_IS_MARKED(WEAKREF_KEY(s)) && !IS_READY_TO_FINALIZE(s))
	    SET_READY_TO_FINALIZE(s);
	if (IS_READY_TO_FINALIZE(s))
	    s_R_finalizers_pending = true;
    }
}

/* C finalizers are stored in a RAWSXP.  It would be nice if we could
   use EXTPTRSXP's but these only hold a void *, and function pointers
   are not guaranteed to be compatible with a void *.  There should be
   a cleaner way of doing this, but this will do for now. --LT */
/* Changed to RAWSXP in 2.8.0 */
static bool isCFinalizer(SEXP fun)
{
    return TYPEOF(fun) == RAWSXP;
    /*return TYPEOF(fun) == EXTPTRSXP;*/
}

static SEXP MakeCFinalizer(R_CFinalizer_t cfun)
{
    RawVector *s = RawVector::create(sizeof(R_CFinalizer_t));
    *((R_CFinalizer_t *) RAW(s)) = cfun;
    return s;
    /*return R_MakeExternalPtr((void *) cfun, R_NilValue, R_NilValue);*/
}

static R_CFinalizer_t GetCFinalizer(SEXP fun)
{
    return *((R_CFinalizer_t *) RAW(fun));
    /*return (R_CFinalizer_t) R_ExternalPtrAddr(fun);*/
}

SEXP R_WeakRefKey(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error("%s", _("not a weak reference"));
    return WEAKREF_KEY(w);
}

SEXP R_WeakRefValue(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error("%s", _("not a weak reference"));
    SEXP v = WEAKREF_VALUE(w);
    if (v != R_NilValue)
	ENSURE_NAMEDMAX(v);
    return v;
}

void R_RunWeakRefFinalizer(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error("%s", _("not a weak reference"));
    GCStackRoot<> key, fun;
    key = WEAKREF_KEY(w);
    fun = WEAKREF_FINALIZER(w);
    SET_WEAKREF_KEY(w, R_NilValue);
    SET_WEAKREF_VALUE(w, R_NilValue);
    SET_WEAKREF_FINALIZER(w, R_NilValue);
    if (!IS_READY_TO_FINALIZE(w))
	SET_READY_TO_FINALIZE(w); /* insures removal from list on next gc */
    bool oldintrsusp = Evaluator::interruptsSuspended();
    Evaluator::setInterruptsSuspended(true);
    if (isCFinalizer(fun)) {
	/* Must be a C finalizer. */
	R_CFinalizer_t cfun = GetCFinalizer(fun);
	cfun(key);
    }
    else if (fun != R_NilValue) {
	GCStackRoot<> e;
	/* An R finalizer. */
	e = LCONS(fun, CONS(key, R_NilValue));
	Evaluator::evaluate(e, Environment::global());
    }
    Evaluator::setInterruptsSuspended(oldintrsusp);
}

bool R::RunFinalizers(void)
{
    R_CHECK_THREAD;

    if (s_R_weak_refs.empty())
        return false;

    /* Prevent this function from running again when already in
       progress. Jumps can only occur inside the top level context
       where they will be caught, so the flag is guaranteed to be
       reset at the end. */
    static bool s_running = false;
    if (s_running) return false;
    s_running = true;

    bool finalizer_run = false;
    std::list<SEXP> pending_refs;

    while (!s_R_weak_refs.empty()) {
        SEXP s = s_R_weak_refs.back();
        s_R_weak_refs.pop_back();
        if (!IS_READY_TO_FINALIZE(s)) {
            pending_refs.push_front(s);
        }
        else {
            size_t savestack;
            GCRoot<> topExp, oldHStack, oldRStack;
            bool oldvis;
            oldHStack = R_HandlerStack;
            oldRStack = R_RestartStack;
            oldvis = Evaluator::resultPrinted();
            R_HandlerStack = R_NilValue;
            R_RestartStack = R_NilValue;

            finalizer_run = true;

            {
                // An Evaluator is declared for the finalizer to
                // insure that any errors that might occur do not spill into
                // the call that triggered the collection:
                Evaluator evalr;
                RCNTXT toplevel(CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv, R_BaseEnv, R_NilValue, R_NilValue);
                savestack = R_PPStackTop;
                topExp = R_CurrentExpr;
                /* The value of 'next' is protected to make it safe
                   for this routine to be called recursively from a
                   gc triggered by a finalizer. */
                try
                {

                    /* The entry in the weak reference list is removed
                       before running the finalizer.  This insures that a
                       finalizer is run only once, even if running it
                       raises an error. */
                    R_RunWeakRefFinalizer(s);
                }
                catch (CommandTerminated)
                {
                }
            }
            ProtectStack::restoreSize(savestack);
            R_CurrentExpr = topExp;
            R_HandlerStack = oldHStack;
            R_RestartStack = oldRStack;
            Evaluator::enableResultPrinting(oldvis);
        }
    }
    if (!pending_refs.empty())
        s_R_weak_refs = std::move(pending_refs);
    s_running = false;
    s_R_finalizers_pending = false;
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    R_checkConstants(true);

    for (auto &s : s_R_weak_refs)
	if (s && FINALIZE_ON_EXIT(s))
	    SET_READY_TO_FINALIZE(s);
    RunFinalizers();
}

void R_RunPendingFinalizers(void)
{
    if (s_R_finalizers_pending)
        RunFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

/* R interface function */

attribute_hidden SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	error("%s", _("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	error("%s", _("second argument must be a function"));

    bool onexit = asLogicalNoNA(CADDR(args), "onexit");

    R_RegisterFinalizerEx(CAR(args), CADR(args), (Rboolean) onexit);
    return R_NilValue;
}

#ifdef PROTECTCHECK
#define FREE_FORWARD_CASE case FREESXP: if (GCManager::gc_inhibit_release()) break;
#else
#define FREE_FORWARD_CASE
#endif

/* The Generational Collector. */
namespace CXXR
{
    void RObject::visitReferents(const_visitor *v) const
    {
        GCNode::visitReferents(v);
        const GCNode *attrib = m_attrib;
        if (attrib != R_NilValue)
            (*v)(attrib);

        if (!altrep())
        {
            switch (sexptype())
            {
            case NILSXP:
            case BUILTINSXP:
            case SPECIALSXP:
            case CHARSXP:
            case LGLSXP:
            case INTSXP:
            case REALSXP:
            case CPLXSXP:
            case RAWSXP:
                break;
            case OBJSXP:
            case WEAKREFSXP:
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
            case ENVSXP:
            case LISTSXP:
            case LANGSXP:
            case DOTSXP:
            case PROMSXP:
            case CLOSXP:
            case SYMSXP:
            case BCODESXP:
            case EXTPTRSXP:
                break;
            FREE_FORWARD_CASE
            default:
                // Rf_error(_("unexpected type %d in %s"), sexptype(), __func__);
                BadObject::register_bad_object(this, __FILE__, __LINE__);
            }
        }
    }

    void RObject::detachReferents()
    {
        if (!refCountEnabled())
            return;

#ifdef PROTECTCHECK
        if (sexptype() == FREESXP)
        {
            sxpinfo.type = SEXPTYPE(sxpinfo.gp);
        }
#endif
        m_attrib.detach();

        if (!altrep())
        {
            switch (sexptype())
            {
            case NILSXP:
            case BUILTINSXP:
            case SPECIALSXP:
            case CHARSXP:
            case LGLSXP:
            case INTSXP:
            case REALSXP:
            case CPLXSXP:
            case RAWSXP:
                break;
            case OBJSXP:
            case WEAKREFSXP:
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
            case ENVSXP:
            case LISTSXP:
            case LANGSXP:
            case DOTSXP:
            case PROMSXP:
            case CLOSXP:
            case SYMSXP:
            case BCODESXP:
            case EXTPTRSXP:
                break;
            FREE_FORWARD_CASE
            default:
                // Rf_error(_("unexpected type %d in %s"), sexptype(), __func__);
                BadObject::register_bad_object(this, __FILE__, __LINE__);
            }
        }
        GCNode::detachReferents();
    }
} // namespace CXXR

void GCNode::propagateAges(unsigned int max_generation)
{
#ifndef EXPEL_OLD_TO_NEW
    /* eliminate old-to-new references in generations to collect by
       transferring referenced nodes to referring generation */
    for (unsigned int gen = 1; gen < max_generation; gen++) {
        Ager ager(gen);
        const GCNode *s = NEXT_NODE(s_OldToNew[gen]);
        while (s != s_OldToNew[gen].get()) {
            const GCNode *next = NEXT_NODE(s);
            s->visitReferents(&ager);
            s_Old[gen]->splice(s);
            s = next;
        }
    }
#endif
}

#define MARK_THRU(s) if (s != R_NilValue) marker(s);

void GCNode::mark(unsigned int max_generation)
{
    /* unmark all marked nodes in old generations to be collected and
       move to New space */
    for (unsigned int gen = 0 /* 1 */; gen < max_generation; gen++) {
        const GCNode *s = NEXT_NODE(s_Old[gen]);
        while (s != s_Old[gen].get()) {
            const GCNode *next = NEXT_NODE(s);
            if (gen < numGenerations() - 1)
            {
                // Advance generation:
                --s_gencount[gen];
                SET_NODE_GENERATION(s, gen + 1);
                ++s_gencount[gen + 1];
            }
            UNMARK_NODE(s);
            s = next;
        }
        if ((gen > 0) && (NEXT_NODE(s_Old[gen]) != s_Old[gen].get()))
            BULK_MOVE(s_Old[gen].get(), s_New.get());
    }

    Marker marker(1 + max_generation); // some of the nodes have already aged (see above)
    GCRootBase::visitRoots(&marker);
    GCStackRootBase::visitRoots(&marker);

#ifndef EXPEL_OLD_TO_NEW
    /* scan nodes in uncollected old generations with old-to-new pointers */
    for (unsigned int gen = max_generation; gen < numGenerations(); gen++)
        for (const GCNode *s = NEXT_NODE(s_OldToNew[gen]);
            s != s_OldToNew[gen].get();
            s = NEXT_NODE(s))
            s->visitReferents(&marker);
#endif

    /* forward all roots */
    MARK_THRU(R_BlankScalarString);
    MARK_THRU(R_CurrentExpression);
    MARK_THRU(R_UnboundValue);
    MARK_THRU(R_RestartToken);
    MARK_THRU(R_MissingArg);
    MARK_THRU(R_InBCInterpreter);

    MARK_THRU(R_GlobalEnv);	           /* Global environment */
    MARK_THRU(R_BaseEnv);
    MARK_THRU(R_EmptyEnv);
    MARK_THRU(R_Warnings);	           /* Warnings, if any */

    MARK_THRU(R_HandlerStack);          /* Condition handler stack */
    MARK_THRU(R_RestartStack);          /* Available restarts stack */

    MARK_THRU(R_Srcref);                /* Current source reference */

    MARK_THRU(R_TrueValue);
    MARK_THRU(R_FalseValue);
    MARK_THRU(R_LogicalNAValue);

    MARK_THRU(R_print.na_string);
    MARK_THRU(R_print.na_string_noquote);

    for (auto &[key, symbol] : Symbol::s_symbol_table)
    {
        if (ATTRIB(symbol) != R_NilValue)
            GCManager::gc_error("****found a symbol with attributes\n");
        MARK_THRU(symbol);
    }

    for (RCNTXT *ctxt = R_GlobalContext; ctxt != NULL; ctxt = ctxt->nextcontext) {
        if (ctxt->returnValue.tag == 0)    /* For on.exit calls */
            MARK_THRU(ctxt->returnValue.u.sxpval);
    }

    for (size_t i = 0; i < R_PPStackTop; i++)	   /* Protected pointers */
        MARK_THRU(R_PPStack[i]);

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++) {
        if (sp->tag == RAWMEM_TAG)
            sp += sp->u.ival;
        else if (sp->tag == NILSXP || IS_PARTIAL_SXP_TAG(sp->tag))
            MARK_THRU(sp->u.sxpval);
    }

    /* identify weakly reachable nodes */
    {
        bool recheck_weak_refs;
        do {
            recheck_weak_refs = false;
            for (auto &s : s_R_weak_refs) {
                if (s && WEAKREF_KEY(s) && NODE_IS_MARKED(WEAKREF_KEY(s))) {
                    if (WEAKREF_VALUE(s) && (NODE_GENERATION(WEAKREF_VALUE(s)) < max_generation) && !NODE_IS_MARKED(WEAKREF_VALUE(s))) {
                        recheck_weak_refs = true;
                        MARK_THRU(WEAKREF_VALUE(s));
                    }
                    if (WEAKREF_FINALIZER(s) && (NODE_GENERATION(WEAKREF_FINALIZER(s)) < max_generation) && !NODE_IS_MARKED(WEAKREF_FINALIZER(s))) {
                        recheck_weak_refs = true;
                        MARK_THRU(WEAKREF_FINALIZER(s));
                    }
                }
            }
        } while (recheck_weak_refs);
    }

    /* mark nodes ready for finalizing */
    CheckFinalizers();

    /* process the weak reference chain */
    for (auto &s : s_R_weak_refs) {
        MARK_THRU(s);
        MARK_THRU(WEAKREF_KEY(s));
        MARK_THRU(WEAKREF_VALUE(s));
        MARK_THRU(WEAKREF_FINALIZER(s));
    }

    /* process CHARSXP cache */
    String::visitTable();


#ifdef PROTECTCHECK
    auto isVectorType = [](const GCNode *node) -> bool
        {
            static constexpr std::array<unsigned int, 9> types = {
                RAWSXP, VECSXP, EXPRSXP, CHARSXP,
                LGLSXP, INTSXP, REALSXP, CPLXSXP, STRSXP
            };
            return std::find(types.begin(), types.end(), TYPEOF(node)) != types.end();
        };
    const GCNode *s = NEXT_NODE(s_New);
    while (s != s_New.get())
    {
        const GCNode *next = NEXT_NODE(s);
        if (TYPEOF(s) != NEWSXP)
        {
            if (TYPEOF(s) != FREESXP)
            {
                if (isVectorType(s) && !ALTREP(s))
                {
                    {
                        VectorBase *vec = static_cast<VectorBase *>(const_cast<GCNode *>(s));
                        if (IS_GROWABLE(vec))
                            SET_STDVEC_LENGTH(vec, XTRUELENGTH(vec));
                    }

                    switch (TYPEOF(s))
                    {
                    case CHARSXP:
                    case RAWSXP:
                    case LGLSXP:
                    case INTSXP:
                    case REALSXP:
                    case CPLXSXP:
                    case STRSXP:
                    case EXPRSXP:
                    case VECSXP:
                        break;
                    default:
                        BadObject::register_bad_object(s, __FILE__, __LINE__);
                    }
                }
                SETOLDTYPE(s, TYPEOF(s));
                SET_TYPEOF(s, FREESXP);
            }
            if (GCManager::gc_inhibit_release())
                MARK_THRU(s);
        }
        s = next;
    }
#endif
}

void GCNode::sweep(unsigned int max_generation)
{
    const GCNode *s = NEXT_NODE(s_New);
    while (s != s_New.get())
    {
        const GCNode *next = NEXT_NODE(s);
        const_cast<GCNode *>(s)->detachReferents();
        delete s;
        s = next;
    }
}

void GCNode::gc(unsigned int num_old_gens_to_collect /* either 0, 1, or 2 */)
{
    // max_generation is a cut-off generation free from any GC activity
    unsigned int max_generation = 1 + num_old_gens_to_collect;

    DEBUG_CHECK_NODE_COUNTS("before propagating");

    propagateAges(max_generation);

    DEBUG_CHECK_NODE_COUNTS("at start");

    mark(max_generation);

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list");

    sweep(max_generation);

    DEBUG_CHECK_NODE_COUNTS("after releasing allocated nodes");
}

// former RunGenCollect()
unsigned int GCManager::gcGenController(R_size_t size_needed, bool force_full_collection)
{
    static unsigned int level = 0;
    unsigned int gens_collected;

    BadObject::s_firstBadObject.clear();

    /* determine number of generations to collect */
    if (force_full_collection) level = s_num_old_generations;

#ifdef PROTECTCHECK
    level = s_num_old_generations;
#endif

    level = genRota(level);

    bool ok = false;
    while (!ok) {
        ok = true;

        GCNode::gc(level);
        gens_collected = level;

        if (level < s_num_old_generations) {
            if (VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize) {
                ++level;
                if (VHEAP_FREE() < size_needed)
                    ok = false;
            }
            else level = 0;
        }
        else level = 0;
    } // end of while loop
    s_gen_gc_counts[gens_collected]++;

    if (gens_collected > 0)
    {
        if (gens_collected == s_num_old_generations) {
            /**** do some adjustment for intermediate collections? */
            AdjustHeapSize(size_needed);
        }
        DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
    }

#ifdef SORT_NODES
    if (gens_collected == s_num_old_generations)
        SortNodes();
#endif

    return gens_collected;
}


/* public interface for controlling GC torture settings */
/* maybe, but in no header, and now hidden */
attribute_hidden
void R_gc_torture(int gap, int wait, Rboolean inhibit)
{
    if (gap != NA_INTEGER && gap >= 0)
	GCManager::setTortureParameters(gap, gap, false);
    if (gap > 0) {
	if (wait != NA_INTEGER && wait > 0)
	    GCManager::setTortureParameters(gap, wait, false);
    }
#ifdef PROTECTCHECK
    if (gap > 0) {
	if (inhibit != NA_LOGICAL)
	    GCManager::setInhibitor(inhibit);
    }
    else GCManager::setInhibitor(false);
#endif
}

attribute_hidden SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    bool gap;
    SEXP old = ScalarLogical(GCManager::gc_force_wait() > 0);

    if (isLogical(CAR(args))) {
	gap = asRbool(CAR(args), call);
    }
    else gap = asInteger(CAR(args));

    R_gc_torture(gap, 0, FALSE);

    return old;
}

attribute_hidden SEXP do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    int old = GCManager::gc_force_gap();
    int gap = asInteger(CAR(args));
    int wait = asInteger(CADR(args));
    bool inhibit = asRbool(CADDR(args), call);
    R_gc_torture(gap, wait, (Rboolean) inhibit);

    return ScalarInteger(old);
}

/* initialize gctorture settings from environment variables */
static void init_gctorture(void)
{
    const char *arg = getenv("R_GCTORTURE");
    if (arg != NULL) {
	int gap = atoi(arg);
	if (gap > 0) {
	    GCManager::setTortureParameters(gap, gap, false);
	    arg = getenv("R_GCTORTURE_WAIT");
	    if (arg != NULL) {
		int wait = atoi(arg);
		if (wait > 0)
		    GCManager::setTortureParameters(gap, wait, false);
	    }
#ifdef PROTECTCHECK
	    arg = getenv("R_GCTORTURE_INHIBIT_RELEASE");
	    if (arg != NULL) {
		int inhibit = atoi(arg);
		GCManager::setInhibitor(inhibit > 0);
	    }
#endif
	}
    }
}

attribute_hidden SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    std::ostream *old_report_os = GCManager::setReporting(nullptr);
    checkArity(op, args);
    int want_reporting = asLogical(CAR(args));
    if (want_reporting == NA_LOGICAL)
        GCManager::setReporting(old_report_os);
    else
        GCManager::setReporting(want_reporting ? &std::cerr : nullptr);
    return Rf_ScalarLogical(old_report_os != nullptr);
}

/* reports memory use to profiler in eval.c */

attribute_hidden void R::get_current_mem(size_t *smallvsize,
				      size_t *largevsize,
				      size_t *nodes)
{
    *smallvsize = 0;
    *largevsize = MemoryBank::doublesAllocated();
    *nodes = GCNode::numNodes() * sizeof(RObject);
    return;
}

attribute_hidden SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<RealVector> value;

    checkArity(op, args);
    std::ostream *old_report_os = GCManager::setReporting(Rf_asLogical(CAR(args)) ? &std::cerr : nullptr);
    bool reset_max = asLogical(CADR(args));
    bool full = asLogical(CADDR(args));
    R_gc_full(full);

/*
gc() output:
          used (Mb) gc trigger (Mb) limit (Mb) max used  (Mb)
Ncells    v[0] v[2]       v[4] v[6]       v[8]    v[10] v[12]
Vcells    v[1] v[3]       v[5] v[7]       v[9]    v[11] v[13]
*/

    GCManager::setReporting(old_report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    value = RealVector::create(14);
    REAL(value)[0] = GCNode::numNodes();
    REAL(value)[1] = MemoryBank::doublesAllocated();
    REAL(value)[4] = R_NSize;
    REAL(value)[5] = GCManager::memoryThreshold(); // R_VSize
    /* next four are in 0.1MB, rounded up */
    REAL(value)[2] = 0.1*ceil(10. * (GCNode::numNodes())/Mega * sizeof(RObject));
    REAL(value)[3] = 0.1*ceil(10. * (MemoryBank::doublesAllocated())/Mega * vsfac);
    REAL(value)[6] = 0.1*ceil(10. * R_NSize/Mega * sizeof(RObject));
    REAL(value)[7] = 0.1*ceil(10. * R_VSize/Mega * vsfac);
    REAL(value)[8] = (R_MaxNSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxNSize/Mega * sizeof(RObject)) : NA_REAL;
    REAL(value)[9] = (R_MaxVSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxVSize/Mega * vsfac) : NA_REAL;
    if (reset_max) {
	    R_N_maxused = GCNode::numNodes();
	    R_V_maxused = MemoryBank::doublesAllocated();
    }
    REAL(value)[10] = R_N_maxused;
    REAL(value)[11] = R_V_maxused;
    REAL(value)[12] = 0.1*ceil(10. * R_N_maxused/Mega*sizeof(RObject));
    REAL(value)[13] = 0.1*ceil(10. * R_V_maxused/Mega*vsfac);

    return value;
}


static double gctimes[5], gcstarttimes[5];
static bool s_gctime_enabled = false;

/* this is primitive */
attribute_hidden SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if (args == R_NilValue)
	s_gctime_enabled = true;
    else {
	check1arg(args, call, "on");
	s_gctime_enabled = asRbool(CAR(args), call);
    }
    RealVector *ans = RealVector::create(5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (s_gctime_enabled)
	R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (s_gctime_enabled) {
	double times[5], delta;
	R_getProcTime(times);

	/* add delta to compensate for timer resolution */
#if 0
	/* this seems to over-compensate too */
	delta = R_getClockIncrement();
#else
	delta = 0;
#endif

	gctimes[0] += times[0] - gcstarttimes[0] + delta;
	gctimes[1] += times[1] - gcstarttimes[1] + delta;
	gctimes[2] += times[2] - gcstarttimes[2];
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}

/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

attribute_hidden void R::InitMemory(void)
{
    GCManager::setMonitors(gc_start_timing, gc_end_timing);
    init_gctorture();
    init_gc_grow_settings();

    const char *arg = getenv("_R_GC_FAIL_ON_ERROR_");
    if (arg != NULL && StringTrue(arg))
        GCManager::set_gc_fail_on_error(true);
    else if (arg != NULL && StringFalse(arg))
        GCManager::set_gc_fail_on_error(false);

    GCManager::setReporting(R_Verbose ? &std::cerr : nullptr);
    GCManager::enableGC(R_VSIZE, R_NSIZE);
    CXXR::initializeMemorySubsystem();
    // ProtectStack::initialize(R_PPStackSize);

    ByteCode::initialize();

    s_R_weak_refs.clear();

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  The current source line */
    R_Srcref = R_NilValue;

    /* R_TrueValue and R_FalseValue */
    R_TrueValue = mkTrue();
    MARK_NOT_MUTABLE(R_TrueValue);
    R_FalseValue = mkFalse();
    MARK_NOT_MUTABLE(R_FalseValue);
    R_LogicalNAValue = LogicalVector::createScalar(Logical::NA());
    MARK_NOT_MUTABLE(R_LogicalNAValue);
}

/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector. */

char *R_alloc(size_t num_elts, int elt_size)
{
    R_size_t size = num_elts * elt_size;
    /* doubles are a precaution against integer overflow on 32-bit */
    double dsize = (double) num_elts * elt_size;

    /* One programmer has relied on this, but it is undocumented! */
    if (dsize <= 0.0) return NULL;

#ifdef LONG_VECTOR_SUPPORT
	/* 64-bit platform: previous version used REALSXPs */
	if (dsize > (double)R_XLEN_T_MAX)  /* currently 4096 TB */
	    error(_("cannot allocate memory block of size %0.f %s"),
		  dsize/(Giga * 1024.0), "Tb");
#else
	if (dsize > (double)R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f %s"),
		  dsize/Giga, "Gb");
#endif

	return static_cast<char *>(RAllocStack::allocate(size));
}

long double *R_allocLD(size_t num_elts)
{
#if defined(__cplusplus) || defined(__alignof_is_defined)
    // This is C11: picky compilers may warn.
    size_t ld_align = alignof(long double);
#elif __GNUC__
    // This is C99, but do not rely on it.
    // Apple clang warns this is gnu extension.
    #ifdef __clang__
    # pragma clang diagnostic ignored "-Wgnu-offsetof-extensions"
    #endif
    size_t ld_align = offsetof(struct { char __a; long double __b; }, __b);
#else
    size_t ld_align = 0x0F; // value of x86_64, known others are 4 or 8
#endif
    if (ld_align > 8) {
	uintptr_t tmp = (uintptr_t) R_alloc(num_elts + 1, sizeof(long double));
	tmp = (tmp + ld_align - 1) & ~((uintptr_t)ld_align - 1);
	return (long double *) tmp;
    } else {
	return (long double *) R_alloc(num_elts, sizeof(long double));
    }
}


/* S COMPATIBILITY */

char *S_alloc(long num_elts, int elt_size)
{
    R_size_t size  = num_elts * elt_size;
    char *p = R_alloc(num_elts, elt_size);

    if (p) memset(p, 0, size);
    return p;
}


char *S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size)
{
    /* shrinking is a no-op */
    if (new_sz <= old_sz) return prev_block; // so new_sz > 0 below
    char *q = R_alloc((size_t)new_sz, elt_size);
    size_t old_bytes = (size_t)old_sz * elt_size;
    if (old_bytes)
        memcpy(q, prev_block, old_bytes);
    memset(q + old_bytes, 0, (size_t)new_sz*elt_size - old_bytes);
    return q;
}


/* Allocation functions that GC on initial failure */

void *R_malloc_gc(size_t n)
{
    void *np = malloc(n);
    if (np == NULL) {
	R_gc();
	np = malloc(n);
    }
    return np;
}

void *R_calloc_gc(size_t n, size_t s)
{
    void *np = calloc(n, s);
    if (np == NULL) {
	R_gc();
	np = calloc(n, s);
    }
    return np;
}

void *R_realloc_gc(void *p, size_t n)
{
    void *np = realloc(p, n);
    if (np == NULL) {
	R_gc();
	np = realloc(p, n);
    }
    return np;
}


/* "allocSExp" allocate a RObject */
/* call gc if necessary */

SEXP Rf_allocSExp(SEXPTYPE t)
{
    if (t == NILSXP)
        /* R_NilValue should be the only NILSXP object */
        return R_NilValue;

    switch (t)
    {
    case LISTSXP:
        return PairList::create(R_NilValue);
    case LANGSXP:
        return PairList::create<Expression>(R_NilValue);
    case DOTSXP:
        return PairList::create<DottedArgs>(R_NilValue);
    case BCODESXP:
        return ByteCode::create();
    case CLOSXP:
        return Closure::create();
    case ENVSXP:
        return Environment::create();
    case PROMSXP:
        return Promise::create();
    case SYMSXP:
        return Symbol::create();
    case SPECIALSXP:
        return BuiltInFunction::create(true);
    case BUILTINSXP:
        return BuiltInFunction::create(false);
    case EXTPTRSXP:
        return ExternalPointer::create();
    case WEAKREFSXP:
        return WeakRef::create();
    default:
        throw std::runtime_error("Incorrect SEXPTYPE (" + std::string(sexptype2char(t)) + ") for Rf_allocSExp.");
    }

    return R_NilValue;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP Rf_cons(SEXP car, SEXP cdr)
{
    return PairList::create(CHK(car), CHK(cdr), R_NilValue);
}

SEXP Rf_lcons(SEXP car, SEXP cdr)
{
    return PairList::create<Expression>(CHK(car), CHK(cdr), R_NilValue);
}

attribute_hidden SEXP R::CONS_NR(SEXP car, SEXP cdr)
{
    PairList *s = PairList::create(R_NilValue, R_NilValue, R_NilValue);

    DISABLE_REFCNT(s);
    static_cast<ConsCell *>(s)->setCar(CHK(car));
    static_cast<ConsCell *>(s)->setTail(CHK(cdr));

    return s;
}

/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame.
*/
SEXP R::NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v = CHK(valuelist);
    SEXP n = CHK(namelist);
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }

    return Environment::create(valuelist, CHK(rho));
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
attribute_hidden SEXP R::mkPROMISE(SEXP expr, SEXP rho)
{
    return Promise::create(R_UnboundValue, CHK(expr), CHK(rho));
}

attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::R_mkEVPROMISE(SEXP expr, SEXP val)
{
    return Promise::create(val, expr, R_NilValue);
}

attribute_hidden SEXP R::R_mkEVPROMISE_NR(SEXP expr, SEXP val)
{
    Promise *prom = Promise::create();
    DISABLE_REFCNT(prom);
    SET_PRCODE(prom, expr);
    SET_PRVALUE(prom, val);
    return prom;
}

/* All vector objects must be a multiple of sizeof(VectorBase)
   bytes so that alignment is preserved for all objects */

/* Allocate a vector object (and also list-like objects).
   This ensures only validity of list-like (LISTSXP, VECSXP, EXPRSXP),
   STRSXP and CHARSXP types;  e.g., atomic types remain un-initialized
   and must be initialized upstream, e.g., in do_makevector().
*/
#ifndef enum_SEXPTYPE
#define intCHARSXP 73
#endif

SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t n_elem, R_allocator_t *allocator)
{
    if (n_elem > R_XLEN_T_MAX) // n_elem > 2^52
        error(_("cannot allocate vector of length %lld"), (long long)n_elem);
    else if (n_elem < 0)
        error("%s", _("negative length vectors are not allowed"));

    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
        return R_NilValue;
    case CHARSXP:
        error("%s", _("use of allocVector(CHARSXP ...) is defunct\n"));
        break;
    case RAWSXP:
        return RawVector::create(n_elem, allocator);
    case LGLSXP:
        return LogicalVector::create(n_elem, allocator);
    case INTSXP:
        return IntVector::create(n_elem, allocator);
    case REALSXP:
        return RealVector::create(n_elem, allocator);
    case CPLXSXP:
        return ComplexVector::create(n_elem, allocator);
    case STRSXP:
        return StringVector::create(n_elem, allocator);
    case EXPRSXP:
        return ExpressionVector::create(n_elem, allocator);
    case VECSXP:
        return ListVector::create(n_elem, allocator);
    case LANGSXP:
#ifdef LONG_VECTOR_SUPPORT
        if (n_elem > R_SHORT_LEN_MAX) error("%s", _("invalid length for pairlist"));
#endif
        return Rf_allocLang((int)n_elem);
    case LISTSXP:
#ifdef LONG_VECTOR_SUPPORT
        if (n_elem > R_SHORT_LEN_MAX) error("%s", _("invalid length for pairlist"));
#endif
        return Rf_allocList((int)n_elem);
    default:
        error(_("invalid type/length (%s/%lld) in vector allocation"),
            type2char(type), (long long)n_elem);
    }

    return R_NilValue;
}

String *CXXR::CXXR_allocCharsxp(const std::string &name, cetype_t encoding, bool isAscii)
{
    size_t n_elem = name.length();
    if (n_elem > R_XLEN_T_MAX)
        error(_("cannot allocate vector of length %lld"), (long long)length);
    else if (n_elem < 0)
        error("%s", _("negative length vectors are not allowed"));

    return String::create(name, encoding, isAscii);
}

/* For future hiding of allocVector(CHARSXP) */
attribute_hidden SEXP R::allocCharsxp(R_xlen_t n_elem)
{
    std::string str;
    str.resize(n_elem);
    return CXXR_allocCharsxp(str, CE_NATIVE, false);
}

SEXP Rf_allocList(int n)
{
    if (n > 0)
        return PairList::makeList(n);
    return R_NilValue;
}

SEXP Rf_allocLang(int n)
{
    if (n > 0)
        return PairList::create<Expression>(R_NilValue, PairList::makeList(n - 1));
    return R_NilValue;
}

SEXP Rf_allocS4Object(void)
{
   S4Object *s;
   GC_PROT(s = S4Object::create());

   return s;
}

attribute_hidden SEXP R::R_allocObject(void)
{
   S4Object *s;
   GC_PROT(s = S4Object::create(false));

   return s;
}

namespace
{
    SEXP allocFormalsList(std::initializer_list<SEXP> sym_list)
    {
        SEXP res = Rf_allocList(sym_list.size());
        R_PreserveObject(res);

        SEXP n = res;
        for (const auto &sym : sym_list)
        {
            SET_TAG(n, sym);
            MARK_NOT_MUTABLE(n);
            n = CDR(n);
        }

        return res;
    }
} // anonymous namespace


attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::allocFormalsList2(SEXP sym1, SEXP sym2)
{
    return allocFormalsList({sym1, sym2});
}

attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3)
{
    return allocFormalsList({sym1, sym2, sym3});
}

attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4)
{
    return allocFormalsList({sym1, sym2, sym3, sym4});
}

attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5)
{
    return allocFormalsList({sym1, sym2, sym3, sym4, sym5});
}

attribute_hidden /* would need to be in an installed header if not hidden */
SEXP R::allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4,
		       SEXP sym5, SEXP sym6)
{
    return allocFormalsList({sym1, sym2, sym3, sym4, sym5, sym6});
}

/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    R_gc_full(true);
}

void R_gc_full(bool full)
{
    GCManager::gc(0, full);
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif
}

// static void R_gc_no_finalizers(R_size_t size_needed)
// {
//     GCManager::gc(size_needed, true);
// }

#ifdef THREADCHECK
# if !defined(Win32) && defined(HAVE_PTHREAD)
#   include <pthread.h>
attribute_hidden void R::R_check_thread(const char *s)
{
    static bool s_main_thread_inited = false;
    static pthread_t main_thread;
    if (!s_main_thread_inited) {
        main_thread = pthread_self();
        s_main_thread_inited = true;
    }
    if (!pthread_equal(main_thread, pthread_self())) {
        char buf[1024];
	size_t bsize = sizeof buf;
	memset(buf, 0, bsize);
        snprintf(buf, bsize - 1, "Wrong thread calling '%s'", s);
        R_Suicide(buf);
    }
}
# else
/* This could be implemented for Windows using their threading API */
attribute_hidden void R::R_check_thread(const char *s) {}
# endif
#endif

// former R_gc_internal()
void GCManager::gc(R_size_t size_needed, bool force_full_collection)
{
    R_CHECK_THREAD;
    if (GCInhibitor::active() || s_gc_is_running) {
        if (s_gc_is_running)
            gc_error("*** recursive gc invocation\n");

        // if (!force_full_collection &&
        //     VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize)
        //     force_full_collection = true;

        if (size_needed > VHEAP_FREE()) {
            R_size_t expand = size_needed - VHEAP_FREE();
            if (R_VSize + expand > R_MaxVSize)
                mem_err_heap();
            R_VSize += expand;
        }

        s_gc_pending = true;
        return;
    }
    s_gc_pending = false;

    double ncells, vcells, vfrac, nfrac;
    BadObject bad_object;
    unsigned int gens_collected = 0;

#ifdef IMMEDIATE_FINALIZERS
    bool first = true;
    bool ok = false;
    while (!ok) {
        ok = true;
#endif

        ++s_gc_count;

        R_N_maxused = std::max(R_N_maxused, GCNode::numNodes());
        R_V_maxused = std::max(R_V_maxused, MemoryBank::doublesAllocated());

        BEGIN_SUSPEND_INTERRUPTS{
        s_gc_is_running = true;
        if (s_pre_gc) // gc_start_timing();
            (*s_pre_gc)();
        gens_collected = gcGenController(size_needed, force_full_collection);
        if (s_post_gc) // gc_end_timing();
            (*s_post_gc)();
        s_gc_is_running = false;
        } END_SUSPEND_INTERRUPTS;

        if (R_check_constants > 2 ||
            (R_check_constants > 1 && gens_collected == GCNode::numOldGenerations()))
            R_checkConstants(true);

        if (s_os) {
            REprintf("Garbage collection %d = %d", s_gc_count, s_gen_gc_counts[0]);
            for (unsigned int i = 0; i < GCNode::numOldGenerations(); i++)
                REprintf("+%d", s_gen_gc_counts[i + 1]);
            REprintf(" (level %d) ... ", gens_collected);
            DEBUG_GC_SUMMARY(gens_collected == GCNode::numOldGenerations());

            ncells = GCNode::numNodes();
            nfrac = (100.0 * ncells) / R_NSize;
            /* We try to make this consistent with the results returned by gc */
            ncells = 0.1 * ceil(10 * ncells * sizeof(RObject) / Mega);
            REprintf("\n%.1f %s of cons cells used (%d%%)\n",
                ncells, "Mbytes", (int)(nfrac + 0.5));
            vcells = MemoryBank::doublesAllocated();
            vfrac = (100.0 * vcells) / R_VSize;
            vcells = 0.1 * ceil(10 * vcells * vsfac / Mega);
            REprintf("%.1f %s of vectors used (%d%%)\n",
                vcells, "Mbytes", (int)(vfrac + 0.5));
        }

        if (!BadObject::s_firstBadObject.isEmpty()) {
            bad_object = BadObject::s_firstBadObject;
        }

#ifdef IMMEDIATE_FINALIZERS
        if (first) {
            first = false;
            /* Run any eligible finalizers.  The return result of
               RunFinalizers is true if any finalizers are actually run.
               There is a small chance that running finalizers here may
               chew up enough memory to make another immediate collection
               necessary.  If so, we jump back to the beginning and run
               the collection, but on this second pass we do not run
               finalizers. */
            if (RunFinalizers() && (size_needed > VHEAP_FREE()))
                ok = false;
        }
    } // end of while loop
#endif

    bad_object.printSummary();

    /* sanity check on logical scalar values */
    if (R_TrueValue != NULL && LOGICAL(R_TrueValue)[0] != TRUE) {
        LOGICAL(R_TrueValue)[0] = TRUE;
        gc_error(_("internal TRUE value has been modified"));
    }
    if (R_FalseValue != NULL && LOGICAL(R_FalseValue)[0] != FALSE) {
        LOGICAL(R_FalseValue)[0] = FALSE;
        gc_error(_("internal FALSE value has been modified"));
    }
    if (R_LogicalNAValue != NULL &&
        LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL) {
        LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
        gc_error(_("internal logical NA value has been modified"));
    }
}


attribute_hidden SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    GCStackRoot<IntVector> ans;
    GCStackRoot<StringVector> nms;
    unsigned int tmp;

    ans = IntVector::create(24);
    nms = StringVector::create(24);
    for (unsigned int i = 0; i < 24; i++) {
	INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str((SEXPTYPE) (i > LGLSXP? i+2 : i)));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {

      /* run a full GC to make sure that all stuff in use is in Old space */
      R_gc();
      for (unsigned int gen = 1; gen < GCNode::numGenerations(); gen++) {
	  for (const GCNode *s = NEXT_NODE(GCNode::s_Old[gen]);
	       s != GCNode::s_Old[gen].get();
	       s = NEXT_NODE(s)) {
	      tmp = TYPEOF(s);
	      if (tmp > LGLSXP) tmp -= 2;
	      INTEGER(ans)[tmp]++;
	  }
      }
    } END_SUSPEND_INTERRUPTS;

    return ans;
}

/* "protect" push a single argument onto R_PPStack */

/* In handling a stack overflow we have to be careful not to use
   PROTECT. error("protect(): stack overflow") would call deparse1,
   which uses PROTECT and segfaults.*/

/* However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT */

NORET void R::R_signal_protect_error(void)
{
    error("%s", _("R_signal_protect_error() is no-op in CXXR"));
}

NORET void R::R_signal_unprotect_error(void)
{
    error(n_("unprotect(): only %td protected item",
		   "unprotect(): only %td protected items", R_PPStackTop),
	  R_PPStackTop);
}

unsigned int ProtectStack::protect_(SEXP s)
{
    R_CHECK_THREAD;
    ProtectStack::s_stack.push_back(CHK(s));
    return R_PPStackTop - 1;
}


/* "unprotect" pop argument list from top of R_PPStack */

void ProtectStack::unprotect_(unsigned int l)
{
    R_CHECK_THREAD;
    if (R_PPStackTop < l)
        R_signal_unprotect_error();

    while (l--)
    {
        ProtectStack::s_stack.pop_back();
    }
}

/* "Rf_unprotect_ptr" remove pointer from somewhere in R_PPStack */

void ProtectStack::unprotectPtr(SEXP s)
{
    R_CHECK_THREAD;
    for (auto it = ProtectStack::s_stack.end(); it != ProtectStack::s_stack.begin();) {
        --it; // Move the iterator backwards
        if (*it == s) {
            ProtectStack::s_stack.erase(it);
            return;
        }
    }
    error("%s", _("unprotect_ptr: pointer not found"));
}

/* Debugging function:  is s protected? */

attribute_hidden std::pair<bool, unsigned int> R::Rf_isProtected(SEXP s)
{
    R_CHECK_THREAD;
    size_t i = R_PPStackTop;
    for (auto it = ProtectStack::s_stack.end(); it != ProtectStack::s_stack.begin();) {
        --it; // Move the iterator backwards
        --i;
        if (*it == s) {
            return std::pair(true, i);
        }
    }
    return std::pair(false, 0);
}


NORET void R::R_signal_reprotect_error(PROTECT_INDEX i)
{
    error(n_("R_Reprotect: only %td protected item, can't reprotect index %d",
		   "R_Reprotect: only %td protected items, can't reprotect index %d",
		   R_PPStackTop),
	  R_PPStackTop, i);
}

void ProtectStack::reprotect(SEXP s, PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}

#ifdef UNUSED
/* remove all objects from the protection stack from index i upwards
   and return them in a vector. The order in the vector is from new
   to old. */
SEXP R_CollectFromIndex(PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    SEXP res;
    size_t top = R_PPStackTop;
    int j = 0;
    if (i > top) i = top;
    res = Rf_protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    ProtectStack::restoreSize(top); /* this includes the protect we used above */
    return res;
}
#endif

/* "initStack" initialize environment stack */
attribute_hidden
void initStack(void)
{
}


/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if (nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if (!p)
	error(_("'R_Calloc' could not allocate memory (%llu of %llu bytes)"),
	      (unsigned long long)nelem, (unsigned long long)elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if (ptr) p = realloc(ptr, size); else p = malloc(size);
    if (!p)
	error(n_("'R_Realloc' could not re-allocate memory (%llu byte)",
	      "'R_Realloc' could not re-allocate memory (%llu bytes)",
	      (unsigned long long)size), (unsigned long long)size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if (!ptr) warning("attempt to free NULL pointer by Free"); */
    if (ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

void *R_chk_memcpy(void *dest, const void *src, size_t n)
{
    if (n >= PTRDIFF_MAX)
	error(n_("object is too large (%llu byte)", "object is too large (%llu bytes)", (unsigned long long)n), (unsigned long long)n);
    return n ? memcpy(dest, src, n) : dest;
}

void *R_chk_memset(void *s, int c, size_t n)
{
    if (n >= PTRDIFF_MAX)
	error(n_("object is too large (%llu byte)", "object is too large (%llu bytes)", (unsigned long long)n), (unsigned long long)n);
    return n ? memset(s, c, n) : s;
}

/** @brief List of Persistent Objects
 *
 * @details This code keeps a list of objects which are not assigned to variables
 * but which are required to persist across garbage collections.  The
 * objects are registered with R_PreserveObject and deregistered with
 * R_ReleaseObject.
 * 
 * @note CR allows preserving the same object multiple times.
 * In CXXR it is supported by keeping track of duplicate Preserves.
 */

static std::unordered_map<const RObject *, std::pair<GCRoot<>, unsigned int /* keeps track of duplicate Preserves*/>,
    std::hash<const RObject *>,
    std::equal_to<const RObject *>>
    precious;

void R_PreserveObject(SEXP object)
{
    R_CHECK_THREAD;
    if (precious.find(object) == precious.end())
    {
        precious[object] = std::make_pair(GCRoot<>(object), 0);
    }
    else
    {
        ++(precious[object].second);
    }
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    if (precious.find(object) != precious.end())
    {
        if (precious[object].second > 0)
        {
            --(precious[object].second);
        }
        else
        {
            precious.erase(object);
        }
    }
}

/* This code is similar to R_PreserveObject/R_ReleasObject, but objects are
   kept in a provided multi-set (which needs to be itself protected).
   When protected via PROTECT, the multi-set is automatically unprotected
   during long jump, and thus all its members are eventually reclaimed.
   These functions were introduced for parsers generated by bison, because
   one cannot instruct bison to use PROTECT/UNPROTECT when working with
   the stack of semantic values. */

/* Multi-set is defined by a triple (store, npreserved, initialSize)
     npreserved is the number of elements in the store (counting each instance
       of the same value)
     store is a VECSXP or R_NilValue
       when VECSXP, preserved values are stored at the beginning, filled up by
       R_NilValue
     initialSize is the size for the VECSXP to be allocated if preserving values
       while store is R_NilValue

    The representation is CONS(store, npreserved) with TAG()==initialSize
*/

/* Create new multi-set for protecting objects. initialSize may be zero
   (a hardcoded default is then used). */
SEXP R_NewPreciousMSet(int initialSize)
{
    GCStackRoot<> mset;

    /* npreserved is modified in place */
    IntVector *npreserved = IntVector::create(1);
    SET_INTEGER_ELT(npreserved, 0, 0);
    mset = CONS(R_NilValue, npreserved);
    /* isize is not modified in place */
    if (initialSize < 0)
	error("%s", _("'initialSize' must be non-negative"));
    SEXP isize = ScalarInteger(initialSize);
    SET_TAG(mset, isize);

    return mset;
}

static void checkMSet(SEXP mset)
{
    SEXP store = CAR(mset);
    SEXP npreserved = CDR(mset);
    SEXP isize = TAG(mset);
    if (/*MAYBE_REFERENCED(mset) ||*/
	((store != R_NilValue) &&
	 (TYPEOF(store) != VECSXP /*|| MAYBE_REFERENCED(store)*/)) ||
	(TYPEOF(npreserved) != INTSXP || XLENGTH(npreserved) != 1 /*||
	 MAYBE_REFERENCED(npreserved)*/) ||
	(TYPEOF(isize) != INTSXP || XLENGTH(isize) != 1))

	error("%s", _("Invalid mset"));
}

/* Add object to multi-set. The object will be protected as long as the
   multi-set is protected. */
void R_PreserveInMSet(SEXP x_, SEXP mset)
{
    if (x_ == R_NilValue || isSymbol(x_))
	return; /* no need to preserve */
    GCStackRoot<> x(x_);
    checkMSet(mset);
    SEXP store = CAR(mset);
    int *n = INTEGER(CDR(mset));
    if (store == R_NilValue) {
	R_xlen_t newsize = INTEGER_ELT(TAG(mset), 0);
	if (newsize == 0)
	    newsize = 4; /* default minimum size */
	store = ListVector::create(newsize);
	SETCAR(mset, store);
    }
    R_xlen_t size = XLENGTH(store);
    if (*n == size) {
	R_xlen_t newsize = 2 * size;
	if (newsize >= INT_MAX || newsize < size)
	    error("%s", _("Multi-set overflow"));
	GCStackRoot<ListVector> newstore;
	newstore = ListVector::create(newsize);
	for (R_xlen_t i = 0; i < size; i++)
	    SET_VECTOR_ELT(newstore, i, VECTOR_ELT_0(store, i));
	SETCAR(mset, newstore);
	store = newstore;
    }

    SET_VECTOR_ELT(store, (*n)++, x);
}

/* Remove (one instance of) the object from the multi-set. If there is another
   instance of the object in the multi-set, it will still be protected. If there
   is no instance of the object, the function does nothing. */
void R_ReleaseFromMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
	return; /* not preserved */
    checkMSet(mset);
    SEXP store = CAR(mset);
    if (store == R_NilValue)
	return; /* not preserved */
    int *n = INTEGER(CDR(mset));
    for (R_xlen_t i = (*n) - 1; i >= 0; i--) {
	if (VECTOR_ELT_0(store, i) == x) {
	    for (;i < (*n) - 1; i++)
		SET_VECTOR_ELT(store, i, VECTOR_ELT_0(store, i + 1));
	    SET_VECTOR_ELT(store, i, R_NilValue);
	    (*n)--;
	    return;
	}
    }
    /* not preserved */
}

/* Release all objects from the multi-set, but the multi-set can be used for
   preserving more objects. */
attribute_hidden void R::R_ReleaseMSet(SEXP mset, int keepSize)
{
    checkMSet(mset);
    SEXP store = CAR(mset);
    if (store == R_NilValue)
	return; /* already empty */
    int *n = INTEGER(CDR(mset));
    if (XLENGTH(store) <= keepSize) {
	/* just free the entries */
	for (R_xlen_t i = 0; i < *n; i++)
	    SET_VECTOR_ELT(store, i, R_NilValue);
    } else
	SETCAR(mset, R_NilValue);
    *n = 0;
}

/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    ExternalPointer *s = ExternalPointer::create();
    EXTPTR_PTR(s) = p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

#define CHKEXTPTRSXP(x)							\
    if (TYPEOF(x) != EXTPTRSXP)						\
	error(_("%s: argument of type %s is not an external pointer"),	\
	      __func__, sexptype2char(TYPEOF(x)))

void *R_ExternalPtrAddr(SEXP s)
{
    CHKEXTPTRSXP(s);
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    CHKEXTPTRSXP(s);
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    CHKEXTPTRSXP(s);
    return CHK(EXTPTR_PROT(CHK(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    CHKEXTPTRSXP(s);
    EXTPTR_PTR(s) = NULL;
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    CHKEXTPTRSXP(s);
    EXTPTR_PTR(s) = p;
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    CHKEXTPTRSXP(s);

    CHECK_OLD_TO_NEW(s, tag);
    EXTPTR_TAG(s).retarget(s, tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    CHKEXTPTRSXP(s);

    CHECK_OLD_TO_NEW(s, p);
    EXTPTR_PROT(s).retarget(s, p);
}

/*
   Added to API in R 3.4.0.
   Work around casting issues: works where it is needed.
 */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    ExternalPointer *s = ExternalPointer::create();
    tmp.fn = p;
    EXTPTR_PTR(s) = tmp.p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    CHKEXTPTRSXP(s);
    fn_ptr tmp;
    tmp.p = EXTPTR_PTR(CHK(s));
    return tmp.fn;
}



/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x) { return CHK(ATTRIB(CHK(x))); }
int (ANY_ATTRIB)(SEXP x) { CR_ASSERT(x); return ANY_ATTRIB(CHK(x)); }
int (OBJECT)(SEXP x) { return OBJECT(CHK(x)); }
SEXPTYPE (TYPEOF)(SEXP x) { return TYPEOF(CHK(x)); }
int (NAMED)(SEXP x) { return NAMED(CHK(x)); }
attribute_hidden int (R::RTRACE)(SEXP x) { return RTRACE(CHK(x)); }
int (LEVELS)(SEXP x) { CR_ASSERT(x); return LEVELS(CHK(x)); }
int (REFCNT)(SEXP x) { return REFCNT(CHK(x)); }
attribute_hidden bool (R::REFCNT_ENABLED)(SEXP x) { CR_ASSERT(x); return REFCNT_ENABLED(CHK(x)); }
int (ALTREP)(SEXP x) { return ALTREP(CHK(x)); }
void (MARK_NOT_MUTABLE)(SEXP x) { MARK_NOT_MUTABLE(CHK(x)); }
int (MAYBE_SHARED)(SEXP x) { return MAYBE_SHARED(CHK(x)); }
int (NO_REFERENCES)(SEXP x) { return NO_REFERENCES(CHK(x)); }

// this is NOT a function version of the IS_SCALAR macro!
int (IS_SCALAR)(SEXP x, SEXPTYPE type)
{
    return TYPEOF(CHK(x)) == type && XLENGTH(x) == 1;
}

attribute_hidden int (R::MARK)(SEXP x) { CR_ASSERT(x); return MARK(CHK(x)); }
attribute_hidden
void (R::DECREMENT_REFCNT)(SEXP x) { DECREMENT_REFCNT(CHK(x)); }
attribute_hidden
void (R::INCREMENT_REFCNT)(SEXP x) { INCREMENT_REFCNT(CHK(x)); }
attribute_hidden
void (R::DISABLE_REFCNT)(SEXP x)  { CR_ASSERT(x); DISABLE_REFCNT(CHK(x)); }
attribute_hidden
void (R::ENABLE_REFCNT)(SEXP x) { CR_ASSERT(x); ENABLE_REFCNT(CHK(x)); }
attribute_hidden
bool (R::ASSIGNMENT_PENDING)(SEXP x) { CR_ASSERT(x); return ASSIGNMENT_PENDING(CHK(x)); }
attribute_hidden void (R::SET_ASSIGNMENT_PENDING)(SEXP x, int v)
{
    CR_ASSERT(x);
    SET_ASSIGNMENT_PENDING(CHK(x), v);
}
attribute_hidden
bool (R::IS_ASSIGNMENT_CALL)(SEXP x) { CR_ASSERT(x); return IS_ASSIGNMENT_CALL(CHK(x)); }
attribute_hidden
void (R::MARK_ASSIGNMENT_CALL)(SEXP x) { CR_ASSERT(x); MARK_ASSIGNMENT_CALL(CHK(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    CR_ASSERT(x);
    if (TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
	error(_("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'"),
	      R_typeToChar(v));

    CHECK_OLD_TO_NEW(x, v);
    x->m_attrib.retarget(x, v);
}
void (SET_OBJECT)(SEXP x, int v) { CR_ASSERT(x); SET_OBJECT(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v)
{
    CR_ASSERT(x);
#ifndef SWITCH_TO_REFCNT
    SET_NAMED(CHK(x), v);
#endif
}
attribute_hidden
void (R::SET_RTRACE)(SEXP x, int v) { CR_ASSERT(x); SET_RTRACE(CHK(x), v); }
void (SETLEVELS)(SEXP x, int v) { CR_ASSERT(x); SETLEVELS(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    to->setS4Object(IS_S4_OBJECT(from));
}
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), shallow_duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    to->setS4Object(IS_S4_OBJECT(from));
}
void CLEAR_ATTRIB(SEXP x)
{
    CR_ASSERT(x);
    SET_ATTRIB(CHK(x), R_NilValue);
    SET_OBJECT(x, 0);
    UNSET_S4_OBJECT(x);
}

NORET static void bad_SET_TYPEOF(SEXPTYPE from, SEXPTYPE to)
{
    error(_("can't change type from %s to %s"),
	  sexptype2char(from), sexptype2char(to));
}

static void check_SET_TYPEOF(SEXP x, SEXPTYPE v)
{
    CR_ASSERT(x);
    if (ALTREP(x))
	error(_("can't change the type of an ALTREP object from %s to %s"),
	      sexptype2char(TYPEOF(x)), sexptype2char(v));
    switch (TYPEOF(x)) {
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	if (BNDCELL_TAG(x))
	    error("%s", _("can't change the type of a binding cell"));
	switch (v) {
	case LISTSXP:
	case LANGSXP:
	case DOTSXP:
	case BCODESXP: return;
	default: bad_SET_TYPEOF(TYPEOF(x), v);
	}
    case INTSXP:
    case LGLSXP:
	switch (v) {
	case INTSXP:
	case LGLSXP: return;
	default: bad_SET_TYPEOF(TYPEOF(x), v);
	}
    case VECSXP:
    case EXPRSXP:
	switch (v) {
	case VECSXP:
	case EXPRSXP: return;
	default: bad_SET_TYPEOF(TYPEOF(x), v);
	}
    default: bad_SET_TYPEOF(TYPEOF(x), v);
    }
}

void (SET_TYPEOF)(SEXP x, SEXPTYPE v)
{
    /* Ideally this should not exist as a function outsie of base, but
       it was shown in WRE and is used in a good number of packages.
       So try to make it a little safer by only allowing some type
       changes.
    */
    CR_ASSERT(x);
    if (TYPEOF(CHK(x)) != v) {
	check_SET_TYPEOF(x, v);
	SET_TYPEOF(CHK(x), v);
    }
}

attribute_hidden
void (R::ALTREP_SET_TYPEOF)(SEXP x, SEXPTYPE v) { CR_ASSERT(x); SET_TYPEOF(CHK(x), v); }

void (R::ENSURE_NAMEDMAX)(SEXP x) { ENSURE_NAMEDMAX(CHK(x)); }
attribute_hidden void (R::ENSURE_NAMED)(SEXP x) { ENSURE_NAMED(CHK(x)); }
attribute_hidden
void (R::SETTER_CLEAR_NAMED)(SEXP x) { SETTER_CLEAR_NAMED(CHK(x)); }
attribute_hidden
void (R::RAISE_NAMED)(SEXP x, int n) { RAISE_NAMED(CHK(x), n); }

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ CR_ASSERT(x); SET_S4_OBJECT(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ CR_ASSERT(x); UNSET_S4_OBJECT(CHK(x)); }

/* JIT optimization support */
attribute_hidden bool (R::NOJIT)(SEXP x) { CR_ASSERT(x); return NOJIT(CHK(x)); }
attribute_hidden bool (R::MAYBEJIT)(SEXP x) { CR_ASSERT(x); return MAYBEJIT(CHK(x)); }
attribute_hidden void (R::SET_NOJIT)(SEXP x) { CR_ASSERT(x); SET_NOJIT(CHK(x)); }
attribute_hidden void (R::SET_MAYBEJIT)(SEXP x) { CR_ASSERT(x); SET_MAYBEJIT(CHK(x)); }
attribute_hidden void (R::UNSET_MAYBEJIT)(SEXP x) { CR_ASSERT(x); UNSET_MAYBEJIT(CHK(x)); }

/* Growable vector support */
int (IS_GROWABLE)(SEXP x) { CR_ASSERT(x); return IS_GROWABLE(CHK(x)); }
int (R::GROWABLE_BIT_SET)(SEXP x) { return GROWABLE_BIT_SET(CHK(x)); }
void (SET_GROWABLE_BIT)(SEXP x) { CR_ASSERT(x); SET_GROWABLE_BIT(CHK(x)); }

namespace
{
    std::map<SEXPTYPE, bool> not_a_vec{
        {NILSXP, true},      /* nil = NULL */
        {SYMSXP, true},      /* symbols */
        {LISTSXP, true},     /* lists of dotted pairs */
        {CLOSXP, true},      /* closures */
        {ENVSXP, true},      /* environments */
        {PROMSXP, true},     /* promises: [un]evaluated closure arguments */
        {LANGSXP, true},     /* language constructs (special lists) */
        {SPECIALSXP, true},  /* special forms */
        {BUILTINSXP, true},  /* builtin non-special forms */
        {CHARSXP, false},    /* "scalar" string type (internal only)*/
        {LGLSXP, false},     /* logical vectors */
        {INTSXP, false},     /* integer vectors */
        {REALSXP, false},    /* real variables */
        {CPLXSXP, false},    /* complex variables */
        {STRSXP, false},     /* string vectors */
        {DOTSXP, true},      /* dot-dot-dot object */
        {ANYSXP, true},      /* make "any" args work */
        {VECSXP, false},     /* generic vectors */
        {EXPRSXP, false},    /* expressions vectors */
        {BCODESXP, true},    /* byte code */
        {EXTPTRSXP, true},   /* external pointer */
        {WEAKREFSXP, true}, /* weak reference */
        {RAWSXP, false},     /* raw bytes */
        {OBJSXP, true},      /* S4 non-vector */
        {NEWSXP, true},      /* fresh node creaed in new page */
        {FREESXP, true},     /* node released by GC */
        {FUNSXP, true}       /* Closure or Builtin */
    };
} // anonymous namespace

static R_INLINE SEXP CHK2(SEXP x)
{
    x = CHK(x);
    if (not_a_vec[TYPEOF(x)])
	error(_("'%s' or similar applied to %s object"), "LENGTH", R_typeToChar(x));
    return x;
}

/* Vector Accessors */
int (LENGTH)(SEXP x) { return x == R_NilValue ? 0 : LENGTH(CHK2(x)); }
R_xlen_t (XLENGTH)(SEXP x) { return XLENGTH(CHK2(x)); }
R_xlen_t (TRUELENGTH)(SEXP x) { CR_ASSERT(x); return TRUELENGTH(CHK2(x)); }

void (SETLENGTH)(SEXP x, R_xlen_t v)
{
    CR_ASSERT(x);
    if (ALTREP(x))
	error("%s", _("SETLENGTH() cannot be applied to an ALTVEC object."));
    if (!isVector(x))
	error(_("SETLENGTH() can only be applied to a standard vector, not a '%s'"), R_typeToChar(x));
    SET_STDVEC_LENGTH(CHK2(x), v);
}

void (SET_TRUELENGTH)(SEXP x, R_xlen_t v) { CR_ASSERT(x); SET_TRUELENGTH(CHK2(x), v); }
int  (IS_LONG_VEC)(SEXP x) { CR_ASSERT(x); return IS_LONG_VEC(CHK2(x)); }
attribute_hidden
R_xlen_t (R::STDVEC_LENGTH)(SEXP x) { CR_ASSERT(x); return STDVEC_LENGTH(CHK2(x)); }
attribute_hidden
R_xlen_t (R::STDVEC_TRUELENGTH)(SEXP x) { CR_ASSERT(x); return STDVEC_TRUELENGTH(CHK2(x)); }
attribute_hidden void (R::SETALTREP)(SEXP x, int v) { CR_ASSERT(x); SETALTREP(x, v); }

/* temporary, to ease transition away from remapping */
R_xlen_t Rf_XLENGTH(SEXP x) { CR_ASSERT(x); return XLENGTH(CHK2(x)); }

const char *(R_CHAR)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "CHAR", "CHARSXP", R_typeToChar(x));
    return (const char *) CHAR(CHK(x));
}

SEXP (STRING_ELT)(SEXP x, R_xlen_t i) {
    CR_ASSERT(x);
    if (TYPEOF(x) != STRSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "STRING_ELT", "character vector", R_typeToChar(x));
    if (ALTREP(x))
	return CHK(ALTSTRING_ELT(CHK(x), i));
    else {
	SEXP *ps = (SEXP *) STDVEC_DATAPTR(CHK(x));
	return CHK(ps[i]);
    }
}

SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i) {
    if (TYPEOF(x) == EXPRSXP)
    {
        return XVECTOR_ELT(x, i);
    }
    /* We need to allow vector-like types here */
    if (TYPEOF(x) != VECSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "VECTOR_ELT", "list", R_typeToChar(x));
    if (ALTREP(x)) {
	SEXP ans = CHK(ALTLIST_ELT(CHK(x), i));
	/* the element is marked as not mutable since complex
	   assignment can't see reference counts on any intermediate
	   containers in an ALTREP */
	MARK_NOT_MUTABLE(ans);
        return ans;
    }
    else
        return CHK(VECTOR_ELT_0(CHK(x), i));
}

SEXP (XVECTOR_ELT)(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if (TYPEOF(x) != EXPRSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "XVECTOR_ELT", "expression", R_typeToChar(x));
    if (ALTREP(x)) {
	SEXP ans = CHK(ALTLIST_ELT(CHK(x), i));
	/* the element is marked as not mutable since complex
	   assignment can't see reference counts on any intermediate
	   containers in an ALTREP */
	MARK_NOT_MUTABLE(ans);
        return ans;
    }
    else
        return CHK(VECTOR_ELT_0(CHK(x), i));
}

#ifdef CATCH_ZERO_LENGTH_ACCESS
/* Attempts to read or write elements of a zero length vector will
   result in a segfault, rather than read and write random memory.
   Returning NULL would be more natural, but Matrix seems to assume
   that even zero-length vectors have non-NULL data pointers, so
   return (void *) 1 instead. Zero-length CHARSXP objects still have a
   trailing zero byte so they are not handled. */
# define CHKZLN(x, T) do {				   \
	CR_ASSERT(x);					   \
	VOID_CHK(x);					   \
	if (!ALTREP(x) && STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP) \
	    return (T *) 1;				   \
    } while (0)
#else
# define CHKZLN(x, T) do { } while (0)
#endif

void *(STDVEC_DATAPTR)(SEXP x)
{
    CR_ASSERT(x);
    if (ALTREP(x))
	error("%s", _("cannot get STDVEC_DATAPTR from ALTREP object"));
    if (!isVector(x))
	error(_("STDVEC_DATAPTR can only be applied to a vector, not a '%s'"),
	      R_typeToChar(x));
    CHKZLN(x, void);
    return STDVEC_DATAPTR(x);
}

/* nedded for implementing Dataptr ALTREP methods */
void *DATAPTR_RW(SEXP x) { return DATAPTR(x); }

int *(LOGICAL)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != LGLSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "LOGICAL",  "logical", R_typeToChar(x));
    CHKZLN(x, int);
    return LOGICAL(x);
}

const int *(LOGICAL_RO)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != LGLSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "LOGICAL",  "logical", R_typeToChar(x));
    CHKZLN(x, const int);
    return LOGICAL_RO(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *(INTEGER)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "INTEGER", "integer", R_typeToChar(x));
    CHKZLN(x, int);
    return INTEGER(x);
}

const int *(INTEGER_RO)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "INTEGER", "integer", R_typeToChar(x));
    CHKZLN(x, const int);
    return INTEGER_RO(x);
}

Rbyte *(RAW)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != RAWSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "RAW", "raw", R_typeToChar(x));
    CHKZLN(x, Rbyte);
    return RAW(x);
}

const Rbyte *(RAW_RO)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != RAWSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "RAW", "raw", R_typeToChar(x));
    CHKZLN(x, const Rbyte);
    return RAW(x);
}

double *(REAL)(SEXP x) {
    if (TYPEOF(x) != REALSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "REAL", "numeric", R_typeToChar(x));
    CHKZLN(x, double);
    return REAL(x);
}

const double *(REAL_RO)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != REALSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "REAL", "numeric", R_typeToChar(x));
    CHKZLN(x, const double);
    return REAL_RO(x);
}

Rcomplex *(COMPLEX)(SEXP x) {
    CR_ASSERT(x);
    if (TYPEOF(x) != CPLXSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "COMPLEX", "complex", R_typeToChar(x));
    CHKZLN(x, Rcomplex);
    return COMPLEX(x);
}
namespace CXXR {
Complex *(CXXR_COMPLEX)(SEXP x) {
    if (TYPEOF(x) != CPLXSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "CXXR_COMPLEX", "complex", R_typeToChar(x));
    CHKZLN(x, Complex);
    return CXXR_COMPLEX(x);
}
} // namespace CXXR
const Rcomplex *(COMPLEX_RO)(SEXP x) {
    if (TYPEOF(x) != CPLXSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "COMPLEX", "complex", R_typeToChar(x));
    CHKZLN(x, const Rcomplex);
    return COMPLEX_RO(x);
}

SEXP *(STRING_PTR)(SEXP x) {
    if (TYPEOF(x) != STRSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "STRING_PTR", "character", R_typeToChar(x));
    CHKZLN(x, SEXP);
    return STRING_PTR(x);
}

const SEXP *(STRING_PTR_RO)(SEXP x) {
    if (TYPEOF(x) != STRSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      __func__, "character", R_typeToChar(x));
    CHKZLN(x, const SEXP);
    return STRING_PTR_RO(x);
}

NORET SEXP * (VECTOR_PTR)(SEXP x)
{
  error("%s", _("not safe to return vector pointer"));
}

const SEXP *(VECTOR_PTR_RO)(SEXP x) {
    if (TYPEOF(x) != VECSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      __func__, "list", R_typeToChar(x));
    CHKZLN(x, const SEXP);
    return VECTOR_PTR_RO(x);
}

void (SET_STRING_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    CR_ASSERT(x);
    if (TYPEOF(CHK(x)) != STRSXP)
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "SET_STRING_ELT", "character vector", R_typeToChar(x));
    if (TYPEOF(CHK(v)) != CHARSXP)
       error(_("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'"),
	     R_typeToChar(v));
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lld/%lld in SET_STRING_ELT"),
	      (long long)i, (long long)XLENGTH(x));
    CHECK_OLD_TO_NEW(x, v);
    if (ALTREP(x))
	ALTSTRING_SET_ELT(x, i, v);
    else {
	SEXP *ps = (SEXP *) STDVEC_DATAPTR(x);
	FIX_REFCNT(x, ps[i], v);
	ps[i] = v;
    }
}

SEXP (SET_VECTOR_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    if (TYPEOF(x) == EXPRSXP)
    {
        return SET_XVECTOR_ELT(x, i, v);
    }
    /*  we need to allow vector-like types here */
    if (TYPEOF(x) != VECSXP) {
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "SET_VECTOR_ELT", "list", R_typeToChar(x));
    }
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lld/%lld in SET_VECTOR_ELT"),
	      (long long)i, (long long)XLENGTH(x));
    if (ALTREP(x))
        ALTLIST_SET_ELT(x, i, v);
    else {
        FIX_REFCNT(x, VECTOR_ELT_0(x, i), v);
        CHECK_OLD_TO_NEW(x, v);
        SET_VECTOR_ELT_0(x, i, v);
    }
    return v;
}

SEXP (SET_XVECTOR_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    /*  we need to allow vector-like types here */
    if (TYPEOF(x) != EXPRSXP) {
	error(_("%s() can only be applied to a '%s', not a '%s'"),
	      "SET_XVECTOR_ELT", "list", R_typeToChar(x));
    }
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lld/%lld in SET_XVECTOR_ELT"),
	      (long long)i, (long long)XLENGTH(x));
    if (ALTREP(x))
        ALTLIST_SET_ELT(x, i, v);
    else {
        FIX_REFCNT(x, VECTOR_ELT_0(x, i), v);
        CHECK_OLD_TO_NEW(x, v);
        SET_VECTOR_ELT_0(x, i, v);
    }
    return v;
}

/* check for a CONS-like object */
#ifdef TESTING_WRITE_BARRIER
static R_INLINE SEXP CHKCONS(SEXP e)
{
    if (ALTREP(e))
	return CHK(e);
    switch (TYPEOF(e)) {
    case LISTSXP:
    case LANGSXP:
    case NILSXP:
    case DOTSXP:
    case CLOSXP:    /**** use separate accessors? */
    case BCODESXP:  /**** use separate accessors? */
    case ENVSXP:    /**** use separate accessors? */
    case PROMSXP:   /**** use separate accessors? */
    case EXTPTRSXP: /**** use separate accessors? */
	return CHK(e);
    default:
	error(_("'%s' or similar applied to %s object"), "CAR/CDR/TAG",
	      R_typeToChar(e));
    }
}
#else
#define CHKCONS(e) CHK(e)
#endif

attribute_hidden
SEXPTYPE (R::BNDCELL_TAG)(SEXP cell) { return BNDCELL_TAG(cell); }
attribute_hidden
void (R::SET_BNDCELL_TAG)(SEXP cell, SEXPTYPE val) { CR_ASSERT(cell); SET_BNDCELL_TAG(cell, val); }
attribute_hidden
double (R::BNDCELL_DVAL)(SEXP cell) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); return BNDCELL_DVAL(cell); }
attribute_hidden
int (R::BNDCELL_IVAL)(SEXP cell) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); return BNDCELL_IVAL(cell); }
attribute_hidden
int (R::BNDCELL_LVAL)(SEXP cell) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); return BNDCELL_LVAL(cell); }
attribute_hidden
void (R::SET_BNDCELL_DVAL)(SEXP cell, double v) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); SET_BNDCELL_DVAL(cell, v); }
attribute_hidden
void (R::SET_BNDCELL_IVAL)(SEXP cell, int v) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); SET_BNDCELL_IVAL(cell, v); }
attribute_hidden
void (R::SET_BNDCELL_LVAL)(SEXP cell, int v) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); SET_BNDCELL_LVAL(cell, v); }
attribute_hidden
void (R::INIT_BNDCELL)(SEXP cell, SEXPTYPE type) { CR_ASSERT(cell); CR_CONSCELL_ASSERT(cell); INIT_BNDCELL(cell, type); }
attribute_hidden
SEXPTYPE (R::PROMISE_TAG)(SEXP cell) { CR_ASSERT(cell); return PROMISE_TAG(cell); }
attribute_hidden
void (R::SET_PROMISE_TAG)(SEXP cell, SEXPTYPE val) { CR_ASSERT(cell); SET_PROMISE_TAG(cell, val); }

#define CLEAR_BNDCELL_TAG(cell) do {		\
	if (BNDCELL_TAG(cell)) {		\
	    CR_CONSCELL_ASSERT(cell);		\
	    static_cast<ConsCell *>(cell)->clearCar();		\
	    static_cast<ConsCell *>(cell)->setCar(R_NilValue);	\
	    SET_BNDCELL_TAG(cell, NILSXP);	\
	}					\
    } while (0)

attribute_hidden
void R::SET_BNDCELL(SEXP cell, SEXP val)
{
    CR_ASSERT(cell);
    CLEAR_BNDCELL_TAG(cell);
    SETCAR(cell, val);
}

attribute_hidden void R::R_expand_binding_value(SEXP b)
{
#if BOXED_BINDING_CELLS
    SET_BNDCELL_TAG(b, NILSXP);
#else
    GCManager::GCInhibitor no_gc;
    SEXPTYPE typetag = BNDCELL_TAG(b);
    if (typetag) {
	union {
	    SEXP sxpval;
	    double dval;
	    int ival;
	    int lval;
	} vv;
	SEXP val;
	vv.sxpval = CAR0(b);
	switch (typetag) {
	case REALSXP:
	    PROTECT(b);
	    val = ScalarReal(vv.dval);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(b);
	    val = ScalarInteger(vv.ival);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    UNPROTECT(1);
	    break;
	case LGLSXP:
	    PROTECT(b);
	    val = ScalarLogical(vv.lval);
	    SET_BNDCELL(b, val);
	    INCREMENT_NAMED(val);
	    UNPROTECT(1);
	    break;
	default:
	    break;
	}
    }
#endif
}

#ifdef IMMEDIATE_PROMISE_VALUES
attribute_hidden SEXP R::R_expand_promise_value(SEXP x)
{
    if (PROMISE_TAG(x))
	R_expand_binding_value(x);
    return PRVALUE0(x);
}
#endif

attribute_hidden void R::R_args_enable_refcnt(SEXP args)
{
#ifdef SWITCH_TO_REFCNT
    /* args is escaping into user C code and might get captured, so
       make sure it is reference counting. Should be able to get rid
       of this function if we reduce use of CONS_NR. */
    for (SEXP a = args; a != R_NilValue; a = CDR(a))
	if (!REFCNT_ENABLED(a)) {
	    ENABLE_REFCNT(a);
	    INCREMENT_REFCNT(CAR(a));
	    INCREMENT_REFCNT(CDR(a));
#ifdef TESTING_WRITE_BARRIER
	    /* this should not see non-tracking arguments */
	    if ((CAR(a) != R_NilValue) && !REFCNT_ENABLED(CAR(a)))
		error("%s", _("argument not tracking references"));
#endif
	}
#endif
}

attribute_hidden void R::R_try_clear_args_refcnt(SEXP args)
{
#ifdef SWITCH_TO_REFCNT
    /* If args excapes properly its reference count will have been
       incremented. If it has no references, then it can be reverted
       to NR and the reference counts on its CAR and CDR can be
       decremented. */
    while (args != R_NilValue && NO_REFERENCES(args)) {
	SEXP next = CDR(args);
	DECREMENT_REFCNT(CAR(args));
	DECREMENT_REFCNT(CDR(args));
	DISABLE_REFCNT(args);
	args = next;
    }
#endif
}

/* WeakRef Object Accessors */
void R::SET_WEAKREF_KEY(SEXP x, SEXP v) { CR_ASSERT(x); CR_WEAKREF_ASSERT(x); CHECK_OLD_TO_NEW(x, v); WEAKREF_KEY(x).retarget(x, v); }
void R::SET_WEAKREF_VALUE(SEXP x, SEXP v) { CR_ASSERT(x); CR_WEAKREF_ASSERT(x); CHECK_OLD_TO_NEW(x, v); WEAKREF_VALUE(x).retarget(x, v); }
void R::SET_WEAKREF_FINALIZER(SEXP x, SEXP v) { CR_ASSERT(x); CR_WEAKREF_ASSERT(x); CHECK_OLD_TO_NEW(x, v); WEAKREF_FINALIZER(x).retarget(x, v); }

/* S4 Object Accessors */
SEXP (S4TAG)(SEXP e) { CR_ASSERT(e); CR_S4OBJECT_ASSERT(e); return CHK(S4TAG(CHKCONS(e))); }
void SET_S4TAG(SEXP x, SEXP v) { CR_ASSERT(x); CR_S4OBJECT_ASSERT(x); CHECK_OLD_TO_NEW(x, v); S4TAG(x).retarget(x, v); }

/* AltRep Accessors */
SEXP (DATA1)(SEXP e) { CR_ASSERT(e); CR_ALTREP_ASSERT(e); return CHK(DATA1(CHKCONS(e))); }
SEXP (DATA2)(SEXP e) { CR_ASSERT(e); CR_ALTREP_ASSERT(e); return CHK(DATA2(CHKCONS(e))); }
SEXP (CLASS)(SEXP e) { CR_ASSERT(e); CR_ALTREP_ASSERT(e); return CHK(CLASS(CHKCONS(e))); }
void (SET_DATA1)(SEXP x, SEXP v) { CR_ASSERT(x); CR_ALTREP_ASSERT(x); CHECK_OLD_TO_NEW(x, v); DATA1(x).retarget(x, v); }
void (SET_DATA2)(SEXP x, SEXP v) { CR_ASSERT(x); CR_ALTREP_ASSERT(x); CHECK_OLD_TO_NEW(x, v); DATA2(x).retarget(x, v); }
void (SET_CLASS)(SEXP x, SEXP v) { CR_ASSERT(x); CR_ALTREP_ASSERT(x); CHECK_OLD_TO_NEW(x, v); CLASS(x).retarget(x, v); }

/* ByteCode Accessors */
SEXP (CODE0)(SEXP e) { CR_ASSERT(e); CR_BYTECODE_ASSERT(e); return CHK(CODE0(CHKCONS(e))); }
SEXP (CONSTS)(SEXP e) { CR_ASSERT(e); CR_BYTECODE_ASSERT(e); return CHK(CONSTS(CHKCONS(e))); }
SEXP (EXPR)(SEXP e) { CR_ASSERT(e); CR_BYTECODE_ASSERT(e); return CHK(EXPR(CHKCONS(e))); }
void (SET_CODE)(SEXP x, SEXP v) { CR_ASSERT(x); CR_BYTECODE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); CODE0(x).retarget(x, v); }
void (SET_CONSTS)(SEXP x, SEXP v) { CR_ASSERT(x); CR_BYTECODE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); CONSTS(x).retarget(x, v); }
void (SET_EXPR)(SEXP x, SEXP v) { CR_ASSERT(x); CR_BYTECODE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); EXPR(x).retarget(x, v); }

/* List Accessors */
SEXP (TAG)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(TAG(CHKCONS(e))); }
attribute_hidden SEXP (R::CAR0)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CAR0(CHKCONS(e))); }
SEXP (CDR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CDR(CHKCONS(e))); }
SEXP (CAAR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CAAR(CHKCONS(e))); }
SEXP (CDAR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CDAR(CHKCONS(e))); }
SEXP (CADR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CADR(CHKCONS(e))); }
SEXP (CDDR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CDDR(CHKCONS(e))); }
SEXP (CDDDR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CDDDR(CHKCONS(e))); }
SEXP (CD4R)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CD4R(CHKCONS(e))); }
SEXP (CADDR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CADDR(CHKCONS(e))); }
SEXP (CADDDR)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CADDDR(CHKCONS(e))); }
SEXP (CAD3R)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CADDDR(CHKCONS(e))); }
SEXP (CAD4R)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CAD4R(CHKCONS(e))); }
SEXP (CAD5R)(SEXP e) { CR_CONSCELL_ASSERT(e); return CHK(CAD5R(CHKCONS(e))); }
attribute_hidden int (R::MISSING)(SEXP x) { CR_ASSERT(x); return MISSING(CHKCONS(x)); }

void (SET_TAG)(SEXP x, SEXP v)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));

    CR_CONSCELL_ASSERT(x);
    CHECK_OLD_TO_NEW(x, v);
    static_cast<ConsCell *>(x)->setTag(v);
}

SEXP (SETCAR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));

    CR_CONSCELL_ASSERT(x);
    static_cast<PairList *>(x)->setCar(y);
    CHECK_OLD_TO_NEW(x, y);

    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));

#ifdef TESTING_WRITE_BARRIER
    /* this should not add a non-tracking CDR to a tracking cell */
    if (REFCNT_ENABLED(x) && y && !REFCNT_ENABLED(y))
	error("%s", _("inserting non-tracking CDR in tracking cell"));
#endif
    CR_CONSCELL_ASSERT(x);
    CHECK_OLD_TO_NEW(x, y);
    static_cast<ConsCell *>(x)->setTail(y);
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue)
	error("%s", _("bad value"));
    CR_CONSCELL_ASSERT(x);
    SEXP cell = CDR(x);

    static_cast<PairList *>(cell)->setCar(y);
    CHECK_OLD_TO_NEW(cell, y);

    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHKCONS(CDDR(x)) == NULL || CDDR(x) == R_NilValue)
	error("%s", _("bad value"));
    CR_CONSCELL_ASSERT(x);
    SEXP cell = CDDR(x);

    static_cast<PairList *>(cell)->setCar(y);
    CHECK_OLD_TO_NEW(cell, y);

    return y;
}

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHKCONS(CDDR(x)) == NULL || CDDR(x) == R_NilValue ||
	CHKCONS(CDDDR(x)) == NULL || CDDDR(x) == R_NilValue)
	error("%s", _("bad value"));
    CR_CONSCELL_ASSERT(x);
    SEXP cell = CDDDR(x);

    static_cast<PairList *>(cell)->setCar(y);
    CHECK_OLD_TO_NEW(cell, y);

    return y;
}

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHKCONS(CDDR(x)) == NULL || CDDR(x) == R_NilValue ||
	CHKCONS(CDDDR(x)) == NULL || CDDDR(x) == R_NilValue ||
	CHKCONS(CD4R(x)) == NULL || CD4R(x) == R_NilValue)
	error("%s", _("bad value"));
    CR_CONSCELL_ASSERT(x);
    SEXP cell = CD4R(x);

    static_cast<PairList *>(cell)->setCar(y);
    CHECK_OLD_TO_NEW(cell, y);

    return y;
}

/* ExternalPointer Accessors */
SEXP (EXTPTR_PROT)(SEXP x) { CR_ASSERT(x); CHKEXTPTRSXP(x); return EXTPTR_PROT(CHK(x)); }
SEXP (EXTPTR_TAG)(SEXP x) { CR_ASSERT(x); CHKEXTPTRSXP(x); return EXTPTR_TAG(CHK(x)); }
void *(EXTPTR_PTR)(SEXP x) { CR_ASSERT(x); CHKEXTPTRSXP(x); return EXTPTR_PTR(CHK(x)); }

attribute_hidden
void (R::SET_MISSING)(SEXP x, unsigned int v) { CR_ASSERT(x); SET_MISSING(CHKCONS(x), v); }

/* Closure Accessors */
/* some internals seem to depend on allowing a LISTSXP */
#define CHKCLOSXP(x) \
    if (TYPEOF(x) != CLOSXP && TYPEOF(x) != LISTSXP) \
	error(_("%s: argument of type %s is not a closure"), \
	      __func__, sexptype2char(TYPEOF(x)))
SEXP (FORMALS)(SEXP x) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHKCLOSXP(x); return CHK(FORMALS(CHK(x))); }
SEXP (BODY)(SEXP x) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHKCLOSXP(x); return CHK(BODY(CHK(x))); }
SEXP (CLOENV)(SEXP x) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHKCLOSXP(x); return CHK(CLOENV(CHK(x))); }
int (RDEBUG)(SEXP x) { CR_ASSERT(x); return RDEBUG(CHK(x)); }
attribute_hidden int (R::RSTEP)(SEXP x) { CR_ASSERT(x); return RSTEP(CHK(x)); }
SEXP R_ClosureFormals(SEXP x) { CR_ASSERT(x); return (FORMALS)(x); }
SEXP R_ClosureBody(SEXP x) { CR_ASSERT(x); return (BODY)(x); }
SEXP R_ClosureEnv(SEXP x) { CR_ASSERT(x); return (CLOENV)(x); }

void (SET_FORMALS)(SEXP x, SEXP v) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); FORMALS(x).retarget(x, v); }
void (SET_BODY)(SEXP x, SEXP v) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); BODY(x).retarget(x, v); }
void (SET_CLOENV)(SEXP x, SEXP v) { CR_ASSERT(x); CR_CLOSURE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); CLOENV(x).retarget(x, v); }
void (SET_RDEBUG)(SEXP x, int v) { CR_ASSERT(x); SET_RDEBUG(CHK(x), v); }
attribute_hidden
void (R::SET_RSTEP)(SEXP x, int v) { CR_ASSERT(x); SET_RSTEP(CHK(x), v); }

/* These are only needed with the write barrier on */
namespace R
{
/* Primitive Accessors */
/* not hidden since needed in some base packages */
int (PRIMOFFSET)(SEXP x) { CR_ASSERT(x); CR_BUILTIN_ASSERT(x); return PRIMOFFSET(CHK(x)); }
attribute_hidden
void (SET_PRIMOFFSET)(SEXP x, int v) { CR_ASSERT(x); CR_BUILTIN_ASSERT(x); SET_PRIMOFFSET(CHK(x), v); }
} // namespace R

/* Symbol Accessors */
/* looks like R_NilValue is also being passed to tome of these */
#define CHKSYMSXP(x) \
    if (x != R_NilValue && TYPEOF(x) != SYMSXP) \
	error(_("%s: argument of type %s is not a symbol or NULL"), \
	      __func__, sexptype2char(TYPEOF(x)))
SEXP (PRINTNAME)(SEXP x) { CHKSYMSXP(x); CR_SYMBOL_ASSERT(x); return CHK(PRINTNAME(CHK(x))); }
SEXP (R::SYMVALUE)(SEXP x) { CR_ASSERT(x); CR_SYMBOL_ASSERT(x); CHKSYMSXP(x); return CHK(SYMVALUE(CHK(x))); }
SEXP (R::INTERNAL)(SEXP x) { CR_ASSERT(x); CR_SYMBOL_ASSERT(x); CHKSYMSXP(x); return CHK(INTERNAL(CHK(x))); }
int (R::DDVAL)(SEXP x) { CR_ASSERT(x); CHKSYMSXP(x); return DDVAL(CHK(x)); }

attribute_hidden
void (R::SET_PRINTNAME)(SEXP x, SEXP v) { CR_ASSERT(x); CR_SYMBOL_ASSERT(x); CHECK_OLD_TO_NEW(x, v); x->u.symsxp.m_pname.retarget(x, v); }

attribute_hidden
void (R::SET_SYMVALUE)(SEXP x, SEXP v)
{
    CR_ASSERT(x);
    CR_SYMBOL_ASSERT(x);
    if (SYMVALUE(x) == v)
	return;

    CHECK_OLD_TO_NEW(x, v);
    (x)->u.symsxp.m_value.retarget2(x, v);
}

attribute_hidden
void (R::SET_INTERNAL)(SEXP x, SEXP v) {

    CR_ASSERT(x);
    CR_SYMBOL_ASSERT(x);
    CHECK_OLD_TO_NEW(x, v);
    INTERNAL(x).retarget(x, v);
}
attribute_hidden void (R::SET_DDVAL)(SEXP x, int v) { CR_ASSERT(x); SET_DDVAL(CHK(x), v); }

/* Environment Accessors */
/* looks like R_NilValue is still showing up in internals */
#define CHKENVSXP(x)						\
    if (x != R_NilValue && TYPEOF(x) != ENVSXP)				\
	error(_("%s: argument of type %s is not an environment or NULL"), \
	      __func__, sexptype2char(TYPEOF(x)))
SEXP (FRAME)(SEXP x) { CR_ENVIRONMENT_ASSERT(x); CHKENVSXP(x); return CHK(FRAME(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { CR_ASSERT(x); CR_ENVIRONMENT_ASSERT(x); CHKENVSXP(x); return CHK(ENCLOS(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { CR_ENVIRONMENT_ASSERT(x); CHKENVSXP(x); return CHK(HASHTAB(CHK(x))); }
int (ENVFLAGS)(SEXP x) { CR_ASSERT(x); CHKENVSXP(x); return ENVFLAGS(CHK(x)); }
SEXP R_ParentEnv(SEXP x) { CR_ASSERT(x); return (ENCLOS)(x); }
int (ENV_RDEBUG)(SEXP x) { CR_ASSERT(x); return ENV_RDEBUG(CHK(x)); }
void (SET_ENV_RDEBUG)(SEXP x, int v) { CR_ASSERT(x); SET_ENV_RDEBUG(CHK(x), v); }
void (SET_FRAME)(SEXP x, SEXP v) { CR_ASSERT(x); CR_ENVIRONMENT_ASSERT(x); CHECK_OLD_TO_NEW(x, v); x->u.envsxp.m_frame.retarget(x, v); }

void (SET_ENCLOS)(SEXP x, SEXP v)
{
    CR_ASSERT(x);
    CR_ENVIRONMENT_ASSERT(x);
    if (v == R_NilValue)
	/* mainly to handle unserializing old files */
	v = R_EmptyEnv;
    if (TYPEOF(v) != ENVSXP)
	error(_("'%s' is not an environment"), "parent");
    for (SEXP e = v; e != R_NilValue; e = ENCLOS(e))
	if (e == x)
	    error("%s", _("cycles in parent chains are not allowed"));

    CHECK_OLD_TO_NEW(x, v);
    ENCLOS(x).retarget(x, v);
}

void (SET_HASHTAB)(SEXP x, SEXP v) { CR_ASSERT(x); CR_ENVIRONMENT_ASSERT(x); CHECK_OLD_TO_NEW(x, v); x->u.envsxp.m_hashtab.retarget(x, v); }
void (SET_ENVFLAGS)(SEXP x, int v) { CR_ASSERT(x); SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { CR_ASSERT(x); CR_PROMISE_ASSERT(x); return CHK(PRCODE(CHK(x))); }
SEXP (PRENV)(SEXP x) { CR_ASSERT(x); CR_PROMISE_ASSERT(x); return CHK(PRENV(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { CR_ASSERT(x); CR_PROMISE_ASSERT(x); return CHK(static_cast<Promise *>(CHK(x))->value()); }
int (R::PRSEEN)(SEXP x) { CR_ASSERT(x); return PRSEEN(CHK(x)); }
attribute_hidden
bool (R::PROMISE_IS_EVALUATED)(SEXP x)
{
    CR_ASSERT(x);
    CR_PROMISE_ASSERT(x); 
    x = CHK(x);
    return static_cast<Promise *>(x)->evaluated();
}

void (SET_PRENV)(SEXP x, SEXP v){ CR_ASSERT(x); CR_PROMISE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); PRENV(x).retarget(x, v); }
void (SET_PRCODE)(SEXP x, SEXP v) { CR_ASSERT(x); CR_PROMISE_ASSERT(x); CHECK_OLD_TO_NEW(x, v); PRCODE(x).retarget(x, v); }
void (R::SET_PRSEEN)(SEXP x, int v) { CR_ASSERT(x); SET_PRSEEN(CHK(x), v); }

void (SET_PRVALUE)(SEXP x, SEXP v)
{
    CR_ASSERT(x);
    CR_PROMISE_ASSERT(x); 
    if (TYPEOF(x) != PROMSXP)
	error(_("expecting a 'PROMSXP', not a '%s'"), R_typeToChar(x));

    static_cast<Promise *>(x)->setValue(v);
    CHECK_OLD_TO_NEW(x, v);
}

attribute_hidden
void R::IF_PROMSXP_SET_PRVALUE(SEXP x, SEXP v)
{
    CR_ASSERT(x);
    CR_PROMISE_ASSERT(x); 
    /* promiseArgs produces a list containing promises or R_MissingArg.
       Using IF_PROMSXP_SET_PRVALUE avoids corrupting R_MissingArg. */
    if (TYPEOF(x) == PROMSXP)
        SET_PRVALUE(x, v);
}

/* Hashing Accessors */
attribute_hidden
bool (R::HASHASH)(SEXP x) { CR_ASSERT(x); return HASHASH(CHK(x)); }
attribute_hidden
int (R::HASHVALUE)(SEXP x) { CR_ASSERT(x); return HASHVALUE(CHK(x)); }

attribute_hidden
void (R::SET_HASHASH)(SEXP x, int v) { CR_ASSERT(x); SET_HASHASH(CHK(x), v); }
attribute_hidden
void (R::SET_HASHVALUE)(SEXP x, int v) { CR_ASSERT(x); SET_HASHVALUE(CHK(x), v); }

attribute_hidden
SEXP (R::SET_CXTAIL)(SEXP x, SEXP v) {
    CR_ASSERT(x);
#ifdef USE_TYPE_CHECKING
    if (TYPEOF(v) != CHARSXP && TYPEOF(v) != NILSXP)
	error(_("value of 'SET_CXTAIL' must be a char or NULL, not a '%s'"),
	      R_typeToChar(v));
#endif
    /*CHECK_OLD_TO_NEW(x, v); *//* not needed since not properly traced */
    x->m_attrib = v;
    return x;
}

/* Test functions */
Rboolean Rf_isNull(SEXP s) { return (Rboolean) isNull(CHK(s)); }
Rboolean Rf_isSymbol(SEXP s) { return (Rboolean) isSymbol(CHK(s)); }
Rboolean Rf_isLogical(SEXP s) { return (Rboolean) isLogical(CHK(s)); }
Rboolean Rf_isReal(SEXP s) { return (Rboolean) isReal(CHK(s)); }
Rboolean Rf_isComplex(SEXP s) { return (Rboolean) isComplex(CHK(s)); }
Rboolean Rf_isRaw(SEXP s) { return (Rboolean) isRaw(CHK(s)); }
Rboolean Rf_isExpression(SEXP s) { return (Rboolean) isExpression(CHK(s)); }
Rboolean Rf_isEnvironment(SEXP s) { return (Rboolean) isEnvironment(CHK(s)); }
Rboolean Rf_isString(SEXP s) { return (Rboolean) isString(CHK(s)); }
Rboolean Rf_isObject(SEXP s) { return (Rboolean) isObject(CHK(s)); }
namespace R {
/* Bindings accessors */
attribute_hidden bool (IS_ACTIVE_BINDING)(SEXP b) { CR_ASSERT(b); return IS_ACTIVE_BINDING(CHK(b));}
attribute_hidden bool (BINDING_IS_LOCKED)(SEXP b) {return BINDING_IS_LOCKED(CHK(b));}
attribute_hidden void (SET_ACTIVE_BINDING_BIT)(SEXP b) { CR_ASSERT(b); SET_ACTIVE_BINDING_BIT(CHK(b));}
attribute_hidden void (LOCK_BINDING)(SEXP b) { CR_ASSERT(b); LOCK_BINDING(CHK(b));}
attribute_hidden void (UNLOCK_BINDING)(SEXP b) { CR_ASSERT(b); UNLOCK_BINDING(CHK(b));}

attribute_hidden
void (SET_BASE_SYM_CACHED)(SEXP b) { CR_ASSERT(b); SET_BASE_SYM_CACHED(CHK(b)); }
attribute_hidden
void (UNSET_BASE_SYM_CACHED)(SEXP b) { CR_ASSERT(b); UNSET_BASE_SYM_CACHED(CHK(b)); }
attribute_hidden
bool (BASE_SYM_CACHED)(SEXP b) { CR_ASSERT(b); return BASE_SYM_CACHED(CHK(b)); }

attribute_hidden
void (SET_SPECIAL_SYMBOL)(SEXP b) { CR_ASSERT(b); SET_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
void (UNSET_SPECIAL_SYMBOL)(SEXP b) { CR_ASSERT(b); UNSET_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
bool (IS_SPECIAL_SYMBOL)(SEXP b) { CR_ASSERT(b); return IS_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b) { CR_ASSERT(b); SET_NO_SPECIAL_SYMBOLS(CHK(b)); }
attribute_hidden
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b) { CR_ASSERT(b); UNSET_NO_SPECIAL_SYMBOLS(CHK(b)); }
attribute_hidden
bool (NO_SPECIAL_SYMBOLS)(SEXP b) { CR_ASSERT(b); return NO_SPECIAL_SYMBOLS(CHK(b)); }
} // namespace R
/* R_FunTab accessors, only needed when write barrier is on */
/* Might want to not hide for experimentation without rebuilding R - LT */
attribute_hidden int (PRIMVAL)(SEXP x) { CR_ASSERT(x); return PRIMVAL(CHK(x)); }
attribute_hidden CCODE (PRIMFUN)(SEXP x) { CR_ASSERT(x); return PRIMFUN(CHK(x)); }
attribute_hidden void (SET_PRIMFUN)(SEXP x, CCODE f) { CR_ASSERT(x); PRIMFUN(CHK(x)) = f; }

/* for use when testing the write barrier */
namespace R {
attribute_hidden bool (IS_NATIVE)(SEXP x) { CR_ASSERT(x); return IS_NATIVE(CHK(x)); }
attribute_hidden bool (IS_BYTES)(SEXP x) { CR_ASSERT(x); return IS_BYTES(CHK(x)); }
attribute_hidden bool (IS_LATIN1)(SEXP x) { CR_ASSERT(x); return IS_LATIN1(CHK(x)); }
/* Next two are used in package utils */
bool (IS_ASCII)(SEXP x) { CR_ASSERT(x); return IS_ASCII(CHK(x)); }
bool (IS_UTF8)(SEXP x) { CR_ASSERT(x); return IS_UTF8(CHK(x)); }
attribute_hidden void (SET_BYTES)(SEXP x) { CR_ASSERT(x); SET_BYTES(CHK(x)); }
attribute_hidden void (SET_LATIN1)(SEXP x) { CR_ASSERT(x); SET_LATIN1(CHK(x)); }
attribute_hidden void (SET_UTF8)(SEXP x) { CR_ASSERT(x); SET_UTF8(CHK(x)); }
attribute_hidden void (SET_ASCII)(SEXP x) { CR_ASSERT(x); SET_ASCII(CHK(x)); }
/*attribute_hidden*/ int (ENC_KNOWN)(SEXP x) { CR_ASSERT(x); return ENC_KNOWN(CHK(x)); }
attribute_hidden void (SET_CACHED)(SEXP x) { CR_ASSERT(x); SET_CACHED(CHK(x)); }
/*attribute_hidden*/ bool (IS_CACHED)(SEXP x) { CR_ASSERT(x); return IS_CACHED(CHK(x)); }
} // namespace R
/*********************************************/
/* Non-sampling memory use profiler
   reports all large vector heap allocations */
/*********************************************/

#ifndef R_MEMORY_PROFILING

NORET SEXP do_Rprofmem(SEXP args)
{
    error("%s", _("memory profiling is not available on this system"));
}

#else
static FILE *R_MemReportingOutfile;

static void R_OutputStackTrace(FILE *file)
{
    for (RCNTXT *cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    fprintf(file, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
}

static void R_ReportAllocation(R_size_t size)
{
    fprintf(R_MemReportingOutfile, "%lu :", (unsigned long) size);
    R_OutputStackTrace(R_MemReportingOutfile);
    fprintf(R_MemReportingOutfile, "\n");
}

static void R_EndMemReporting(void)
{
    if (R_MemReportingOutfile != NULL) {
	/* does not fclose always flush? */
	fflush(R_MemReportingOutfile);
	fclose(R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    MemoryBank::setMonitor(nullptr);
}

static void R_InitMemReporting(SEXP filename, bool append,
			       R_size_t threshold)
{
    if (R_MemReportingOutfile != NULL) R_EndMemReporting();
    R_MemReportingOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_MemReportingOutfile == NULL)
	error(_("Rprofmem: cannot open output file '%s'"),
	      translateChar(filename));
    MemoryBank::setMonitor(R_ReportAllocation, threshold);
}

SEXP do_Rprofmem(SEXP args)
{
    SEXP filename;
    R_size_t threshold = 0;

    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	error(_("invalid '%s' argument"), "filename");
    bool append_mode = asLogical(CADR(args));
    filename = STRING_ELT(CAR(args), 0);
    double tdbl = REAL(CADDR(args))[0];
    if (tdbl > 0) {
	if (tdbl >= (double) R_SIZE_T_MAX)
	    threshold = R_SIZE_T_MAX;
	else
	    threshold = (R_size_t) tdbl;
    }
    if (strlen(CHAR(filename)))
	R_InitMemReporting(filename, append_mode, threshold);
    else
	R_EndMemReporting();
    return R_NilValue;
}

#endif /* R_MEMORY_PROFILING */

/* RBufferUtils, moved from deparse.c */

void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, this used to free the buffer */
    if (blen == (size_t)-1)
	error("%s", _("R_AllocStringBuffer( (size_t)-1 ) is no longer allowed"));

    if (blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if (blen < blen1) blen += bsize;

    /* Result may be accessed as `wchar_t *` and other types; malloc /
      realloc guarantee correct memory alignment for all object types */
    if (buf->data == NULL) {
	buf->data = (char *) malloc(blen);
	if (buf->data)
	    buf->data[0] = '\0';
    } else
	buf->data = (char *) realloc(buf->data, blen);
    buf->bufsize = blen;
    if (!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error(_("could not allocate memory (%u %s) in C function 'R_AllocStringBuffer'"),
	      (unsigned int) (blen/Mega), "Mb");
    }
    return buf->data;
}

void R_FreeStringBuffer(R_StringBuffer *buf)
{
    if (buf->data != NULL) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

attribute_hidden void R_FreeStringBufferL(R_StringBuffer *buf)
{
    if (buf->bufsize > buf->defaultSize) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

/* ======== This needs direct access to gp field for efficiency ======== */

/* this has NA_STRING = NA_STRING */
attribute_hidden
bool R::Seql(SEXP a, SEXP b)
{
    /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
    if (a == b) return true;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
	return false;
    else if (IS_BYTES(a) || IS_BYTES(b)) {
	if (IS_BYTES(a) && IS_BYTES(b))
	    /* only get here if at least one is not cached */
	    return streql(CHAR(a), CHAR(b));
	else
	    return false;
    }
    else {
	CXXR::RAllocStack::Scope rscope;
	return streql(translateCharUTF8(a), translateCharUTF8(b));
    }
}


#ifdef LONG_VECTOR_SUPPORT
NORET R_len_t R::R_BadLongVector(SEXP x, const char *file, int line)
{
    error(_("long vectors not supported yet: %s:%d"), file, line);
}
#endif

/* Highly experimental resizable vector support */

bool R_isResizable(SEXP x)
{
    return isVector(x) && ! ALTREP(x) && GROWABLE_BIT_SET(x) &&
	XTRUELENGTH(x) != 0 && XLENGTH(x) <= XTRUELENGTH(x);
}

R_xlen_t R_maxLength(SEXP x)
{
    return GROWABLE_BIT_SET(x) ? XTRUELENGTH(x) : xlength(x);
}

SEXP R_allocResizableVector(SEXPTYPE type, R_xlen_t maxlen)
{
    switch (type) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
    case RAWSXP:
	break;
    default:
	error(_("cannot make a resizable vector of type '%s'"),
	      sexptype2char(type));
    }
    SEXP val = allocVector(type, maxlen);
    SET_TRUELENGTH(val, maxlen);
    SET_GROWABLE_BIT(val);
    return val;
}

SEXP R_duplicateAsResizable(SEXP x)
{
    if (ALTREP(x))
	error("%s", _("ALTREP objects cannot be made resizable"));
    if (!isVector(x))
	error("%s", _("cannot make non-vector objects resizable"));
    SEXP val = duplicate(x);
    SET_TRUELENGTH(val, XLENGTH(val));
    SET_GROWABLE_BIT(val);
    return val;
}

static R_INLINE void clear_elements(SEXP x, R_xlen_t from, R_xlen_t to)
{
    switch (TYPEOF(x)) {
    case STRSXP:
	for (R_xlen_t i = from; i < to; i++)
	    SET_STRING_ELT(x, i, R_BlankString);
	break;
    case EXPRSXP:
    case VECSXP:
	for (R_xlen_t i = from; i < to; i++)
	    SET_VECTOR_ELT(x, i, R_NilValue);
	break;
    default:
	break;
    }
}

void R_resizeVector(SEXP x, R_xlen_t newlen)
{
    if (newlen < 0)
	error("%s", _("invalid negative 'newlen'"));
    if (newlen != xlength(x)) {
	if (!R_isResizable(x))
	    error("%s", _("not a resizable vector"));
	if (newlen > XTRUELENGTH(x))
	    error("%s", _("'newlen' is too large"));
	if (ATTRIB(x) != R_NilValue) {
	    // clear length-dependent attributes
	    if (getAttrib(x, R_DimSymbol) != R_NilValue)
		setAttrib(x, R_DimSymbol, R_NilValue);
	    if (getAttrib(x, R_DimNamesSymbol) != R_NilValue)
		setAttrib(x, R_DimNamesSymbol, R_NilValue);
	    if (getAttrib(x, R_NamesSymbol) != R_NilValue)
		setAttrib(x, R_NamesSymbol, R_NilValue);
	}
	R_xlen_t len = XLENGTH(x);
	if (newlen < len) // clear dropped elements to drop refcounts
	    clear_elements(x, newlen, len);
	SET_STDVEC_LENGTH(x, newlen);
	if (len < newlen) // initialize new elements
	    clear_elements(x, len, newlen);
    }
}
