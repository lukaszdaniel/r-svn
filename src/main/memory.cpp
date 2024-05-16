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

#include <forward_list>
#include <iostream>
#include <list>
#include <cstdarg>
#include <map>
#include <CXXR/Complex.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/GCManager.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/JMPException.hpp>
#include <R_ext/Minmax.h>

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
#include <CXXR/GCRoot.hpp>

using namespace R;
using namespace CXXR;

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

void GCManager::gc_error(const char *msg)
{
    if (s_gc_fail_on_error)
	R_Suicide(msg);
    else if (gcIsRunning())
	REprintf("%s", msg);
    else
	error("%s", msg);
}

/* These are used in profiling to separate out time in GC */
int R_gc_running(void) { return GCManager::gcIsRunning(); }

#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
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

static R_INLINE SEXP CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x != NULL && TYPEOF(x) == FREESXP)
	error("unprotected object (%p) encountered (was %s)",
	      x, sexptype2char(OLDTYPE(x)));
    return x;
}
#else
#define CHK(x) x
#endif

/* The following class is used to record the
   address and type of the first bad type seen during a collection,
   and for FREESXP nodes they record the old type as well. */
class BadObject
{
public:
    BadObject() : m_bad_sexp_type_seen(NILSXP),
                  m_bad_sexp_type_sexp(nullptr),
#ifdef PROTECTCHECK
                  m_bad_sexp_type_old_type(NILSXP),
#endif
                  m_bad_sexp_type_line(0)
    {
    }

    bool isEmpty() const { return m_bad_sexp_type_seen == NILSXP; }
    void clear() { m_bad_sexp_type_seen = NILSXP; }

    static void register_bad_object(GCNode *s, int line);
    void printSummary();

    BadObject &operator=(const BadObject &other)
    {
    m_bad_sexp_type_seen = other.m_bad_sexp_type_seen;
    m_bad_sexp_type_sexp = other.m_bad_sexp_type_sexp;
#ifdef PROTECTCHECK
    m_bad_sexp_type_old_type = other.m_bad_sexp_type_old_type;
#endif
    m_bad_sexp_type_line = other.m_bad_sexp_type_line;
    return *this;
    }

    static BadObject s_firstBadObject;

private:
    SEXPTYPE m_bad_sexp_type_seen;
    GCNode *m_bad_sexp_type_sexp;
#ifdef PROTECTCHECK
    SEXPTYPE m_bad_sexp_type_old_type;
#endif
    unsigned int m_bad_sexp_type_line;
};

BadObject BadObject::s_firstBadObject;

inline void BadObject::printSummary()
{
    if (m_bad_sexp_type_seen != NILSXP)
    {
    char msg[256];
#ifdef PROTECTCHECK
    if (m_bad_sexp_type_seen == FREESXP)
        snprintf(msg, 256,
                 "GC encountered a node (%p) with type FREESXP (was %s)"
                 " at memory.c:%d",
                 (void *)m_bad_sexp_type_sexp,
                 sexptype2char(m_bad_sexp_type_old_type),
                 m_bad_sexp_type_line);
    else
        snprintf(msg, 256,
                 "GC encountered a node (%p) with an unknown SEXP type: %d"
                 " at memory.c:%d",
                 (void *)m_bad_sexp_type_sexp,
                 m_bad_sexp_type_seen,
                 m_bad_sexp_type_line);
#else
    snprintf(msg, 256,
             "GC encountered a node (%p) with an unknown SEXP type: %d"
             " at memory.c:%d",
             (void *)m_bad_sexp_type_sexp,
             m_bad_sexp_type_seen,
             m_bad_sexp_type_line);
    GCManager::gc_error(msg);
#endif
    }
}

inline void BadObject::register_bad_object(GCNode *s, int line) 
{
    if (s_firstBadObject.isEmpty()) {
	s_firstBadObject.m_bad_sexp_type_seen = TYPEOF(s);
	s_firstBadObject.m_bad_sexp_type_sexp = s;
	s_firstBadObject.m_bad_sexp_type_line = line;
#ifdef PROTECTCHECK
	if (TYPEOF(s) == FREESXP)
	    s_firstBadObject.m_bad_sexp_type_old_type = OLDTYPE(s);
#endif
    }
}

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

bool GCManager::FORCE_GC()
{
#ifdef GC_TORTURE
    if (s_gc_pending)
    {
        return true;
    }
    else if (s_gc_force_wait > 0)
    {
        --s_gc_force_wait;
        if (s_gc_force_wait > 0)
        {
            return false;
        }
        else
        {
            s_gc_force_wait = s_gc_force_gap;
            return true;
        }
    }
    return false;
#else
    return s_gc_pending;
#endif
}

#ifdef R_MEMORY_PROFILING
static void R_ReportAllocation(R_size_t);
static void R_ReportNewPage(void);
#endif

#define GC_PROT(X) do { \
    unsigned int __wait__ = GCManager::gc_force_wait();	\
    unsigned int __gap__ = GCManager::gc_force_gap();	\
    bool __release__ = GCManager::gc_inhibit_release();	\
    X;						\
    GCManager::setTortureParameters(__gap__, __wait__, __release__);			\
}  while(0)

static void R_gc_no_finalizers(R_size_t size_needed);
static void R_gc_lite(void);
static void mem_err_heap(R_size_t size);
static void mem_err_malloc(R_size_t size);

static CXXR::RObject UnmarkedNodeTemplate;
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

/* There are three levels of collections.  Level 0 collects only the
   youngest generation, level 1 collects the two youngest generations,
   and level 2 collects all generations.  Higher level collections
   occur at least after specified numbers of lower level ones.  After
   LEVEL_0_FREQ level zero collections a level 1 collection is done;
   after every LEVEL_1_FREQ level 1 collections a level 2 collection
   occurs.  Thus, roughly, every LEVEL_0_FREQ-th collection is a level
   1 collection and every (LEVEL_0_FREQ * LEVEL_1_FREQ)-th collection
   is a level 2 collection.  */
#define LEVEL_0_FREQ 20
#define LEVEL_1_FREQ 5
static unsigned int s_collect_counts_max[] = { LEVEL_0_FREQ, LEVEL_1_FREQ };

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
static double R_MaxKeepFrac = 0.5;
static unsigned int R_PageReleaseFreq = 1;

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
static R_size_t R_MaxVSize = R_SIZE_T_MAX;
static R_size_t R_MaxNSize = R_SIZE_T_MAX;
static unsigned int vsfac = 1; /* current units for vsize: changes at initialization */

attribute_hidden R_size_t R::R_GetMaxVSize(void)
{
    if (R_MaxVSize == R_SIZE_T_MAX) return R_SIZE_T_MAX;
    return R_MaxVSize * vsfac;
}

attribute_hidden bool R::R_SetMaxVSize(R_size_t size)
{
    if (size == R_SIZE_T_MAX) {
	R_MaxVSize = R_SIZE_T_MAX;
	return TRUE;
    }
    if (vsfac == 1) {
	if (size >= R_VSize) {
	    R_MaxVSize = size;
	    return TRUE;
	}
    } else 
	if (size / vsfac >= R_VSize) {
	    R_MaxVSize = (size + 1) / vsfac;
	    return TRUE;
	}
    return FALSE;
}

attribute_hidden R_size_t R::R_GetMaxNSize(void)
{
    return R_MaxNSize;
}

attribute_hidden bool R::R_SetMaxNSize(R_size_t size)
{
    if (size >= R_NSize) {
	R_MaxNSize = size;
	return TRUE;
    }
    return FALSE;
}

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
	    else if (!R_SetMaxVSize((R_size_t) newbytes))
		warning("%s", _("a limit lower than current usage, so ignored"));
	}
    }

    if (R_MaxVSize == R_SIZE_T_MAX)
	return ScalarReal(R_PosInf);
    else
	return ScalarReal(R_GetMaxVSize() / Mega);
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
	    else if (!R_SetMaxNSize((R_size_t) newval))
		warning("%s", _("a limit lower than current usage, so ignored"));
	}
    }

    if (R_MaxNSize == R_SIZE_T_MAX)
	return ScalarReal(R_PosInf);
    else
	return ScalarReal(R_GetMaxNSize());
}


/* Miscellaneous Globals. */

static SEXP R_VStack = NULL;		/* R_alloc stack pointer */
static SEXP R_PreciousList = NULL;      /* List of Persistent Objects */
static R_size_t R_LargeVallocSize = 0;
static R_size_t R_SmallVallocSize = 0;
static R_size_t orig_R_NSize;
static R_size_t orig_R_VSize;

static R_size_t R_N_maxused=0;
static R_size_t R_V_maxused=0;

/* Node Classes.  Non-vector nodes are of class zero. Small vector
   nodes are in classes 1, ..., NUM_SMALL_NODE_CLASSES, and large
   vector nodes are in class LARGE_NODE_CLASS. Vectors with
   custom allocators are in CUSTOM_NODE_CLASS. For vector nodes the
   node header is followed in memory by the vector data, offset from
   the header by VectorBase. */

#define NUM_NODE_CLASSES 8

/* sxpinfo allocates 3 bits for the node class, so at most 8 are allowed */
#if NUM_NODE_CLASSES > 8
# error NUM_NODE_CLASSES must be at most 8
#endif

#define LARGE_NODE_CLASS  (NUM_NODE_CLASSES - 1)
#define CUSTOM_NODE_CLASS (NUM_NODE_CLASSES - 2)
#define NUM_SMALL_NODE_CLASSES (NUM_NODE_CLASSES - 2)

/* the number of VECREC's in nodes of the small node classes */
static unsigned int NodeClassSize[NUM_SMALL_NODE_CLASSES] = { 0, 1, 2, 4, 8, 16 };

/* Node Generations. */

#define NODE_GEN_IS_YOUNGER(s,g) \
  (! NODE_IS_MARKED(s) || NODE_GENERATION(s) < (g))
#define NODE_IS_OLDER(x, y) \
    (NODE_IS_MARKED(x) && (y) && \
   (! NODE_IS_MARKED(y) || NODE_GENERATION(x) > NODE_GENERATION(y)))

namespace CXXR
{
    unsigned int GCManager::s_gen_gc_counts[GCNode::numGenerations()];
} // namespace CXXR

/* Node Pages.  Non-vector nodes and small vector nodes are allocated
   from fixed size pages.  The pages for each node class are kept in a
   linked list. */

#define SIZE_OF_PAGE_HEADER 0

#if ( SIZEOF_SIZE_T > 4 )
# define BASE_PAGE_SIZE 8000
#else
# define BASE_PAGE_SIZE 2000
#endif
#define R_PAGE_SIZE \
  (((BASE_PAGE_SIZE - SIZE_OF_PAGE_HEADER) / sizeof(RObject)) \
   * sizeof(RObject) \
   + SIZE_OF_PAGE_HEADER)
#define NODE_SIZE(c) \
  ((c) == 0 ? sizeof(RObject) : \
   sizeof(VectorBase) + NodeClassSize[c] * sizeof(VECREC))

#define PAGE_DATA(p) (p)
#define VHEAP_FREE() (R_VSize - R_LargeVallocSize - R_SmallVallocSize)


/* The Heap Structure.  Nodes for each class/generation combination
   are arranged in circular doubly-linked lists.  The double linking
   allows nodes to be removed in constant time; this is used by the
   collector to move reachable nodes out of free space and into the
   appropriate generation.  The circularity eliminates the need for
   end checks.  In addition, each link is anchored at an artificial
   node, the Peg RObject's in the structure below, which simplifies
   pointer maintenance.  The circular doubly-linked arrangement is
   taken from Baker's in-place incremental collector design; see
   ftp://ftp.netcom.com/pub/hb/hbaker/NoMotionGC.html or the Jones and
   Lins GC book.  The linked lists are implemented by adding two
   pointer fields to the RObject structure, which increases its size
   from 5 to 7 doubles. Other approaches are possible but don't seem
   worth pursuing for R.

   There are two options for dealing with old-to-new pointers.  The
   first option is to make sure they never occur by transferring all
   referenced younger objects to the generation of the referrer when a
   reference to a newer object is assigned to an older one.  This is
   enabled by defining EXPEL_OLD_TO_NEW.  The second alternative is to
   keep track of all nodes that may contain references to newer nodes
   and to "age" the nodes they refer to at the beginning of each
   collection.  This is the default.  The first option is simpler in
   some ways, but will create more floating garbage and add a bit to
   the execution time, though the difference is probably marginal on
   both counts.*/
/*#define EXPEL_OLD_TO_NEW*/
static struct {
    GCNode *Old[GCNode::s_num_old_generations];
    GCNode *New;
    GCNode *Free;
    CXXR::GCNode OldPeg[GCNode::s_num_old_generations];
    CXXR::GCNode NewPeg;
#ifndef EXPEL_OLD_TO_NEW
    GCNode *OldToNew[GCNode::s_num_old_generations];
    CXXR::GCNode OldToNewPeg[GCNode::s_num_old_generations];
#endif
    unsigned int OldCount[GCNode::s_num_old_generations];
    unsigned int AllocCount;
    unsigned int PageCount;
    std::forward_list<char *> pages;
} R_GenHeap[NUM_NODE_CLASSES];

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
  GCNode *un__n__ = (s); \
  LINK_NODE(PREV_NODE(un__n__), NEXT_NODE(un__n__)); \
} while(0)

/* snap in node s before node t */
#define SNAP_NODE(s,t) do { \
  GCNode *sn__n__ = (s); \
  GCNode *tn__n__ = (t); \
  LINK_NODE(PREV_NODE(tn__n__), sn__n__); \
  LINK_NODE(sn__n__, tn__n__); \
} while (0)

/* move all nodes on from_peg to to_peg */
#define BULK_MOVE(from_peg,to_peg) do { \
  GCNode *__from__ = (from_peg); \
  GCNode *__to__ = (to_peg); \
  GCNode *first_old = NEXT_NODE(__from__); \
  GCNode *last_old = PREV_NODE(__from__); \
  GCNode *first_new = NEXT_NODE(__to__); \
  LINK_NODE(__to__, first_old); \
  LINK_NODE(last_old, first_new); \
  LINK_NODE(__from__, __from__); \
} while (0);


/* Processing Node Children */

/* This macro calls dc__action__ for each child of __n__, passing
   dc__extra__ as a second argument for each call. */
/* When the CHARSXP hash chains are maintained through the ATTRIB
   field it is important that we NOT trace those fields otherwise too
   many CHARSXPs will be kept alive artificially. As a safety we don't
   ignore all non-NULL ATTRIB values for CHARSXPs but only those that
   are themselves CHARSXPs, which is what they will be if they are
   part of a hash chain.  Theoretically, for CHARSXPs the ATTRIB field
   should always be either R_NilValue or a CHARSXP. */
#ifdef PROTECTCHECK
# define HAS_GENUINE_ATTRIB(x) \
    (TYPEOF(x) != FREESXP && ATTRIB(x) != R_NilValue && \
     (TYPEOF(x) != CHARSXP || TYPEOF(ATTRIB(x)) != CHARSXP))
#else
# define HAS_GENUINE_ATTRIB(x) \
    (ATTRIB(x) != R_NilValue && \
     (TYPEOF(x) != CHARSXP || TYPEOF(ATTRIB(x)) != CHARSXP))
#endif

#ifdef PROTECTCHECK
#define FREE_FORWARD_CASE case FREESXP: if (GCManager::gc_inhibit_release()) break;
#else
#define FREE_FORWARD_CASE
#endif
/*** assume for now all ALTREP nodes are based on CONS nodes */
#define DO_CHILDREN4(__n__2,dc__action__,dc__str__action__,dc__extra__) do { \
  SEXP __n__ = (SEXP) (__n__2); \
  if (HAS_GENUINE_ATTRIB(__n__)) \
    dc__action__(ATTRIB(__n__), dc__extra__); \
  if (ALTREP(__n__)) {					\
	  dc__action__(TAG(__n__), dc__extra__);	\
	  dc__action__(CAR(__n__), dc__extra__);	\
	  dc__action__(CDR(__n__), dc__extra__);	\
      }							\
  else \
  switch (TYPEOF(__n__)) { \
  case NILSXP: \
  case BUILTINSXP: \
  case SPECIALSXP: \
  case CHARSXP: \
  case LGLSXP: \
  case INTSXP: \
  case REALSXP: \
  case CPLXSXP: \
  case WEAKREFSXP: \
  case RAWSXP: \
  case OBJSXP: \
    break; \
  case STRSXP: \
    { \
      R_xlen_t i; \
      for (i = 0; i < XLENGTH(__n__); i++) \
	dc__str__action__(VECTOR_ELT_0(__n__, i), dc__extra__); \
    } \
    break; \
  case EXPRSXP: \
  case VECSXP: \
    { \
      R_xlen_t i; \
      for (i = 0; i < XLENGTH(__n__); i++) \
	dc__action__(VECTOR_ELT_0(__n__, i), dc__extra__); \
    } \
    break; \
  case ENVSXP: \
    dc__action__(FRAME(__n__), dc__extra__); \
    dc__action__(ENCLOS(__n__), dc__extra__); \
    dc__action__(HASHTAB(__n__), dc__extra__); \
    break; \
  case LISTSXP: \
  case PROMSXP: \
    dc__action__(TAG(__n__), dc__extra__); \
    if (BOXED_BINDING_CELLS || BNDCELL_TAG(__n__) == 0) \
      dc__action__(CAR0(__n__), dc__extra__); \
    dc__action__(CDR(__n__), dc__extra__); \
    break; \
  case CLOSXP: \
  case LANGSXP: \
  case DOTSXP: \
  case SYMSXP: \
  case BCODESXP: \
    dc__action__(TAG(__n__), dc__extra__); \
    dc__action__(CAR0(__n__), dc__extra__); \
    dc__action__(CDR(__n__), dc__extra__); \
    break; \
  case EXTPTRSXP: \
    dc__action__(EXTPTR_PROT(__n__), dc__extra__); \
    dc__action__(EXTPTR_TAG(__n__), dc__extra__); \
    break; \
  FREE_FORWARD_CASE \
  default: \
    BadObject::register_bad_object(__n__, __LINE__);		\
  } \
} while(0)

#define DO_CHILDREN(__n__,dc__action__,dc__extra__) \
    DO_CHILDREN4(__n__,dc__action__,dc__action__,dc__extra__)


/* Forwarding Nodes.  These macros mark nodes or children of nodes and
   place them on the forwarding list.  The forwarding list is assumed
   to be in a local variable of the caller named
   forwarded_nodes. */

#define MARK_AND_UNSNAP_NODE(s) do {		\
	GCNode *mu__n__ = (s);			\
	CHECK_FOR_FREE_NODE(mu__n__);		\
	MARK_NODE(mu__n__);			\
	UNSNAP_NODE(mu__n__);			\
    } while (0)

#define FORWARD_NODE(s) do { \
  GCNode *fn__n__ = (s); \
  if (fn__n__ && ! NODE_IS_MARKED(fn__n__)) { \
    MARK_AND_UNSNAP_NODE(fn__n__); \
    forwarded_nodes.push_front(fn__n__); \
  } \
} while (0)

#define PROCESS_ONE_NODE(s) do {				\
	GCNode *pn__n__ = (s);					\
	int __cls__ = NODE_CLASS(pn__n__);			\
	int __gen__ = NODE_GENERATION(pn__n__);			\
	SNAP_NODE(pn__n__, R_GenHeap[__cls__].Old[__gen__]);	\
	R_GenHeap[__cls__].OldCount[__gen__]++;			\
    } while (0)

/* avoid pushing on the forwarding stack when possible */
#define FORWARD_AND_PROCESS_ONE_NODE(s, tp) do {	\
	GCNode *fpn__n__ = (s);				\
	int __tp__ = (tp);				\
	if (fpn__n__ && ! NODE_IS_MARKED(fpn__n__)) {	\
	    if (TYPEOF(fpn__n__) == __tp__ &&		\
		! HAS_GENUINE_ATTRIB(fpn__n__)) {	\
		MARK_AND_UNSNAP_NODE(fpn__n__);		\
		PROCESS_ONE_NODE(fpn__n__);		\
	    }						\
	    else FORWARD_NODE(fpn__n__);		\
	}						\
    } while (0)

#define PROCESS_CHARSXP(__n__) FORWARD_AND_PROCESS_ONE_NODE(__n__, CHARSXP)
#define FC_PROCESS_CHARSXP(__n__,__dummy__) PROCESS_CHARSXP(__n__)
#define FC_FORWARD_NODE(__n__,__dummy__) FORWARD_NODE(__n__)
#define FORWARD_CHILDREN(__n__) \
    DO_CHILDREN4(__n__, FC_FORWARD_NODE, FC_PROCESS_CHARSXP, 0)

/* This macro should help localize where a FREESXP node is encountered
   in the GC */
#ifdef PROTECTCHECK
#define CHECK_FOR_FREE_NODE(s) { \
    GCNode *cf__n__ = (s); \
    if (TYPEOF(cf__n__) == FREESXP && ! GCManager::gc_inhibit_release()) \
	BadObject::register_bad_object(cf__n__, __LINE__); \
}
#else
#define CHECK_FOR_FREE_NODE(s)
#endif


/* Node Allocation. */

#define CLASS_GET_FREE_NODE(c,s) do { \
  GCNode *__n__ = R_GenHeap[c].Free; \
  if (__n__ == R_GenHeap[c].New) { \
    GetNewPage(c); \
    __n__ = R_GenHeap[c].Free; \
  } \
  R_GenHeap[c].Free = NEXT_NODE(__n__); \
  GCNode::s_num_nodes++; \
  (s) = (SEXP) __n__; \
} while (0)

#define NO_FREE_NODES() (GCNode::s_num_nodes >= R_NSize)
#define GET_FREE_NODE(s) CLASS_GET_FREE_NODE(0,s)

/* versions that assume nodes are available without adding a new page */
#define CLASS_QUICK_GET_FREE_NODE(c,s) do {		\
	GCNode *__n__ = R_GenHeap[c].Free;			\
	if (__n__ == R_GenHeap[c].New)			\
	    error("need new page - should not happen");	\
	R_GenHeap[c].Free = NEXT_NODE(__n__);		\
	GCNode::s_num_nodes++;					\
	(s) = (SEXP) __n__;					\
    } while (0)

#define QUICK_GET_FREE_NODE(s) CLASS_QUICK_GET_FREE_NODE(0,s)

/* QUICK versions can be used if (CLASS_)NEED_NEW_PAGE returns FALSE */
#define CLASS_NEED_NEW_PAGE(c) (R_GenHeap[c].Free == R_GenHeap[c].New)
#define NEED_NEW_PAGE() CLASS_NEED_NEW_PAGE(0)


/* Debugging Routines. */

#ifdef DEBUG_GC
static void CheckNodeGeneration(SEXP x, int g)
{
    if (x && NODE_GENERATION(x) < g) {
	gc_error("untraced old-to-new reference\n");
    }
}

static void DEBUG_CHECK_NODE_COUNTS(const char *where)
{
    REprintf("Node counts %s:\n", where);
    unsigned int NewCount = 0;
    for (int i = 0; i < NUM_NODE_CLASSES; i++) {
	for (GCNode *s = NEXT_NODE(R_GenHeap[i].New);
	     s != R_GenHeap[i].New;
	     s = NEXT_NODE(s)) {
	    NewCount++;
	    if (i != NODE_CLASS(s))
		gc_error("Inconsistent class assignment for node!\n");
	}
	unsigned int OldCount = 0;
	unsigned int OldToNewCount = 0;
	for (unsigned int gen = 0;
	     gen < GCNode::numOldGenerations();
	     gen++) {
	    for (GCNode *s = NEXT_NODE(R_GenHeap[i].Old[gen]);
		 s != R_GenHeap[i].Old[gen];
		 s = NEXT_NODE(s)) {
		OldCount++;
		if (i != NODE_CLASS(s))
		    gc_error("Inconsistent class assignment for node!\n");
		if (gen != NODE_GENERATION(s))
		    gc_error("Inconsistent node generation\n");
		DO_CHILDREN(s, CheckNodeGeneration, gen);
	    }
	    for (GCNode *s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
		 s != R_GenHeap[i].OldToNew[gen];
		 s = NEXT_NODE(s)) {
		OldToNewCount++;
		if (i != NODE_CLASS(s))
		    gc_error("Inconsistent class assignment for node!\n");
		if (gen != NODE_GENERATION(s))
		    gc_error("Inconsistent node generation\n");
	    }
	}
	REprintf("Class: %d, New = %d, Old = %d, OldToNew = %d, Total = %d\n",
		 i,
		 NewCount, OldCount, OldToNewCount,
		 NewCount + OldCount + OldToNewCount);
    }
}

static void DEBUG_GC_SUMMARY(int full_gc)
{
    REprintf("\n%s, VSize = %lu", full_gc ? "Full" : "Minor",
	     R_SmallVallocSize + R_LargeVallocSize);
    for (int i = 1; i < NUM_NODE_CLASSES; i++) {
	unsigned int OldCount = 0;
	for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++)
	    OldCount += R_GenHeap[i].OldCount[gen];
	REprintf(", class %d: %d", i, OldCount);
    }
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
    R_size_t alloc = R_LargeVallocSize +
	sizeof(VectorBase) * R_GenHeap[LARGE_NODE_CLASS].AllocCount;
    for (int i = 0; i < NUM_SMALL_NODE_CLASSES; i++)
	alloc += R_PAGE_SIZE * R_GenHeap[i].PageCount;
    REprintf("Total allocation: %lu\n", alloc);
    REprintf("Ncells %lu\nVcells %lu\n", R_NSize, R_VSize);
}
#else
#define DEBUG_ADJUST_HEAP_PRINT(node_occup, vect_occup)
#endif /* DEBUG_ADJUST_HEAP */

#ifdef DEBUG_RELEASE_MEM
static void DEBUG_RELEASE_PRINT(int rel_pages, int maxrel_pages, int i)
{
    if (maxrel_pages > 0) {
	REprintf("Class: %d, pages = %d, maxrel = %d, released = %d\n", i,
		 R_GenHeap[i].PageCount, maxrel_pages, rel_pages);
	unsigned int n = 0;
	for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++)
	    n += R_GenHeap[i].OldCount[gen];
	REprintf("Allocated = %d, in use = %d\n", R_GenHeap[i].AllocCount, n);
    }
}
#else
#define DEBUG_RELEASE_PRINT(rel_pages, maxrel_pages, i)
#endif /* DEBUG_RELEASE_MEM */

#ifdef COMPUTE_REFCNT_VALUES
#define INIT_REFCNT(x) do {	\
	GCNode *__x__ = (x);	\
	SET_REFCNT(__x__, 0);	\
	ENABLE_REFCNT(__x__);	\
    } while (0)
#else
#define INIT_REFCNT(x) do {} while (0)
#endif

/* Page Allocation and Release. */

static void GetNewPage(int node_class)
{
    unsigned int node_size = NODE_SIZE(node_class);
    unsigned int page_count = (R_PAGE_SIZE - SIZE_OF_PAGE_HEADER) / node_size;

    char *page = new char[R_PAGE_SIZE];
    if (page == NULL) {
	R_gc_no_finalizers(0);
	page = (char *) malloc(R_PAGE_SIZE);
	if (page == NULL)
	    mem_err_malloc((R_size_t) R_PAGE_SIZE);
    }
#ifdef R_MEMORY_PROFILING
    R_ReportNewPage();
#endif
    R_GenHeap[node_class].pages.push_front(page);
    R_GenHeap[node_class].PageCount++;


    char *data = PAGE_DATA(page);
    GCNode *base = R_GenHeap[node_class].New;
    GCNode *s;
    for (unsigned int i = 0; i < page_count; i++) {
#if 1
	if (node_class == 0)
	{
	    s = (RObject *) data;
	    LINK_NODE(s, s);
	    CAR0((SEXP(s))) = nullptr;
	    CDR((SEXP(s))) = nullptr;
	    TAG((SEXP(s))) = nullptr;
	    ATTRIB(s) = nullptr;
	}
	else
	{
	    s = (VectorBase *) data;
	    LINK_NODE(s, s);
	    STDVEC_LENGTH(s) = 0;
	    STDVEC_TRUELENGTH(s) = 0;
	    ATTRIB(s) = nullptr;
	}
#else
	if (node_class == 0)
	{
	    s = new (data) RObject();
	}
	else
	{
	    s = new (data) VectorBase();
	    // static_cast<VectorBase *>(s)->vecsxp.m_data = (data + sizeof(VectorBase));
	}
#endif
	data += node_size;
	R_GenHeap[node_class].AllocCount++;
	SNAP_NODE(s, base);
#if  VALGRIND_LEVEL > 1
	if (NodeClassSize[node_class] > 0)
	    VALGRIND_MAKE_MEM_NOACCESS(STDVEC_DATAPTR(s), NodeClassSize[node_class]*sizeof(VECREC));
#endif
	s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	INIT_REFCNT(s);
	SET_NODE_CLASS(s, node_class);
#ifdef PROTECTCHECK
	SET_TYPEOF(s, NEWSXP);
#endif
	base = s;
	R_GenHeap[node_class].Free = s;
    }
}

static void ReleasePage(char *page, int node_class)
{
    unsigned int node_size = NODE_SIZE(node_class);
    unsigned int page_count = (R_PAGE_SIZE - SIZE_OF_PAGE_HEADER) / node_size;
    char *data = PAGE_DATA(page);

    GCNode *s;
    for (unsigned int i = 0; i < page_count; i++) {
	s = (GCNode *) data;
	data += node_size;
	UNSNAP_NODE(s);
	// s->~GCNode();
	R_GenHeap[node_class].AllocCount--;
    }
    R_GenHeap[node_class].PageCount--;
    delete[] page;
}

static void TryToReleasePages(void)
{
    GCNode *s;
    static unsigned int release_count = 0;
    if (release_count > 0)
    {
        --release_count;
        return;
    }

	release_count = R_PageReleaseFreq;
	for (int i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
	    unsigned int node_size = NODE_SIZE(i);
	    unsigned int page_count = (R_PAGE_SIZE - SIZE_OF_PAGE_HEADER) / node_size;
	    int maxrel_pages;

	    int maxrel = R_GenHeap[i].AllocCount;
	    for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++)
		maxrel -= (int)((1.0 + R_MaxKeepFrac) * R_GenHeap[i].OldCount[gen]);
	    maxrel_pages = maxrel > 0 ? maxrel / page_count : 0;

	    /* all nodes in New space should be both free and unmarked */
	    int rel_pages = 0;
	    std::vector<char *> to_delete;
	    for (auto &page : R_GenHeap[i].pages) {
		if (rel_pages >= maxrel_pages) break;
		char *data = PAGE_DATA(page);

		bool in_use = false;
		for (int j = 0; j < page_count; j++) {
		    s = (GCNode *) data;
		    data += node_size;
		    if (NODE_IS_MARKED(s)) {
			in_use = 1;
			break;
		    }
		}
		if (! in_use) {
		    to_delete.push_back(page);
		    rel_pages++;
		}
	    }
	    for (auto &page : to_delete)
	    {
		R_GenHeap[i].pages.remove(page);
		ReleasePage(page, i);
	    }
	    DEBUG_RELEASE_PRINT(rel_pages, maxrel_pages, i);
	    R_GenHeap[i].Free = NEXT_NODE(R_GenHeap[i].New);
	}
}

/* compute size in VEC units so result will fit in LENGTH field for FREESXPs */
static R_INLINE R_size_t getVecSizeInVEC(SEXP s)
{
    if (IS_GROWABLE(s))
	SET_STDVEC_LENGTH(s, XTRUELENGTH(s));

    R_size_t size;
    switch (TYPEOF(s)) {	/* get size in bytes */
    case CHARSXP:
	size = XLENGTH(s) + 1;
	break;
    case RAWSXP:
	size = XLENGTH(s);
	break;
    case LGLSXP:
    case INTSXP:
	size = XLENGTH(s) * sizeof(int);
	break;
    case REALSXP:
	size = XLENGTH(s) * sizeof(double);
	break;
    case CPLXSXP:
	size = XLENGTH(s) * sizeof(Complex);
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	size = XLENGTH(s) * sizeof(SEXP);
	break;
    default:
	BadObject::register_bad_object(s, __LINE__);
	size = 0;
    }
    return BYTE2VEC(size);
}

static void custom_node_free(void *ptr);

/* Heap Size Adjustment. */

static void AdjustHeapSize(R_size_t size_needed)
{
    R_size_t R_MinNFree = (R_size_t)(orig_R_NSize * R_MinFreeFrac);
    R_size_t R_MinVFree = (R_size_t)(orig_R_VSize * R_MinFreeFrac);
    R_size_t NNeeded = GCNode::s_num_nodes + R_MinNFree;
    R_size_t VNeeded = R_SmallVallocSize + R_LargeVallocSize + size_needed + R_MinVFree;
    double node_occup = ((double) NNeeded) / R_NSize;
    double vect_occup =	((double) VNeeded) / R_VSize;

    if (node_occup > R_NGrowFrac) {
	R_size_t change =
	    (R_size_t)(R_NGrowIncrMin + R_NGrowIncrFrac * R_NSize);

	/* for early adjustments grow more aggressively */
	static R_size_t last_in_use = 0;
	static unsigned int adjust_count = 1;
	if (adjust_count < 50) {
	    adjust_count++;

	    /* estimate next in-use count by assuming linear growth */
	    R_size_t next_in_use = GCNode::s_num_nodes + (GCNode::s_num_nodes - last_in_use);
	    last_in_use = GCNode::s_num_nodes;

	    /* try to achieve and occupancy rate of R_NGrowFrac */
	    R_size_t next_nsize = (R_size_t) (next_in_use / R_NGrowFrac);
	    if (next_nsize > R_NSize + change)
		change = next_nsize - R_NSize;
	}

	if (R_MaxNSize >= R_NSize + change)
	    R_NSize += change;
    }
    else if (node_occup < R_NShrinkFrac) {
	R_NSize -= (R_size_t)(R_NShrinkIncrMin + R_NShrinkIncrFrac * R_NSize);
	if (R_NSize < NNeeded)
	    R_NSize = (NNeeded < R_MaxNSize) ? NNeeded: R_MaxNSize;
	if (R_NSize < orig_R_NSize)
	    R_NSize = orig_R_NSize;
    }

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

#define AGE_NODE(s,g) do { \
  GCNode *an__n__ = (s); \
  int an__g__ = (g); \
  if (an__n__ && NODE_GEN_IS_YOUNGER(an__n__, an__g__)) { \
    if (NODE_IS_MARKED(an__n__)) \
       R_GenHeap[NODE_CLASS(an__n__)].OldCount[NODE_GENERATION(an__n__)]--; \
    else \
      MARK_NODE(an__n__); \
    SET_NODE_GENERATION(an__n__, an__g__); \
    UNSNAP_NODE(an__n__); \
    forwarded_nodes.push_front(an__n__); \
  } \
} while (0)

static void AgeNodeAndChildren(GCNode *s, int gen)
{
    std::forward_list<GCNode *> forwarded_nodes;
    AGE_NODE(s, gen);
    while (!forwarded_nodes.empty()) {
	s = forwarded_nodes.front();
	forwarded_nodes.pop_front();
	if (NODE_GENERATION(s) != gen)
	    GCManager::gc_error("****snapping into wrong generation\n");
	SNAP_NODE(s, R_GenHeap[NODE_CLASS(s)].Old[gen]);
	R_GenHeap[NODE_CLASS(s)].OldCount[gen]++;
	DO_CHILDREN(s, AGE_NODE, gen);
    }
}

static void old_to_new(SEXP x, SEXP y)
{
#ifdef EXPEL_OLD_TO_NEW
    AgeNodeAndChildren(y, NODE_GENERATION(x));
#else
    UNSNAP_NODE(x);
    SNAP_NODE(x, R_GenHeap[NODE_CLASS(x)].OldToNew[NODE_GENERATION(x)]);
#endif
}

#ifdef COMPUTE_REFCNT_VALUES
#define FIX_REFCNT_EX(x, old, new_, chkpnd) do {				\
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
#define FIX_BINDING_REFCNT(x, old, new_) do {\
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
    for (int i = 0; i < NUM_SMALL_NODE_CLASSES; i++) {
	unsigned int node_size = NODE_SIZE(i);
	unsigned int page_count = (R_PAGE_SIZE - SIZE_OF_PAGE_HEADER) / node_size;

	LINK_NODE(R_GenHeap[i].New, R_GenHeap[i].New);

	GCNode *s;
	for (auto &page : R_GenHeap[i].pages) {
	    char *data = PAGE_DATA(page);

	    for (unsigned int j = 0; j < page_count; j++) {
		s = (GCNode *) data;
		data += node_size;
		if (! NODE_IS_MARKED(s))
		    SNAP_NODE(s, R_GenHeap[i].New);
	    }
	}
	R_GenHeap[i].Free = NEXT_NODE(R_GenHeap[i].New);
    }
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
    SEXP w = Rf_allocSExp(WEAKREFSXP);
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

static bool s_R_finalizers_pending = FALSE;
static void CheckFinalizers(void)
{
    s_R_finalizers_pending = FALSE;
    for (auto &s : s_R_weak_refs) {
	if (s && WEAKREF_KEY(s) && !NODE_IS_MARKED(WEAKREF_KEY(s)) && ! IS_READY_TO_FINALIZE(s))
	    SET_READY_TO_FINALIZE(s);
	if (IS_READY_TO_FINALIZE(s))
	    s_R_finalizers_pending = TRUE;
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
    SEXP s = allocVector(RAWSXP, sizeof(R_CFinalizer_t));
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
    SEXP key = WEAKREF_KEY(w);
    SEXP fun = WEAKREF_FINALIZER(w);
    SET_WEAKREF_KEY(w, R_NilValue);
    SET_WEAKREF_VALUE(w, R_NilValue);
    SET_WEAKREF_FINALIZER(w, R_NilValue);
    if (! IS_READY_TO_FINALIZE(w))
	SET_READY_TO_FINALIZE(w); /* insures removal from list on next gc */
    PROTECT(key);
    PROTECT(fun);
    bool oldintrsusp = R_interrupts_suspended;
    R_interrupts_suspended = TRUE;
    if (isCFinalizer(fun)) {
	/* Must be a C finalizer. */
	R_CFinalizer_t cfun = GetCFinalizer(fun);
	cfun(key);
    }
    else if (fun != R_NilValue) {
	SEXP e;
	/* An R finalizer. */
	PROTECT(e = LCONS(fun, LCONS(key, R_NilValue)));
	eval(e, R_GlobalEnv);
	UNPROTECT(1);
    }
    R_interrupts_suspended = oldintrsusp;
    UNPROTECT(2);
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
    static bool s_running = FALSE;
    if (s_running) return FALSE;
    s_running = TRUE;

    volatile bool finalizer_run = FALSE;
    std::list<SEXP> pending_refs;

    while (!s_R_weak_refs.empty()) {
	SEXP s = s_R_weak_refs.back();
	s_R_weak_refs.pop_back();
	if (!IS_READY_TO_FINALIZE(s)) {
        pending_refs.push_front(s);
	} else {
	    /**** use R_ToplevelExec here? */
	    RCNTXT * volatile saveToplevelContext;
	    volatile size_t savestack;
	    volatile SEXP topExp, oldHStack, oldRStack, oldRVal;
	    volatile bool oldvis;
	    PROTECT(oldHStack = R_HandlerStack);
	    PROTECT(oldRStack = R_RestartStack);
	    PROTECT(oldRVal = R_ReturnedValue);
	    oldvis = Evaluator::resultPrinted();
	    R_HandlerStack = R_NilValue;
	    R_RestartStack = R_NilValue;

	    finalizer_run = TRUE;

	    /* A top level context is established for the finalizer to
	       insure that any errors that might occur do not spill
	       into the call that triggered the collection. */
	    RCNTXT thiscontext;
	    begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    saveToplevelContext = R_ToplevelContext;
	    savestack = R_PPStackTop;
	    PROTECT(topExp = R_CurrentExpr);
	    /* The value of 'next' is protected to make it safe
	       for this routine to be called recursively from a
	       gc triggered by a finalizer. */
        try
        {
            R_GlobalContext = R_ToplevelContext = &thiscontext;

            /* The entry in the weak reference list is removed
               before running the finalizer.  This insures that a
               finalizer is run only once, even if running it
               raises an error. */
            R_RunWeakRefFinalizer(s);
        }
        catch (JMPException &e)
        {
            if (e.context() != &thiscontext)
                throw;
        }
	    UNPROTECT(1); // topExp
	    endcontext(&thiscontext);
	    R_ToplevelContext = saveToplevelContext;
	    R_PPStackTop = savestack;
	    R_CurrentExpr = topExp;
	    R_HandlerStack = oldHStack;
	    R_RestartStack = oldRStack;
	    R_ReturnedValue = oldRVal;
	    Evaluator::enableResultPrinting(oldvis);
	    UNPROTECT(3);/* oldRVal, oldRStack, oldHStack */
	}
    }
    if (!pending_refs.empty())
        s_R_weak_refs = std::move(pending_refs);
    s_running = FALSE;
    s_R_finalizers_pending = FALSE;
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    R_checkConstants(TRUE);

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


/* The Generational Collector. */

#define PROCESS_NODES() do { \
    while (!forwarded_nodes.empty()) { \
	GCNode *s = forwarded_nodes.front(); \
	forwarded_nodes.pop_front(); \
	PROCESS_ONE_NODE(s); \
	FORWARD_CHILDREN(s); \
    } \
} while (0)

void GCNode::propagateAges(unsigned int num_old_gens_to_collect)
{
#ifndef EXPEL_OLD_TO_NEW
    /* eliminate old-to-new references in generations to collect by
       transferring referenced nodes to referring generation */
    for (unsigned int gen = 0; gen < num_old_gens_to_collect; gen++) {
	for (int i = 0; i < NUM_NODE_CLASSES; i++) {
	    GCNode *s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
	    while (s != R_GenHeap[i].OldToNew[gen]) {
		GCNode *next = NEXT_NODE(s);
		DO_CHILDREN(s, AgeNodeAndChildren, gen);
		UNSNAP_NODE(s);
		if (NODE_GENERATION(s) != gen)
		    GCManager::gc_error("****snapping into wrong generation\n");
		SNAP_NODE(s, R_GenHeap[i].Old[gen]);
		s = next;
	    }
	}
    }
#endif
}

void GCNode::mark(unsigned int num_old_gens_to_collect)
{
    /* unmark all marked nodes in old generations to be collected and
       move to New space */
    for (unsigned int gen = 0; gen < num_old_gens_to_collect; gen++) {
	for (int i = 0; i < NUM_NODE_CLASSES; i++) {
	    R_GenHeap[i].OldCount[gen] = 0;
	    GCNode *s = NEXT_NODE(R_GenHeap[i].Old[gen]);
	    while (s != R_GenHeap[i].Old[gen]) {
		GCNode *next = NEXT_NODE(s);
		if (gen < s_num_old_generations - 1)
		    SET_NODE_GENERATION(s, gen + 1);
		UNMARK_NODE(s);
		s = next;
	    }
	    if (NEXT_NODE(R_GenHeap[i].Old[gen]) != R_GenHeap[i].Old[gen])
		BULK_MOVE(R_GenHeap[i].Old[gen], R_GenHeap[i].New);
	}
    }

    std::forward_list<GCNode *> forwarded_nodes;

#ifndef EXPEL_OLD_TO_NEW
    /* scan nodes in uncollected old generations with old-to-new pointers */
    for (unsigned int gen = num_old_gens_to_collect; gen < s_num_old_generations; gen++)
	for (int i = 0; i < NUM_NODE_CLASSES; i++)
	    for (GCNode *s = NEXT_NODE(R_GenHeap[i].OldToNew[gen]);
		 s != R_GenHeap[i].OldToNew[gen];
		 s = NEXT_NODE(s))
		FORWARD_CHILDREN(s);
#endif

    /* forward all roots */
    FORWARD_NODE(R_NilValue);	           /* Builtin constants */
    FORWARD_NODE(NA_STRING);
    FORWARD_NODE(R_BlankString);
    FORWARD_NODE(R_BlankScalarString);
    FORWARD_NODE(R_CurrentExpression);
    FORWARD_NODE(R_UnboundValue);
    FORWARD_NODE(R_RestartToken);
    FORWARD_NODE(R_MissingArg);
    FORWARD_NODE(R_InBCInterpreter);

    FORWARD_NODE(R_GlobalEnv);	           /* Global environment */
    FORWARD_NODE(R_BaseEnv);
    FORWARD_NODE(R_EmptyEnv);
    FORWARD_NODE(R_Warnings);	           /* Warnings, if any */
    FORWARD_NODE(R_ReturnedValue);

    FORWARD_NODE(R_HandlerStack);          /* Condition handler stack */
    FORWARD_NODE(R_RestartStack);          /* Available restarts stack */

    FORWARD_NODE(R_BCbody);                /* Current byte code object */
    FORWARD_NODE(R_Srcref);                /* Current source reference */

    FORWARD_NODE(R_TrueValue);
    FORWARD_NODE(R_FalseValue);
    FORWARD_NODE(R_LogicalNAValue);

    FORWARD_NODE(R_print.na_string);
    FORWARD_NODE(R_print.na_string_noquote);

    if (R_SymbolTable != NULL)             /* in case of GC during startup */
	for (int i = 0; i < HSIZE; i++) {      /* Symbol table */
	    FORWARD_NODE(R_SymbolTable[i]);
	    for (SEXP s = R_SymbolTable[i]; s != R_NilValue; s = CDR(s))
		if (ATTRIB(CAR(s)) != R_NilValue)
		    GCManager::gc_error("****found a symbol with attributes\n");
	}

    if (R_CurrentExpr != NULL)	           /* Current expression */
	FORWARD_NODE(R_CurrentExpr);

#if CXXR_FALSE
    for (int i = 0; i < R_MaxDevices; i++) {   /* Device display lists */
	pGEDevDesc gdd = GEgetDevice(i);
	if (gdd) {
	    FORWARD_NODE(gdd->displayList);
	    FORWARD_NODE(gdd->savedSnapshot);
	    if (gdd->dev)
		FORWARD_NODE(gdd->dev->eventEnv);
	}
    }
#endif

    for (RCNTXT *ctxt = R_GlobalContext; ctxt != NULL ; ctxt = ctxt->nextcontext) {
	FORWARD_NODE(ctxt->conexit);       /* on.exit expressions */
	FORWARD_NODE(ctxt->promargs);	   /* promises supplied to closure */
	FORWARD_NODE(ctxt->callfun);       /* the closure called */
	FORWARD_NODE(ctxt->sysparent);     /* calling environment */
	FORWARD_NODE(ctxt->call);          /* the call */
	FORWARD_NODE(ctxt->cloenv);        /* the closure environment */
	FORWARD_NODE(ctxt->bcbody);        /* the current byte code object */
	FORWARD_NODE(ctxt->handlerstack);  /* the condition handler stack */
	FORWARD_NODE(ctxt->restartstack);  /* the available restarts stack */
	FORWARD_NODE(ctxt->srcref);	   /* the current source reference */
	if (ctxt->returnValue.tag == 0)    /* For on.exit calls */
	    FORWARD_NODE(ctxt->returnValue.u.sxpval);
    }

    FORWARD_NODE(R_PreciousList);

    for (size_t i = 0; i < R_PPStackTop; i++)	   /* Protected pointers */
	FORWARD_NODE(R_PPStack[i]);

    for (GCRootBase *node = GCRootBase::s_list_head; node; node = node->m_next)
    {
        if (node->ptr())
            FORWARD_NODE(node->ptr());
    }

    FORWARD_NODE(R_VStack);		   /* R_alloc stack */

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++) {
	if (sp->tag == RAWMEM_TAG)
	    sp += sp->u.ival;
	else if (sp->tag == 0 || IS_PARTIAL_SXP_TAG(sp->tag))
	    FORWARD_NODE(sp->u.sxpval);
    }

    /* main processing loop */
    PROCESS_NODES();

    /* identify weakly reachable nodes */
    {
	bool recheck_weak_refs;
	do {
	    recheck_weak_refs = FALSE;
	    for (auto &s : s_R_weak_refs) {
		if (s && WEAKREF_KEY(s) && NODE_IS_MARKED(WEAKREF_KEY(s))) {
		    if (WEAKREF_VALUE(s) && !NODE_IS_MARKED(WEAKREF_VALUE(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_VALUE(s));
		    }
		    if (WEAKREF_FINALIZER(s) && !NODE_IS_MARKED(WEAKREF_FINALIZER(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_FINALIZER(s));
		    }
		}
	    }
	    PROCESS_NODES();
	} while (recheck_weak_refs);
    }

    /* mark nodes ready for finalizing */
    CheckFinalizers();

    /* process the weak reference chain */
    for (auto &s : s_R_weak_refs) {
	FORWARD_NODE(s);
	FORWARD_NODE(WEAKREF_KEY(s));
	FORWARD_NODE(WEAKREF_VALUE(s));
	FORWARD_NODE(WEAKREF_FINALIZER(s));
    }
    PROCESS_NODES();

    /* process CHARSXP cache */
    if (R_StringHash != NULL) /* in case of GC during initialization */
    {
	SEXP t;
	SEXP s;
	int nc = 0;
	for (int i = 0; i < LENGTH(R_StringHash); i++) {
	    s = VECTOR_ELT_0(R_StringHash, i);
	    t = R_NilValue;
	    while (s != R_NilValue) {
		if (! NODE_IS_MARKED(CXHEAD(s))) { /* remove unused CHARSXP and cons cell */
		    if (t == R_NilValue) /* head of list */
			VECTOR_ELT_0(R_StringHash, i) = CXTAIL(s);
		    else
			CXTAIL(t) = CXTAIL(s);
		    s = CXTAIL(s);
		    continue;
		}
		FORWARD_NODE(s);
		FORWARD_NODE(CXHEAD(s));
		t = s;
		s = CXTAIL(s);
	    }
	    if(VECTOR_ELT_0(R_StringHash, i) != R_NilValue) nc++;
	}
	SET_TRUELENGTH(R_StringHash, nc); /* SET_HASHPRI, really */
    }
    /* chains are known to be marked so don't need to scan again */
    FORWARD_AND_PROCESS_ONE_NODE(R_StringHash, VECSXP);
    PROCESS_NODES(); /* probably nothing to process, but just in case ... */

#ifdef PROTECTCHECK
    for (int i=0; i< NUM_SMALL_NODE_CLASSES;i++){
	GCNode *s = NEXT_NODE(R_GenHeap[i].New);
	while (s != R_GenHeap[i].New) {
	    GCNode *next = NEXT_NODE(s);
	    if (TYPEOF(s) != NEWSXP) {
		if (TYPEOF(s) != FREESXP) {
		    SETOLDTYPE(s, TYPEOF(s));
		    SET_TYPEOF(s, FREESXP);
		}
		if (GCManager::gc_inhibit_release())
		    FORWARD_NODE(s);
	    }
	    s = next;
	}
    }
    for (int i = CUSTOM_NODE_CLASS; i <= LARGE_NODE_CLASS; i++) {
	GCNode *s = NEXT_NODE(R_GenHeap[i].New);
	while (s != R_GenHeap[i].New) {
	    GCNode *next = NEXT_NODE(s);
	    if (TYPEOF(s) != NEWSXP) {
		if (TYPEOF(s) != FREESXP) {
		    /**** could also leave this alone and restore the old
			  node type in GCNode::sweep() before
			  calculating size */
		    if (1 /* CHAR(s) != NULL*/) {
			/* see comment in GCNode::sweep() */
			R_size_t size = getVecSizeInVEC((SEXP) s);
			SET_STDVEC_LENGTH((SEXP) s, size);
		    }
		    SETOLDTYPE(s, TYPEOF(s));
		    SET_TYPEOF(s, FREESXP);
		}
		if (GCManager::gc_inhibit_release())
		    FORWARD_NODE(s);
	    }
	    s = next;
	}
    }
    if (GCManager::gc_inhibit_release())
	PROCESS_NODES();
#endif
}

void GCNode::sweep()
{
#if CXXR_FALSE
    /* reset RObject allocations */
	GCNode *s = NEXT_NODE(R_GenHeap[0].New);
	while (s != R_GenHeap[0].New) {
	    GCNode *next = NEXT_NODE(s);
	    CAR0((SEXP(s))) = nullptr;
	    CDR((SEXP(s))) = nullptr;
	    TAG((SEXP(s))) = nullptr;
	    ATTRIB(s) = nullptr;
	    SET_TYPEOF(s, NILSXP);
	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	    INIT_REFCNT(s);
	    s = next;
	}

    /* reset small vector allocations */
    for (int node_class = 1; node_class < NUM_SMALL_NODE_CLASSES; node_class++) {
	GCNode *s = NEXT_NODE(R_GenHeap[node_class].New);
	while (s != R_GenHeap[node_class].New) {
	    GCNode *next = NEXT_NODE(s);
	    R_size_t size = NodeClassSize[node_class];
	    memset(STDVEC_DATAPTR(s), 0, size);
	    STDVEC_LENGTH(s) = 0;
	    STDVEC_TRUELENGTH(s) = 0;
	    ATTRIB(s) = nullptr;
	    SET_TYPEOF(s, NILSXP);
	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	    INIT_REFCNT(s);
	    s = next;
	}
    }
#endif
    /* release large vector allocations */
    for (int node_class = CUSTOM_NODE_CLASS; node_class <= LARGE_NODE_CLASS; node_class++) {
	GCNode *s = NEXT_NODE(R_GenHeap[node_class].New);
	while (s != R_GenHeap[node_class].New) {
	    GCNode *next = NEXT_NODE(s);
	    if (1 /* CHAR(s) != NULL*/) {
		/* Consecutive representation of large vectors with header followed
		   by data. An alternative representation (currently not implemented)
		   could have CHAR(s) == NULL. */
		R_size_t size;
#ifdef PROTECTCHECK
		if (TYPEOF(s) == FREESXP)
		    size = STDVEC_LENGTH(s);
		else
		    /* should not get here -- arrange for a warning/error? */
		    size = getVecSizeInVEC((SEXP) s);
#else
		size = getVecSizeInVEC((SEXP) s);
#endif
		UNSNAP_NODE(s);
		// s->~GCNode();
		R_GenHeap[node_class].AllocCount--;
		if (node_class == LARGE_NODE_CLASS) {
		    R_LargeVallocSize -= size;
		    free(s);
		} else { // CUSTOM_NODE_CLASS
		    custom_node_free(s);
		}
	    }
	    s = next;
	}
    }

    /* tell Valgrind about free nodes */
#if VALGRIND_LEVEL > 1
    for (int node_class = 1; node_class < NUM_NODE_CLASSES; node_class++) {
	for (GCNode *s = NEXT_NODE(R_GenHeap[node_class].New);
	    s != R_GenHeap[node_class].Free;
	    s = NEXT_NODE(s)) {
	    VALGRIND_MAKE_MEM_NOACCESS(STDVEC_DATAPTR(s),
				       NodeClassSize[node_class]*sizeof(VECREC));
	}
    }
#endif

    /* reset Free pointers */
    for (int node_class = 0; node_class < NUM_NODE_CLASSES; node_class++)
	R_GenHeap[node_class].Free = NEXT_NODE(R_GenHeap[node_class].New);
}

void GCNode::gc(unsigned int num_old_gens_to_collect /* either 0, 1, or 2 */)
{
    propagateAges(num_old_gens_to_collect);

    DEBUG_CHECK_NODE_COUNTS("at start");

    mark(num_old_gens_to_collect);

    DEBUG_CHECK_NODE_COUNTS("after processing forwarded list");

    sweep();

    DEBUG_CHECK_NODE_COUNTS("after releasing large allocated nodes");
}

unsigned int GCManager::genRota(unsigned int num_old_gens_to_collect)
{
    static unsigned int s_collect_counts[GCNode::numOldGenerations()] = { 0, 0 };
    /* determine number of generations to collect */
    while (num_old_gens_to_collect < GCNode::numOldGenerations()) {
	if (s_collect_counts[num_old_gens_to_collect]-- <= 0) {
	    s_collect_counts[num_old_gens_to_collect] =
		s_collect_counts_max[num_old_gens_to_collect];
	    ++num_old_gens_to_collect;
	}
	else break;
    }
    return num_old_gens_to_collect;
}

// former RunGenCollect()
unsigned int GCManager::gcGenController(R_size_t size_needed, bool force_full_collection)
{
    static unsigned int level = 0;
    unsigned int gens_collected;

    BadObject::s_firstBadObject.clear();

    /* determine number of generations to collect */
    if (force_full_collection) level = GCNode::numOldGenerations();

#ifdef PROTECTCHECK
    level = GCNode::numOldGenerations();
#endif

    level = genRota(level);

    bool ok = false;
    while (!ok) {
    ok = true;

    GCNode::gc(level);
    gens_collected = level;

    /* update heap statistics */
    R_Collected = R_NSize;
    R_SmallVallocSize = 0;
    for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++) {
	for (int i = 1; i < NUM_SMALL_NODE_CLASSES; i++)
	    R_SmallVallocSize += R_GenHeap[i].OldCount[gen] * NodeClassSize[i];
	for (int i = 0; i < NUM_NODE_CLASSES; i++)
	    R_Collected -= R_GenHeap[i].OldCount[gen];
    }
    GCNode::s_num_nodes = R_NSize - R_Collected;

    if (level < GCNode::numOldGenerations()) {
	if (R_Collected < R_MinFreeFrac * R_NSize ||
	    VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize) {
	    ++level;
	    if (R_Collected <= 0 || VHEAP_FREE() < size_needed)
		ok = false;
	}
	else level = 0;
    }
    else level = 0;
    } // end of while loop
    s_gen_gc_counts[gens_collected]++;

    if (gens_collected == GCNode::numOldGenerations()) {
	/**** do some adjustment for intermediate collections? */
	AdjustHeapSize(size_needed);
	TryToReleasePages();
	DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
    }
    else if (gens_collected > 0) {
	TryToReleasePages();
	DEBUG_CHECK_NODE_COUNTS("after heap adjustment");
    }
#ifdef SORT_NODES
    if (gens_collected == GCNode::numOldGenerations())
	SortNodes();
#endif

    return gens_collected;
}


/* public interface for controlling GC torture settings */
/* maybe, but in no header */
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
    int gap;
    SEXP old = ScalarLogical(GCManager::gc_force_wait() > 0);

    if (isLogical(CAR(args))) {
	int on = asLogical(CAR(args));
	if (on == NA_LOGICAL) gap = NA_INTEGER;
	else if (on) gap = 1;
	else gap = 0;
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
    int inhibit = asLogical(CADDR(args));
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
    *smallvsize = R_SmallVallocSize;
    *largevsize = R_LargeVallocSize;
    *nodes = GCNode::s_num_nodes * sizeof(RObject);
    return;
}

attribute_hidden SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    R_size_t onsize = R_NSize /* can change during collection */;

    checkArity(op, args);
    std::ostream *old_report_os = GCManager::setReporting(Rf_asLogical(CAR(args)) ? &std::cerr : nullptr);
    bool reset_max = asLogical(CADR(args));
    bool full = asLogical(CADDR(args));
    if (full)
	R_gc();
    else
	R_gc_lite();

    GCManager::setReporting(old_report_os);
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 14));
    REAL(value)[0] = onsize - R_Collected;
    REAL(value)[1] = R_VSize - VHEAP_FREE();
    REAL(value)[4] = R_NSize;
    REAL(value)[5] = R_VSize;
    /* next four are in 0.1Mb, rounded up */
    REAL(value)[2] = 0.1*ceil(10. * (onsize - R_Collected)/Mega * sizeof(RObject));
    REAL(value)[3] = 0.1*ceil(10. * (R_VSize - VHEAP_FREE())/Mega * vsfac);
    REAL(value)[6] = 0.1*ceil(10. * R_NSize/Mega * sizeof(RObject));
    REAL(value)[7] = 0.1*ceil(10. * R_VSize/Mega * vsfac);
    REAL(value)[8] = (R_MaxNSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxNSize/Mega * sizeof(RObject)) : NA_REAL;
    REAL(value)[9] = (R_MaxVSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxVSize/Mega * vsfac) : NA_REAL;
    if (reset_max){
	    R_N_maxused = onsize - R_Collected;
	    R_V_maxused = R_VSize - VHEAP_FREE();
    }
    REAL(value)[10] = R_N_maxused;
    REAL(value)[11] = R_V_maxused;
    REAL(value)[12] = 0.1*ceil(10. * R_N_maxused/Mega*sizeof(RObject));
    REAL(value)[13] = 0.1*ceil(10. * R_V_maxused/Mega*vsfac);
    UNPROTECT(1);
    return value;
}

NORET static void mem_err_heap(R_size_t size)
{
    if (R_MaxVSize == R_SIZE_T_MAX)
	errorcall(R_NilValue, "%s", _("vector memory exhausted"));
    else {
	double l = R_GetMaxVSize() / Kilo;
	const char *unit = "Kb";

	if (l > Mega) {
	    l /= Mega;
	    unit = "Gb";
	} else if (l > Kilo) {
	    l /= Kilo;
	    unit = "Mb";
	}
	errorcall(R_NilValue,
	          _("vector memory limit of %0.1f %s reached, see mem.maxVSize()"),
	          l, unit);
    }
}

NORET static void mem_err_cons(void)
{
    if (R_MaxNSize == R_SIZE_T_MAX)
        errorcall(R_NilValue, "%s", _("cons memory exhausted"));
    else
        errorcall(R_NilValue,
	          _("cons memory limit of %llu nodes reached, see mem.maxNSize()"),
	          (unsigned long long)R_MaxNSize);
}

static double gctimes[5], gcstarttimes[5];
static bool s_gctime_enabled = FALSE;

/* this is primitive */
attribute_hidden SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if (args == R_NilValue)
	s_gctime_enabled = TRUE;
    else {
	check1arg(args, call, "on");
	s_gctime_enabled = asLogical(CAR(args));
    }
    SEXP ans = allocVector(REALSXP, 5);
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

NORET static void mem_err_malloc(R_size_t size)
{
    errorcall(R_NilValue, "%s", _("memory exhausted"));
}

/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

#define PP_REDZONE_SIZE 1000L
static R_size_t R_StandardPPStackSize, R_RealPPStackSize;

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
    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    R_PPStack.reserve(R_RealPPStackSize);
    // if (!(R_PPStack = (SEXP *) malloc(R_RealPPStackSize * sizeof(SEXP))))
	// R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;
#if VALGRIND_LEVEL > 1
    // VALGRIND_MAKE_MEM_NOACCESS(R_PPStack+R_PPStackSize, PP_REDZONE_SIZE);
#endif
    vsfac = sizeof(VECREC);
    R_VSize = (R_VSize + 1)/vsfac;
    if (R_MaxVSize < R_SIZE_T_MAX) R_MaxVSize = (R_MaxVSize + 1)/vsfac;

    UNMARK_NODE(&UnmarkedNodeTemplate);

    for (int i = 0; i < NUM_NODE_CLASSES; i++) {
      for (int gen = 0; gen < GCNode::numOldGenerations(); gen++) {
	R_GenHeap[i].Old[gen] = &R_GenHeap[i].OldPeg[gen];
	LINK_NODE(R_GenHeap[i].Old[gen], R_GenHeap[i].Old[gen]);

#ifndef EXPEL_OLD_TO_NEW
	R_GenHeap[i].OldToNew[gen] = &R_GenHeap[i].OldToNewPeg[gen];
	LINK_NODE(R_GenHeap[i].OldToNew[gen], R_GenHeap[i].OldToNew[gen]);
#endif

	R_GenHeap[i].OldCount[gen] = 0;
      }
      R_GenHeap[i].New = &R_GenHeap[i].NewPeg;
      LINK_NODE(R_GenHeap[i].New, R_GenHeap[i].New);
    }

    for (int i = 0; i < NUM_NODE_CLASSES; i++)
	R_GenHeap[i].Free = NEXT_NODE(R_GenHeap[i].New);

    SET_NODE_CLASS(&UnmarkedNodeTemplate, 0);
    orig_R_NSize = R_NSize;
    orig_R_VSize = R_VSize;

    /* R_NilValue */
    /* THIS MUST BE THE FIRST CONS CELL ALLOCATED */
    /* OR ARMAGEDDON HAPPENS. */
    /* Field assignments for R_NilValue must not go through write barrier
       since the write barrier prevents assignments to R_NilValue's fields.
       because of checks for nil */
    GET_FREE_NODE(R_NilValue);
    R_NilValue->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(R_NilValue);
    SET_REFCNT(R_NilValue, REFCNTMAX);
    SET_TYPEOF(R_NilValue, NILSXP);
    CAR0(R_NilValue) = R_NilValue;
    CDR(R_NilValue) = R_NilValue;
    TAG(R_NilValue) = R_NilValue;
    ATTRIB(R_NilValue) = R_NilValue;
    MARK_NOT_MUTABLE(R_NilValue);

    R_BCNodeStackBase =
	(R_bcstack_t *) malloc(R_BCNODESTACKSIZE * sizeof(R_bcstack_t));
    if (R_BCNodeStackBase == NULL)
	R_Suicide("couldn't allocate node stack");
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
    R_BCProtTop = R_BCNodeStackTop;

    s_R_weak_refs.clear();

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;

    /*  The current source line */
    R_Srcref = R_NilValue;

    /* R_TrueValue and R_FalseValue */
    R_TrueValue = mkTrue();
    MARK_NOT_MUTABLE(R_TrueValue);
    R_FalseValue = mkFalse();
    MARK_NOT_MUTABLE(R_FalseValue);
    R_LogicalNAValue = allocVector(LGLSXP, 1);
    LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
    MARK_NOT_MUTABLE(R_LogicalNAValue);
}

/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector. */
void *vmaxget(void)
{
    return (void *) R_VStack;
}

void vmaxset(const void *ovmax)
{
    R_VStack = (SEXP) ovmax;
}

char *R_alloc(size_t num_elts, int elt_size)
{
    R_size_t size = num_elts * elt_size;
    /* doubles are a precaution against integer overflow on 32-bit */
    double dsize = (double) num_elts * elt_size;
    if (dsize > 0) {
#ifdef LONG_VECTOR_SUPPORT
	/* 64-bit platform: previous version used REALSXPs */
	if(dsize > (double)R_XLEN_T_MAX)  /* currently 4096 TB */
	    error(_("cannot allocate memory block of size %0.f Tb"),
		  dsize/(Giga * 1024.0));
#else
	if(dsize > (double)R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/Giga);
#endif
	SEXP s = allocVector(RAWSXP, size + 1);
	ATTRIB(s) = R_VStack;
	R_VStack = s;
	return (char *) DATAPTR(s);
    }
    /* One programmer has relied on this, but it is undocumented! */
    else return NULL;
}

#ifdef HAVE_STDALIGN_H
# include <stdalign.h>
#endif

#include <cstdint>

long double *R_allocLD(size_t num_elts)
{
#if defined(__cplusplus) || defined(__alignof_is_defined)
    // This is C11: picky compilers may warn.
    size_t ld_align = alignof(long double);
#elif __GNUC__
    // This is C99, but do not rely on it.
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

    if(p) memset(p, 0, size);
    return p;
}


char *S_realloc(char *prev_block, long new_sz, long old_sz, int elt_size)
{
    /* shrinking is a no-op */
    if(new_sz <= old_sz) return prev_block; // so new_sz > 0 below
    char *q = R_alloc((size_t)new_sz, elt_size);
    size_t old_bytes = (size_t)old_sz * elt_size;
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
    SEXP s;
    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	GCManager::gc(0);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }
    GET_FREE_NODE(s);
    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(s);
    SET_TYPEOF(s, t);
    CAR0(s) = R_NilValue;
    CDR(s) = R_NilValue;
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

static SEXP allocSExpNonCons(SEXPTYPE t)
{
    SEXP s;
    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	GCManager::gc(0);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }
    GET_FREE_NODE(s);
    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(s);
    SET_TYPEOF(s, t);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP Rf_cons(SEXP car, SEXP cdr)
{
    SEXP s;
    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	PROTECT(car);
	PROTECT(cdr);
	GCManager::gc(0);
	UNPROTECT(2);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }

    if (NEED_NEW_PAGE()) {
	PROTECT(car);
	PROTECT(cdr);
	GET_FREE_NODE(s);
	UNPROTECT(2);
    }
    else
	QUICK_GET_FREE_NODE(s);

    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(s);
    SET_TYPEOF(s, LISTSXP);
    CAR0(s) = CHK(car); if (car) INCREMENT_REFCNT(car);
    CDR(s) = CHK(cdr); if (cdr) INCREMENT_REFCNT(cdr);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

attribute_hidden SEXP R::CONS_NR(SEXP car, SEXP cdr)
{
    SEXP s;
    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	PROTECT(car);
	PROTECT(cdr);
	GCManager::gc(0);
	UNPROTECT(2);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }

    if (NEED_NEW_PAGE()) {
	PROTECT(car);
	PROTECT(cdr);
	GET_FREE_NODE(s);
	UNPROTECT(2);
    }
    else
	QUICK_GET_FREE_NODE(s);

    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(s);
    DISABLE_REFCNT(s);
    SET_TYPEOF(s, LISTSXP);
    CAR0(s) = CHK(car);
    CDR(s) = CHK(cdr);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
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
    SEXP v, n, newrho;

    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	PROTECT(namelist);
	PROTECT(valuelist);
	PROTECT(rho);
	GCManager::gc(0);
	UNPROTECT(3);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }

    if (NEED_NEW_PAGE()) {
	PROTECT(namelist);
	PROTECT(valuelist);
	PROTECT(rho);
	GET_FREE_NODE(newrho);
	UNPROTECT(3);
    }
    else
	QUICK_GET_FREE_NODE(newrho);

    newrho->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(newrho);
    SET_TYPEOF(newrho, ENVSXP);
    FRAME(newrho) = valuelist; INCREMENT_REFCNT(valuelist);
    ENCLOS(newrho) = CHK(rho); if (rho != NULL) INCREMENT_REFCNT(rho);
    HASHTAB(newrho) = R_NilValue;
    ATTRIB(newrho) = R_NilValue;

    v = CHK(valuelist);
    n = CHK(namelist);
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }
    return (newrho);
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
attribute_hidden SEXP R::mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP s;
    if (GCManager::FORCE_GC() || NO_FREE_NODES()) {
	PROTECT(expr);
	PROTECT(rho);
	GCManager::gc(0);
	UNPROTECT(2);
	if (NO_FREE_NODES())
	    mem_err_cons();
    }

    if (NEED_NEW_PAGE()) {
	PROTECT(expr);
	PROTECT(rho);
	GET_FREE_NODE(s);
	UNPROTECT(2);
    }
    else
	QUICK_GET_FREE_NODE(s);

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    ENSURE_NAMEDMAX(expr);

    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
    INIT_REFCNT(s);
    SET_TYPEOF(s, PROMSXP);
    PRCODE(s) = CHK(expr); INCREMENT_REFCNT(expr);
    PRENV(s) = CHK(rho); INCREMENT_REFCNT(rho);
    PRVALUE0(s) = R_UnboundValue;
    PRSEEN(s) = DEFAULT;
    ATTRIB(s) = R_NilValue;
    return s;
}

SEXP R::R_mkEVPROMISE(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    SET_PRVALUE(prom, val);
    return prom;
}

attribute_hidden SEXP R::R_mkEVPROMISE_NR(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    DISABLE_REFCNT(prom);
    SET_PRVALUE(prom, val);
    return prom;
}

/* support for custom allocators that allow vectors to be allocated
   using non-standard means such as COW mmap() */

static void *custom_node_alloc(R_allocator_t *allocator, size_t size) {
    if (!allocator || !allocator->mem_alloc) return NULL;
    void *ptr = allocator->mem_alloc(allocator, size + sizeof(R_allocator_t));
    if (ptr) {
	R_allocator_t *ca = (R_allocator_t*) ptr;
	*ca = *allocator;
	return (void*) (ca + 1);
    }
    return NULL;
}

static void custom_node_free(void *ptr) {
    if (ptr) {
	R_allocator_t *allocator = ((R_allocator_t*) ptr) - 1;
	allocator->mem_free(allocator, (void*)allocator);
    }
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

SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t length, R_allocator_t *allocator)
{
    SEXP s;     /* For the generational collector it would be safer to
		   work in terms of a VECSXP here, but that would
		   require several casts below... */
    R_size_t size = 0, alloc_size, old_R_VSize;
    int node_class;
#if VALGRIND_LEVEL > 0
    R_size_t actual_size = 0;
#endif

    /* Handle some scalars directly to improve speed. */
    if (length == 1) {
	switch(type) {
	case REALSXP:
	case INTSXP:
	case LGLSXP:
	    node_class = 1;
	    alloc_size = NodeClassSize[1];
	    if (GCManager::FORCE_GC() || NO_FREE_NODES() || VHEAP_FREE() < alloc_size) {
		GCManager::gc(alloc_size);
		if (NO_FREE_NODES())
		    mem_err_cons();
		if (VHEAP_FREE() < alloc_size)
		    mem_err_heap(size);
	    }

	    CLASS_GET_FREE_NODE(node_class, s);
#if VALGRIND_LEVEL > 1
	    switch(type) {
	    case REALSXP: actual_size = sizeof(double); break;
	    case INTSXP: actual_size = sizeof(int); break;
	    case LGLSXP: actual_size = sizeof(int); break;
	    default: break;
	    }
	    VALGRIND_MAKE_MEM_UNDEFINED(STDVEC_DATAPTR(s), actual_size);
#endif
	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	    SETSCALAR(s, 1);
	    SET_NODE_CLASS(s, node_class);
	    R_SmallVallocSize += alloc_size;
	    /* Note that we do not include the header size into VallocSize,
	       but it is counted into memory usage via GCNode::s_num_nodes. */
	    ATTRIB(s) = R_NilValue;
	    SET_TYPEOF(s, type);
	    SET_STDVEC_LENGTH(s, (R_len_t) length); // is 1
	    SET_STDVEC_TRUELENGTH(s, 0);
	    INIT_REFCNT(s);
	    return(s);
	default: break;
	}
    }

    if (length > R_XLEN_T_MAX)
	error("%s", _("vector is too large")); /**** put length into message */
    else if (length < 0 )
	error("%s", _("negative length vectors are not allowed"));
    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
	return R_NilValue;
    case RAWSXP:
	size = BYTE2VEC(length);
#if VALGRIND_LEVEL > 0
	actual_size = length;
#endif
	break;
    case CHARSXP:
	error("use of allocVector(CHARSXP ...) is defunct\n");
    case intCHARSXP:
	type = CHARSXP;
	size = BYTE2VEC(length + 1);
#if VALGRIND_LEVEL > 0
	actual_size = length + 1;
#endif
	break;
    case LGLSXP:
    case INTSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(int)))
		error(_("cannot allocate vector of length %lld"),
		      (long long)length);
	    size = INT2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length*sizeof(int);
#endif
	}
	break;
    case REALSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(double)))
		error(_("cannot allocate vector of length %lld"),
		      (long long)length);
	    size = FLOAT2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(double);
#endif
	}
	break;
    case CPLXSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(Complex)))
		error(_("cannot allocate vector of length %lld"),
		      (long long)length);
	    size = COMPLEX2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(Complex);
#endif
	}
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	if (length <= 0)
	    size = 0;
	else {
	    if (length > (R_xlen_t) (R_SIZE_T_MAX / sizeof(SEXP)))
		error(_("cannot allocate vector of length %lld"),
		      (long long)length);
	    size = PTR2VEC(length);
#if VALGRIND_LEVEL > 0
	    actual_size = length * sizeof(SEXP);
#endif
	}
	break;
    case LANGSXP:
	if(length == 0) return R_NilValue;
#ifdef LONG_VECTOR_SUPPORT
	if (length > R_SHORT_LEN_MAX) error("invalid length for pairlist");
#endif
	s = allocList((int) length);
	SET_TYPEOF(s, LANGSXP);
	return s;
    case LISTSXP:
#ifdef LONG_VECTOR_SUPPORT
	if (length > R_SHORT_LEN_MAX) error("invalid length for pairlist");
#endif
	return allocList((int) length);
    default:
	error(_("invalid type/length (%s/%lld) in vector allocation"),
	      type2char(type), (long long)length);
    }

    if (allocator) {
	node_class = CUSTOM_NODE_CLASS;
	alloc_size = size;
    } else {
	if (size <= NodeClassSize[1]) {
	    node_class = 1;
	    alloc_size = NodeClassSize[1];
	}
	else {
	    node_class = LARGE_NODE_CLASS;
	    alloc_size = size;
	    for (int i = 2; i < NUM_SMALL_NODE_CLASSES; i++) {
		if (size <= NodeClassSize[i]) {
		    node_class = i;
		    alloc_size = NodeClassSize[i];
		    break;
		}
	    }
	}
    }

    /* save current R_VSize to roll back adjustment if malloc fails */
    old_R_VSize = R_VSize;

    /* we need to do the gc here so allocSExp doesn't! */
    if (GCManager::FORCE_GC() || NO_FREE_NODES() || VHEAP_FREE() < alloc_size) {
	GCManager::gc(alloc_size);
	if (NO_FREE_NODES())
	    mem_err_cons();
	if (VHEAP_FREE() < alloc_size)
	    mem_err_heap(size);
    }

    if (size > 0) {
	if (node_class < NUM_SMALL_NODE_CLASSES) {
	    CLASS_GET_FREE_NODE(node_class, s);
#if VALGRIND_LEVEL > 1
	    VALGRIND_MAKE_MEM_UNDEFINED(STDVEC_DATAPTR(s), actual_size);
#endif
	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	    INIT_REFCNT(s);
	    SET_NODE_CLASS(s, node_class);
	    R_SmallVallocSize += alloc_size;
	    SET_STDVEC_LENGTH(s, (R_len_t) length);
	}
	else {
	    bool success = FALSE;
	    R_size_t hdrsize = sizeof(VectorBase);
	    void *mem = NULL; /* initialize to suppress warning */
	    if (size < (R_SIZE_T_MAX / sizeof(VECREC)) - hdrsize) { /*** not sure this test is quite right -- why subtract the header? LT */
		/* I think subtracting the header is fine, "size" (*VSize)
		   variables do not count the header, but the header is
		   included into memory usage via NodesInUse, instead.
		   We want the whole object including the header to be
		   indexable by size_t. - TK */
		mem = allocator ?
		    custom_node_alloc(allocator, hdrsize + size * sizeof(VECREC)) :
		    malloc(hdrsize + size * sizeof(VECREC));
		if (mem == NULL) {
		    /* If we are near the address space limit, we
		       might be short of address space.  So return
		       all unused objects to malloc and try again. */
		    R_gc_no_finalizers(alloc_size);
		    mem = allocator ?
			custom_node_alloc(allocator, hdrsize + size * sizeof(VECREC)) :
			malloc(hdrsize + size * sizeof(VECREC));
		}
		if (mem != NULL) {
#if 1
		    s = (SEXP) mem;
		    LINK_NODE(s, s);
#else
		    s = new (mem) RObject(type);
		    // ((VectorBase *)(s))->vecsxp.m_data = (((char *)mem) + hdrsize);
#endif
		    SET_STDVEC_TRUELENGTH(s, 0);
		    SET_STDVEC_LENGTH(s, length);
		    success = TRUE;
		}
		else s = NULL;
#ifdef R_MEMORY_PROFILING
		R_ReportAllocation(hdrsize + size * sizeof(VECREC));
#endif
	    } else s = NULL; /* suppress warning */
	    if (! success) {
		double dsize = (double)size * sizeof(VECREC)/1024.0;
		/* reset the vector heap limit */
		R_VSize = old_R_VSize;
		if(dsize > 1024.0*1024.0)
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.1f Gb"),
			      dsize/1024.0/1024.0);
		if(dsize > 1024.0)
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.1f Mb"),
			      dsize/1024.0);
		else
		    errorcall(R_NilValue,
			      _("cannot allocate vector of size %0.f Kb"),
			      dsize);
	    }
	    s->sxpinfo = UnmarkedNodeTemplate.sxpinfo;
	    INIT_REFCNT(s);
	    SET_NODE_CLASS(s, node_class);
	    if (!allocator) R_LargeVallocSize += size;
	    R_GenHeap[node_class].AllocCount++;
	    GCNode::s_num_nodes++;
	    SNAP_NODE(s, R_GenHeap[node_class].New);
	}
	ATTRIB(s) = R_NilValue;
	SET_TYPEOF(s, type);
    }
    else {
	GC_PROT(s = allocSExpNonCons(type));
	SET_STDVEC_LENGTH(s, (R_len_t) length);
    }
    SETALTREP(s, 0);
    SET_STDVEC_TRUELENGTH(s, 0);
    INIT_REFCNT(s);

    /* The following prevents disaster in the case */
    /* that an uninitialised string vector is marked */
    /* Direct assignment is OK since the node was just allocated and */
    /* so is at least as new as R_NilValue and R_BlankString */
    if (type == EXPRSXP || type == VECSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_MEM_DEFINED(STRING_PTR(s), actual_size);
#endif
	for (R_xlen_t i = 0; i < length; i++)
	    data[i] = R_NilValue;
    }
    else if(type == STRSXP) {
	SEXP *data = STRING_PTR(s);
#if VALGRIND_LEVEL > 1
	VALGRIND_MAKE_MEM_DEFINED(STRING_PTR(s), actual_size);
#endif
	for (R_xlen_t i = 0; i < length; i++)
	    data[i] = R_BlankString;
    }
    else if (type == CHARSXP || type == intCHARSXP) {
#if VALGRIND_LEVEL > 0
	VALGRIND_MAKE_MEM_UNDEFINED(CHAR(s), actual_size);
#endif
	CHAR_RW(s)[length] = 0;
    }
#if VALGRIND_LEVEL > 0
    else if (type == REALSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(REAL(s), actual_size);
    else if (type == INTSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(INTEGER(s), actual_size);
    else if (type == LGLSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(LOGICAL(s), actual_size);
    else if (type == CPLXSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(COMPLEX(s), actual_size);
    else if (type == RAWSXP)
	VALGRIND_MAKE_MEM_UNDEFINED(RAW(s), actual_size);
#endif
    return s;
}

/* For future hiding of allocVector(CHARSXP) */
attribute_hidden SEXP R::allocCharsxp(R_len_t len)
{
    return allocVector(intCHARSXP, len);
}

SEXP Rf_allocList(int n)
{
    SEXP result = R_NilValue;
    for (int i = 0; i < n; i++)
	result = CONS(R_NilValue, result);
    return result;
}

SEXP Rf_allocS4Object(void)
{
   SEXP s;
   GC_PROT(s = allocSExpNonCons(OBJSXP));
   SET_S4_OBJECT(s);
   return s;
}

attribute_hidden SEXP R::R_allocObject(void)
{
   SEXP s;
   GC_PROT(s = allocSExpNonCons(OBJSXP));
   return s;
}

static SEXP allocFormalsList(int nargs, ...)
{
    SEXP res = R_NilValue;
    SEXP n;
    va_list syms;
    va_start(syms, nargs);

    for (int i = 0; i < nargs; i++) {
	res = CONS(R_NilValue, res);
    }
    R_PreserveObject(res);

    n = res;
    for (int i = 0; i < nargs; i++) {
	SET_TAG(n, (SEXP) va_arg(syms, SEXP));
	MARK_NOT_MUTABLE(n);
	n = CDR(n);
    }
    va_end(syms);

    return res;
}


SEXP R::allocFormalsList2(SEXP sym1, SEXP sym2)
{
    return allocFormalsList(2, sym1, sym2);
}

SEXP R::allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3)
{
    return allocFormalsList(3, sym1, sym2, sym3);
}

SEXP R::allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4)
{
    return allocFormalsList(4, sym1, sym2, sym3, sym4);
}

SEXP R::allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5)
{
    return allocFormalsList(5, sym1, sym2, sym3, sym4, sym5);
}

SEXP R::allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4,
		       SEXP sym5, SEXP sym6)
{
    return allocFormalsList(6, sym1, sym2, sym3, sym4, sym5, sym6);
}

/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    GCManager::gc(0, true);
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif
}

void R_gc_lite(void)
{
    GCManager::gc(0);
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif
}

static void R_gc_no_finalizers(R_size_t size_needed)
{
    GCManager::gc(size_needed, true);
}

#ifdef THREADCHECK
# if !defined(Win32) && defined(HAVE_PTHREAD)
#   include <pthread.h>
attribute_hidden void R::R_check_thread(const char *s)
{
    static bool s_main_thread_inited = FALSE;
    static pthread_t main_thread;
    if (!s_main_thread_inited) {
        main_thread = pthread_self();
        s_main_thread_inited = TRUE;
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
      if (NO_FREE_NODES())
	R_NSize = GCNode::s_num_nodes + 1;

      if (!force_full_collection &&
	  VHEAP_FREE() < size_needed + R_MinFreeFrac * R_VSize)
	force_full_collection = true;

      if (size_needed > VHEAP_FREE()) {
	  R_size_t expand = size_needed - VHEAP_FREE();
	  if (R_VSize + expand > R_MaxVSize)
	      mem_err_heap(size_needed);
	  R_VSize += expand;
      }

      s_gc_pending = TRUE;
      return;
    }
    s_gc_pending = FALSE;

    R_size_t onsize = R_NSize /* can change during collection */;
    double ncells, vcells, vfrac, nfrac;
    BadObject bad_object;
    unsigned int gens_collected = 0;

#ifdef IMMEDIATE_FINALIZERS
    bool first = TRUE;
    bool ok = false;
    while (!ok) {
    ok = true;
#endif

    ++s_gc_count;

    R_N_maxused = std::max(R_N_maxused, GCNode::s_num_nodes);
    R_V_maxused = std::max(R_V_maxused, R_VSize - VHEAP_FREE());

    BEGIN_SUSPEND_INTERRUPTS {
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
	R_checkConstants(TRUE);

    if (s_os) {
	REprintf("Garbage collection %d = %d", s_gc_count, s_gen_gc_counts[0]);
	for (unsigned int i = 0; i < GCNode::numOldGenerations(); i++)
	    REprintf("+%d", s_gen_gc_counts[i + 1]);
	REprintf(" (level %d) ... ", gens_collected);
	DEBUG_GC_SUMMARY(gens_collected == GCNode::numOldGenerations());

	ncells = onsize - R_Collected;
	nfrac = (100.0 * ncells) / R_NSize;
	/* We try to make this consistent with the results returned by gc */
	ncells = 0.1*ceil(10*ncells * sizeof(RObject)/Mega);
	REprintf("\n%.1f Mbytes of cons cells used (%d%%)\n", ncells, (int) (nfrac + 0.5));
	vcells = R_VSize - VHEAP_FREE();
	vfrac = (100.0 * vcells) / R_VSize;
	vcells = 0.1*ceil(10*vcells * vsfac/Mega);
	REprintf("%.1f Mbytes of vectors used (%d%%)\n", vcells, (int) (vfrac + 0.5));
    }

    if (!BadObject::s_firstBadObject.isEmpty()) {
        bad_object = BadObject::s_firstBadObject;
    }

#ifdef IMMEDIATE_FINALIZERS
    if (first) {
	first = FALSE;
	/* Run any eligible finalizers.  The return result of
	   RunFinalizers is TRUE if any finalizers are actually run.
	   There is a small chance that running finalizers here may
	   chew up enough memory to make another immediate collection
	   necessary.  If so, we jump back to the beginning and run
	   the collection, but on this second pass we do not run
	   finalizers. */
	if (RunFinalizers() &&
	    (NO_FREE_NODES() || size_needed > VHEAP_FREE()))
	    ok = false;
    }
    } // end of while loop
#endif

    bad_object.printSummary();

    /* sanity check on logical scalar values */
    if (R_TrueValue != NULL && LOGICAL(R_TrueValue)[0] != TRUE) {
	LOGICAL(R_TrueValue)[0] = TRUE;
	gc_error("internal TRUE value has been modified");
    }
    if (R_FalseValue != NULL && LOGICAL(R_FalseValue)[0] != FALSE) {
	LOGICAL(R_FalseValue)[0] = FALSE;
	gc_error("internal FALSE value has been modified");
    }
    if (R_LogicalNAValue != NULL &&
	LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL) {
	LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
	gc_error("internal logical NA value has been modified");
    }
}


attribute_hidden SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    int tmp;

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 24));
    PROTECT(nms = allocVector(STRSXP, 24));
    for (unsigned int i = 0; i < 24; i++) {
	INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str((SEXPTYPE) (i > LGLSXP? i+2 : i)));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {

      /* run a full GC to make sure that all stuff in use is in Old space */
      R_gc();
      for (unsigned int gen = 0; gen < GCNode::numOldGenerations(); gen++) {
	for (int i = 0; i < NUM_NODE_CLASSES; i++) {
	  for (GCNode *s = NEXT_NODE(R_GenHeap[i].Old[gen]);
	       s != R_GenHeap[i].Old[gen];
	       s = NEXT_NODE(s)) {
	      tmp = TYPEOF(s);
	      if(tmp > LGLSXP) tmp -= 2;
	      INTEGER(ans)[tmp]++;
	  }
	}
      }
    } END_SUSPEND_INTERRUPTS;
    UNPROTECT(2);
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

static void reset_pp_stack(void *data)
{
    size_t *poldpps = (size_t *) data;
    R_PPStackSize =  *poldpps;
}

NORET void R::R_signal_protect_error(void)
{
    RCNTXT cntxt;
    size_t oldpps = R_PPStackSize;

    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &reset_pp_stack;
    cntxt.cenddata = &oldpps;

    /* condition is pre-allocated and protected with R_PreserveObject */
    SEXP cond = R_getProtectStackOverflowError();

    if (R_PPStackSize < R_RealPPStackSize) {
	R_PPStackSize = R_RealPPStackSize;
	/* allow calling handlers */
	R_signalErrorCondition(cond, R_NilValue);
    }

    /* calling handlers at this point might produce a C stack
       overflow/SEGFAULT so treat them as failed and skip them */
    R_signalErrorConditionEx(cond, R_NilValue, TRUE);

    endcontext(&cntxt); /* not reached */
}

NORET void R::R_signal_unprotect_error(void)
{
    error(ngettext("unprotect(): only %td protected item",
		   "unprotect(): only %td protected items", R_PPStackTop),
	  R_PPStackTop);
}

#ifndef INLINE_PROTECT
SEXP Rf_protect(SEXP s)
{
    R_CHECK_THREAD;
    if (R_PPStackTop >= R_PPStackSize)
	R_signal_protect_error();
    R_PPStack[R_PPStackTop++] = CHK(s);
    return s;
}


/* "unprotect" pop argument list from top of R_PPStack */

void Rf_unprotect(unsigned int l)
{
    R_CHECK_THREAD;
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else R_signal_unprotect_error();
}
#endif

/* "Rf_unprotect_ptr" remove pointer from somewhere in R_PPStack */

void Rf_unprotect_ptr(SEXP s)
{
    R_CHECK_THREAD;
    size_t i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
	if (i == 0)
	    error("%s", _("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it, if any */

    while (++i < R_PPStackTop) R_PPStack[i - 1] = R_PPStack[i];

    R_PPStackTop--;
}

/* Debugging function:  is s protected? */

int Rf_isProtected(SEXP s)
{
    R_CHECK_THREAD;
    size_t i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    do {
	if (i == 0)
	    return(i);
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    return(i);
}


#ifndef INLINE_PROTECT
void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    protect(s);
    *pi = R_PPStackTop - 1;
}
#endif

NORET void R::R_signal_reprotect_error(PROTECT_INDEX i)
{
    error(ngettext("R_Reprotect: only %td protected item, can't reprotect index %d",
		   "R_Reprotect: only %td protected items, can't reprotect index %d",
		   R_PPStackTop),
	  R_PPStackTop, i);
}

#ifndef INLINE_PROTECT
void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    R_CHECK_THREAD;
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}
#endif

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
    res = protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    R_PPStackTop = top; /* this includes the protect we used above */
    return res;
}
#endif

/* "initStack" initialize environment stack */
attribute_hidden
void initStack(void)
{
    R_PPStackTop = 0;
}


/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p)
	error(_("'R_Calloc' could not allocate memory (%llu of %llu bytes)"),
	      (unsigned long long)nelem, (unsigned long long)elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if(ptr) p = realloc(ptr, size); else p = malloc(size);
    if(!p)
	error(_("'R_Realloc' could not re-allocate memory (%llu bytes)"),
	      (unsigned long long)size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. */

static SEXP DeleteFromList(SEXP object, SEXP list)
{
    if (CAR(list) == object)
	return CDR(list);
    else {
	SEXP last = list;
	for (SEXP head = CDR(list); head != R_NilValue; head = CDR(head)) {
	    if (CAR(head) == object) {
		SETCDR(last, CDR(head));
		return list;
	    }
	    else last = head;
	}
	return list;
    }
}

#define ALLOW_PRECIOUS_HASH
#ifdef ALLOW_PRECIOUS_HASH
/* This allows using a fixed size hash table. This makes deleting much
   more efficient for applications that don't follow the "sparing use"
   advice in R-exts.texi. Using the hash table is enabled by starting
   R with the environment variable R_HASH_PRECIOUS set.

   Pointer hashing as used here isn't entirely portable (we do it in
   at least one other place, in serialize.c) but it could be made so
   by computing a unique value based on the allocation page and
   position in the page. */

#define PHASH_SIZE 1069
#define PTRHASH(obj) (((R_size_t) (obj)) >> 3)

static bool use_precious_hash = FALSE;
static bool precious_inited = FALSE;

void R_PreserveObject(SEXP object)
{
    R_CHECK_THREAD;
    if (!precious_inited) {
	precious_inited = TRUE;
	if (getenv("R_HASH_PRECIOUS"))
	    use_precious_hash = TRUE;
    }
    if (use_precious_hash) {
	if (R_PreciousList == R_NilValue)
	    R_PreciousList = allocVector(VECSXP, PHASH_SIZE);
	int bin = PTRHASH(object) % PHASH_SIZE;
	SET_VECTOR_ELT(R_PreciousList, bin,
		       CONS(object, VECTOR_ELT_0(R_PreciousList, bin)));
    }
    else
	R_PreciousList = CONS(object, R_PreciousList);
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    if (!precious_inited)
	return; /* can't be anything to delete yet */
    if (use_precious_hash) {
	int bin = PTRHASH(object) % PHASH_SIZE;
	SET_VECTOR_ELT(R_PreciousList, bin,
		       DeleteFromList(object,
				      VECTOR_ELT_0(R_PreciousList, bin)));
    }
    else
	R_PreciousList =  DeleteFromList(object, R_PreciousList);
}
#else
void R_PreserveObject(SEXP object)
{
    R_CHECK_THREAD;
    R_PreciousList = CONS(object, R_PreciousList);
}

void R_ReleaseObject(SEXP object)
{
    R_CHECK_THREAD;
    R_PreciousList =  DeleteFromList(object, R_PreciousList);
}
#endif


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
    SEXP npreserved, mset, isize;

    /* npreserved is modified in place */
    npreserved = allocVector(INTSXP, 1);
    SET_INTEGER_ELT(npreserved, 0, 0);
    PROTECT(mset = CONS(R_NilValue, npreserved));
    /* isize is not modified in place */
    if (initialSize < 0)
	error("'initialSize' must be non-negative");
    isize = ScalarInteger(initialSize);
    SET_TAG(mset, isize);
    UNPROTECT(1); /* mset */
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

	error("Invalid mset");
}

/* Add object to multi-set. The object will be protected as long as the
   multi-set is protected. */
void R_PreserveInMSet(SEXP x, SEXP mset)
{
    if (x == R_NilValue || isSymbol(x))
	return; /* no need to preserve */
    PROTECT(x);
    checkMSet(mset);
    SEXP store = CAR(mset);
    int *n = INTEGER(CDR(mset));
    if (store == R_NilValue) {
	R_xlen_t newsize = INTEGER_ELT(TAG(mset), 0);
	if (newsize == 0)
	    newsize = 4; /* default minimum size */
	store = allocVector(VECSXP, newsize);
	SETCAR(mset, store);
    }
    R_xlen_t size = XLENGTH(store);
    if (*n == size) {
	R_xlen_t newsize = 2 * size;
	if (newsize >= INT_MAX || newsize < size)
	    error("Multi-set overflow");
	SEXP newstore = PROTECT(allocVector(VECSXP, newsize));
	for(R_xlen_t i = 0; i < size; i++)
	    SET_VECTOR_ELT(newstore, i, VECTOR_ELT_0(store, i));
	SETCAR(mset, newstore);
	UNPROTECT(1); /* newstore */
	store = newstore;
    }
    UNPROTECT(1); /* x */
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
    for(R_xlen_t i = (*n) - 1; i >= 0; i--) {
	if (VECTOR_ELT_0(store, i) == x) {
	    for(;i < (*n) - 1; i++)
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
void R_ReleaseMSet(SEXP mset, int keepSize)
{
    checkMSet(mset);
    SEXP store = CAR(mset);
    if (store == R_NilValue)
	return; /* already empty */
    int *n = INTEGER(CDR(mset));
    if (XLENGTH(store) <= keepSize) {
	/* just free the entries */
	for(R_xlen_t i = 0; i < *n; i++)
	    SET_VECTOR_ELT(store, i, R_NilValue);
    } else
	SETCAR(mset, R_NilValue);
    *n = 0;
}

/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = allocSExp(EXTPTRSXP);
    EXTPTR_PTR(s) = (SEXP) p;
    EXTPTR_PROT(s) = CHK(prot); if (prot) INCREMENT_REFCNT(prot);
    EXTPTR_TAG(s) = CHK(tag); if (tag) INCREMENT_REFCNT(tag);
    return s;
}

void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return CHK(EXTPTR_PROT(CHK(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    EXTPTR_PTR(s) = NULL;
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    EXTPTR_PTR(s) = (SEXP) p;
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    FIX_REFCNT(s, EXTPTR_TAG(s), tag);
    CHECK_OLD_TO_NEW(s, tag);
    EXTPTR_TAG(s) = tag;
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    FIX_REFCNT(s, EXTPTR_PROT(s), p);
    CHECK_OLD_TO_NEW(s, p);
    EXTPTR_PROT(s) = p;
}

/*
   Added to API in R 3.4.0.
   Work around casting issues: works where it is needed.
 */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    SEXP s = allocSExp(EXTPTRSXP);
    tmp.fn = p;
    EXTPTR_PTR(s) = (SEXP) (tmp.p);
    EXTPTR_PROT(s) = CHK(prot); if (prot) INCREMENT_REFCNT(prot);
    EXTPTR_TAG(s) = CHK(tag); if (tag) INCREMENT_REFCNT(tag);
    return s;
}

DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
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
int (OBJECT)(SEXP x) { return OBJECT(CHK(x)); }
int (MARK)(SEXP x) { return MARK(CHK(x)); }
SEXPTYPE (TYPEOF)(SEXP x) { return TYPEOF(CHK(x)); }
int (NAMED)(SEXP x) { return NAMED(CHK(x)); }
int (RTRACE)(SEXP x) { return RTRACE(CHK(x)); }
int (LEVELS)(SEXP x) { return LEVELS(CHK(x)); }
int (REFCNT)(SEXP x) { return REFCNT(CHK(x)); }
bool (R::REFCNT_ENABLED)(SEXP x) { return REFCNT_ENABLED(CHK(x)); }
int (ALTREP)(SEXP x) { return ALTREP(CHK(x)); }
int (IS_SCALAR)(SEXP x, SEXPTYPE type) { return IS_SCALAR(CHK(x), type); }
void (R::DECREMENT_REFCNT)(SEXP x) { DECREMENT_REFCNT(CHK(x)); }
void (R::INCREMENT_REFCNT)(SEXP x) { INCREMENT_REFCNT(CHK(x)); }
void (R::DISABLE_REFCNT)(SEXP x)  { DISABLE_REFCNT(CHK(x)); }
void (R::ENABLE_REFCNT)(SEXP x) { ENABLE_REFCNT(CHK(x)); }
void (MARK_NOT_MUTABLE)(SEXP x) { MARK_NOT_MUTABLE(CHK(x)); }
bool (R::ASSIGNMENT_PENDING)(SEXP x) { return ASSIGNMENT_PENDING(CHK(x)); }
void (R::SET_ASSIGNMENT_PENDING)(SEXP x, int v)
{
    SET_ASSIGNMENT_PENDING(CHK(x), v);
}
bool (R::IS_ASSIGNMENT_CALL)(SEXP x) { return IS_ASSIGNMENT_CALL(CHK(x)); }
void (R::MARK_ASSIGNMENT_CALL)(SEXP x) { MARK_ASSIGNMENT_CALL(CHK(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if(TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'",
	      R_typeToChar(v));
    FIX_REFCNT(x, ATTRIB(x), v);
    CHECK_OLD_TO_NEW(x, v);
    ATTRIB(x) = v;
}
void (SET_OBJECT)(SEXP x, int v) { SET_OBJECT(CHK(x), v); }
void (SET_TYPEOF)(SEXP x, SEXPTYPE v) { SET_TYPEOF(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v)
{
#ifndef SWITCH_TO_REFCNT
    SET_NAMED(CHK(x), v);
#endif
}
void (SET_RTRACE)(SEXP x, int v) { SET_RTRACE(CHK(x), v); }
void (SETLEVELS)(SEXP x, int v) { SETLEVELS(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    if (IS_S4_OBJECT(from)) { SET_S4_OBJECT(to); } else { UNSET_S4_OBJECT(to); }
}
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), shallow_duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    if (IS_S4_OBJECT(from)) { SET_S4_OBJECT(to); } else { UNSET_S4_OBJECT(to); }
}

void (R::ENSURE_NAMEDMAX)(SEXP x) { ENSURE_NAMEDMAX(CHK(x)); }
void (R::ENSURE_NAMED)(SEXP x) { ENSURE_NAMED(CHK(x)); }
void (R::SETTER_CLEAR_NAMED)(SEXP x) { SETTER_CLEAR_NAMED(CHK(x)); }
void (R::RAISE_NAMED)(SEXP x, int n) { RAISE_NAMED(CHK(x), n); }

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ SET_S4_OBJECT(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ UNSET_S4_OBJECT(CHK(x)); }

/* JIT optimization support */
bool (R::NOJIT)(SEXP x) { return NOJIT(CHK(x)); }
bool (R::MAYBEJIT)(SEXP x) { return MAYBEJIT(CHK(x)); }
void (R::SET_NOJIT)(SEXP x) { SET_NOJIT(CHK(x)); }
void (R::SET_MAYBEJIT)(SEXP x) { SET_MAYBEJIT(CHK(x)); }
void (R::UNSET_MAYBEJIT)(SEXP x) { UNSET_MAYBEJIT(CHK(x)); }

/* Growable vector support */
int (IS_GROWABLE)(SEXP x) { return IS_GROWABLE(CHK(x)); }
void (SET_GROWABLE_BIT)(SEXP x) { SET_GROWABLE_BIT(CHK(x)); }

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
        {WEAKREFSXP, false}, /* weak reference */
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
    if(not_a_vec[TYPEOF(x)])
	error("LENGTH or similar applied to %s object", R_typeToChar(x));
    return x;
}

/* Vector Accessors */
int (LENGTH)(SEXP x) { return x == R_NilValue ? 0 : LENGTH(CHK2(x)); }
R_xlen_t (XLENGTH)(SEXP x) { return XLENGTH(CHK2(x)); }
R_xlen_t (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK2(x)); }

void (SETLENGTH)(SEXP x, R_xlen_t v)
{
    if (ALTREP(x))
	error("SETLENGTH() cannot be applied to an ALTVEC object.");
    if (!isVector(x))
	error(_("SETLENGTH() can only be applied to a standard vector, not a '%s'"), R_typeToChar(x));
    SET_STDVEC_LENGTH(CHK2(x), v);
}

void (SET_TRUELENGTH)(SEXP x, R_xlen_t v) { SET_TRUELENGTH(CHK2(x), v); }
int  (IS_LONG_VEC)(SEXP x) { return IS_LONG_VEC(CHK2(x)); }
#ifdef TESTING_WRITE_BARRIER
R_xlen_t (R::STDVEC_LENGTH)(SEXP x) { return STDVEC_LENGTH(CHK2(x)); }
R_xlen_t (R::STDVEC_TRUELENGTH)(SEXP x) { return STDVEC_TRUELENGTH(CHK2(x)); }
void (R::SETALTREP)(SEXP x, int v) { SETALTREP(x, v); }
#endif

/* temporary, to ease transition away from remapping */
R_xlen_t Rf_XLENGTH(SEXP x) { return XLENGTH(x); }

const char *(R_CHAR)(SEXP x) {
    if(TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
	error("%s() can only be applied to a '%s', not a '%s'",
	      "CHAR", "CHARSXP", R_typeToChar(x));
    return (const char *) CHAR(CHK(x));
}

SEXP (STRING_ELT)(SEXP x, R_xlen_t i) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_ELT", "character vector", R_typeToChar(x));
    if (ALTREP(x))
	return CHK(ALTSTRING_ELT(CHK(x), i));
    else {
	SEXP *ps = (SEXP *) STDVEC_DATAPTR(CHK(x));
	return CHK(ps[i]);
    }
}

SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
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

#ifdef CATCH_ZERO_LENGTH_ACCESS
/* Attempts to read or write elements of a zero length vector will
   result in a segfault, rather than read and write random memory.
   Returning NULL would be more natural, but Matrix seems to assume
   that even zero-length vectors have non-NULL data pointers, so
   return (void *) 1 instead. Zero-length CHARSXP objects still have a
   trailing zero byte so they are not handled. */
# define CHKZLN(x, T) do {				   \
	CHK(x);						   \
	if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP) \
	    return (T *) 1;				   \
    } while (0)
#else
# define CHKZLN(x, T) do { } while (0)
#endif

void *(STDVEC_DATAPTR)(SEXP x)
{
    if (ALTREP(x))
	error("cannot get STDVEC_DATAPTR from ALTREP object");
    if (!isVector(x))
	error("STDVEC_DATAPTR can only be applied to a vector, not a '%s'",
	      R_typeToChar(x));
    CHKZLN(x, void);
    return STDVEC_DATAPTR(x);
}

int *(LOGICAL)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", R_typeToChar(x));
    CHKZLN(x, int);
    return LOGICAL(x);
}

const int *(LOGICAL_RO)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", R_typeToChar(x));
    CHKZLN(x, const int);
    return LOGICAL_RO(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *(INTEGER)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "INTEGER", "integer", R_typeToChar(x));
    CHKZLN(x, int);
    return INTEGER(x);
}

const int *(INTEGER_RO)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "INTEGER", "integer", R_typeToChar(x));
    CHKZLN(x, const int);
    return INTEGER_RO(x);
}

Rbyte *(RAW)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "RAW", "raw", R_typeToChar(x));
    CHKZLN(x, Rbyte);
    return RAW(x);
}

const Rbyte *(RAW_RO)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "RAW", "raw", R_typeToChar(x));
    CHKZLN(x, const Rbyte);
    return RAW(x);
}

double *(REAL)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "REAL", "numeric", R_typeToChar(x));
    CHKZLN(x, double);
    return REAL(x);
}

const double *(REAL_RO)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "REAL", "numeric", R_typeToChar(x));
    CHKZLN(x, const double);
    return REAL_RO(x);
}

Rcomplex *(COMPLEX)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "COMPLEX", "complex", R_typeToChar(x));
    CHKZLN(x, Rcomplex);
    return COMPLEX(x);
}

const Rcomplex *(COMPLEX_RO)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "COMPLEX", "complex", R_typeToChar(x));
    CHKZLN(x, const Rcomplex);
    return COMPLEX_RO(x);
}

SEXP *(STRING_PTR)(SEXP x) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_PTR", "character", R_typeToChar(x));
    CHKZLN(x, SEXP);
    return STRING_PTR(x);
}

const SEXP *(STRING_PTR_RO)(SEXP x) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_PTR_RO", "character", R_typeToChar(x));
    CHKZLN(x, const SEXP);
    return STRING_PTR_RO(x);
}

NORET SEXP * (VECTOR_PTR)(SEXP x)
{
  error("%s", _("not safe to return vector pointer"));
}

void (SET_STRING_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    if(TYPEOF(CHK(x)) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_STRING_ELT", "character vector", R_typeToChar(x));
    if(TYPEOF(CHK(v)) != CHARSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'",
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
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
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
	error("CAR/CDR/TAG or similar applied to %s object",
	      R_typeToChar(e));
    }
}
#else
#define CHKCONS(e) CHK(e)
#endif

attribute_hidden
SEXPTYPE (R::BNDCELL_TAG)(SEXP cell) { return BNDCELL_TAG(cell); }
attribute_hidden
void (R::SET_BNDCELL_TAG)(SEXP cell, SEXPTYPE val) { SET_BNDCELL_TAG(cell, val); }
attribute_hidden
double (R::BNDCELL_DVAL)(SEXP cell) { return BNDCELL_DVAL(cell); }
attribute_hidden
int (R::BNDCELL_IVAL)(SEXP cell) { return BNDCELL_IVAL(cell); }
attribute_hidden
int (R::BNDCELL_LVAL)(SEXP cell) { return BNDCELL_LVAL(cell); }
attribute_hidden
void (R::SET_BNDCELL_DVAL)(SEXP cell, double v) { SET_BNDCELL_DVAL(cell, v); }
attribute_hidden
void (R::SET_BNDCELL_IVAL)(SEXP cell, int v) { SET_BNDCELL_IVAL(cell, v); }
attribute_hidden
void (R::SET_BNDCELL_LVAL)(SEXP cell, int v) { SET_BNDCELL_LVAL(cell, v); }
attribute_hidden
void (R::INIT_BNDCELL)(SEXP cell, SEXPTYPE type) { INIT_BNDCELL(cell, type); }
attribute_hidden
SEXPTYPE (R::PROMISE_TAG)(SEXP cell) { return PROMISE_TAG(cell); }
attribute_hidden
void (R::SET_PROMISE_TAG)(SEXP cell, SEXPTYPE val) { SET_PROMISE_TAG(cell, val); }

#define CLEAR_BNDCELL_TAG(cell) do {		\
	if (BNDCELL_TAG(cell)) {		\
	    CAR0(cell) = R_NilValue;		\
	    SET_BNDCELL_TAG(cell, NILSXP);		\
	}					\
    } while (0)

attribute_hidden
void R::SET_BNDCELL(SEXP cell, SEXP val)
{
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
	    if (!REFCNT_ENABLED(CAR(a)))
		error("argument not tracking references");
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
void R::SET_WEAKREF_KEY(SEXP x, SEXP v) { FIX_REFCNT(x, WEAKREF_KEY(x), v); CHECK_OLD_TO_NEW(x, v); WEAKREF_KEY(x) = v; }
void R::SET_WEAKREF_VALUE(SEXP x, SEXP v) { FIX_REFCNT(x, WEAKREF_VALUE(x), v); CHECK_OLD_TO_NEW(x, v); WEAKREF_VALUE(x) = v; }
void R::SET_WEAKREF_FINALIZER(SEXP x, SEXP v) { FIX_REFCNT(x, WEAKREF_FINALIZER(x), v); CHECK_OLD_TO_NEW(x, v); WEAKREF_FINALIZER(x) = v; }

/* S4 Object Accessors */
SEXP (S4TAG)(SEXP e) { return CHK(S4TAG(CHKCONS(e))); }
void SET_S4TAG(SEXP x, SEXP v) { FIX_REFCNT(x, S4TAG(x), v); CHECK_OLD_TO_NEW(x, v); S4TAG(x) = v; }

/* AltRep Accessors */
SEXP (DATA1)(SEXP e) { return CHK(DATA1(CHKCONS(e))); }
SEXP (DATA2)(SEXP e) { return CHK(DATA2(CHKCONS(e))); }
SEXP (CLASS)(SEXP e) { return CHK(CLASS(CHKCONS(e))); }
void (SET_DATA1)(SEXP x, SEXP v) { FIX_REFCNT(x, DATA1(x), v); CHECK_OLD_TO_NEW(x, v); DATA1(x) = v; }
void (SET_DATA2)(SEXP x, SEXP v) { FIX_REFCNT(x, DATA2(x), v); CHECK_OLD_TO_NEW(x, v); DATA2(x) = v; }
void (SET_CLASS)(SEXP x, SEXP v) { FIX_REFCNT(x, CLASS(x), v); CHECK_OLD_TO_NEW(x, v); CLASS(x) = v; }

/* ByteCode Accessors */
SEXP (CODE0)(SEXP e) { return CHK(CODE0(CHKCONS(e))); }
SEXP (CONSTS)(SEXP e) { return CHK(CONSTS(CHKCONS(e))); }
SEXP (EXPR)(SEXP e) { return CHK(EXPR(CHKCONS(e))); }
void (SET_CODE)(SEXP x, SEXP v) { FIX_REFCNT(x, CODE0(x), v); CHECK_OLD_TO_NEW(x, v); CODE0(x) = v; }
void (SET_CONSTS)(SEXP x, SEXP v) { FIX_REFCNT(x, CONSTS(x), v); CHECK_OLD_TO_NEW(x, v); CONSTS(x) = v; }
void (SET_EXPR)(SEXP x, SEXP v) { FIX_REFCNT(x, EXPR(x), v); CHECK_OLD_TO_NEW(x, v); EXPR(x) = v; }

/* List Accessors */
SEXP (TAG)(SEXP e) { return CHK(TAG(CHKCONS(e))); }
SEXP (R::CAR0)(SEXP e) { return CHK(CAR0(CHKCONS(e))); }
SEXP (CDR)(SEXP e) { return CHK(CDR(CHKCONS(e))); }
SEXP (CAAR)(SEXP e) { return CHK(CAAR(CHKCONS(e))); }
SEXP (CDAR)(SEXP e) { return CHK(CDAR(CHKCONS(e))); }
SEXP (CADR)(SEXP e) { return CHK(CADR(CHKCONS(e))); }
SEXP (CDDR)(SEXP e) { return CHK(CDDR(CHKCONS(e))); }
SEXP (CDDDR)(SEXP e) { return CHK(CDDDR(CHKCONS(e))); }
SEXP (CD4R)(SEXP e) { return CHK(CD4R(CHKCONS(e))); }
SEXP (CADDR)(SEXP e) { return CHK(CADDR(CHKCONS(e))); }
SEXP (CADDDR)(SEXP e) { return CHK(CADDDR(CHKCONS(e))); }
SEXP (CAD3R)(SEXP e) { return CHK(CADDDR(CHKCONS(e))); }
SEXP (CAD4R)(SEXP e) { return CHK(CAD4R(CHKCONS(e))); }
SEXP (CAD5R)(SEXP e) { return CHK(CAD5R(CHKCONS(e))); }
int (MISSING)(SEXP x) { return MISSING(CHKCONS(x)); }

void (SET_TAG)(SEXP x, SEXP v)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));
    FIX_REFCNT(x, TAG(x), v);
    CHECK_OLD_TO_NEW(x, v);
    TAG(x) = v;
}

SEXP (SETCAR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));
    CLEAR_BNDCELL_TAG(x);
    if (y == CAR(x))
	return y;
    FIX_BINDING_REFCNT(x, CAR(x), y);
    CHECK_OLD_TO_NEW(x, y);
    CAR0(x) = y;
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue)
	error("%s", _("bad value"));
    FIX_REFCNT(x, CDR(x), y);
#ifdef TESTING_WRITE_BARRIER
    /* this should not add a non-tracking CDR to a tracking cell */
    if (REFCNT_ENABLED(x) && y && !REFCNT_ENABLED(y))
	error("inserting non-tracking CDR in tracking cell");
#endif
    CHECK_OLD_TO_NEW(x, y);
    CDR(x) = y;
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue)
	error("%s", _("bad value"));
    SEXP cell = CDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    CAR0(cell) = y;
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHKCONS(CDDR(x)) == NULL || CDDR(x) == R_NilValue)
	error("%s", _("bad value"));
    SEXP cell = CDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    CAR0(cell) = y;
    return y;
}

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    if (CHKCONS(x) == NULL || x == R_NilValue ||
	CHKCONS(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHKCONS(CDDR(x)) == NULL || CDDR(x) == R_NilValue ||
	CHKCONS(CDDDR(x)) == NULL || CDDDR(x) == R_NilValue)
	error("%s", _("bad value"));
    SEXP cell = CDDDR(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    CAR0(cell) = y;
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
    SEXP cell = CD4R(x);
    CLEAR_BNDCELL_TAG(cell);
    FIX_REFCNT(cell, CAR(cell), y);
    CHECK_OLD_TO_NEW(cell, y);
    CAR0(cell) = y;
    return y;
}

SEXP (EXTPTR_PROT)(SEXP x) { return EXTPTR_PROT(CHK(x)); }
SEXP (EXTPTR_TAG)(SEXP x) { return EXTPTR_TAG(CHK(x)); }
void *(EXTPTR_PTR)(SEXP x) { return EXTPTR_PTR(CHK(x)); }

void (R::SET_MISSING)(SEXP x, int v) { SET_MISSING(CHKCONS(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return CHK(FORMALS(CHK(x))); }
SEXP (BODY)(SEXP x) { return CHK(BODY(CHK(x))); }
SEXP (CLOENV)(SEXP x) { return CHK(CLOENV(CHK(x))); }
int (RDEBUG)(SEXP x) { return RDEBUG(CHK(x)); }
int (RSTEP)(SEXP x) { return RSTEP(CHK(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { FIX_REFCNT(x, FORMALS(x), v); CHECK_OLD_TO_NEW(x, v); FORMALS(x) = v; }
void (SET_BODY)(SEXP x, SEXP v) { FIX_REFCNT(x, BODY(x), v); CHECK_OLD_TO_NEW(x, v); BODY(x) = v; }
void (SET_CLOENV)(SEXP x, SEXP v) { FIX_REFCNT(x, CLOENV(x), v); CHECK_OLD_TO_NEW(x, v); CLOENV(x) = v; }
void (SET_RDEBUG)(SEXP x, int v) { SET_RDEBUG(CHK(x), v); }
void (SET_RSTEP)(SEXP x, int v) { SET_RSTEP(CHK(x), v); }

/* These are only needed with the write barrier on */
#ifdef TESTING_WRITE_BARRIER
namespace R
{
/* Primitive Accessors */
/* not hidden since needed in some base packages */
int (PRIMOFFSET)(SEXP x) { return PRIMOFFSET(CHK(x)); }
attribute_hidden
void (SET_PRIMOFFSET)(SEXP x, int v) { SET_PRIMOFFSET(CHK(x), v); }
} // namespace R
#endif

/* Symbol Accessors */
SEXP (PRINTNAME)(SEXP x) { return CHK(PRINTNAME(CHK(x))); }
SEXP (SYMVALUE)(SEXP x) { return CHK(SYMVALUE(CHK(x))); }
SEXP (INTERNAL)(SEXP x) { return CHK(INTERNAL(CHK(x))); }
int (DDVAL)(SEXP x) { return DDVAL(CHK(x)); }

void (R::SET_PRINTNAME)(SEXP x, SEXP v) { FIX_REFCNT(x, PRINTNAME(x), v); CHECK_OLD_TO_NEW(x, v); PRINTNAME(x) = v; }

void (R::SET_SYMVALUE)(SEXP x, SEXP v)
{
    if (SYMVALUE(x) == v)
	return;
    FIX_BINDING_REFCNT(x, SYMVALUE(x), v);
    CHECK_OLD_TO_NEW(x, v);
    SYMVALUE(x) = v;
}

void (R::SET_INTERNAL)(SEXP x, SEXP v) { FIX_REFCNT(x, INTERNAL(x), v); CHECK_OLD_TO_NEW(x, v); INTERNAL(x) = v; }
void (R::SET_DDVAL)(SEXP x, int v) { SET_DDVAL(CHK(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return CHK(FRAME(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { return CHK(ENCLOS(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { return CHK(HASHTAB(CHK(x))); }
int (ENVFLAGS)(SEXP x) { return ENVFLAGS(CHK(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { FIX_REFCNT(x, FRAME(x), v); CHECK_OLD_TO_NEW(x, v); FRAME(x) = v; }

void (SET_ENCLOS)(SEXP x, SEXP v)
{
    if (v == R_NilValue)
	/* mainly to handle unserializing old files */
	v = R_EmptyEnv;
    if (TYPEOF(v) != ENVSXP)
	error(_("'parent' is not an environment"));
    for (SEXP e = v; e != R_NilValue; e = ENCLOS(e))
	if (e == x)
	    error(_("cycles in parent chains are not allowed"));
    FIX_REFCNT(x, ENCLOS(x), v);
    CHECK_OLD_TO_NEW(x, v);
    ENCLOS(x) = v;
}

void (SET_HASHTAB)(SEXP x, SEXP v) { FIX_REFCNT(x, HASHTAB(x), v); CHECK_OLD_TO_NEW(x, v); HASHTAB(x) = v; }
void (SET_ENVFLAGS)(SEXP x, int v) { SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return CHK(PRCODE(CHK(x))); }
SEXP (PRENV)(SEXP x) { return CHK(PRENV(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { return CHK(PRVALUE(CHK(x))); }
int (PRSEEN)(SEXP x) { return PRSEEN(CHK(x)); }
attribute_hidden
bool (R::PROMISE_IS_EVALUATED)(SEXP x)
{
    x = CHK(x);
    return PROMISE_IS_EVALUATED(x);
}

void (SET_PRENV)(SEXP x, SEXP v){ FIX_REFCNT(x, PRENV(x), v); CHECK_OLD_TO_NEW(x, v); PRENV(x) = v; }
void (SET_PRCODE)(SEXP x, SEXP v) { FIX_REFCNT(x, PRCODE(x), v); CHECK_OLD_TO_NEW(x, v); PRCODE(x) = v; }
void (R::SET_PRSEEN)(SEXP x, int v) { SET_PRSEEN(CHK(x), v); }

void (SET_PRVALUE)(SEXP x, SEXP v)
{
    if (TYPEOF(x) != PROMSXP)
	error("expecting a 'PROMSXP', not a '%s'", R_typeToChar(x));
#ifdef IMMEDIATE_PROMISE_VALUES
    if (PROMISE_TAG(x)) {
	SET_PROMISE_TAG(x, NILSXP);
	PRVALUE0(x) = R_UnboundValue;
    }
#endif
    FIX_REFCNT(x, PRVALUE0(x), v);
    CHECK_OLD_TO_NEW(x, v);
    PRVALUE0(x) = v;
}

attribute_hidden
void R::IF_PROMSXP_SET_PRVALUE(SEXP x, SEXP v)
{
    /* promiseArgs produces a list containing promises or R_MissingArg.
       Using IF_PROMSXP_SET_PRVALUE avoids corrupting R_MissingArg. */
    if (TYPEOF(x) == PROMSXP)
        SET_PRVALUE(x, v);
}

/* Hashing Accessors */
#ifdef TESTING_WRITE_BARRIER
attribute_hidden
bool (R::HASHASH)(SEXP x) { return HASHASH(CHK(x)); }
attribute_hidden
int (R::HASHVALUE)(SEXP x) { return HASHVALUE(CHK(x)); }

attribute_hidden
void (R::SET_HASHASH)(SEXP x, int v) { SET_HASHASH(CHK(x), v); }
attribute_hidden
void (R::SET_HASHVALUE)(SEXP x, int v) { SET_HASHVALUE(CHK(x), v); }
#endif

attribute_hidden
SEXP (R::SET_CXTAIL)(SEXP x, SEXP v) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(v) != CHARSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_CXTAIL' must be a char or NULL, not a '%s'",
	      R_typeToChar(v));
#endif
    /*CHECK_OLD_TO_NEW(x, v); *//* not needed since not properly traced */
    ATTRIB(x) = v;
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
attribute_hidden bool (IS_ACTIVE_BINDING)(SEXP b) {return IS_ACTIVE_BINDING(CHK(b));}
attribute_hidden bool (BINDING_IS_LOCKED)(SEXP b) {return BINDING_IS_LOCKED(CHK(b));}
attribute_hidden void (SET_ACTIVE_BINDING_BIT)(SEXP b) {SET_ACTIVE_BINDING_BIT(CHK(b));}
attribute_hidden void (LOCK_BINDING)(SEXP b) {LOCK_BINDING(CHK(b));}
attribute_hidden void (UNLOCK_BINDING)(SEXP b) {UNLOCK_BINDING(CHK(b));}

attribute_hidden
void (SET_BASE_SYM_CACHED)(SEXP b) { SET_BASE_SYM_CACHED(CHK(b)); }
attribute_hidden
void (UNSET_BASE_SYM_CACHED)(SEXP b) { UNSET_BASE_SYM_CACHED(CHK(b)); }
attribute_hidden
bool (BASE_SYM_CACHED)(SEXP b) { return BASE_SYM_CACHED(CHK(b)); }

attribute_hidden
void (SET_SPECIAL_SYMBOL)(SEXP b) { SET_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
void (UNSET_SPECIAL_SYMBOL)(SEXP b) { UNSET_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
bool (IS_SPECIAL_SYMBOL)(SEXP b) { return IS_SPECIAL_SYMBOL(CHK(b)); }
attribute_hidden
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b) { SET_NO_SPECIAL_SYMBOLS(CHK(b)); }
attribute_hidden
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b) { UNSET_NO_SPECIAL_SYMBOLS(CHK(b)); }
attribute_hidden
bool (NO_SPECIAL_SYMBOLS)(SEXP b) { return NO_SPECIAL_SYMBOLS(CHK(b)); }
} // namespace R
/* R_FunTab accessors, only needed when write barrier is on */
/* Not hidden to allow experimentation without rebuilding R - LT */
/* attribute_hidden */
int (PRIMVAL)(SEXP x) { return PRIMVAL(CHK(x)); }
/* attribute_hidden */
CCODE (PRIMFUN)(SEXP x) { return PRIMFUN(CHK(x)); }
/* attribute_hidden */
void (SET_PRIMFUN)(SEXP x, CCODE f) { PRIMFUN(CHK(x)) = f; }

/* for use when testing the write barrier */
namespace R {
attribute_hidden bool (IS_NATIVE)(SEXP x) { return IS_NATIVE(CHK(x)); }
attribute_hidden bool (IS_BYTES)(SEXP x) { return IS_BYTES(CHK(x)); }
attribute_hidden bool (IS_LATIN1)(SEXP x) { return IS_LATIN1(CHK(x)); }
/* Next two are used in package utils */
bool (IS_ASCII)(SEXP x) { return IS_ASCII(CHK(x)); }
bool (IS_UTF8)(SEXP x) { return IS_UTF8(CHK(x)); }
attribute_hidden void (SET_BYTES)(SEXP x) { SET_BYTES(CHK(x)); }
attribute_hidden void (SET_LATIN1)(SEXP x) { SET_LATIN1(CHK(x)); }
attribute_hidden void (SET_UTF8)(SEXP x) { SET_UTF8(CHK(x)); }
attribute_hidden void (SET_ASCII)(SEXP x) { SET_ASCII(CHK(x)); }
int (ENC_KNOWN)(SEXP x) { return ENC_KNOWN(CHK(x)); }
attribute_hidden void (SET_CACHED)(SEXP x) { SET_CACHED(CHK(x)); }
bool (IS_CACHED)(SEXP x) { return IS_CACHED(CHK(x)); }
} // namespace R
/*******************************************/
/* Non-sampling memory use profiler
   reports all large vector heap
   allocations and all calls to GetNewPage */
/*******************************************/

#ifndef R_MEMORY_PROFILING

NORET SEXP do_Rprofmem(SEXP args)
{
    error("%s", _("memory profiling is not available on this system"));
}

#else
static bool R_IsMemReporting;
static FILE *R_MemReportingOutfile;
static R_size_t R_MemReportingThreshold;

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
    if (R_IsMemReporting) {
	if(size > R_MemReportingThreshold) {
	    fprintf(R_MemReportingOutfile, "%lu :", (unsigned long) size);
	    R_OutputStackTrace(R_MemReportingOutfile);
	    fprintf(R_MemReportingOutfile, "\n");
	}
    }
    return;
}

static void R_ReportNewPage(void)
{
    if (R_IsMemReporting) {
	fprintf(R_MemReportingOutfile, "new page:");
	R_OutputStackTrace(R_MemReportingOutfile);
	fprintf(R_MemReportingOutfile, "\n");
    }
    return;
}

static void R_EndMemReporting(void)
{
    if(R_MemReportingOutfile != NULL) {
	/* does not fclose always flush? */
	fflush(R_MemReportingOutfile);
	fclose(R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    R_IsMemReporting = 0;
    return;
}

static void R_InitMemReporting(SEXP filename, bool append,
			       R_size_t threshold)
{
    if(R_MemReportingOutfile != NULL) R_EndMemReporting();
    R_MemReportingOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_MemReportingOutfile == NULL)
	error(_("Rprofmem: cannot open output file '%s'"),
	      translateChar(filename));
    R_MemReportingThreshold = threshold;
    R_IsMemReporting = 1;
    return;
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

#include "RBufferUtils.h"

void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, this used to free the buffer */
    if(blen == (size_t)-1)
	error("R_AllocStringBuffer( (size_t)-1 ) is no longer allowed");

    if(blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    /* Result may be accessed as `wchar_t *` and other types; malloc /
      realloc guarantee correct memory alignment for all object types */
    if(buf->data == NULL) {
	buf->data = (char *) malloc(blen);
	if(buf->data)
	    buf->data[0] = '\0';
    } else
	buf->data = (char *) realloc(buf->data, blen);
    buf->bufsize = blen;
    if(!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error("could not allocate memory (%u Mb) in C function 'R_AllocStringBuffer'",
	      (unsigned int) (blen/Mega));
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
    if (a == b) return 1;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
	return 0;
    else if (IS_BYTES(a) || IS_BYTES(b)) {
	if (IS_BYTES(a) && IS_BYTES(b))
	    /* only get here if at least one is not cached */
	    return streql(CHAR(a), CHAR(b));
	else
	    return 0;
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

