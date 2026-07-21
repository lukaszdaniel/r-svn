/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
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

/** @file config.hpp
 *
 * @brief Build configuration options specific to CXXR.
 *
 * This file contains documentation and definitions of preprocessor
 * variables that configure the behaviour of CXXR.  However, it
 * excludes preprocessor variables that are controlled by options to
 * the autoconf-generated <tt>configure</tt> script: for information
 * on these see <tt>./configure --help</tt>.
 *
 * As distributed this file represents a configuration suitable for speed.
 * development, with numerous facilities enable for checking and
 * debugging.  For maximum speed, it is recommended that this file be
 * modified by enabling the definitions of NDEBUG and
 * disabling all other definitions.
 */

#ifndef CXXR_CONFIG_HPP
#define CXXR_CONFIG_HPP


/** @def AGGRESSIVE_GC
 *
 * By default, GCNode::operator new initiates a reference-count-based
 * garbage collection (GCNode::gclite()) only when the number of bytes
 * allocated has risen by a certain margin from the number allocated
 * after the last such collection.  However, if CXXR is compiled with
 * the preprocessor variable AGGRESSIVE_GC define, every call to
 * GCNode::operator new will initiate a reference-count-based garbage
 * collection.  When used in conjunction with NO_CELLPOOLS and address
 * sanitizer (or valgrind), this can help to detect and diagnose gaps in
 * the protection of nodes against garbage collection.
 */
// #define AGGRESSIVE_GC

/** @def NDEBUG
 *
 * @brief Suppress some runtime checks.
 *
 * By default, CXXR includes code to check that CXXR::GCStackRoot
 * objects are destroyed in the reverse order of creation, and that a
 * node is <code>UNPROTECT</code>ed in the same RCNTXT as it was
 * <code>PROTECT</code>ed. If NDEBUG is defined, these checks are
 * omitted. Not recommended during development.
 */
// #define NDEBUG


/* PROVENANCE_TRACKING is *not yet* controlled by the
 * --enable-provenance-tracking option to configure, and will be
 * defined (or not) in config.h .
 */
// #define PROVENANCE_TRACKING

/** @def RARE_GC
 *
 * @brief Suppress reference-counting garbage collection.
 *
 * By default, CXXR will delete any CXXR::GCNode whose reference count
 * has fallen to zero as a preliminary to allocating memory for a new
 * CXXR::GCNode.
 * Defining RARE_GC suppresses the default behaviour, and results in
 * CXXR::GCNode objects being deleted only as part of the mark-sweep
 * garbage collection process, initiated when a memory utilisation
 * threshold is exceeded.
 */
// #define RARE_GC


#ifdef __GNUC__
#  ifdef __i386__
#    define HOT_FUNCTION __attribute__((hot, fastcall))
#  else
#    define HOT_FUNCTION __attribute__((hot))
#  endif
#  define COLD_FUNCTION __attribute__((cold))
#else
#  define HOT_FUNCTION
#  define COLD_FUNCTION
#endif

#ifndef SWITCH_TO_REFCNT
#define SWITCH_TO_REFCNT
#endif

#ifndef COMPUTE_REFCNT_VALUES
#define COMPUTE_REFCNT_VALUES
#endif

#ifndef ADJUST_ENVIR_REFCNTS
#define ADJUST_ENVIR_REFCNTS
#endif

/** Definition that allows C/C++ code to determine it is being compiled as
 * part of CXXR or legacy CR code.
 * 
 * This differentiation is needed as CXXR and CR APIs do not fully overlap.
 * For example, in CXXR R_NilValue is simply a null pointer while in CR
 * R_NilValue is an RObject with non-null address.
 */
#define CXXR_PROJECT

/** @def CHECKED_SEXP_DOWNCAST
 *
 * @brief Check downcasts within the CXXR::RObject class hierarchy.
 *
 * If enabled, CXXR implements the templated function
 * CXXR::SEXP_downcast<PtrOut, PtrIn>() using
 * <code>dynamic_cast</code>, to verify that the argument object is of
 * an appropriate type for the requested cast.
 * This adds a run-time check that the cast is valid,
 * at the cost of some performance.
 */
// #define CHECKED_SEXP_DOWNCAST

/** To test the write barrier used by the generational collector,
 * define TESTING_WRITE_BARRIER.  This makes the internal structure of
 * SEXPRECs visible only inside of files that explicitly define
 * USE_RINTERNALS, and all uses of RObject fields that do not go
 * through the appropriate functions or macros will become compilation
 * errors.  Since this does impose a small but noticable performance
 * penalty, code that includes Defn.h (or code that explicitly defines
 * USE_RINTERNALS) can access a RObject's fields directly.
 */
// #define TESTING_WRITE_BARRIER

#ifndef TESTING_WRITE_BARRIER
#ifndef USE_RINTERNALS
# define USE_RINTERNALS
#endif
#endif

#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
#endif

/** Define R_MEMORY_PROFILING to enable memory profiling support in R.
 * This is primarily intended for use in conjunction with
 * TESTING_WRITE_BARRIER to help track down memory management bugs.
 */
// #define R_MEMORY_PROFILING

#ifndef R_NilValue
#ifdef __cplusplus
#define R_NilValue nullptr
#else
#define R_NilValue NULL
#endif
#endif

#endif // CXXR_CONFIG_HPP
