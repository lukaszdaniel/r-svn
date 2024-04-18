/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
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

#ifndef SEXPTYPE_HPP
#define SEXPTYPE_HPP

/** @brief CR's object type identification.
 *
 * @enum SEXPTYPE This enumeration is used within CR to identify different types
 * of R object.  In rho the same purpose could be (and sometimes
 * is) achieved by C++ run-time type information (RTTI), virtual
 * function despatch etc.  However, a ::SEXPTYPE field is retained
 * within each rho::RObject for backwards compatibility, and indeed
 * efficiency.
 *
 * @note when not compiling rho, SEXPTYPE is a typedef for unsigned int.
 * This is done to support C++ packages that expect implicit int to
 * SEXPTYPE conversions.
 */
#ifdef COMPILING_IVORY
#define enum_SEXPTYPE
#endif
#ifndef enum_SEXPTYPE
/* For interfaces to objects created with as.single */
#define SINGLESXP 302

/* NOT YET using enum:
 *  1)	The internal RObject struct has 'SEXPTYPE type : 5'
 *	(making FUNSXP and CLOSXP equivalent in there),
 *	giving (-Wall only ?) warnings all over the place
 * 2)	Many switch(type) { case ... } statements need a final `default:'
 *	added in order to avoid warnings like [e.g. l.170 of ../main/util.cpp]
 *	  "enumeration value `FUNSXP' not handled in switch"
 */
typedef unsigned int SEXPTYPE;
#else
typedef
#endif
enum
{
    NILSXP = 0,      /**< NULL. In rho no CXXR::RObject has
                      * this type, but for backward
                      * compatibility TYPEOF will return ::NILSXP
                      * if passed a null pointer.
                      */
    SYMSXP = 1,      /* symbols */
    LISTSXP = 2,     /* lists of dotted pairs */
    CLOSXP = 3,      /* closures */
    ENVSXP = 4,      /* environments */
    PROMSXP = 5,     /* promises: [un]evaluated closure arguments */
    LANGSXP = 6,     /* language constructs (special lists) */
    SPECIALSXP = 7,  /* special forms */
    BUILTINSXP = 8,  /* builtin non-special forms */
    CHARSXP = 9,     /* "scalar" string type (internal only)*/
    LGLSXP = 10,     /* logical vectors */
/* 11 and 12 were factors and ordered factors in the 1990s */
    INTSXP = 13,     /* integer vectors */
    REALSXP = 14,    /* real variables */
    CPLXSXP = 15,    /* complex variables */
    STRSXP = 16,     /* string vectors */
    DOTSXP = 17,     /* dot-dot-dot object */
    ANYSXP = 18,     /* make "any" args work.
	        		    Used in specifying types for symbol
			            registration to mean anything is okay  */
    VECSXP = 19,     /* generic vectors */
    EXPRSXP = 20,    /* expressions vectors */
    BCODESXP = 21,   /* byte code */
    EXTPTRSXP = 22,  /* external pointer */
    WEAKREFSXP = 23, /* weak reference */
    RAWSXP = 24,     /* raw bytes */
    OBJSXP = 25,      /* object, non-vector  */
    S4SXP = 25,      /* same as OBJSXP, retained for back compatability */
/* used for detecting PROTECT issues in memory.c */
    NEWSXP = 30,   /* fresh node created in new page */
    FREESXP = 31,   /* node released by GC */

#ifdef enum_SEXPTYPE
    intCHARSXP = 73,
    SINGLESXP = 200,
    REFSXP = 255,
    NILVALUE_SXP = 254,
    GLOBALENV_SXP = 253,
    UNBOUNDVALUE_SXP = 252,
    MISSINGARG_SXP = 251,
    BASENAMESPACE_SXP = 250,
    NAMESPACESXP = 249,
    PACKAGESXP = 248,
    PERSISTSXP = 247,
    CLASSREFSXP = 246,
    GENERICREFSXP = 245,
    BCREPDEF = 244,
    BCREPREF = 243,
    EMPTYENV_SXP = 242,
    BASEENV_SXP = 241,
    ATTRLANGSXP = 240,
    ATTRLISTSXP = 239,
    ALTREP_SXP = 238,
#endif
    FUNSXP = 99 /* Closure or Builtin or Special */
}
#ifdef enum_SEXPTYPE
SEXPTYPE
#endif
;

/* These are also used with the write barrier on, in attrib.cpp and util.cpp */
#define TYPE_BITS 8
#define MAX_NUM_SEXPTYPE (1 << TYPE_BITS)

#endif /* SEXPTYPE_HPP */
