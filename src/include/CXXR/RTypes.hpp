/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
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

#ifndef RTYPES_HPP
#define RTYPES_HPP

#ifdef __cplusplus
#include <cstdio>
#include <climits>
// #include <limits>
#include <cstddef>
// #include <string>
#include <cstdint>
#else
#include <stdio.h>
#include <limits.h> /* for INT_MAX */
#include <stddef.h> /* for ptrdiff_t, which is required by C99 */
#include <stdint.h>
#endif

#include <CXXR/config.hpp>

#if defined(COMPILING_IVORY) && defined(__cplusplus)
namespace R
{
    class RObject;
}
using SEXP = R::RObject *;
#else
#define SEXPREC RObject
typedef struct RObject *SEXP;
#endif

#define CXXR_TRUE 1
#define CXXR_FALSE 0

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
// #ifndef R_CONFIG_H
// #include <Rconfig.h>
// #endif

typedef unsigned char Rbyte;

/* type for length of (standard, not long) vectors etc */
typedef int R_len_t;
#define R_LEN_T_MAX INT_MAX
#define R_INT_MAX INT_MAX
#define R_INT_MIN (1 + INT_MIN) //INT_MIN is NA_INTEGER

#if (SIZEOF_SIZE_T > 4)
#define LONG_VECTOR_SUPPORT
#endif

#ifdef LONG_VECTOR_SUPPORT
typedef ptrdiff_t R_xlen_t;
#define R_XLEN_T_MAX 4503599627370496
#define R_SHORT_LEN_MAX 2147483647
#else
typedef int R_xlen_t;
#define R_XLEN_T_MAX R_LEN_T_MAX
#endif

typedef size_t R_size_t;
#define R_SIZE_T_MAX SIZE_MAX

#endif /* RTYPES_HPP */
