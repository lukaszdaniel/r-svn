/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000, 2025 The R Core Team.
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

/** @file Boolean.h
 *
 * @brief Representation of bool in CR.
 */

/* Included by R.h: API */

#ifndef R_EXT_BOOLEAN_H_
#define R_EXT_BOOLEAN_H_

#undef FALSE
#undef TRUE

#ifdef  __cplusplus
extern "C" {
#endif
    typedef enum { FALSE = 0, TRUE } Rboolean;  // so NOT NA
#ifdef  __cplusplus
} //extern "C"
#endif

/* Rboolean can hold one of 3 values: TRUE, FALSE, NA
 * bool is meant for cases where NA is no-op.
 */
#ifndef __cplusplus
#include <stdbool.h>
#endif

#define false_ 0
#define true_ 1

#endif /* R_EXT_BOOLEAN_H_ */
