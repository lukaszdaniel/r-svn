/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2024  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/* Internal header, not installed */

#ifndef PRINT_H_
#define PRINT_H_

#ifndef __cplusplus
#error Print.h can only be included in C++ files
#endif

#include <Defn.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Print.h>

#if 0
#define formatRaw           Rf_formatRaw
#define formatString        Rf_formatString
#define formatRawS          Rf_formatRawS
#define formatStringS       Rf_formatStringS
#define EncodeElement       Rf_EncodeElement
#define EncodeElement0      Rf_EncodeElement0
#define EncodeElement       Rf_EncodeElement
#define printArray          Rf_printArray
#define printMatrix         Rf_printMatrix
#define printNamedVector    Rf_printNamedVector
#define printVector         Rf_printVector
#endif

/* For backward compatibility */
#define R_print_par_t R_PrintData

namespace R {
extern R_print_par_t R_print;

/* Computation of printing formats */
void formatRaw(const Rbyte *, R_xlen_t, int *);
void formatString(const SEXP *, R_xlen_t, int *, int);
void formatRawS(SEXP, R_xlen_t, int *);
void formatStringS(SEXP, R_xlen_t, int*, bool);

/* Formating of values */
const char *EncodeElement0(SEXP, R_xlen_t, int, const char *);
const char *EncodeEnvironment(SEXP);
const char *EncodeExtptr(SEXP);
/* Legacy, for R.app */
const char *EncodeElement(SEXP, int, int, char);

/* In Rinternals.h (and MUST be there):
   CustomPrintValue,  PrintValue, PrintValueRec */
void printArray(SEXP, SEXP, int, int, SEXP);
void printMatrix(SEXP, int, SEXP, int, int, SEXP, SEXP,
		 const char*, const char*);
void printNamedVector(SEXP, SEXP, int, const char*);
void printVector(SEXP, int, int);
// void PrintClosure(SEXP, Rboolean);
// void PrintLanguage(SEXP, Rboolean);

/* Utilities for S compatibility and debugging */
void R_PV(SEXP s);
} // namespace R

/* Offset for rowlabels if there are named dimnames */
#define R_MIN_LBLOFF 2

/* Enforced in  ../main/options.c : */
#define R_MIN_WIDTH_OPT		10
#define R_MAX_WIDTH_OPT		10000
#define R_MIN_DIGITS_OPT	1
#define R_MAX_DIGITS_OPT	22
#define R_MIN_SCIPEN_OPT        -9
#define R_MAX_SCIPEN_OPT	9999

#endif /* PRINT_H_ */
