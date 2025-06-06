/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2024	The R Core Team.
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
 *
 */
/*
 *  This file replaces the previously used ROUTINES file and is used to
 *  explicitly register native routines that are located in the R
 *  executable (e.g. R.bin, Rgui.exe) but which are intended to be
 *  accessible to S code via .C(), .Fortran(), .Call(), .External().
 *  The approach we use here is the regular registration mechanism that
 *  packages can use to explicitly list the symbols to be exported.
 *  For .C() and .Call() routines, we give the number of arguments
 *  expected.
 *  For .C() routines, we also specify the types of the arguments.
 *  For .Fortran() and .External() routines, we specify only the name
 *  and symbol.

 *  To add an entry, first determine by which interface the routine will
 *  be accessed:
 *   .C, .Call, .External or .Fortran
 *  Then add an entry to
 *    cMethods, callMethods, externalMethods, or fortranMethods
 *  respectively
 *
 *  DTL 14-Dec-2002
 */

/** @file registration.cpp
 *
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Rdynpriv.h>
#include <Defn.h>
/*  These get the declarations of some routines referenced here but
    not explicitly declared.    This is necessary when we link with
    a C++ compiler because the linkage changes as the declarations
    are (currently) within extern "C" blocks.
*/
#include <R_ext/Callbacks.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Applic.h>
#include <R_ext/Linpack.h>
#include <CXXR/MemoryBank.hpp>

#include "basedecl.h"

/** @brief Dummy function callable from R code.
 * 
 * Function to test various c routines.
 * 
 * @param x Dummy argument
 * @param y Dummy argument
 * @param z Dummy argument
 * @param w Dummy argument
 *
 * @return Dummy output
 *
 * @note Callable from R via .Call(.C_R_dummy, NULL, NULL, NULL, NULL)
 */
attribute_hidden SEXP R_dummy(SEXP x, SEXP y, SEXP z, SEXP w)
{
    return R_NilValue;
}


#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef callMethods [] = {
    /* Top-level task callbacks: .Call as .Internal does not work */
    CALLDEF(R_addTaskCallback, 4),
    CALLDEF(R_getTaskCallbackNames, 0),
    CALLDEF(R_removeTaskCallback, 1),
    CALLDEF(R_dummy, 4),
    CALLDEF(allocstats, 0),

    {NULL, NULL, 0}
};

#include <R_ext/RS.h>
#define FDEF(name, n)  {#name, (DL_FUNC) &F77_SUB(name), n, NULL}
static R_FortranMethodDef fortranMethods[] = {
    /* LINPACK */
    FDEF(dqrcf, 8), // qr and auxiliaries
    FDEF(dqrdc2, 9),
    FDEF(dqrqty, 7),
    FDEF(dqrqy, 7),
    FDEF(dqrrsd, 7),
    FDEF(dqrxb, 7),
    FDEF(dtrco, 6), // .kappa_tri

    {NULL, NULL, 0}
};


attribute_hidden void R::R_init_base(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, fortranMethods, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
