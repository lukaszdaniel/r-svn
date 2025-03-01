/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2014  The R Core Team
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

/** @file colors.cpp
 *
 */

/* This should be regarded as part of the graphics engine:
   it is now a stub for code in grDevices */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Localization.h>
#include <Defn.h>
#include <R_ext/GraphicsEngine.h>

/* same as src/library/grDevices/src/colors.cpp */
typedef rcolor (*F1)(SEXP x, int i, rcolor bg);
typedef const char * (*F2)(rcolor col);
typedef rcolor (*F3)(const char *s);
typedef void (*F4)(bool save);

static F1 ptr_RGBpar3;
static F2 ptr_col2name;
static F3 ptr_R_GE_str2col;
static F4 ptr_savePalette;

void Rg_set_col_ptrs(F1 f1, F2 f2, F3 f3, F4 f4)
{
    ptr_RGBpar3 = f1;
    ptr_col2name = f2;
    ptr_R_GE_str2col = f3;
    ptr_savePalette = f4;
}

/* used in grid/src/gpar.c with bg = R_TRANWHITE,
   in packages Cairo, canvas and jpeg */
/* in GraphicsEngine.h */
rcolor Rf_RGBpar3(SEXP x, int i, rcolor bg)
{
    if (!ptr_RGBpar3) error("%s", _("package grDevices must be loaded"));
    return (ptr_RGBpar3)(x, i, bg);
}

/* in GraphicsEngine.h, used by devices */
rcolor Rf_RGBpar(SEXP x, int i)
{
    return RGBpar3(x, i, R_TRANWHITE);
}

/* used in grid */
/* in GraphicsEngine.h */
const char *Rf_col2name(rcolor col)
{
    if (!ptr_col2name) error("%s", _("package grDevices must be loaded"));
    return (ptr_col2name)(col);
}

/* used in grDevices for fg and bg of devices */
/* in GraphicsEngine.h */
rcolor R_GE_str2col(const char *s)
{
    if (!ptr_R_GE_str2col) error("%s", _("package grDevices must be loaded"));
    return (ptr_R_GE_str2col)(s);
}

/* used in engine.c */
attribute_hidden
void R::savePalette(bool save)
{
    if (!ptr_savePalette) error("%s", _("package grDevices must be loaded"));
    (ptr_savePalette)(save);
}
