/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2020-2022 The R Core Team.
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

#include <cctype>
#include <CXXR/ProtectStack.hpp>
#include <R.h>
#include "tools.h"
#include "localization.h"

static SEXP package_dependencies_scan_one(SEXP this_) {
    SEXP y;
    bool save;
    int size = 256, i, j, nb = 0, ne = 0, u, v, w;
    int *beg, *end;
    const char *s;
    char c, *t, *p, q = '\0';
    cetype_t e;

    if(this_ == NA_STRING) {
        return Rf_allocVector(STRSXP, 0);
    }

    beg = R_Calloc(size, int);
    end = R_Calloc(size, int);

    e = getCharCE(this_);
    s = CHAR(this_);
    i = 0;
    save = false;
    /* A package dependency spec is a comma-separated list of package
       names optionally followed by a comment in parentheses specifying
       a version requirement (see "Package Dependencies" in WRE).
       The package name can be 'R' or a valid package names matching
       "[[:alpha:]][[:alnum:].]*[[:alnum:]]".
       So for valid package dependency specs we can simply iteratively
       "save" from the first alpha until the next not-alnum-or-period
       (and ignore if this gave 'R'): this will also skip the field
       separators and optional comments.
       One could arrange to skip from the end of package names until the
       next comma, but that still would assume valid package names.
    */
    while((c = *s++) != '\0') {
	if(save) {
	    if(!isalnum(c) && (c != '.')) {
		save = false;
		if((q == 'R') && (beg[ne] == (i - 1)))
		    nb--;
		else {
		    end[ne] = i - 1;
		    ne++;
		}
	    }
	} else {
	    if(isalpha(c)) {
		save = true;
		q = c;
		if(nb >= size) {
		    if(size > INT_MAX / 2)
			error("%s", _("too many items"));
		    size *= 2;
		    beg = R_Realloc(beg, size, int);
		    end = R_Realloc(end, size, int);
		}
		beg[nb] = i;
		nb++;
	    }
	}
	i++;
    }
    if(ne < nb) {
	if((q == 'R') && (beg[ne] == (i - 1)))
	    nb--;
	else
	    end[ne] = i - 1;
    }

    PROTECT(y = Rf_allocVector(STRSXP, nb));
    s = CHAR(this_);
    v = -1;
    for(i = 0; i < nb; i++) {
        u = beg[i];
        s += (u - v - 1);
        v = end[i];
        w = v - u + 1;
        p = t = (char *) R_alloc(w + 1, sizeof(char));
        for(j = 0; j < w; j++) {
            *t++ = *s++;
        }
        *t = '\0';
        SET_STRING_ELT(y, i, mkCharCE(p, e));
    }

    R_Free(beg);
    R_Free(end);

    UNPROTECT(1);

    return y;
}

SEXP package_dependencies_scan(SEXP x) {
    SEXP y, z, this_;
    R_xlen_t i, j, k, nx, ny;

    if(TYPEOF(x) != STRSXP)
	error("%s", _("non-character argument"));

    nx = LENGTH(x);

    if(nx < 1)
        return Rf_allocVector(STRSXP, 0);

    if(nx == 1)
        return package_dependencies_scan_one(STRING_ELT(x, 0));

    PROTECT(z = Rf_allocVector(VECSXP,nx));
    ny = 0;
    for(i = 0; i < nx; i++) {
        this_ = package_dependencies_scan_one(STRING_ELT(x, i));
        SET_VECTOR_ELT(z, i, this_);
        ny += LENGTH(this_);
    }
    // Now unlist.
    k = 0;
    PROTECT(y = Rf_allocVector(STRSXP,ny));
    for(i = 0; i < nx; i++) {
        this_ = VECTOR_ELT(z, i);
        for(j = 0; j < LENGTH(this_); j++, k++)
            SET_STRING_ELT(y, k, STRING_ELT(this_, j));
    }

    UNPROTECT(2);

    return y;
}
