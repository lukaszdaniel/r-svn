/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002   The R Core Team.
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

/** @file strncasecmp.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <sys/types.h>

/* This version uses locale-specific case folding */

int strncasecmp(const char *s1, const char *s2, size_t n)
{
    char c1, c2;

    for (size_t i = 0; i < n; i++) {
	c1 = s1[i]; c2 = s2[i];
	c1 = isupper(c1) ? char(tolower(c1)) : c1;
	c2 = isupper(c2) ? char(tolower(c2)) : c2;
	if (c1 == '\0') return ((c2 == '\0') ? 0 : -1);
	if (c2 == '\0') return 1;
	if (c1 < c2) return -1;
	if (c1 > c2) return 1;
    }
    return 0;
}
