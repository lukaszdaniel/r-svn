/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file String.cpp
 *
 * Implementation of class String and related functions.
 */

#include <algorithm>
#include <cstring>
#include <CXXR/String.hpp>

using namespace std;
using namespace CXXR;

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENC_KNOWNptr = R::ENC_KNOWN;
        const auto &IS_ASCIIptr = R::IS_ASCII;
        const auto &IS_BYTESptr = R::IS_BYTES;
        const auto &IS_LATIN1ptr = R::IS_LATIN1;
        const auto &IS_UTF8ptr = R::IS_UTF8;
        const auto &R_CHARptr = R_CHAR;
        const auto &mkCharptr = Rf_mkChar;
        const auto &mkCharCEptr = Rf_mkCharCE;
        const auto &mkCharLenptr = Rf_mkCharLen;
    } // namespace ForceNonInline

    bool String::s_known_to_be_latin1 = false;
    bool String::s_known_to_be_utf8 = false;

    bool isASCII(const std::string &str)
    {
        return !std::any_of(str.begin(), str.end(), [](char c)
                            { return static_cast<unsigned char>(c) > 127; });
    }
} // namespace CXXR

namespace R
{
    bool streql(const char *s, const char *t)
    {
        return (strcmp(s, t) == 0);
    }

    bool streqln(const char *s, const char *t, size_t n)
    {
        return (strncmp(s, t, n) == 0);
    }
} // namespace R

// ***** C interface *****
