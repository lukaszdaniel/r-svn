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
#include <cassert>
#include <Localization.h>
#include <CXXR/GCRoot.hpp>
#include <CXXR/String.hpp>
#include <Defn.h> // for LATIN1_MASK, etc

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

    std::hash<std::string> String::Hasher::s_string_hasher;
    String::map String::s_hash_table;

    // String::Comparator::operator()(const String*, const String*) is in sort.cpp

    bool String::s_known_to_be_latin1 = false;
    bool String::s_known_to_be_utf8 = false;

    String::~String()
    {
    }

    const char *String::c_str() const
    {
        return static_cast<const char *>(u.vecsxp.m_data);
    }

    std::string String::stdstring() const
    {
        assert(u.vecsxp.m_data);
        return std::string(static_cast<const char *>(u.vecsxp.m_data), size());
    }

    String *String::blank()
    {
        static GCRoot<String> blank = String::obtain("");
        return blank;
    }

    String *String::NA()
    {
        /* Note: we don't want NA_STRING to be in the CHARSXP cache, so that
           mkChar("NA") is distinct from NA_STRING */
        static GCRoot<String> na(String::create("NA", CE_NATIVE, false));
        return na;
    }

    void String::initialize()
    {
        R_NaString = String::NA();
        R_BlankString = String::blank();
    }

    cetype_t String::GPBits2Encoding(unsigned int gpbits)
    {
        if ((gpbits & LATIN1_MASK) != 0)
            return CE_LATIN1;
        if ((gpbits & UTF8_MASK) != 0)
            return CE_UTF8;
        if ((gpbits & BYTES_MASK) != 0)
            return CE_BYTES;
        return CE_NATIVE;
    }

    String *String::create(const std::string &name, cetype_t enc, bool is_ascii)
    {
        switch (enc)
        {
        case CE_NATIVE:
        case CE_UTF8:
        case CE_LATIN1:
        case CE_BYTES:
            break;
        default:
            Rf_error(_("unknown encoding: %d"), enc);
        }

        return new String(name, enc, is_ascii);
    }

    String::String(const std::string &name, cetype_t encoding, bool isAscii)
        : VectorBase(CHARSXP, name.length(), nullptr)
    {
        int n_elem = name.length();
        if (n_elem)
            memcpy(u.vecsxp.m_data, name.c_str(), n_elem);
        ((char *)u.vecsxp.m_data)[n_elem] = 0;

        switch (encoding)
        {
        case CE_NATIVE:
            break; /* don't set encoding */
        case CE_UTF8:
            SET_UTF8(this);
            break;
        case CE_LATIN1:
            SET_LATIN1(this);
            break;
        case CE_BYTES:
            SET_BYTES(this);
            break;
        default:
            break;
        }
        if (isAscii)
            SET_ASCII(this);
        SET_CACHED(this); /* Mark it */
    }

    void String::visitTable()
    {
        for (auto it = String::s_hash_table.begin(); it != String::s_hash_table.end(); )
        {
            if (!it->second->isMarked())
                it = String::s_hash_table.erase(it);  // erase returns the next iterator
            else
                ++it;
        }
    }

    bool String::isASCII() const
    {
        return IS_ASCII(this);
    }

    cetype_t String::encoding() const
    {
        if (IS_BYTES(this))
            return CE_BYTES;
        if (IS_LATIN1(this))
            return CE_LATIN1;
        if (IS_UTF8(this))
            return CE_UTF8;

        Rf_error("String::encoding(): unknown encoding mask"); // for now
        return CE_NATIVE;
    }

    bool isASCII(const std::string &str)
    {
        return !std::any_of(str.begin(), str.end(), [](char c)
            { return static_cast<unsigned char>(c) > 127; });
    }

    const char *String::typeName() const
    {
        return String::staticTypeName();
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
