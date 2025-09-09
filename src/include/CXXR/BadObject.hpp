/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file BadObject.hpp
 * @brief Class CXXR::BadObject.
 */

#ifndef BADOBJECT_HPP
#define BADOBJECT_HPP

#include <CXXR/config.hpp>
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/GCNode.hpp>

namespace CXXR
{
    /** The following class is used to record the address
     * and type of the first bad type seen during a collection,
     * and for FREESXP nodes they record the old type as well.
     */
    class BadObject
    {
    public:
        BadObject(): m_bad_sexp_type_seen(NILSXP),
            m_bad_sexp_type_sexp(nullptr),
#ifdef PROTECTCHECK
            m_bad_sexp_type_old_type(NILSXP),
#endif
            m_bad_sexp_type_file(nullptr), m_bad_sexp_type_line(0)
        {
        }

        bool isEmpty() const { return m_bad_sexp_type_seen == NILSXP; }
        void clear() { m_bad_sexp_type_seen = NILSXP; }

        static void register_bad_object(const GCNode *s, const char *file, int line);
        void printSummary();

        BadObject &operator=(const BadObject &other)
        {
            m_bad_sexp_type_seen = other.m_bad_sexp_type_seen;
            m_bad_sexp_type_sexp = other.m_bad_sexp_type_sexp;
#ifdef PROTECTCHECK
            m_bad_sexp_type_old_type = other.m_bad_sexp_type_old_type;
#endif
            m_bad_sexp_type_file = other.m_bad_sexp_type_file;
            m_bad_sexp_type_line = other.m_bad_sexp_type_line;
            return *this;
        }

        static BadObject s_firstBadObject;

    private:
        SEXPTYPE m_bad_sexp_type_seen;
        const GCNode *m_bad_sexp_type_sexp;
#ifdef PROTECTCHECK
        SEXPTYPE m_bad_sexp_type_old_type;
#endif
        const char *m_bad_sexp_type_file;
        unsigned int m_bad_sexp_type_line;
    };
}
#endif /* BADOBJECT_HPP */