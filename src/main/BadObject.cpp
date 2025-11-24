/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2024  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

 /** @file BadObject.cpp
 *
 * Utility to record the address and type of the first bad type.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/BadObject.hpp>
#include <CXXR/GCManager.hpp>
#include <Defn.h> // for sexptype2char

namespace CXXR
{
    BadObject BadObject::s_firstBadObject;

    void BadObject::printSummary()
    {
        if (m_bad_sexp_type_seen != NILSXP)
        {
            char msg[256];
#ifdef PROTECTCHECK
            if (m_bad_sexp_type_seen == FREESXP)
                snprintf(msg, 256,
                    "GC encountered a node (%p) with type FREESXP (was %s)"
                    " at %s:%d",
                    (void *)m_bad_sexp_type_sexp,
                    sexptype2char(m_bad_sexp_type_old_type), m_bad_sexp_type_file,
                    m_bad_sexp_type_line);
            else
                snprintf(msg, 256,
                    "GC encountered a node (%p) with an unknown SEXP type: %d"
                    " at %s:%d",
                    (void *)m_bad_sexp_type_sexp,
                    m_bad_sexp_type_seen, m_bad_sexp_type_file,
                    m_bad_sexp_type_line);
#else
            snprintf(msg, 256,
                "GC encountered a node (%p) with an unknown SEXP type: %d"
                " at %s:%d",
                (void *)m_bad_sexp_type_sexp,
                m_bad_sexp_type_seen, m_bad_sexp_type_file,
                m_bad_sexp_type_line);
            GCManager::gc_error(msg);
#endif
        }
    }

    void BadObject::register_bad_object(const GCNode *s, const char *file, int line)
    {
        if (s_firstBadObject.isEmpty()) {
            s_firstBadObject.m_bad_sexp_type_seen = s->sxpinfo.type;
            s_firstBadObject.m_bad_sexp_type_sexp = s;
            s_firstBadObject.m_bad_sexp_type_file = file;
            s_firstBadObject.m_bad_sexp_type_line = line;
#ifdef PROTECTCHECK
            if (s->sxpinfo.type == FREESXP)
                s_firstBadObject.m_bad_sexp_type_old_type = (SEXPTYPE)(s->sxpinfo.gp); // OLDTYPE(s);
#endif
        }
    }
}
