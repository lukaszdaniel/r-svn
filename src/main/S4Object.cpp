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

/** @file S4Object.cpp
 *
 * @brief Class S4Object and associated C interface.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/S4Object.hpp>
#include <Defn.h> // for SET_S4_OBJECT

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &IS_S4_OBJECTptr = IS_S4_OBJECT;
        const auto &SET_S4_OBJECTptr = SET_S4_OBJECT;
        const auto &UNSET_S4_OBJECTptr = UNSET_S4_OBJECT;
    } // namespace ForceNonInline

    S4Object::S4Object(bool is_s4_object) : RObject(OBJSXP)
    {
        u.s4ptr.m_tag = R_NilValue;
        if (is_s4_object)
            SET_S4_OBJECT(this);
    }

    const char *S4Object::typeName() const
    {
        return S4Object::staticTypeName();
    }

    void S4Object::detachReferents()
    {
        if (!this->refCountEnabled())
            return;
        u.s4ptr.m_tag.detach();
        RObject::detachReferents();
    }

    void S4Object::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        const GCNode *s4tag = u.s4ptr.m_tag;

        if (s4tag != R_NilValue)
            (*v)(s4tag);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

