/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

/** @file WeakRef.cpp
 *
 * Class WeakRef.
 */

#include <CXXR/WeakRef.hpp>
#include <Defn.h> // for WEAKREF_KEY, WEAKREF_VALUE, WEAKREF_FINALIZER macros

namespace CXXR
{
    void WeakRef::detachReferents()
    {
        // if (!this->refCountEnabled())
        //     return;
        // WEAKREF_KEY(this).detach();
        // WEAKREF_VALUE(this).detach();
        // WEAKREF_FINALIZER(this).detach();
        RObject::detachReferents();
    }

    void WeakRef::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        const GCNode *weakref_key = WEAKREF_KEY(this);
        const GCNode *weakref_value = WEAKREF_VALUE(this);
        const GCNode *weakref_finalizer = WEAKREF_FINALIZER(this);

        if (weakref_key != R_NilValue)
            (*v)(weakref_key);
        if (weakref_value != R_NilValue)
            (*v)(weakref_value);
        if (weakref_finalizer != R_NilValue)
            (*v)(weakref_finalizer);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****
