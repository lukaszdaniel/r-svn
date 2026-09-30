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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/WeakRef.hpp>
#include <Defn.h> // for WEAKREF_KEY, WEAKREF_VALUE, WEAKREF_FINALIZER macros

#define READY_TO_FINALIZE_MASK 1
#define SET_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp |= READY_TO_FINALIZE_MASK)
#define IS_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp & READY_TO_FINALIZE_MASK)

namespace CXXR
{
    std::list<SEXP> WeakRef::s_R_weak_refs;
    bool WeakRef::s_R_finalizers_pending = false;

    void WeakRef::markThru(GCNode::Marker *v)
    {
        unsigned int max_generation = v->maxgen() - 1;
        {
            bool recheck_weak_refs;
            const auto mark_referent = [&](SEXP referent) {
                if (referent != R_NilValue && NODE_GENERATION(referent) < max_generation && !referent->isMarked()) {
                    recheck_weak_refs = true;
                    (*v)(referent);
                }
                };

            do {
                recheck_weak_refs = false;
                for (auto &s : s_R_weak_refs) {
                    if (s != R_NilValue) {
                        const auto key = WEAKREF_KEY(s);
                        if (key && key->isMarked()) {
                            mark_referent(WEAKREF_VALUE(s));
                            mark_referent(WEAKREF_FINALIZER(s));
                        }
                    }
                }
            } while (recheck_weak_refs);
        }

        /* CheckFinalizers: mark nodes ready for finalizing */
        {
            s_R_finalizers_pending = false;
            for (auto &s : s_R_weak_refs) {
                if (s != R_NilValue) {
                    const auto key = WEAKREF_KEY(s);
                    if (key && !key->isMarked() && !IS_READY_TO_FINALIZE(s))
                        SET_READY_TO_FINALIZE(s);
                    if (IS_READY_TO_FINALIZE(s))
                        s_R_finalizers_pending = true;
                }
            }
        }

        /* process the weak reference chain */
        for (auto &s : s_R_weak_refs) {
            if (s != R_NilValue) (*v)(s);
            const auto key = WEAKREF_KEY(s);
            const auto value = WEAKREF_VALUE(s);
            const auto finalizer = WEAKREF_FINALIZER(s);
            if (key != R_NilValue) (*v)(key);
            if (value != R_NilValue) (*v)(value);
            if (finalizer != R_NilValue) (*v)(finalizer);
        }
    }

    void WeakRef::detachReferents()
    {
        if (!this->refCountEnabled())
            return;
        m_key.detach();
        m_value.detach();
        m_finalizer.detach();
        RObject::detachReferents();
    }

    void WeakRef::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        const GCNode *weakref_key = m_key;
        const GCNode *weakref_value = m_value;
        const GCNode *weakref_finalizer = m_finalizer;

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
