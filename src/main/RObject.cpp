/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2020  The R Core Team.
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

/** @file RObject.cpp
 *
 * Class RObject and associated C interface functions.
 */

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/PairList.hpp>
#include <Localization.h>

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &DUPLICATE_ATTRIBptr = DUPLICATE_ATTRIB;
        const auto &SHALLOW_DUPLICATE_ATTRIBptr = SHALLOW_DUPLICATE_ATTRIB;
        const auto &isNullptr = Rf_isNull;
        const auto &isObjectptr = Rf_isObject;
        const auto &NAMEDptr = NAMED;
        const auto &OBJECTptr = OBJECT;
        const auto &SET_NAMEDptr = SET_NAMED;
        const auto &ENSURE_NAMEDMAXptr = R::ENSURE_NAMEDMAX;
        const auto &ENSURE_NAMEDptr = R::ENSURE_NAMED;
        const auto &SETTER_CLEAR_NAMEDptr = R::SETTER_CLEAR_NAMED;
        const auto &RAISE_NAMEDptr = R::RAISE_NAMED;
        const auto &TYPEOFptr = TYPEOF;
        const auto &LEVELSptr = LEVELS;
        const auto &SETLEVELSptr = SETLEVELS;
    } // namespace ForceNonInline

    bool GlobalParameter::s_mbcslocale = false;
#ifdef _WIN32
    bool GlobalParameter::s_UserBreak = false;
#endif

    RObject::RObject() : GCNode(NILSXP)
    {
        // u.listsxp.m_car = nullptr;
        // u.listsxp.m_tail = nullptr;
        // u.listsxp.m_tag = nullptr;
        // m_attrib = nullptr;
    }

    RObject::RObject(SEXPTYPE stype) : GCNode(stype)
    {
        // u.listsxp.m_car = nullptr;
        // u.listsxp.m_tail = nullptr;
        // u.listsxp.m_tag = nullptr;
        m_attrib = R_NilValue;
    }

    bool RObject::hasAttributes() const
    {
        return attributes() != R_NilValue;
    }

    void RObject::clearAttributes()
    {
        if (m_attrib != R_NilValue)
        {
            SET_ATTRIB(this, R_NilValue);
            sxpinfo.obj = 0;
        }
    }

    RObject *RObject::getAttribute(const Symbol *name) const
    {
        for (const PairList *node = static_cast<PairList *>(m_attrib.get()); node && node != R_NilValue; node = node->tail())
            if (node->tag() == name)
                return node->car0();
        return R_NilValue;
    }

    void RObject::copyAttribute(const Symbol *name, const RObject *source)
    {
        RObject *att = source->getAttribute(name);
        if (att != R_NilValue)
            setAttribute(name, att);
    }

    /* Tweaks here based in part on PR#14934 */
    // This follows CR in adding new attributes at the end of the list,
    // though it would be easier to add them at the beginning.
    void RObject::setAttribute(const Symbol *name, RObject *value)
    {
        if (name == R_NilValue)
            Rf_error("%s", _("attempt to set an attribute on NULL"));
        if (sexptype() == CHARSXP)
            Rf_error("%s", _("cannot set attribute on a 'CHARSXP'"));
        if (sexptype() == SYMSXP)
            Rf_error("%s", _("cannot set attribute on a symbol"));
        // Update m_has_class if necessary:
        if (name == R_ClassSymbol)
            sxpinfo.obj = (value != R_NilValue);
        // Find attribute:
        /* this does no allocation */
        PairList *prev = nullptr;
        PairList *node = static_cast<PairList *>(m_attrib.get());
        while (node && node != R_NilValue && node->tag() != name)
        {
            prev = node; // record last attribute, if any
            node = node->tail();
        }

        if (node && node != R_NilValue)
        { // Attribute already present
            // Update existing attribute:
            if (value && value != R_NilValue)
            {
                if (MAYBE_REFERENCED(value) && value != node->car0())
                    value = R::R_FixupRHS(this, value);
                node->setCar(value);
            }
            else if (prev && prev != R_NilValue)
            { // Delete existing attribute:
                prev->setTail(node->tail());
            }
            else
            {
                m_attrib.retarget(this, node->tail());
            }
        }
        else if (value && value != R_NilValue)
        {
            // Create new node:
            /* The usual convention is that the caller protects,
               but a lot of existing code depends assume that
               setAttrib/installAttrib protects its arguments */
            GCStackRoot<const Symbol> namer(name);
            GCStackRoot<> valuer(value);
            if (MAYBE_REFERENCED(value))
                R::ENSURE_NAMEDMAX(value);
            PairList *newnode = PairList::create(value, R_NilValue, const_cast<Symbol *>(name));
            if (prev && prev != R_NilValue)
                prev->setTail(newnode);
            else
            { // No preexisting attributes at all:
                m_attrib.retarget(this, newnode);
            }
        }
    }

    // This has complexity O(n^2) where n is the number of attributes, but
    // we assume n is very small.
    void RObject::setAttributes(PairList *new_attributes)
    {
        clearAttributes();
#if CXXR_TRUE // temporarily
        // TODO: Such retarget is needed here because RObject might be in older
        // generation than the newly assigned new_attributes
        m_attrib.retarget(this, new_attributes);

        for (const PairList *node = static_cast<PairList *>(m_attrib.get()); node != R_NilValue; node = node->tail())
            if (node->tag() == R_ClassSymbol)
            {
                sxpinfo.obj = (node->car0() != R_NilValue);
                return;
            }
#else
        // TODO: Below code results in installation error for package "vctrs".
        // Error: Can't bind data because some elements are not named.
        // Error: unable to load R code in package ‘vctrs’
        // This is because vctrs package modifies the attributes
        // after SET_ATTRIB has been called.
        while (new_attributes && new_attributes != R_NilValue)
        {
            const Symbol *name = static_cast<const Symbol *>(new_attributes->tag());
            setAttribute(const_cast<Symbol *>(name), new_attributes->car0());
            new_attributes = new_attributes->tail();
        }
#endif
    }

    // The implementation of RObject::traceMemory() is in debug.cpp
} // namespace CXXR

namespace R
{
    void maybeTraceMemory1(SEXP dest, SEXP src)
    {
#ifdef R_MEMORY_PROFILING
        dest->maybeTraceMemory(src);
#endif
    }

    void maybeTraceMemory2(SEXP dest, SEXP src1, SEXP src2)
    {
#ifdef R_MEMORY_PROFILING
        dest->maybeTraceMemory(src1, src2);
#endif
    }
} // namespace R

// ***** C interface *****

