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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file RAltRep.cpp
 *
 * @brief Class CXXR::AltRep.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <format>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAltRep.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/ComplexVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/StringVector.hpp>
#include <Defn.h> // for CLASS, DATA1, DATA2 macros

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
    } // namespace ForceNonInline

    AltRep *AltRep::create(SEXP data1, SEXP data2, SEXP sclass)
    {
        GCStackRoot<> data1rt(data1);
        GCStackRoot<> data2rt(data2);
        GCStackRoot<> sclassrt(sclass);

        return new AltRep(data1, data2, sclass);
    }

    const char *AltRep::typeName() const
    {
        return staticTypeName();
    }

    RObject *AltRep::clone(Duplication depth) const
    {
        GCStackRoot<const AltRep> thisroot(this);
        SEXP source = const_cast<AltRep *>(this);
        SEXP ans = R::ALTREP_DUPLICATE_EX(source, Rboolean(depth == Duplication::DEEP));
        if (ans)
        {
            return SEXP_downcast<RObject *>(ans);
        }

        R_xlen_t sz = XLENGTH(source);
        GCStackRoot<RObject> t;
        switch (sexptype())
        {
        case LGLSXP:
            t = LogicalVector::create(sz);
            if (sz)
                memcpy(LOGICAL(t), LOGICAL_RO(source), sz * sizeof(int));
            break;
        case INTSXP:
            t = IntVector::create(sz);
            if (sz)
                memcpy(INTEGER(t), INTEGER_RO(source), sz * sizeof(int));
            break;
        case REALSXP:
            t = RealVector::create(sz);
            if (sz)
                memcpy(REAL(t), REAL_RO(source), sz * sizeof(double));
            break;
        case CPLXSXP:
            t = ComplexVector::create(sz);
            if (sz)
                memcpy(COMPLEX(t), COMPLEX_RO(source), sz * sizeof(Rcomplex));
            break;
        case RAWSXP:
            t = RawVector::create(sz);
            if (sz)
                memcpy(RAW(t), RAW_RO(source), sz * sizeof(Rbyte));
            break;
        case STRSXP:
            t = StringVector::create(sz);
            for (R_xlen_t i = 0; i < sz; ++i)
            {
                SEXP value = STRING_ELT(source, i);
                SET_STRING_ELT(t, i, value);
            }
            break;
        case EXPRSXP:
            t = ExpressionVector::create(sz);
            for (R_xlen_t i = 0; i < sz; ++i)
            {
                SEXP value = XVECTOR_ELT(source, i);
                SET_XVECTOR_ELT(t, i, depth == Duplication::DEEP ? RObject::clone(value, depth) : value);
            }
            break;
        case VECSXP:
            t = ListVector::create(sz);
            for (R_xlen_t i = 0; i < sz; ++i)
            {
                SEXP value = VECTOR_ELT(source, i);
                SET_VECTOR_ELT(t, i, depth == Duplication::DEEP ? RObject::clone(value, depth) : value);
            }
            break;
        default:
            throw std::runtime_error(std::format("AltRep::clone(): cloning not yet implemented for {}.", static_cast<int>(sexptype())));
        }
        t->cloneAttributes(*this, depth);
        if (!(R::IS_GROWABLE)(source))
            SET_TRUELENGTH(t, XTRUELENGTH(source));
        return t;
    }

    void AltRep::detachReferents()
    {
        if (!this->refCountEnabled())
            return;
        m_altclass.detach();
        m_data1.detach();
        m_data2.detach();
        RObject::detachReferents();
    }

    void AltRep::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        const GCNode *altclass = m_altclass;
        const GCNode *altdata1 = m_data1;
        const GCNode *altdata2 = m_data2;

        if (altclass != R_NilValue)
            (*v)(altclass);
        if (altdata1 != R_NilValue)
            (*v)(altdata1);
        if (altdata2 != R_NilValue)
            (*v)(altdata2);
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

