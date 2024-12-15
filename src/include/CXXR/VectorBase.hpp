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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file VectorBase.hpp
 * @brief Class CXXR::VectorBase and associated C interface.
 */

#ifndef VECTORBASE_HPP
#define VECTORBASE_HPP

#include <CXXR/RObject.hpp>
#include <R_ext/Rallocators.h>

namespace CXXR
{
/* The generational collector uses a reduced version of RObject as a
   header in vector nodes.  The layout MUST be kept consistent with
   the RObject definition. The standard RObject takes up the size of 7 doubles
   and the reduced version takes 6 doubles on most 64-bit systems. On most
   32-bit systems, RObject takes 8 doubles and the reduced version 7 doubles. */

    /** @brief Untemplated base class for R vectors.
     */
    class VectorBase: public RObject
    {
    public:
        using size_type = R_xlen_t;

        /**
         * @param stype The required ::SEXPTYPE.
         *
         * @param sz The required number of elements in the vector.
         */
        VectorBase(SEXPTYPE stype, size_type sz, R_allocator_t *allocator);

        VectorBase(SEXPTYPE stype): RObject(stype)
        {
            u.vecsxp.m_length = 0;
            u.vecsxp.m_truelength = 0;
            u.vecsxp.m_data = nullptr;
        }

        /** @brief Number of elements in the vector.
         *
         * @return The number of elements in the vector.
         *
         * @note AltRep uses its own version of size().
         */
        size_type size() const
        {
            return u.vecsxp.m_length;
        }

        /** @brief Number of occupied elements in the vector.
         *
         * @return The number of occupied elements in the vector.
         */
        size_type truelength() const
        {
            return u.vecsxp.m_truelength;
        }

        /** @brief Is an RObject a Vector?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a Vector (or, in future, an AltRep of Vector type).
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;

            switch (obj->sexptype())
            {
            case VECSXP:
            case EXPRSXP:
            case CPLXSXP:
            case RAWSXP:
            case LGLSXP:
            case INTSXP:
            case REALSXP:
            case STRSXP:
                return true;
            default:
                return false;
            }
        }

        /** @brief Raise error on attempt to allocate overlarge vector.
         *
         * @param bytes Size of data block for which allocation failed.
         */
        static void tooBig(size_type bytes);

    protected:
        // Declared protected to ensure that VectorBase objects are
        // allocated only using 'new':
        ~VectorBase();

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        VectorBase(const VectorBase &);
        VectorBase &operator=(const VectorBase &);
    };
    typedef class VectorBase *VECSEXP;
} // namespace CXXR

#endif /* VECTORBASE_HPP */
