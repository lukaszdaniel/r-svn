/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file FixedVector.hpp
 *
 * @brief Class template CXXR::FixedVector.
 */

#ifndef FIXEDVECTOR_HPP
#define FIXEDVECTOR_HPP

#include <Localization.h>
#include <CXXR/VectorBase.hpp>

namespace CXXR
{
    /** @brief R data vector primarily intended for fixed-size use.
     *
     * This is a general-purpose class template to represent an R data
     * vector, and is intended particularly for the case where the
     * size of the vector is fixed when it is constructed.
     *
     * Having said that, the template \e does implement resizeInPlace(),
     * primarily to service CR code's occasional use of SETLENGTH().
     *
     * CXXR implements all of CR's built-in vector types using this
     * template.
     *
     * @tparam T The type of the elements of the vector.
     *
     * @tparam ST The required ::SEXPTYPE of the vector.
     */
    template <typename T, SEXPTYPE ST>
    class FixedVector: public VectorBase
    {
    public:
        using const_iterator = const T *;
        using iterator = T *;
        using value_type = T;

        /** @brief Create a vector, leaving its contents
         *         uninitialized (for POD types) or default
         *         constructed.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        static FixedVector *create(size_type sz, R_allocator_t *allocator = nullptr);

        /** @brief Create a vector containing a single value.
         *
         * @param value The value to store in the vector.
         */
        template <typename U>
        static FixedVector *createScalar(const U &value)
        {
            FixedVector *result = create(1);
            (*result)[0] = value;
            return result;
        }

        /** @brief Element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return Reference to the specified element.
         */
        T &operator[](size_type index)
        {
            return static_cast<T *>(u.vecsxp.m_data)[index];
        }

        /** @brief Read-only element access.
         *
         * @param index Index of required element (counting from
         *          zero).  No bounds checking is applied.
         *
         * @return \c const reference to the specified element.
         */
        const T &operator[](size_type index) const
        {
            return static_cast<const T *>(u.vecsxp.m_data)[index];
        }

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         *
         * @note This function is declared but not defined as part of
         * the FixedVector template.  It must be defined as a
         * specialization for each instantiation of the template for
         * which it or typeName() is used.
         */
        static const char *staticTypeName();

        // Virtual functions of RObject:
        const char *typeName() const override;

    protected:
        /**
         * Declared protected to ensure that FixedVector objects are
         * allocated only using 'new'.
         */
        ~FixedVector()
        {
        }

    private:
        /** @brief Create a vector, leaving its contents
         *         uninitialized (for POD types) or default
         *         constructed.
         *
         * @param sz Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        FixedVector(size_type sz, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, sz, allocator)
        {
        }

        FixedVector &operator=(const FixedVector &) = delete;
    };

    // VectorTypeFor<T>::type is the type of vector that can hold elements of
    // type T.
    template <class T>
    struct VectorTypeFor
    {
    };

    template <typename T, SEXPTYPE ST>
    FixedVector<T, ST> *FixedVector<T, ST>::create(size_type sz, R_allocator_t *allocator)
    {
        if (sz > R_XLEN_T_MAX) // sz > 2^52
            Rf_error(_("cannot allocate vector of length %lld"), (long long)sz);
        else if (sz < 0)
            Rf_error("%s", _("negative length vectors are not allowed"));
        return new FixedVector(sz, allocator);
    }

    template <typename T, SEXPTYPE ST>
    const char *FixedVector<T, ST>::typeName() const
    {
        return FixedVector<T, ST>::staticTypeName();
    }
} // namespace CXXR

namespace R
{
} // namespace R

extern "C"
{
    /** @brief Is an object of numeric type.
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if CXXR::RObject is integer, logical or real.
     *
     * @todo the LGLSXP case should be excluded here
     *       (really? in many places we affirm they are treated like INTs)
     */
    Rboolean Rf_isNumeric(SEXP s);

    /** @brief Is an object "Numeric" or complex?
     *
     * @param s Pointer to an CXXR::RObject.
     *
     * @return \c true if CXXR::RObject is integer, logical, real or complex.
     */
    Rboolean Rf_isNumber(SEXP s);
} // extern "C"

#endif // FIXEDVECTOR_HPP
