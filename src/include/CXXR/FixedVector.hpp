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

#include <algorithm>
#include <Localization.h>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/Logical.hpp>
#include <CXXR/Complex.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/BadObject.hpp>

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

        /** @brief Number of elements in the vector.
         *
         * @return The number of elements in the vector.
         *
         * @note AltRep uses its own version of size().
         */
        virtual size_type size() const override
        {
            return m_length;
        }

        virtual void setSize(size_type new_val) override
        {
            m_length = new_val;
        }

        /** @brief Number of occupied elements in the vector.
         *
         * @return The number of occupied elements in the vector.
         */
        virtual size_type truelength() const override
        {
            return m_truelength;
        }

        virtual void setTruelength(size_type new_val) override
        {
            m_truelength = new_val;
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
            return m_data[index];
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
            return m_data[index];
        }

        /** @brief Iterator designating first element.
         *
         * @return An iterator designating the first element of the
         * vector.  Returns end() if the vector is empty.
         */
        iterator begin() { return m_data; }

        /** @brief Const iterator designating first element.
         *
         * @return A const_iterator designating the first element of
         * the vector.  Returns end() if the vector is empty.
         */
        const_iterator begin() const { return m_data; }

        /** @brief One-past-the-end iterator.
         *
         * @return An iterator designating a position 'one past the
         * end' of the vector.
         */
        iterator end() { return begin() + size(); }

        /** @brief One-past-the-end const_iterator.
         *
         * @return A const_iterator designating a position 'one past
         * the end' of the vector.
         */
        const_iterator end() const { return begin() + size(); }

        // Virtual functions of VectorBase:
        virtual void *data() override
        {
            return m_data;
        }

        virtual const void *data() const override
        {
            return m_data;
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
        // Virtual functions of GCNode:
        void visitReferents(const_visitor *v) const override;
        void detachReferents() override;

        /**
         * Declared protected to ensure that FixedVector objects are
         * allocated only using 'new'.
         */
        ~FixedVector()
        {
#ifdef PROTECTCHECK
            if (sexptype() == FREESXP)
            {
                sxpinfo.type = SEXPTYPE(sxpinfo.gp);
            }
#endif
            switch (sexptype())
            {
            case RAWSXP:
            case LGLSXP:
            case INTSXP:
            case REALSXP:
            case CPLXSXP:
                break;
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
                break;
            default:
                BadObject::register_bad_object(this, __FILE__, __LINE__);
            }

            if (IS_GROWABLE(this))
            {
                m_length = m_truelength;
                sxpinfo.scalar = (m_length == 1);
            }

            destructElementsIfNeeded();

            R_size_t databytes = (size()) * sizeof(T);
            if (databytes != 0)
            {
                MemoryBank::deallocate(m_data, databytes, sxpinfo.m_ext_allocator);
            }
        }

    private:
        T *m_data; // pointer to the vector's data block.
        size_type m_length;
        size_type m_truelength; // the number of non-null elements in the vector or hash value in case of char (aka String class)

        /** @brief Create a vector, leaving its contents
         *         uninitialized (for POD types) or default
         *         constructed.
         *
         * @param n_elem Number of elements required.  Zero is
         *          permissible.
         *
         * @param allocator Custom allocator.
         */
        FixedVector(size_type n_elem, R_allocator_t *allocator = nullptr)
            : VectorBase(ST)
        {
            m_data = allocate(n_elem, allocator);
            m_length = n_elem;
            m_truelength = 0;
            sxpinfo.scalar = (n_elem == 1);
            sxpinfo.m_ext_allocator = (allocator != nullptr);
            constructElementsIfNeeded();
        }

        FixedVector &operator=(const FixedVector &) = delete;

        // If there is more than one element, this function is used to
        // allocate the required memory block from CXXR::MemoryBank :
        static T *allocate(size_type n_elem, R_allocator_t *allocator = nullptr);

        static void constructElements(iterator from, iterator to);
        static void constructElementsIfNeeded(iterator from, iterator to)
        {
            // This is essential for e.g. GCEdges, otherwise they
            // may contain junk pointers.
            if (ElementTraits::MustConstruct<T>::value) // compile-time constant
                constructElements(from, to);
        }

        void constructElementsIfNeeded()
        {
            constructElementsIfNeeded(begin(), end());
        }

        void destructElementsIfNeeded(iterator from, iterator to)
        {
            if (ElementTraits::MustDestruct<T>::value) // compile-time constant
                destructElements(from, to);
        }

        void destructElementsIfNeeded()
        {
            destructElementsIfNeeded(begin(), end());
        }

        void destructElements(iterator from, iterator to);

        // Helper functions for detachReferents():
        void detachElements(std::true_type);
        void detachElements(std::false_type) {}

        // Helper functions for visitReferents():
        void visitElements(const_visitor *v, std::true_type) const;
        void visitElements(const_visitor *v, std::false_type) const {}
    };

    // VectorTypeFor<T>::type is the type of vector that can hold elements of
    // type T.
    template <class T>
    struct VectorTypeFor
    {
    };

    template <typename T, SEXPTYPE ST>
    T *FixedVector<T, ST>::allocate(size_type n_elem, R_allocator_t *allocator)
    {
        // We allocate enough space for n_elem + 1 elements so that the stack scanner
        // recognizes end() as belonging to this object.
        size_type blocksize = (n_elem + 1) * sizeof(T);
        // Check for integer overflow:
        if (size_type(blocksize / sizeof(T)) != n_elem + 1)
            tooBig(blocksize);

        try
        {
            return static_cast<T *>(MemoryBank::allocate(blocksize, false, allocator));
        }
        catch (std::bad_alloc &e)
        {
            tooBig(blocksize);
            return nullptr;
        }
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::constructElements(iterator from, iterator to)
    {
        for (iterator p = from; p != to; ++p)
            new (p) T;
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::destructElements(iterator from, iterator to)
    {
        for (iterator p = from; p != to; ++p)
            p->~T();
    }

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

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::detachElements(std::true_type)
    {
        std::fill(begin(), end(), nullptr);
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::detachReferents()
    {
        if (!refCountEnabled())
            return;
        detachElements(typename ElementTraits::IsGCEdge<T>());
        VectorBase::detachReferents();
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::visitElements(const_visitor *v, std::true_type) const
    {
        std::for_each(begin(), end(), [=](GCNode *n)
                      {
                          if (n != R_NilValue)
                              (*v)(n);
                      });
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::visitReferents(const_visitor *v) const
    {
        VectorBase::visitReferents(v);
        visitElements(v, typename ElementTraits::IsGCEdge<T>());
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
