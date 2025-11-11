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
#include <CXXR/MemoryBank.hpp>
#include <CXXR/Logical.hpp>
#include <CXXR/Complex.hpp>
#include <CXXR/VectorBase.hpp>
#include <CXXR/String.hpp> // for String::blank()
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

        // Virtual functions of VectorBase:
        virtual void *data() override
        {
            return u.vecsxp.m_data;
        }

        virtual const void *data() const override
        {
            return u.vecsxp.m_data;
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
            if (u.vecsxp.m_data)
            {
                const auto getVecSizeInBytes = [](VectorBase *s) -> R_size_t {
#ifdef PROTECTCHECK
                    if (s->sexptype() == FREESXP)
                    {
                        s->sxpinfo.type = SEXPTYPE(s->sxpinfo.gp);
                    }
#endif
                    if (IS_GROWABLE(s))
                    {
                        s->u.vecsxp.m_length = s->truelength();
                        s->sxpinfo.scalar = (s->u.vecsxp.m_length == 1);
                    }

                    R_size_t size = 0;
                    R_size_t n_elem = s->size();
                    switch (s->sexptype())
                    { /* get size in bytes */
                    case RAWSXP:
                        size = n_elem * sizeof(Rbyte);
                        break;
                    case LGLSXP:
                        size = n_elem * sizeof(Logical);
                        break;
                    case INTSXP:
                        size = n_elem * sizeof(int);
                        break;
                    case REALSXP:
                        size = n_elem * sizeof(double);
                        break;
                    case CPLXSXP:
                        size = n_elem * sizeof(Complex);
                        break;
                    case STRSXP:
                    case EXPRSXP:
                    case VECSXP:
                        size = n_elem * sizeof(SEXP);
                        break;
                    default:
                        BadObject::register_bad_object(s, __FILE__, __LINE__);
                        size = 0;
                    }
                    return size;
                    };
                R_size_t databytes = getVecSizeInBytes(this);
                MemoryBank::deallocate(u.vecsxp.m_data, databytes, sxpinfo.m_ext_allocator);
            }
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
        FixedVector(size_type n_elem, R_allocator_t *allocator = nullptr)
            : VectorBase(ST, n_elem, allocator)
        {
            R_size_t actual_size = 0; // in bytes
            switch (ST)
            {
            case RAWSXP:
                actual_size = n_elem * sizeof(Rbyte);
                break;
            case LGLSXP:
                actual_size = n_elem * sizeof(Logical);
                break;
            case INTSXP:
                actual_size = n_elem * sizeof(int);
                break;
            case REALSXP:
                actual_size = n_elem * sizeof(double);
                break;
            case CPLXSXP:
                actual_size = n_elem * sizeof(Complex);
                break;
            case STRSXP:
            case EXPRSXP:
            case VECSXP:
                actual_size = n_elem * sizeof(SEXP);
                break;
            default:
                break;
            }

            u.vecsxp.m_data = (MemoryBank::allocate(actual_size, false, allocator));
            u.vecsxp.m_length = n_elem;
            sxpinfo.scalar = (n_elem == 1);

            /* The following prevents disaster in the case */
            /* that an uninitialised string vector is marked */
            /* Direct assignment is OK since the node was just allocated and */
            /* so is at least as new as R_NilValue and R_BlankString */
            if (ST == EXPRSXP || ST == VECSXP)
            {
                SEXP *data = ((SEXP *)(u.vecsxp.m_data)); // VECTOR_PTR(this);
                for (R_xlen_t i = 0; i < n_elem; i++)
                    data[i] = R_NilValue;
            }
            else if (ST == STRSXP)
            {
                SEXP *data = ((SEXP *)(u.vecsxp.m_data)); // STRING_PTR(this);
                for (R_xlen_t i = 0; i < n_elem; i++)
                    data[i] = String::blank();
            }
            else
            {
                std::memset(u.vecsxp.m_data, 0, actual_size);
            }
        }

        FixedVector &operator=(const FixedVector &) = delete;

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
        for (R_xlen_t i = 0; i < this->size(); i++)
        {
            auto el = static_cast<GCEdge<> *>(this->u.vecsxp.m_data)[i];
            el = R_NilValue;
        }
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::detachReferents()
    {
        if (!this->refCountEnabled())
            return;
        detachElements(typename ElementTraits::IsGCEdge<T>());
        VectorBase::detachReferents();
    }

    template <typename T, SEXPTYPE ST>
    void FixedVector<T, ST>::visitElements(const_visitor *v, std::true_type) const
    {
        for (R_xlen_t i = 0; i < this->size(); i++)
        {
            const RObject *el = static_cast<GCEdge<> *>(this->u.vecsxp.m_data)[i];
            if (el != R_NilValue)
                (*v)(el);
        }
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
