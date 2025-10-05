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

#include <CXXR/config.hpp>
#include <CXXR/Complex.hpp>
#include <CXXR/RObject.hpp>
#include <R_ext/Rallocators.h>
#include <R_ext/Error.h> // for NORET

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

        /** @brief The name by which this type is known in R.
         *
         * @return The name by which this type is known in R.
         */
        static const char *staticTypeName()
        {
            return "(vector type)";
        }

    protected:
        // Declared protected to ensure that VectorBase objects are
        // allocated only using 'new':
        ~VectorBase();

        /** @brief Raise error on attempt to allocate overlarge vector.
         *
         * @param bytes Size of data block for which allocation failed.
         */
        static void tooBig(size_type bytes);

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        VectorBase(const VectorBase &);
        VectorBase &operator=(const VectorBase &);
    };
    typedef class VectorBase *VECSEXP;
} // namespace CXXR

namespace R
{
    /**
     * @param x Pointer to an CXXR::VectorBase.
     *
     * @return The length of \a x, or 0 if \a x is a null pointer.  (In
     *         the case of certain hash tables, this means the 'capacity'
     *         of \a x , not all of which may be used.)
     */
    R_xlen_t (STDVEC_LENGTH)(SEXP x);

    /**
     * @param x Pointer to a CXXR::VectorBase.
     *
     * @return The 'true length' of \a x.  According to the R Internals
     *         document for R 2.4.1, this is only used for certain hash
     *         tables, and signifies the number of used slots in the
     *         table.
     *
     * @deprecated May be withdrawn in the future.
     */
    R_xlen_t (STDVEC_TRUELENGTH)(SEXP x);

#ifdef LONG_VECTOR_SUPPORT
    NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif
} // namespace R

extern "C"
{
    /* Accessor functions */

    /* Vector Access Functions */

    /**
     * @param x Pointer to a CXXR::RObject.
     *
     * @return The length of \a x, or 0 if \a x is a null pointer, or is
     *         not a pointer to a vector object (VectorBase).  (In
     *         the case of certain hash tables, this means the 'capacity'
     *         of \a x , not all of which may be used.)
     */
    int (LENGTH)(SEXP x);

    /**
     * @param x Pointer to a CXXR::VectorBase.
     *
     * @return The 'true length' of \a x.  According to the R Internals
     *         document for R 2.4.1, this is only used for certain hash
     *         tables, and signifies the number of used slots in the
     *         table.
     */
    R_xlen_t (TRUELENGTH)(SEXP x);

    /** @brief Set length of vector.
     *
     * @param x Pointer to a CXXR::VectorBase.
     *
     * @param v The required new length, which must not be greater than
     *          the current length.
     *
     * @deprecated May be withdrawn in future.  Currently used in
     * library/stats/src/isoreg.cpp , and possibly in packages.
     */
    void (SETLENGTH)(SEXP x, R_xlen_t v);

    /** @brief Set 'true length' of vector.
     *
     * @param x Pointer to a CXXR::VectorBase.
     *
     * @param v The required new 'true length'.
     *
     * @deprecated May be withdrawn in the future.
     */
    void (SET_TRUELENGTH)(SEXP x, R_xlen_t v);

    /** @brief Create a vector object.
     *
     *  Allocate a vector object.  This ensures only validity of
     *  ::SEXPTYPE values representing lists (as the elements must be
     *  initialized).  Initializing of other vector types is done in
     *  do_makevector().
     *  Regular Rf_allocVector() as a special case of allocVector3()
     *  with no custom allocator.
     *
     * @param stype The type of vector required.
     *
     * @param length The length of the vector to be created.
     *
     * @return Pointer to the created vector.
     */
    SEXP Rf_allocVector(SEXPTYPE stype, R_xlen_t length);

    /** @brief Create a vector object.
     *
     *  Allocate a vector object.  This ensures only validity of
     *  ::SEXPTYPE values representing lists (as the elements must be
     *  initialized).  Initializing of other vector types is done in
     *  do_makevector().
     *
     * @param stype The type of vector required.
     *
     * @param length The length of the vector to be created.
     *
     * @param length Custom allocator to be used.
     *
     * @return Pointer to the created vector.
     */
    SEXP Rf_allocVector3(SEXPTYPE type, R_xlen_t length, R_allocator_t *allocator);

    /** @brief Is an RObject a vector?
     *
     * Vector in this context embraces R matrices and arrays.
     *
     * @param s Pointer to the RObject to be tested.  The pointer may be
     *          null, in which case the function returns FALSE.
     *
     * @return TRUE iff \a s points to a vector object.
     */
    Rboolean Rf_isVector(SEXP s);

    /** @fn SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names)
     *
     * @brief Create a named vector of type TYP
     *
     * @example const char *nms[] = {"xi", "yi", "zi", ""};
     *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
     *
     * @param TYP a vector SEXP type (e.g. REALSXP)
     *
     * @param names names of list elements with null string appended
     *
     * @return (pointer to a) named vector of type TYP
     */
    SEXP Rf_mkNamed(SEXPTYPE TYP, const char **names);

    /** @brief shortcut for ScalarString(Rf_mkChar(s))
     *
     * @return string scalar
     *
     * @note from gram.y
     */
    SEXP Rf_mkString(const char *s);

    Rboolean Rf_isVectorList(SEXP s);
    Rboolean Rf_isVectorAtomic(SEXP s);
    Rboolean Rf_isMatrix(SEXP s);
    Rboolean Rf_isArray(SEXP s);
    Rboolean Rf_isTs(SEXP s);

    int IS_SCALAR(SEXP x, SEXPTYPE type);

    /** @brief The general (read only) data pointer function
     *
     * Function works as a dispatcher between ALTREP
     * or STDVEC representation of data.
     *
     * @return pointer to the (read only) data block
     */
    const void *DATAPTR_RO(SEXP x);

    /* Growable vector support */
    int (IS_GROWABLE)(SEXP x);
    void (SET_GROWABLE_BIT)(SEXP x);
    R_xlen_t (XLENGTH)(SEXP x);
    int (IS_LONG_VEC)(SEXP x);

    /* temporary, to ease transition away from remapping */
    R_xlen_t Rf_XLENGTH(SEXP x);

    int (ALTREP)(SEXP x);
    SEXP R_tryWrap(SEXP x);
    SEXP (ALTREP_CLASS)(SEXP x);
    SEXP R_altrep_data1(SEXP x);
    SEXP R_altrep_data2(SEXP x);
    void R_set_altrep_data1(SEXP x, SEXP v);
    void R_set_altrep_data2(SEXP x, SEXP v);

    R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
    R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf);
    R_xlen_t LOGICAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf);
    R_xlen_t COMPLEX_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rcomplex *buf);
    R_xlen_t RAW_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, Rbyte *buf);

    /* metadata access */
    int INTEGER_IS_SORTED(SEXP x);
    int INTEGER_NO_NA(SEXP x);
    int REAL_IS_SORTED(SEXP x);
    int REAL_NO_NA(SEXP x);
    int LOGICAL_IS_SORTED(SEXP x);
    int LOGICAL_NO_NA(SEXP x);
    int STRING_IS_SORTED(SEXP x);
    int STRING_NO_NA(SEXP x);
} // extern "C"

#endif // VECTORBASE_HPP
