/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file S4Object.hpp
 *
 * @brief Class CXXR::S4Object and associated C interface.
 *
 * (S4Object implements S4SXP.)
 */

#ifndef S4OBJECT_HPP
#define S4OBJECT_HPP

#include <CXXR/RObject.hpp>

namespace CXXR
{
    /** @brief S4 object.
     *
     * This class is used to implement S4 classes that do not extend
     * objects of another R type, and corresponds to the ::SEXPTYPE
     * S4SXP.
     *
     */
    class S4Object : public RObject
    {
    public:
        static S4Object *create(bool is_s4_object = true)
        {
            return new S4Object(is_s4_object);
        }

        /**
         * @return a const pointer to the 'tag' of this S4Object
         * element.
         */
        const RObject *tag() const
        {
            return u.s4ptr.m_tag;
        }

        /**
         * @return a pointer to the 'tag' of this S4Object.
         */
        RObject *tag()
        {
            return u.s4ptr.m_tag;
        }

        /** @brief Set the 'tag' value.
         *
         * @param tg Pointer to the new tag object (or a null
         *           pointer).
         */
        void setS4Tag(RObject *tg)
        {
            u.s4ptr.m_tag.retarget(this, tg);
        }

        /** @brief Is an RObject an S4Object?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is an S4Object.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == OBJSXP;
        }

    private:
        /** @brief Default constructor.
         */
        S4Object(bool is_s4_object = true);

        // Declared private to ensure that S4Object objects are
        // allocated only using 'new':
        ~S4Object() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        S4Object(const S4Object &);
        S4Object &operator=(const S4Object &);
    };
} // namespace CXXR

namespace R
{
    /** @brief Create a non-S4 Object.
     *
     * @return Pointer to the created object.
     */
    SEXP R_allocObject(void);
} // namespace R

extern "C"
{
    /** @brief Get tag of CXXR::S4Object.
     *
     * @param e Pointer to a CXXR::S4Object (checked), or a null pointer.
     *
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    SEXP S4TAG(SEXP e);

    /** @brief Set the tag of a CXXR::S4Object.
     *
     * @param x Pointer to a CXXR::S4Object (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new tag of
     *          the CXXR::S4Object.
     */
    void SET_S4TAG(SEXP x, SEXP y);

    /** @brief Is this an S4 object?
     *
     * @param x Pointer to \c RObject.
     *
     * @return true iff \a x is an S4 object.  Returns false if \a x
     * is 0.
     */
    int IS_S4_OBJECT(SEXP x);

    /**
     * @deprecated Ought to be private.
     */
    void SET_S4_OBJECT(SEXP x);

    /**
     * @deprecated Ought to be private.
     */
    void UNSET_S4_OBJECT(SEXP x);

    /** @brief Create an S4 object.
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocS4Object();
} // extern "C"

#endif /* S4OBJECT_HPP */
