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

/** @file ConsCell.hpp
 * @brief Class CXXR::ConsCell and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, this
 * file also includes the definition of class CXXR::PairList.
 *
 * This file includes C functions for examining and setting the CAR
 * and TAG of a CXXR::ConsCell; functions for examining and setting
 * the CDR, and other operations accessing the tail of the list, are
 * to be found in PairList.hpp.
 */

#ifndef CONSCELL_HPP
#define CONSCELL_HPP

#include <iostream>
#include <CXXR/RObject.hpp>

namespace CXXR
{
    class PairList;

    /** @brief Element of a singly linked list.
     *
     * Element of a LISP-like singly-linked list, containing pointers
     * to a 'car' object (this is LISP terminology, and has nothing to
     * do with automobiles) and to a 'tag' object, as well as a
     * pointer to the next element of the list, which must be of the
     * derived type PairList.  (Any of these pointers may be null.)
     *
     * @note This class is used as a base class to implement CR's
     * LISTSXP, LANGSXP, DOTSXP.
     * Because what these SEXPTYPEs have in common is implementation
     * rather than meaning in the application domain, canons of
     * object-oriented design would argue against their publicly
     * inheriting from a common base class.  Without doing this,
     * however, it would have been difficult efficiently to implement
     * functions such as CAR(), which are ubiquitous in the CR code.
     */
    class ConsCell : public RObject
    {
    public:
        /** @brief iterator for iterating over a ConsCell list.
         */
        template <typename ValueType = ConsCell>
        class iterator_tmpl
        {
        public:
            using iterator_category = std::forward_iterator_tag;
            using value_type = ValueType;
            using difference_type = ptrdiff_t;
            using pointer = ValueType *;
            using reference = ValueType &;

            /** @brief Constructor.
             *
             * @param cc Pointer, possibly null, to the ConsCell to be
             *           designated by the iterator.
             */
            explicit iterator_tmpl(ValueType *cc = nullptr) : m_cc(cc) {}

            ValueType &operator*() const { return *m_cc; }

            ValueType *operator->() const { return m_cc; }

            iterator_tmpl operator++()
            {
                advance();
                return *this;
            }

            iterator_tmpl operator++(int)
            {
                iterator_tmpl ans = *this;
                advance();
                return ans;
            }

            bool operator==(iterator_tmpl other) const
            {
                return m_cc == other.m_cc;
            }
            bool operator!=(iterator_tmpl other) const
            {
                return m_cc != other.m_cc;
            }

        private:
            ValueType *m_cc;

            void advance();
        };

        /** @brief const_iterator for iterating over a ConsCell list.
         */
        template <typename ValueType = ConsCell>
        class const_iterator_tmpl
        {
        public:
            using iterator_category = std::forward_iterator_tag;
            using value_type = const ValueType;
            using difference_type = ptrdiff_t;
            using pointer = const ValueType *;
            using reference = const ValueType &;

            /** @brief Constructor.
             *
             * @param cc Pointer, possibly null, to the ConsCell to be
             *           designated by the const_iterator.
             */
            explicit const_iterator_tmpl(const ValueType *cc = nullptr) : m_cc(cc)
            {
            }

            const ValueType &operator*() const { return *m_cc; }

            const ValueType *operator->() const { return m_cc; }

            const_iterator_tmpl operator++()
            {
                advance();
                return *this;
            }

            const_iterator_tmpl operator++(int)
            {
                const_iterator_tmpl ans = *this;
                advance();
                return ans;
            }

            bool operator==(const_iterator_tmpl other) const
            {
                return m_cc == other.m_cc;
            }
            bool operator!=(const_iterator_tmpl other) const
            {
                return m_cc != other.m_cc;
            }

        private:
            const ValueType *m_cc;

            void advance();
        };

        using iterator = iterator_tmpl<ConsCell>;
        using const_iterator = const_iterator_tmpl<ConsCell>;

        iterator begin() { return iterator(this); }

        const_iterator begin() const { return const_iterator(this); }

        iterator end() { return iterator(); }

        const_iterator end() const { return const_iterator(); }

        /** @brief Get the 'car' value.
         *
         * @return a const pointer to the 'car' of this ConsCell
         * element.
         *
         * @note Binding tag is not checked.
         */
        RObject *car0() const
        {
            return u.listsxp.m_car;
        }

        /** @brief Get the 'car' value.
         *
         * @return a const pointer to the 'car' of this ConsCell
         * element.
         */
        RObject *car() const;

        /** @brief Number of elements in list.
         *
         * @param start Pointer to a ConsCell, possibly null.
         *
         * @return zero if \a start is a null pointer, otherwise the
         * number of elements in the list starting at the ConsCell
         * pointed to by \a start.
         */
        template <typename T = R_xlen_t>
        static T listLength(const ConsCell *start)
        {
            return (start ? T(std::distance(start->begin(), start->end())) : 0);
        }
        /** @brief Is an RObject a ConsCell?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a ConsCell.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == LISTSXP || st == LANGSXP || st == DOTSXP;
        }

        /** @brief Set the 'car' value.
         *
         * @param cr Pointer to the new car object (or a null
         *           pointer).
         */
        void setCar(RObject *cr)
        {
            if (hasUnexpandedValue())
            {
                u.listsxp.m_car.reset();
                markExpanded();
            }
            if (u.listsxp.m_car == cr)
                return;
            if (refCountEnabled() && u.listsxp.m_car && assignmentPending())
            {
                setAssignmentPending(false);
                GCNode::incRefCount(u.listsxp.m_car);
            }
            u.listsxp.m_car.retarget(this, cr);
        }

        /** @brief Set the 'tag' value.
         *
         * @param tg Pointer to the new tag object (or a null
         *           pointer).
         */
        void setTag(RObject *tg)
        {
            u.listsxp.m_tag.retarget(this, tg);
        }

        /** @brief Set the 'tail' value.
         *
         * @param tl Pointer to the new tail list (or a null
         *           pointer).
         *
         * @note Implemented inline in CXXR/PairList.hpp
         */
        void setTail(RObject *tl);

        bool hasUnexpandedValue() const
        {
            return sxpinfo.m_binding_tag != NILSXP;
        }

        void markExpanded()
        {
            sxpinfo.m_binding_tag = NILSXP;
        }

        SEXPTYPE underlyingType() const
        {
            return sxpinfo.m_binding_tag;
        }

        void setUnderlyingType(SEXPTYPE v)
        {
            sxpinfo.m_binding_tag = v;
        }

        /** @brief Get the 'tag' value.
         *
         * @return a pointer to the 'tag' of this ConsCell.
         */
        const RObject *tag() const
        {
            return u.listsxp.m_tag;
        }

        /** @brief Get the 'tail' value.
         *
         * @return a const pointer to the 'tail' of this ConsCell
         * element.
         */
        const PairList *tail() const;

        /** @brief Get the 'tail' value.
         *
         * @return a pointer to the 'tail' of this ConsCell.
         */
        PairList *tail();

        /** @brief Get the pending status for assignment?.
         *
         * @return true if assignment is pending.
         */
        bool assignmentPending() const;

        /** @brief Set pending status for assignment?.
         *
         */
        void setAssignmentPending(bool on);

    protected:
        /**
         * @param st The required ::SEXPTYPE of the ConsCell.  Must
         *           be one of LISTSXP, LANGSXP, DOTSXP or BCODESXP (not
         *           normally checked).
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        ConsCell(SEXPTYPE stype, SEXP cr, SEXP tl, SEXP tg) : RObject(stype)
        {
            u.listsxp.m_car = cr;
            u.listsxp.m_tail = tl;
            u.listsxp.m_tag = tg;
        }

        // Declared protected to ensure that ConsCell objects are
        // allocated only using 'new':
        ~ConsCell() {}

    private:
        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        ConsCell(const ConsCell &);
        ConsCell &operator=(const ConsCell &);
    };

    /** @brief (For debugging.)
     *
     * @note The name and interface of this function may well change.
     */
    void printCC(const ConsCell *node, bool show_refcnt = true, std::ostream &os = std::cerr);
} // namespace CXXR

namespace R
{
    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    void (SET_MISSING)(SEXP x, unsigned int v);

    SEXP CONS_NR(SEXP a, SEXP b);
    SEXPTYPE (BNDCELL_TAG)(SEXP cell);
    void (SET_BNDCELL_TAG)(SEXP cell, SEXPTYPE val);
    double (BNDCELL_DVAL)(SEXP cell);
    int (BNDCELL_IVAL)(SEXP cell);
    int (BNDCELL_LVAL)(SEXP cell);
} // namespace R

extern "C"
{
    // Used in argument handling (within envir.cpp, eval.cpp and
    // match.cpp).  Note comments in the 'R Internals' document.
    int (MISSING)(SEXP x);

    /** @brief Get tag of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the tag of the list element, or 0 if \a e is
     * a null pointer.
     */
    SEXP (TAG)(SEXP e);

    /** @brief Set the tag of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new tag of
     *          the CXXR::ConsCell.
     */
    void SET_TAG(SEXP x, SEXP y);

    /** @brief Create an object of a type derived from CXXR::ConsCell.
     *
     * The object is created with null car, tag and tail pointers.
     *
     * @param t The ::SEXPTYPE of the required object. Must be one of
     *          LISTSXP, LANGSXP, DOTSXP or BCODESXP (not checked).
     *
     * @return Pointer to the created object.
     */
    SEXP Rf_allocSExp(SEXPTYPE t);

    /** @brief Obtain the n-th element of CXXR::ConsCell.
     *
     * @param s Pointer to a CXXR::ConsCell (checked).
     *
     * @param n n-th element to be extracted from CXXR:ConsCell
     *
     * @return Pointer to the n-th element.
     */
    SEXP Rf_nthcdr(SEXP s, int n);
} // extern "C"

#endif /* CONSCELL_HPP */
