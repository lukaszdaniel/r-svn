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

/** @file PairList.hpp
 * @brief Class CXXR::PairList and associated C interface.
 *
 * To facilitate inlining of various ConsCell member functions, the
 * definition of class CXXR::PairList itself is in ConsCell.hpp.
 *
 * This file includes C functions for examining and setting the CDR of
 * a CXXR::ConsCell, and other operations accessing the tail of the
 * list; functions for examining and setting the CAR and TAG of a
 * CXXR:ConsCell are to be found in ConsCell.hpp.
 */

#ifndef PAIRLIST_HPP
#define PAIRLIST_HPP

#include <CXXR/ConsCell.hpp>
#include <CXXR/GCManager.hpp>

namespace CXXR
{
    /** @brief Singly linked list of pairs.
     *
     * LISP-like singly-linked list, each element containing pointers to a
     * 'car' object (this is LISP terminology, and has nothing to do
     * with automobiles) and to a 'tag' object, as well as a pointer to
     * the next element of the list.  (Any of these pointers may be
     * null.)  A PairList object is considered to 'own' its car, its
     * tag, and all its successors.
     */
    class PairList : public ConsCell
    {
    public:
        /**
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the 'tag' of the element to be constructed.
         */
        PairList(SEXP cr, SEXP tl, SEXP tg) : ConsCell(LISTSXP, cr, tl, tg)
        {
        }

        /** @brief Create a PairList element on the free store.
         *
         * Unlike the constructor (and contrary to CXXR conventions
         * generally) this function protects its arguments from the
         * garbage collector.
         *
         * @param cr Pointer to the 'car' of the element to be
         *           constructed.
         *
         * @param tl Pointer to the 'tail' (LISP cdr) of the element
         *           to be constructed.
         *
         * @param tg Pointer to the tag of the element to be constructed.
         *
         * @return Pointer to newly created PairList element.
         */
        template <class T = PairList>
        static T *create(SEXP cr, SEXP tl = R_NilValue, SEXP tg = R_NilValue)
        {
            GCManager::GCInhibitor no_gc;
            // We inhibit garbage collection here to avoid (a) the need
            // to protect the arguments from GC, and (b) the
            // possibility of reentrant calls to this function (from
            // object destructors).  However, calling code should not
            // rely on the fact that no GC will occur, because the
            // implementation may change in the future.

            return new T(cr, tl, tg);
        }

        /** @brief Create a PairList of a specified length.
         *
         * This constructor creates a chain of PairList nodes with a
         * specified number of elements.  On creation, each element
         * has null 'car' and 'tag'.
         *
         * @param sz Number of elements required in the list.  If
         *           zero, the function returns a null pointer.
         */
        static SEXP makeList(size_t sz);

        /** @brief Is an RObject a PairList?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a PairList.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == LISTSXP;
        }

        using iterator = iterator_tmpl<PairList>;
        using const_iterator = const_iterator_tmpl<PairList>;

        iterator begin() { return iterator(this); }
        const_iterator begin() const { return const_iterator(this); }
        iterator end() { return iterator(); }
        const_iterator end() const { return const_iterator(); }

    private:
        // Declared private to ensure that PairList objects are
        // allocated only using 'new':
        ~PairList() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        PairList(const PairList &);
        PairList &operator=(const PairList &);
    };

    template <typename ValueType>
    inline void ConsCell::iterator_tmpl<ValueType>::advance()
    {
        if (m_cc && (static_cast<RObject *>(m_cc->tail()) != R_NilValue))
        {
            m_cc = static_cast<ValueType *>(m_cc->tail());
        }
        else
        {
            m_cc = nullptr;
        }
    }

    template <typename ValueType>
    inline void ConsCell::const_iterator_tmpl<ValueType>::advance()
    {
        if (m_cc && (static_cast<const RObject *>(m_cc->tail()) != R_NilValue))
        {
            m_cc = static_cast<const ValueType *>(m_cc->tail());
        }
        else
        {
            m_cc = nullptr;
        }
    }

    inline void ConsCell::setTail(RObject *tl)
    {
        u.listsxp.m_tail.retarget(this, tl);
    }

    template <class T = PairList>
    T *CXXR_cons(RObject *car, RObject *cdr, RObject *tag = R_NilValue)
    {
        return PairList::create<T>(car, static_cast<PairList *>(cdr), tag);
    }
} // namespace CXXR

namespace R
{
    /** @brief Get car of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
    SEXP (CAR0)(SEXP e);

    /** @brief Destructively removes R_NilValue ('NULL') elements from a pairlist.
     *
     * @param s Pointer to a CXXR::ConsCell.
     *
     * @param keep_initial Indicator whether to keep initial NULL values.
     *
     * @return Pointer to the rearanged CXXR::ConsCell.
     */
    SEXP R_listCompact(SEXP s, bool keep_initial);

    SEXP allocFormalsList2(SEXP sym1, SEXP sym2);
    SEXP allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3);
    SEXP allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4);
    SEXP allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5);
    SEXP allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6);

    /* Bindings */

    /** @brief Is a Binding active?
     *
     * @param b Pointer to a ConsCell object (checked). If \a b points
     *          to any type of ConsCell other than a PairList, the
     *          function returns FALSE.  Otherwise \a b should point
     *          to a PairList object representing a Frame::Binding
     *          (e.g. because it was produced using
     *          Frame::asPairList()).
     *
     * @return true iff this is an active Binding.
     */
    bool (IS_ACTIVE_BINDING)(SEXP b);

    /** @brief Is a Binding locked?
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     *
     * @return true iff this Binding is locked.
     */
    bool (BINDING_IS_LOCKED)(SEXP b);

    /** @brief Designate as active the binding represented by a
     * PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void (SET_ACTIVE_BINDING_BIT)(SEXP b);

    /** @brief Lock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void (LOCK_BINDING)(SEXP b);

    /** @brief Unlock the binding represented by a PairList object.
     *
     * @param b Pointer to a PairList object (checked) representing a
     *          Frame::Binding (e.g. because it was produced using
     *          Frame::asPairList()).
     */
    void (UNLOCK_BINDING)(SEXP b);

    bool (ASSIGNMENT_PENDING)(SEXP x);
    void (SET_ASSIGNMENT_PENDING)(SEXP x, int v);
    bool (IS_ASSIGNMENT_CALL)(SEXP x);
    void (MARK_ASSIGNMENT_CALL)(SEXP x);
} // namespace R

extern "C"
{
    /** @brief Get car of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the value of the list car, or 0 if \a e is
     * a null pointer.
     */
    SEXP (CAR)(SEXP e);

    /** @brief Get tail of CXXR::ConsCell.
     *
     * @param e Pointer to a CXXR::ConsCell (checked), or a null pointer.
     *
     * @return Pointer to the tail of the list, or 0 if \a e is
     * a null pointer.
     */
    SEXP (CDR)(SEXP e);

    /** @brief Equivalent to CAR(CAR(e)).
     */
    SEXP (CAAR)(SEXP e);

    /** @brief Equivalent to CDR(CAR(e)).
     */
    SEXP (CDAR)(SEXP e);

    /** @brief Equivalent to CAR(CDR(e)).
     */
    SEXP (CADR)(SEXP e);

    /** @brief Equivalent to CDR(CDR(e)).
     */
    SEXP (CDDR)(SEXP e);

    /** @brief Equivalent to CDR(CDR(CDR(e))).
     */
    SEXP (CDDDR)(SEXP e);

    /** @brief Equivalent to CDR(CDR(CDR(CDR(e)))).
     */
    SEXP (CD4R)(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(e))).
     */
    SEXP (CADDR)(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP (CADDDR)(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(e)))).
     */
    SEXP (CAD3R)(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(CDR(e))))).
     */
    SEXP (CAD4R)(SEXP e);

    /** @brief Equivalent to CAR(CDR(CDR(CDR(CDR(CDR(e)))))).
     */
    SEXP (CAD5R)(SEXP e);

    /** @brief Set the 'car' value of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          list car.
     *
     * @returns \a y.
     */
    SEXP SETCAR(SEXP x, SEXP y);

    /** @brief Replace the tail of a CXXR::ConsCell.
     *
     * @param x Pointer to a CXXR::ConsCell (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new tail of the list.
     *
     * @returns \a y.
     */
    SEXP SETCDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the second element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least one successor
     *          (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          second element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the third element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least two
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          third element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the fourth element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least three
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          fourth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCADDDR(SEXP x, SEXP y);

    /** @brief Set the 'car' value of the fifth element of list.
     *
     * @param x Pointer to a CXXR::ConsCell with at least four
     *          successors (checked).
     *
     * @param y Pointer to a CXXR::RObject representing the new value of the
     *          fifth element of the list.
     *
     * @returns \a y.
     */
    SEXP SETCAD4R(SEXP x, SEXP y);

    /** @brief Create a CXXR::PairList of a specified length.
     *
     * This constructor creates a CXXR::PairList with a specified
     * number of elements.  On creation, each element has null 'car'
     * and 'tag'.
     *
     * @param n Number of elements required in the list.
     *
     * @return The constructed list, or a null pointer if \a n is zero.
     */
    SEXP Rf_allocList(int n);

    /** @brief Creates a CXXR::PairList with a specified car and tail.
     *
     * This function protects its arguments from the garbage collector.
     *
     * @param cr Pointer to the 'car' of the element to be created.
     *
     * @param tl Pointer to the 'tail' of the element to be created,
     *          which must be of a CXXR::PairList type (checked).
     *
     * @return Pointer to the constructed list.
     */
    SEXP Rf_cons(SEXP cr, SEXP tl);

    /** @brief Get the i-th element of a list.
     *
     * Return a dotted pair with the given CAR and CDR.
     * The (R) TAG slot on the cell is set to NULL.
     *
     * @param list SEXP object.
     *
     * @param i i-th element of that object.
     *
     * @return i-th element.
     *
     * @note from list.cpp
     */
    SEXP Rf_elt(SEXP list, int i);

    /** @brief Return the last element of a list
     */
    SEXP Rf_lastElt(SEXP list);

    /* Shorthands for creating small lists */

    SEXP Rf_list1(SEXP s);
    SEXP Rf_list2(SEXP s, SEXP t);
    SEXP Rf_list3(SEXP s, SEXP t, SEXP u);
    SEXP Rf_list4(SEXP s, SEXP t, SEXP u, SEXP v);
    SEXP Rf_list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w);
    SEXP Rf_list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x);

    /** @brief Destructive list append
     *
     * @note See also ``append''
     */
    SEXP Rf_listAppend(SEXP s, SEXP t);

    /** @brief Check to see if a list can be made into a vector.
     *
     * @note it must have every element being a vector of length 1.
     *       BUT it does not exclude 0!
     *
     * @return true if list can be made into a vector
     */
    Rboolean Rf_isVectorizable(SEXP s);
    Rboolean Rf_isList(SEXP s);
    Rboolean Rf_isPairList(SEXP s);
} // extern "C"

#endif /* PAIRLIST_HPP */
