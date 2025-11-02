/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file ConsCell.cpp
 *
 * @brief Class CXXR::ConsCell and associated C interface.
 */

#define USE_RINTERNALS // always use macro versions

#include <CXXR/ConsCell.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <R_ext/Error.h>
#include <Localization.h>
#include <Defn.h> // for ASSIGNMENT_PENDING, SET_ASSIGNMENT_PENDING
#include <Rinternals.h> // for ForceNonInline

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &CAD4Rptr = CAD4R;
        const auto &CADDDRptr = CADDDR;
        const auto &CADDRptr = CADDR;
        const auto &CADRptr = CADR;
        const auto &CDARptr = CDAR;
        const auto &CDDRptr = CDDR;
        const auto &CDDDRptr = CDDDR;
        const auto &CDRptr = CDR;
    } // namespace ForceNonInline

    RObject *ConsCell::car() const
    {
        if (hasUnexpandedValue())
            Rf_error(_("bad binding access: %d"), underlyingType());
        return u.listsxp.m_car;
    }

    const PairList *ConsCell::tail() const
    {
        return SEXP_downcast<const PairList *>(u.listsxp.m_tail.get());
    }

    PairList *ConsCell::tail()
    {
        return SEXP_downcast<PairList *>(u.listsxp.m_tail.get());
    }

    bool ConsCell::assignmentPending() const
    {
        return ASSIGNMENT_PENDING(this);
    }

    void ConsCell::setAssignmentPending(bool on)
    {
        SET_ASSIGNMENT_PENDING(this, on);
    }

    void ConsCell::detachReferents()
    {
        RObject::detachReferents();
    }

    void ConsCell::visitReferents(const_visitor *v) const
    {
        RObject::visitReferents(v);
        const GCNode *car = R_NilValue;
        const GCNode *cdr = this->u.listsxp.m_tail;
        const GCNode *tag = this->u.listsxp.m_tag;
        if (BOXED_BINDING_CELLS || BNDCELL_TAG(this) == NILSXP) // condition for LISTSXP objects
            car = this->u.listsxp.m_car;

        if (car != R_NilValue)
            (*v)(car);
        if (cdr != R_NilValue)
            (*v)(cdr);
        if (tag != R_NilValue)
            (*v)(tag);
    }

    namespace
    {
        void printAttributes(std::ostream &os, const ConsCell *node)
        {
            os << "attributes [";
            // print MISSING status
            auto missingStatus = MISSING(node);
            os << "origin: " << missingStatus << "|";

            // print underlying value status
            SEXPTYPE underlyingType = node->underlyingType();
            if (underlyingType)
            {
                os << "underlying: " << Rf_type2char(underlyingType) << "|";
            }
            else
            {
                os << "expanded: " << std::boolalpha << !underlyingType << "|";
            }

            // print assignment status
            bool assignmentPendingStatus = node->assignmentPending();
            os << "pending: " << std::boolalpha << assignmentPendingStatus << "|";

            // print active binding status
            bool activeBindingStatus = IS_ACTIVE_BINDING(node);
            os << "active: " << std::boolalpha << activeBindingStatus << "|";

            // print locked binding status
            bool lockedBinding = BINDING_IS_LOCKED(node);
            os << "locked: " << std::boolalpha << lockedBinding;
            os << "]" << std::endl;
        }

        void printCCBody(std::ostream &os, const ConsCell *node, const std::string &prefix, bool show_refcnt)
        {
            if (node == R_NilValue)
            {
                os << "ConsCell is empty" << std::endl;
                return;
            }

            // print node attributes
            os << prefix << "├── ";
            {
                printAttributes(os, node);
            }

            // print the type of the tag node
            os << prefix << "├── ";
            {
                const RObject *tg = node->tag();
                if (tg != R_NilValue)
                {
                    if (show_refcnt)
                        os << "(" << tg->getRefCount() << ") ";
                    if (tg->altrep())
                        os << "altrep ";
                    os << "tag is " << Rf_type2char(tg->sexptype());
                    if (Symbol::isA(tg))
                    {
                        os << "; name is '" << SEXP_downcast<const Symbol *>(tg)->name()->stdstring() << "'";
                    }
                    else if (tg->sexptype() == INTSXP)
                    {
                        os << "; value is '" << INTEGER(const_cast<RObject *>(tg))[0] << "'";
                    }
                    else if (ConsCell::isA(tg))
                    {
                        os << " of length: " << ConsCell::listLength(SEXP_downcast<const ConsCell *>(tg)) << "\n";
                        printCCBody(os, SEXP_downcast<const ConsCell *>(tg), prefix + "│   ", show_refcnt);
                    }
                }
                else
                {
                    os << "tag is empty";
                }
            }
            os << std::endl;

            // print the type of the car node
            os << prefix << "├── ";
            {
                const RObject *cr = node->car0();
                if (cr != R_NilValue)
                {
                    if (show_refcnt)
                        os << "(" << cr->getRefCount() << ") ";
                    SEXPTYPE underlyingType = node->underlyingType();
                    if (underlyingType)
                    {
                        os << "car points to unexpanded underlying value of type " << Rf_type2char(underlyingType);
                    }
                    else
                    {
                        if (cr->altrep())
                            os << "altrep ";
                        os << "car is " << Rf_type2char(cr->sexptype());
                        if (Symbol::isA(cr))
                        {
                            os << "; name is '" << SEXP_downcast<const Symbol *>(cr)->name()->stdstring() << "'";
                        }
                        else if (cr->sexptype() == INTSXP)
                        {
                            os << "; value is '" << INTEGER(const_cast<RObject *>(cr))[0] << "'";
                        }
                        else if (ConsCell::isA(cr))
                        {
                            os << " of length: " << ConsCell::listLength(SEXP_downcast<const ConsCell *>(cr)) << "\n";
                            printCCBody(os, SEXP_downcast<const ConsCell *>(cr), prefix + "│   ", show_refcnt);
                        }
                    }
                }
                else
                {
                    os << "car is empty";
                }
            }
            os << std::endl;

            // print the type of the tail node
            os << prefix << "└──";
            {
                const RObject *tl = node->tail();
                if (tl != R_NilValue)
                {
                    if (ConsCell::isA(tl))
                    {
                        os << "┐";
                        if (show_refcnt)
                            os << " (" << tl->getRefCount() << ")";
                        if (tl->altrep())
                            os << " (altrep)";
                        os << std::endl;
                        printCCBody(os, SEXP_downcast<const ConsCell *>(tl), prefix + "   ", show_refcnt);
                    }
                    else
                    {
                        os << " tail is " << Rf_type2char(tl->sexptype());
                    }
                }
                else
                {
                    os << "o";
                }
            }
        }
    } // anonymous namespace

    void printCC(const ConsCell *node, bool show_refcnt, std::ostream &os)
    {
        os << "\n";

        if (node == R_NilValue)
        {
            os << "node is empty" << std::endl;
            return;
        }

        if (!ConsCell::isA(node))
        {
            os << "node is not a ConsCell" << std::endl;
            return;
        }

        if (show_refcnt)
            os << "(" << node->getRefCount() << ") ";
        if (node->altrep())
            os << "altrep ";
        os << "root of length: " << ConsCell::listLength(node) << "\n";
        printCCBody(os, node, "", show_refcnt);
        os << "\n";
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

