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

/** @file Symbol.hpp
 * @brief Class CXXR::Symbol and associated C interface.
 */

#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>

namespace R
{
    extern void InitNames();
} // namespace R

namespace CXXR
{
} // namespace CXXR

namespace R
{
    SEXP installDDVAL(int i);

    /** Find value of a <tt>..<em>n</em></tt> Symbol.
     *
     * @param symbol Pointer to a Symbol (checked) whose name is of
     *          the form <tt>..<em>n</em></tt>, where <em>n</em> is a
     *          positive integer.
     *
     * @param rho Pointer to an Environment, which must bind the
     *          symbol <tt>...</tt> to a PairList comprising at least
     *          <em>n</em> elements.  (All checked.)
     *
     * @return The 'car' of the <em>n</em>th element of the PairList to
     * which <tt>...</tt> is bound.
     */
    SEXP ddfindVar(SEXP symbol, SEXP rho);
    SEXP installS3Signature(const char *methodName, const char *className);
    void SET_DDVAL(SEXP x, int v);

    /** @brief Set Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param v Pointer to a CXXR::String representing \a x's name.
     */
    void SET_PRINTNAME(SEXP x, SEXP v);

    /** @brief Set symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param val Pointer to the RObject now to be considered as
     *            the value of this symbol.  A null pointer or
     *            R_UnboundValue are permissible values of \a val.
     *
     * @todo No binding to R_UnboundValue ought to be created.
     */
    void SET_SYMVALUE(SEXP x, SEXP v);

    /** @brief Create a CXXR::Symbol object.
     *
     * @param name Pointer to a CXXR::String object (checked) to be
     *          taken as the name of the constructed symbol.
     *
     * @param value Pointer to the CXXR::RObject to be considered as
     *          the value of the constructed symbol.  A null pointer or
     *          R_UnboundValue are permissible values of \a value.
     *
     * @return Pointer to the created CXXR::Symbol object.
     */
    SEXP mkSYMSXP(SEXP name, SEXP value);

    Rboolean IS_SPECIAL_SYMBOL(SEXP sym);
} // namespace R

extern "C"
{
    /** @brief Is this a symbol?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is a symbol.
     */
    Rboolean Rf_isSymbol(SEXP s);

    /** @brief Find value of a <tt>..<em>n</em></tt> Symbol.
     *
     * @param symbol Pointer to a Symbol (checked) whose name is of
     *          the form <tt>..<em>n</em></tt>, where <em>n</em> is a
     *          positive integer.
     *
     * @param rho Pointer to an Environment, which must bind the
     *          symbol <tt>...</tt> to a PairList comprising at least
     *          <em>n</em> elements.  (All checked.)
     *
     * @return The 'car' of the <em>n</em>th element of the PairList to
     * which <tt>...</tt> is bound.
     */
    SEXP Rf_ddfindVar(SEXP symbol, SEXP rho);

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::String representing \a x's name.
     */
    SEXP PRINTNAME(SEXP x);

    /** @brief Get a pointer to a regular Symbol object.
     *
     * If no Symbol with the specified name currently exists, one will
     * be created, and a pointer to it returned.  Otherwise a pointer
     * to the existing Symbol will be returned.
     *
     * @param name The name of the required Symbol (CE_NATIVE encoding
     *          is assumed).
     *
     * @return Pointer to a Symbol (preexisting or newly created) with
     * the required name.
     */
    SEXP Rf_install(const char *name);

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representing \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
    SEXP SYMVALUE(SEXP x);

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
    int DDVAL(SEXP x);

    Rboolean Rf_isUserBinop(SEXP s);

    /* This function is equivalent to Rf_install(R_CHAR(charSXP)), but faster.
   Like the equivalent code pattern, it discards the encoding information,
   hence in almost all cases installTrChar should be used, instead. */
    SEXP Rf_installNoTrChar(SEXP charSXP);
} // extern "C"

#endif /* SYMBOL_HPP */
