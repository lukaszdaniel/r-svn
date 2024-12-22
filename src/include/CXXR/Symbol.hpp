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

#include <vector>
#include <string>
#include <unordered_map>
#include <CXXR/Allocator.hpp>
#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>
#include <CXXR/String.hpp>

namespace R
{
    extern void InitNames();
} // namespace R

namespace CXXR
{
    /** @brief Class used to represent R symbols.
     *
     * A Symbol is an R identifier.  Each Symbol (except for
     * pseudo-symbols, see below) has a name, namely a String giving the
     * textual representation of the identifier.  Generally speaking,
     * however, a Symbol object is identified by its address rather
     * than by its name.  Consequently, the class enforces the
     * invariant that there is a most one Symbol object with a given
     * name (but this does not apply to pseudo-symbols).
     *
     * Symbols come in two varieties, standard symbols and pseudo-symbols,
     * both implemented by this class.  Dot-dot symbols are a
     * subvariety of standard symbols.
     *
     * Standard symbols are generated using the static member function
     * obtain(), and (as explained above) have the property that there
     * is at most one standard symbol with a given name.  This is
     * enforced by an internal table mapping names to standard
     * symbols.
     *
     * Dot-dot symbols have names of the form '<tt>..</tt><i>n</i>',
     * where <i>n</i> is a positive integer.  These are preferably
     * generated using the static member function obtainDotDotSymbol()
     * (though they can also be generated using obtain() ), and are
     * used internally by the interpreter to refer to elements of a
     * '<tt>...</tt>' argument list.  (Note that CR does not
     * consistently enforce the 'at most one Symbol per name' rule for
     * dot-dot symbols; CXXR does.)
     *
     * Pseudo-symbols are used to implement certain pseudo-objects
     * (::R_MissingArg and ::R_UnboundValue) that CR expects to have
     * ::SEXPTYPE SYMSXP.  Each psuedo-symbol has a blank string as
     * its name, but despite this each of them is a distinct symbol.
     *
     * @note Following the practice with CR's symbol table, Symbol
     * objects, once created, are permanently preserved against
     * garbage collection.  There is no inherent reason for this in
     * CXXR, but some packages may rely on it.  Consequently there is
     * no need to use smart pointers such as GCStackRoot<Symbol> or
     * GCEdge<Symbol>: plain pointers will do fine.
     */
    class Symbol : public RObject
    {
    public:
        // The symbol table
        // The cache is implemented as a mapping from keys to pointers
        // to Symbol objects.  Each Symbol simply contains
        // a pointer locating its entry within the cache.
        using Table = std::unordered_map<std::string, Symbol *, std::hash<std::string>, std::equal_to<std::string>,
                                       CXXR::Allocator<std::pair<const std::string, Symbol *>>>;

        static Table s_symbol_table;

        /** @brief const_iterator for iterating over all standard Symbols.
         *
         * This is used in BuiltInSize() and BuiltInNames().
         */
        using const_iterator = Table::const_iterator;

        static const_iterator begin() { return s_symbol_table.begin(); }

        static const_iterator end() { return s_symbol_table.end(); }

        static Symbol *create(SEXP name = R_NilValue, SEXP val = R_NilValue, SEXP internal = R_NilValue);

        /** @brief Is an RObject a Symbol?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a Symbol.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == SYMSXP;
        }

        /** @brief Access name.
         *
         * @return const pointer to the name of this Symbol.
         */
        const String *name() const;

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtain(/*const*/ RObject *name);

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol (CE_UTF8
         *          encoding is assumed).  At present no check is made
         *          that the supplied string is a valid symbol name.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtain(const std::string &name);

        /** @brief Get a pointer to a regular Symbol object.
         *
         * If no Symbol with the specified name currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param name The name of the required Symbol in \a enc
         *          encoding.  At present no check is made
         *          that the supplied string is a valid symbol name.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required name.
         */
        static Symbol *obtainCE(const std::string &name, cetype_t enc = CE_UTF8);

        /** @brief Create a double-dot symbol.
         *
         * @param n Index number of the required symbol; must be
         *          strictly positive.
         *
         * @return a pointer to the created symbol, whose name will be
         * <tt>..</tt><i>n</i>.
         */
        static Symbol *obtainDotDotSymbol(unsigned int n);

        /** @brief Get a pointer to a Symbol for an S3 method.
         *
         * If no Symbol with the specified signature currently exists, one
         * will be created, and a pointer to it returned.  Otherwise a
         * pointer to the existing Symbol will be returned.
         *
         * @param className The name of the class that the method is for.
         *
         * @param methodName The name of the function that the method is for.
         *
         * @return Pointer to a Symbol (preexisting or newly
         * created) with the required signature.
         */
        static Symbol *obtainS3Signature(const char *className, const char *methodName);

        /** @brief Unbound value.
         *
         * This is used as the 'value' of a Symbol that has not been
         * assigned any actual value.
         *
         * @return a pointer to the 'unbound value' pseudo-object,
         * which is identified as such by its address, not by its
         * content.
         */
        static SEXP unboundValue();

        static const std::vector<std::string> s_special_symbol_names;

    private:
        Symbol(SEXP name, SEXP val, SEXP internal) : RObject(SYMSXP)
        {
            u.symsxp.m_pname = name;
            u.symsxp.m_value = val;
            u.symsxp.m_internal = internal;
        }

        // Declared private to ensure that Symbol objects are
        // allocated only using 'new':
        ~Symbol() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        Symbol(const Symbol &);
        Symbol &operator=(const Symbol &);
    };
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
    SEXP installS3Signature(const char *className, const char *methodName);
    void (SET_DDVAL)(SEXP x, int v);

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

    bool (IS_SPECIAL_SYMBOL)(SEXP sym);
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
    SEXP (PRINTNAME)(SEXP x);

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
    SEXP (SYMVALUE)(SEXP x);

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
    int (DDVAL)(SEXP x);

    Rboolean Rf_isUserBinop(SEXP s);

    /* This function is equivalent to Rf_install(R_CHAR(charSXP)), but faster.
   Like the equivalent code pattern, it discards the encoding information,
   hence in almost all cases installTrChar should be used, instead. */
    SEXP Rf_installNoTrChar(SEXP charSXP);
} // extern "C"

#endif /* SYMBOL_HPP */
