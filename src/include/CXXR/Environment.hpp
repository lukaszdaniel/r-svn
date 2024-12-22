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

/** @file Environment.hpp
 * @brief Class CXXR::Environment and associated C interface.
 */

#ifndef ENVIRONMENT_HPP
#define ENVIRONMENT_HPP

#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>

namespace R
{
    void InitBaseEnv();
} // namespace R

namespace CXXR
{
    /** @brief Mapping from Symbols to R objects.
     *
     * An Environment has an associated Frame, which defines a mapping
     * from (pointers to) CXXR::Symbol objects to (pointers to)
     * arbitrary objects of classes derived from RObject.  An
     * Environment will normally have an 'enclosing environment', and
     * the Environment class provides facilities for searching for a
     * binding for a Symbol first in the Environment's own Frame, and
     * then successively in the Frames of enclosing Environments.
     *
     * @note This class does not in itself enforce the requirement
     * that the enclosing relationship must be acyclic.
     */
    class Environment : public RObject
    {
    public:
        static Environment *create(SEXP frame = R_NilValue, SEXP enclosing_env = R_NilValue, SEXP hashtab = R_NilValue);

        /** @brief Is an RObject an Environment?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is an Environment.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == ENVSXP;
        }

        /** @brief Empty environment.
         *
         * CR accords a special status to the empty environment,
         * R_EmptyEnv, which is an Environment whose Frame contains no
         * Bindings, and which has no enclosing Environment.  In CR
         * the search for a Symbol Binding terminates when it reaches
         * the empty environment, without looking inside it.  In rho,
         * although the empty environment still exists (for backwards
         * compatibility)), it is not handled specially.  If the
         * search for a Symbol reaches the empty environment, rho
         * will look for the Symbol inside it - unsuccessfully of
         * course - and the search then terminates because there is no
         * enclosing Environment.
         *
         * @return Pointer to the empty environment.
         *
         * @note rho's own code does not include tests to prohibit
         * the creation of bindings within the empty environment, but
         * the effect of doing so is undefined.
         */
        static Environment *empty();

        /** @brief Base environment.
         *
         * @return Pointer to the base environment.
         */
        static Environment *base();

        /** @brief Global environment.
         *
         * @return Pointer to the global environment.
         */
        static Environment *global();

        /** @brief Base namespace.
         *
         * @return Pointer to the base namespace.
         */
        static Environment *baseNamespace();

    private:
        Environment(SEXP frame, SEXP enclosing_env, SEXP hashtab) : RObject(ENVSXP)
        {
            u.envsxp.m_frame = frame;
            u.envsxp.m_enclos = enclosing_env;
            u.envsxp.m_hashtab = hashtab;
        }

        // Declared private to ensure that Environment objects are
        // allocated only using 'new':
        ~Environment() {}

        static void initialize();
        friend void ::R::InitBaseEnv();

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        Environment(const Environment &);
        Environment &operator=(const Environment &);
    };
} // namespace CXXR

namespace R
{
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

    void R_RestoreHashCount(SEXP rho);

    SEXP R_NewHashedEnv(SEXP enclos, int size);

    int Rf_envlength(SEXP rho);
    R_xlen_t Rf_envxlength(SEXP rho);

    bool (NO_SPECIAL_SYMBOLS)(SEXP env);
    void (SET_NO_SPECIAL_SYMBOLS)(SEXP env);
    void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP env);

    void LOCK_FRAME(SEXP env);
    void UNLOCK_FRAME(SEXP env);
    bool FRAME_IS_LOCKED(SEXP env);
} // namespace R

extern "C"
{
    /** @brief An empty environment at the root of the environment tree
     */
    extern SEXP R_EmptyEnv;

    /** @brief The base environment; formerly R_NilValue
     */
    extern SEXP R_BaseEnv;

    /** @brief The "global" environment
     */
    extern SEXP R_GlobalEnv;

    /** @brief The (fake) namespace for base
     */
    extern SEXP R_BaseNamespace;

    /** @brief Is this an environment?
     *
     * @param s Pointer to CXXR::RObject.
     *
     * @return TRUE iff the RObject pointed to by \a s is an environment.
     */
    Rboolean Rf_isEnvironment(SEXP s);

    /** @brief Symbol's value in the base environment.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representing \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
    SEXP (SYMVALUE)(SEXP x);

    /** @brief Access an environment's Frame, represented as a PairList.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to a PairList representing the contents of the
     * Frame of \a x (may be null).  This PairList is generated on the
     * fly, so this is a relatively expensive operation.  Alterations
     * to the returned PairList will not alter the Environment's Frame.
     *
     * @note Beware that since (unlike CR) this isn't a simple
     * accessor function, its return value will need protection from
     * garbage collection.
     */
    SEXP (FRAME)(SEXP x);

    /** @brief Access enclosing environment.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the enclosing environment of \a x.
     */
    SEXP (ENCLOS)(SEXP x);

    /** @brief Access an environment's hash table.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @return Pointer to the hash table of \a x (may be null).
     */
    SEXP (HASHTAB)(SEXP x);

    /** @brief Access an environment's flags.
     *
     * @param x Pointer to a CXXR::Environment (not currently checked).
     *
     * @return the environment flags of \a x .
     */
    int (ENVFLAGS)(SEXP x);

    /** @brief Should the debugger single-step?
     *
     * @param x Pointer to a CXXR::Environment object (checked).
     *
     * @return \c true if single-stepping is set, i.e. the debugger
     * should single-step within this environment.
     */
    int (ENV_RDEBUG)(SEXP x);

    /** @brief Enable/disable single-stepping of the debugger.
     *
     * @param x Pointer to a CXXR::Environment object (checked).
     *
     * @param v The new single-stepping state (true = enabled).
     */
    void (SET_ENV_RDEBUG)(SEXP x, int v);

    /** @brief Set environment flags.
     *
     * @param x Pointer to a CXXR::Environment (not currently checked).
     *
     * @param v The new flags.
     *
     * @deprecated
     */
    void (SET_ENVFLAGS)(SEXP x, int v);

    /** @brief Set environment's frame.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to the new frame.  This must be a CXXR::PairList
     *          (checked), and every element of this list must have a tag
     *          (not checked), and these tags must be distinct (not
     *          checked).
     *
     * @todo Probably should be private.
     */
    void SET_FRAME(SEXP x, SEXP v);

    /** @brief Set an environment's enclosing environment.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to a CXXR::Environment (checked) intended to be
     *          the new enclosing environment of \a x.
     */
    void SET_ENCLOS(SEXP x, SEXP v);

    /** @brief Set environment's hash table.
     *
     * @param x Pointer to a CXXR::Environment (checked).
     *
     * @param v Pointer to the new hash table, which must be a
     * CXXR::ListVector (checked), and satisfy other conditions.
     *
     * @todo Probably should be private.
     */
    void SET_HASHTAB(SEXP x, SEXP v);

    SEXP R_NewEnv(SEXP enclos, int hash, int size);
    Rboolean R_IsPackageEnv(SEXP rho);
    SEXP R_PackageEnvName(SEXP rho);
    SEXP R_FindPackageEnv(SEXP info);
    Rboolean R_IsNamespaceEnv(SEXP rho);
    SEXP R_NamespaceEnvSpec(SEXP rho);
    SEXP R_FindNamespace(SEXP info);
    void R_LockEnvironment(SEXP env, Rboolean bindings);
    Rboolean R_EnvironmentIsLocked(SEXP env);
    void R_LockBinding(SEXP sym, SEXP env);
    void R_unLockBinding(SEXP sym, SEXP env);
    void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
    Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
    Rboolean R_BindingIsActive(SEXP sym, SEXP env);
    SEXP R_ActiveBindingFunction(SEXP sym, SEXP env);
    Rboolean R_HasFancyBindings(SEXP rho);
} // extern "C"

#endif /* ENVIRONMENT_HPP */
