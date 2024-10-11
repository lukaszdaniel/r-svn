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
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file WeakRef.hpp
 * @brief Class CXXR::WeakRef and associated C interface.
 */

#ifndef WEAKREF_HPP
#define WEAKREF_HPP

#include <R_ext/Boolean.h>
#include <CXXR/RObject.hpp>

extern "C"
{
    using R_CFinalizer_t = void (*)(SEXP);
}

namespace CXXR
{
    /** @brief Weak reference.
     *
     * Refer to <em>Stretching the storage manager: weak pointers and
     * stable names in Haskell</em> by Peyton Jones, Marlow, and
     * Elliott (at <a
     * href="www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz">www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz</a>)
     * for the motivation and implementation of this class.
     *
     * Each WeakRef has a key and, optionally, a value and/or a
     * finalizer.  The finalizer may either be a C function or an R
     * object.  The mark-sweep garbage collector will consider the
     * value and finalizer to be reachable provided the key is
     * reachable.
     *
     * If, during a garbage collection, the key is found not to be
     * reachable then the finalizer (if any) will be run, and the weak
     * reference object will be 'tombstoned', so that subsequent calls
     * to key() and value() will return null pointers.
     *
     * A WeakRef object with a reachable key will not be garbage
     * collected even if the WeakRef object is not itself reachable.
     *
     * @note A WeakRef object takes steps to ensure that the reference
     * counts of itself and its key, value and R finalizer (if they
     * exist) never fall to zero until the WeakRef is tombstoned.
     * Consequently these objects will only be garbage collected as
     * part of a mark-sweep collection.  In particular, it can be
     * guaranteed that the finalizer of a WeakRef will be run as part
     * of the same mark-sweep collection in which the key of that
     * WeakRef is garbage-collected (having been found to be
     * unreachable).
     *
     * @todo It would probably make more sense for this class to
     * inherit directly from GCNode, and for the key, value etc. to be
     * pointers to GCNode.
     */
    class WeakRef : public RObject
    {
    public:
        /** @brief Is an RObject a WeakRef?
         *
         * @param obj Pointer to RObject to be tested.  This may be a
         *          null pointer, in which case the function returns
         *          false.
         *
         * @return true iff \a obj is a WeakRef.
         */
        static bool isA(const RObject *obj)
        {
            // We could of course use dynamic_cast here, but the
            // following is probably faster:
            if (!obj)
                return false;
            SEXPTYPE st = obj->sexptype();
            return st == WEAKREFSXP;
        }

    private:
        // Declared private to ensure that WeakRef objects are
        // allocated only using 'new':
        ~WeakRef() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        WeakRef(const WeakRef &);
        WeakRef &operator=(const WeakRef &);
    };
} // namespace CXXR

namespace R
{
    bool RunFinalizers(void);
} // namespace R

extern "C"
{
    SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);

    SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);

    SEXP R_WeakRefKey(SEXP w);

    SEXP R_WeakRefValue(SEXP w);

    void R_RunWeakRefFinalizer(SEXP x);

    void R_RunExitFinalizers(void);

    void R_RunPendingFinalizers(void);

    void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);

    void R_RegisterFinalizer(SEXP s, SEXP fun);

    void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);

    void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
}

#endif /* WEAKREF_HPP */
