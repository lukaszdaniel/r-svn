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
 *  This program is free software; you can redistribute it and/or modify
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

/** @file Environment.cpp
 *
 *
 * @brief Implementation of class rho:Environment and associated C
 * interface.
 */

#define USE_RINTERNALS // always use macro versions

#include <Localization.h>
#include <CXXR/GCRoot.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Environment.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Defn.h>
#include <Rinternals.h>
#include <Internal.h> // for R_getS4DataSlot

using namespace CXXR;

namespace R
{
    SEXP R_NewHashedEnv(SEXP enclos, int size);
}

namespace CXXR
{
    // Force the creation of non-inline embodiments of functions callable
    // from C:
    namespace ForceNonInline
    {
        const auto &ENCLOSptr = ENCLOS;
        const auto &ENVFLAGSptr = ENVFLAGS;
        const auto &HASHTABptr = HASHTAB;
        const auto &isEnvironmentptr = Rf_isEnvironment;
        const auto &FRAMEptr = FRAME;
        // const auto &ENV_RDEBUGptr = ENV_RDEBUG;
        const auto &SET_ENCLOSptr = SET_ENCLOS;
        const auto &SET_ENVFLAGSptr = SET_ENVFLAGS;
        const auto &SET_FRAMEptr = SET_FRAME;
        // const auto &SET_ENV_RDEBUGptr = SET_ENV_RDEBUG;
        const auto &SET_HASHTABptr = SET_HASHTAB;
    } // namespace ForceNonInline

    Environment *Environment::create(SEXP frame, SEXP enclosing_env, SEXP hashtab)
    {
        GCStackRoot<> framert(frame);
        GCStackRoot<> envrt(enclosing_env);
        GCStackRoot<> hashtabrt(hashtab);

        return new Environment(frame, enclosing_env, hashtab);
    }

    Environment *Environment::empty()
    {
        static GCRoot<Environment> empty(new Environment(R_NilValue, R_NilValue, R_NilValue));
        return empty;
    }

    Environment *Environment::base()
    {
        static GCRoot<Environment> base(new Environment(R_NilValue, empty(), R_NilValue));
        return base;
    }

    Environment *Environment::global()
    {
        return SEXP_downcast<Environment *>(R_GlobalEnv);
        // static GCRoot<Environment> global(createGlobalEnvironment());
        // return global;
    }

    Environment *Environment::baseNamespace()
    {
        return SEXP_downcast<Environment *>(R_BaseNamespace);
    }

    void Environment::initialize()
    {
        R_EmptyEnv = empty();
        R_BaseEnv = base();
    }

    const char *Environment::typeName() const
    {
        return staticTypeName();
    }

    void Environment::nullEnvironmentError()
    {
        Rf_error("%s", _("use of NULL environment is defunct"));
    }

    namespace
    {
        Environment *as_environment_internal(RObject *arg, bool allow_null, bool allow_s4)
        {
            if (arg == R_NilValue)
            {
                if (allow_null)
                    return R_NilValue;
                else
                    Environment::nullEnvironmentError();
            }
            else if (arg->sexptype() == ENVSXP)
            {
                return SEXP_downcast<Environment *>(arg);
            }
            else if (allow_s4 && IS_S4_OBJECT(arg) && arg->sexptype() == OBJSXP)
            {
                return SEXP_downcast<Environment *>(R_getS4DataSlot(arg, ENVSXP));
            }

            return R_NilValue;
        }
    } // anonumous namespace

    Environment *simple_as_environment(RObject *arg, bool allow_null)
    {
        return as_environment_internal(arg, allow_null, /*allow_s4=*/true);
    }

    Environment *downcast_to_env(RObject *arg, bool allow_null)
    {
        return as_environment_internal(arg, allow_null, /*allow_s4=*/false);
    }

    namespace
    {
        template <typename T>
        void printAttributes(std::ostream &os, const T *binding)
        {
            os << "attributes [";
            // print MISSING status
            auto missingStatus = MISSING(binding);
            os << "origin: " << missingStatus << "|";

            // print underlying value status
            SEXPTYPE underlyingType = binding->underlyingType();
            if (underlyingType)
            {
                os << "underlying: " << Rf_type2char(underlyingType) << "|";
            }
            else
            {
                os << "expanded: " << std::boolalpha << !underlyingType << "|";
            }

            // print assignment status
            bool assignmentPendingStatus = binding->assignmentPending();
            os << "pending: " << std::boolalpha << assignmentPendingStatus << "|";

            // print active binding status
            bool activeBindingStatus = IS_ACTIVE_BINDING(binding);
            os << "active: " << std::boolalpha << activeBindingStatus << "|";

            // print locked binding status
            bool lockedBinding = BINDING_IS_LOCKED(binding);
            os << "locked: " << std::boolalpha << lockedBinding;
            os << "]" << std::endl;
        }

        void printFrameBody(std::ostream &os, const PairList *binding, const std::string &prefix, bool show_refcnt)
        {
            if (binding == R_NilValue)
            {
                os << "Frame is empty" << std::endl;
                return;
            }

            // print binding attributes
            os << prefix << "├── ";
            {
                printAttributes(os, binding);
            }

            // print the symbol
            os << prefix << "├── ";
            {
                const Symbol *tg = SEXP_downcast<const Symbol *>(binding->tag());
                if (show_refcnt)
                    os << "(" << tg->getRefCount() << ") ";
                os << "symbol is '" << tg->name()->stdstring() << "'";
            }
            os << std::endl;

            // print the binding value
            os << prefix << "├── ";
            {
                const RObject *cr = binding->car0();
                if (show_refcnt)
                    os << "(" << cr->getRefCount() << ") ";
                SEXPTYPE underlyingType = binding->underlyingType();
                if (underlyingType)
                {
                    os << "value points to unexpanded underlying value of type " << Rf_type2char(underlyingType);
                }
                else
                {
                    os << "value is " << Rf_type2char(cr->sexptype());
                }
            }
            os << std::endl;

            // print the type of the tail node
            os << prefix << "└──";
            {
                const PairList *tl = SEXP_downcast<const PairList *>(binding->tail());
                if (tl != R_NilValue)
                {
                    os << "┐";
                    if (show_refcnt)
                        os << " (" << tl->getRefCount() << ")";
                    os << std::endl;
                    printFrameBody(os, SEXP_downcast<const PairList *>(tl), prefix + "   ", show_refcnt);
                }
                else
                {
                    os << "o";
                }
            }
        }
    } // anonymous namespace

    void printEnvironmentFrame(const Environment *env, bool show_refcnt, std::ostream &os)
    {
        os << "\n";

        if (env == R_NilValue)
        {
            os << "environment is empty" << std::endl;
            return;
        }

        if (!Environment::isA(env))
        {
            os << "env is not an Environment" << std::endl;
            return;
        }

        os << "Frame detais:\n";
        if (show_refcnt)
            os << "(" << env->getRefCount() << ") ";
        printFrameBody(os, SEXP_downcast<PairList *>(env->frame()), "", show_refcnt);
        os << "\n";
    }
} // namespace CXXR

namespace R
{
} // namespace R

// ***** C interface *****

