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

/** @file RObject.hpp
 *
 * @brief Class CXXR::RObject and associated C interface functions.
 */

#ifndef ROBJECT_HPP
#define ROBJECT_HPP

#include <R_ext/Boolean.h>
#include <CXXR/GCNode.hpp>
#include <CXXR/GCEdge.hpp>

/*
Triplet's translation table:
+------------------------------------------------------------------------------+
| Type     | CAR               | CDR                 | TAG                     |
+------------------------------------------------------------------------------+
| LIST     | (SET)CAR          | (SET)CDR            | (SET_)TAG               |
| ENV      | (SET_)FRAME       | (SET_)ENCLOS        | (SET_)HASHTAB           |
| CLO      | (SET_)FORMALS     | (SET_)BODY          | (SET_)CLOENV            |
| PROM     | (SET_)PRVALUE     | (SET_)PRCODE        | (SET_)PRENV             |
| SYM      | (SET_)PRINTNAME   | (SET_)SYMVALUE      | (SET_)INTERNAL          |
| BYTECODE | (SET_)CODE        | (SET_)CONSTS        | (SET_)EXPR              |
| ALTREP   | (SET_)DATA1       | (SET_)DATA2         | (SET_)CLASS             |
| EXTPTR   | (....)EXTPTR_PTR  | (....)EXTPTR_PROT   | (....)EXTPTR_TAG        |
| S4OBJ    | ................. | ................... | (SET_)S4TAG             |
| WEAKREF  | (SET_)WEAKREF_KEY | (SET_)WEAKREF_VALUE | (SET_)WEAKREF_FINALIZER |
+------------------------------------------------------------------------------+
*/

/** @brief Namespace for the CXXR project.
 *
 * CXXR is a project to refactorize the R interpreter into C++.
 */
namespace CXXR
{
    struct vecsxp_struct
    {
        R_xlen_t m_length;
        R_xlen_t m_truelength; // the number of non-null elements in the vector or hash value in case of char (aka String class)
        void *m_data;
    };

    struct primsxp_struct
    {
        int m_offset;
    };

    struct symsxp_struct
    {
        RObject *m_pname;
        RObject *m_value;
        RObject *m_internal;
    };

    struct listsxp_struct
    {
        RObject *m_car;
        RObject *m_tail;
        RObject *m_tag;
    };

    struct envsxp_struct
    {
        GCEdge<> m_frame;
        GCEdge<> m_enclos;
        GCEdge<> m_hashtab;
    };

    struct closxp_struct
    {
        RObject *m_formals;
        RObject *m_body;
        RObject *m_env;
    };

    struct promsxp_struct
    {
        RObject *m_value;
        RObject *m_expr;
        RObject *m_env;
    };

    struct bytecode_struct
    {
        GCEdge<> m_code;
        GCEdge<> m_constants;
        GCEdge<> m_expression;
    };

    struct altrep_struct
    {
        RObject *m_data1;
        RObject *m_data2;
        RObject *m_altclass;
    };

    struct extptr_struct
    {
        void *m_ptr;
        GCEdge<> m_protege;
        GCEdge<> m_tag;
    };

    struct s4ptr_struct
    {
        RObject *m_car_dummy;
        RObject *m_tail_dummy;
        RObject *m_tag;
    };

    struct weakref_struct
    {
        GCEdge<> m_key;
        GCEdge<> m_value;
        GCEdge<> m_finalizer;
    };

    /** @brief Replacement for CR's SEXPREC.
     *
     * This class is the rough equivalent within CXXR of the SEXPREC
     * union within CR.  However, all functionality relating to
     * garbage collection has been factored out into the base class
     * GCNode, and as CXXR development proceeds other functionality
     * will be factored out into derived classes (corresponding
     * roughly, but not exactly, to different ::SEXPTYPE values within
     * CR), or outside the RObject hierarchy altogether.
     *
     * Eventually this class may end up simply as the home of R
     * attributes.
     *
     * @note The word 'object' in the name of this class is used in
     * the sense in which the 'blue book' (Becker <em>et al.</em>
     * [1988]) uses the phrase 'data object'.  Roughly speaking,
     * CXXR::RObject is a base class for the sorts of data items whose
     * existence would be reported by the R function
     * <tt>objects()</tt>.  In particular, it does not imply that
     * the object belongs to an R class.
     *
     * @todo Incorporate further attribute consistency checks within
     * the class interface.  Possibly make setAttribute() virtual so
     * that these consistency checks can be tailored according to the
     * derived class.
     */
    class RObject : public GCNode
    {
    public:
        enum class Duplication
        {
            SHALLOW,
            DEEP
        };

        /** @brief Remove all attributes.
         */
        void clearAttributes();

        /** @brief Special constructor for R_NilValue.
         */
        RObject();

        RObject(SEXPTYPE stype);

        /** @brief Is copying etc. of this object being traced?
         *
         * The property reported by this function is used by R
         * functions such as <tt>tracemem</tt>, and has effect only if
         * rho is built with R_MEMORY_PROFILING defined (as will
         * happen if it is configured with --enable-memory-profiling).
         *
         * @return A return value of true signifies that when a copy
         * is made of this object, or - more generally - some
         * comparably sized object is derived from this object, this
         * fact should be reported, and the 'memory traced' property
         * propagated to the new object.
         */
        bool memoryTraced() const
        {
            return sxpinfo.trace; // m_memory_traced;
        }

        /** @brief Enable/disable tracing of copying etc.
         *
         * The property set by this function is used by R functions
         * such as <tt>tracemem</tt>, and has effect only if rho is
         * built with R_MEMORY_PROFILING defined (as will happen if it
         * is configured with --enable-memory-profiling).
         *
         * @param on A value of true signifies that when a copy
         *         is made of this object, or - more generally - some
         *          comparably sized object is derived from this
         *          object, this fact should be reported, and the
         *          'memory traced' property propagated to the new
         *          object.
         */
        void setMemoryTracing(bool on)
        {
            sxpinfo.trace = on; // m_memory_traced
        }

        /** @brief Carry out memory tracing.
         *
         * This function is a no-op unless rho is built with
         * R_MEMORY_PROFILING defined (as will happen if it is
         * configured with --enable-memory-profiling).
         *
         * This function should be called if <tt>this</tt> has been
         * created as a copy of \a src, or if <tt>this</tt> has been
         * derived in some way from \a src1.  When memory profiling is
         * enabled, if \a src points to an RObject with the
         * memoryTraced() property set, this property will be
         * propagated to <tt>this</tt>.  Also the creation of this
         * object will be reported, along with the current context
         * stack.
         *
         * @param src Non-null pointer to an RObject.
         */
        void maybeTraceMemory(const RObject *src)
        {
#ifdef R_MEMORY_PROFILING
            if (src->memoryTraced())
                traceMemory(src, nullptr, nullptr);
#endif
        }

        /** @brief Carry out memory tracing.
         *
         * This function is a no-op unless rho is built with
         * R_MEMORY_PROFILING defined (as will happen if it is
         * configured with --enable-memory-profiling).
         *
         * This function should be called if <tt>this</tt> has been
         * derived in some way from \a src1 and \a src2.  When memory
         * profiling is enabled, if either \a src1 or \a src2 points
         * to an RObject with the memoryTraced() property set, this
         * property will be propagated to <tt>this</tt>.  Also the
         * creation of this object will be reported, along with the
         * current context stack.
         *
         * @param src1 Non-null pointer to an RObject.
         *
         * @param src2 Non-null pointer to an RObject.
         */
        void maybeTraceMemory(const RObject *src1,
                              const RObject *src2)
        {
#ifdef R_MEMORY_PROFILING
            if (src1->memoryTraced() || src2->memoryTraced())
                traceMemory(src1, src2, nullptr);
#endif
        }

        /** @brief Carry out memory tracing.
         *
         * This function is a no-op unless rho is built with
         * R_MEMORY_PROFILING defined (as will happen if it is
         * configured with --enable-memory-profiling).
         *
         * This function should be called if <tt>this</tt> has been
         * derived in some way from \a src1, \a src2 and \a src 3.  When
         * memory profiling is enabled, if any of \a src1, \a src2 or
         * \a src3 points to an RObject with the memoryTraced()
         * property set, this property will be propagated to
         * <tt>this</tt>.  Also the creation of this object will be
         * reported, along with the current context stack.
         *
         * @param src1 Non-null pointer to an RObject.
         *
         * @param src2 Non-null pointer to an RObject.
         *
         * @param src3 Non-null pointer to an RObject.
         */
        void maybeTraceMemory(const RObject *src1,
                              const RObject *src2,
                              const RObject *src3)
        {
#ifdef R_MEMORY_PROFILING
            if (src1->memoryTraced() || src2->memoryTraced() || src3->memoryTraced())
                traceMemory(src1, src2, src3);
#endif
        }

        /** @brief Get an object's ::SEXPTYPE.
         *
         * @return ::SEXPTYPE of this object.
         */
        SEXPTYPE sexptype() const
        {
            return sxpinfo.type;
        }

        /** @brief Altrep status of this object.
         *
         * @return altrep status of this object.
         */
        bool altrep() const
        {
            return sxpinfo.alt;
        }

        GCEdge<> m_attrib;

        union U
        {
            struct primsxp_struct primsxp;
            struct symsxp_struct symsxp;
            struct listsxp_struct listsxp;
            struct envsxp_struct envsxp;
            struct closxp_struct closxp;
            struct promsxp_struct promsxp;
            struct bytecode_struct bytecode;
            struct altrep_struct altrep;
            struct extptr_struct extptr;
            struct s4ptr_struct s4ptr;
            struct weakref_struct weakrrefptr;
            struct vecsxp_struct vecsxp;
            U()
            {
                listsxp.m_car = nullptr;
                listsxp.m_tail = nullptr;
                listsxp.m_tag = nullptr;
            }
            ~U() {}
        } u;

    protected:
        /** Destructor
         *
         * @note The destructor is protected to ensure that RObjects
         * are allocated on the heap.  (See Meyers 'More Effective
         * C++' Item 27.) Derived classes should likewise declare
         * their constructors private or protected.
         */
        ~RObject() {}

        // Not implemented yet.  Declared to prevent
        // compiler-generated versions:
        RObject(const RObject &);
        RObject &operator=(const RObject &);

    private:
#ifdef R_MEMORY_PROFILING
        // This function implements maybeTraceMemory() (qv.) when
        // memory profiling is enabled.
        void traceMemory(const RObject *src1, const RObject *src2,
                         const RObject *src3);
#endif
    };

    class GlobalParameter
    {
    public:
        static bool s_mbcslocale;
#ifdef _WIN32
        static bool s_UserBreak;
#endif
        GlobalParameter() = delete;
    };
#define UserBreak CXXR::GlobalParameter::s_UserBreak
} // namespace CXXR

namespace R
{
    /** @brief Set object max copying status.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    void (ENSURE_NAMEDMAX)(SEXP x);

    /** @brief Set object copying status to one.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    void (ENSURE_NAMED)(SEXP x);

    /** @brief Set object copying status to zero.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    void (SETTER_CLEAR_NAMED)(SEXP x);

    /** @brief Raise object copying status if possible.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    void (RAISE_NAMED)(SEXP x, int n);

    SEXP R_FixupRHS(SEXP x, SEXP y);

    const char *typeName(SEXP v);

    /** @brief C interface to RObject::traceMemory().
     *
     * This function provides a C language interface to
     * <tt>dest->maybeTraceMemory(src)</tt>: see the documentation of
     * that method for details.
     *
     * @param dest Non-null pointer to an RObject.
     *
     * @param src Non-null pointer to an RObject.
     */
    void maybeTraceMemory1(SEXP dest, SEXP src);

    /** @brief C interface to RObject::traceMemory().
     *
     * This function provides a C language interface to
     * <tt>dest->maybeTraceMemory(src1, src2)</tt>: see the documentation
     * of that method for details.
     *
     * @param dest Non-null pointer to an RObject.
     *
     * @param src1 Non-null pointer to an RObject.
     *
     * @param src2 Non-null pointer to an RObject.
     */
    void maybeTraceMemory2(SEXP dest, SEXP src1, SEXP src2);
} // namespace R

extern "C"
{
    /** @brief The nil object
     */
    extern SEXP R_NilValue;

    /** @brief Get object's ::SEXPTYPE.
     *
     * @param x Pointer to CXXR::RObject.
     *
     * @return ::SEXPTYPE of \a x, or ::NILSXP if x is a null pointer.
     */
    SEXPTYPE (TYPEOF)(SEXP x);

    /** @brief Name of type within R.
     *
     * Translate a ::SEXPTYPE to the name by which it is known within R.
     *
     * @param st The ::SEXPTYPE whose name is required.
     *
     * @return The ::SEXPTYPE's name within R.
     */
    const char *Rf_type2char(SEXPTYPE st);
    SEXP Rf_type2rstr(SEXPTYPE);
    SEXP Rf_type2str(SEXPTYPE);
    SEXP Rf_type2str_nowarn(SEXPTYPE);
    SEXPTYPE Rf_str2type(const char *const s);

    /** @brief Copy attributes, with some exceptions.
     *
     * This is called in the case of binary operations to copy most
     * attributes from one of the input arguments to the output.
     * Note that the Dim, Dimnames and Names attributes are not
     * copied: these should have been assigned elsewhere.  The
     * function also copies the S4 object status.
     *
     * @param inp Pointer to the CXXR::RObject from which attributes are to
     *          be copied.
     *
     * @param ans Pointer to the CXXR::RObject to which attributes are to be
     *          copied.
     *
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    void Rf_copyMostAttrib(SEXP inp, SEXP ans);

    /** @brief Access a named attribute.
     *
     * @param vec Pointer to the CXXR::RObject whose attributes are to be
     *          accessed.
     *
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a CXXR::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     *
     * @return Pointer to the requested attribute, or a null pointer
     *         if there is no such attribute.
     *
     * @note The above documentation is incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_getAttrib(SEXP vec, SEXP name);

    /** @brief Set or remove a named attribute.
     *
     * @param vec Pointer to the CXXR::RObject whose attributes are to be
     *          modified.
     *
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a CXXR::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     *
     * @param val Either the value to which the attribute is to be
     *          set, or a null pointer.  In the latter case the
     *          attribute (if present) is removed.
     *
     * @return Refer to source code.  (Sometimes \a vec, sometimes \a
     * val, sometime a null pointer ...)
     *
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val);

    /** @brief Does an object have a class attribute?
     *
     * @param x Pointer to a CXXR::RObject.
     *
     * @return true iff \a x has a class attribute.  Returns false if \a x
     * is 0.
     */
    int (OBJECT)(SEXP x);

    /** @brief Is this the null object pointer?
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff the CXXR::RObject pointed to by \a s is either a null
     * pointer (i.e. <tt>== R_NilValue</tt> in CXXR), or is a CXXR::RObject
     * with ::SEXPTYPE ::NILSXP (should not happen in CXXR).
     */
    Rboolean Rf_isNull(SEXP s);

    /** @brief Does an object have a class attribute?
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff the CXXR::RObject pointed to by \a s has a
     * class attribute.
     */
    Rboolean Rf_isObject(SEXP s);

    /** @brief Get the attributes of a CXXR::RObject.
     *
     * @param x Pointer to the CXXR::RObject whose attributes are required.
     *
     * @return Pointer to the attributes object of \a x , or 0 if \a x is
     * a null pointer.
     */
    SEXP (ATTRIB)(SEXP x);

    /** @brief (For use only in serialization.)
     */
    int (LEVELS)(SEXP x);

    /** @brief Get object copying status.
     *
     * @param x Pointer to CXXR::RObject.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     * null pointer.
     */
    int (NAMED)(SEXP x);

    /** @brief (For use only in deserialization.)
     *
     */
    void (SETLEVELS)(SEXP x, int v);

    /** @brief Replace an object's attributes.
     *
     * @param x Pointer to a CXXR::RObject.
     *
     * @param v Pointer to a PairList giving the new attributes of \a
     *          x.  \a x should be considered to assume ownership of
     *          the 'car' values in \a v ; they should therefore not
     *          be subsequently altered externally.
     *
     * @note Unlike CR, \a v isn't simply plugged into the attributes
     * field of \a x : refer to the documentation for \c
     * RObject::setAttributes() .  In particular, do not attempt to
     * modify the attributes by changing \a v \e after SET_ATTRIB
     * has been called.
     *
     * @note For compatibility with CR, garbage collection is
     * inhibited within this function.
     */
    void (SET_ATTRIB)(SEXP x, SEXP v);

    /** @brief Set object copying status.
     *
     * @param x Pointer to CXXR::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    void (SET_NAMED)(SEXP x, int v);

    /**
     * @deprecated This has no effect in CXXR.
     * Object status is determined in setAttributes().
     */
    void (SET_OBJECT)(SEXP x, int v);

    /**
     * @deprecated Ought to be private.
     */
    void (SET_TYPEOF)(SEXP x, SEXPTYPE dest_type);

    /** @brief Replace the attributes of \a to by those of \a from.
     *
     * The status of \a to as an S4 Object is also copied from \a from .
     *
     * @param to Pointer to CXXR::RObject.
     *
     * @param from Pointer to another CXXR::RObject.
     */
    void DUPLICATE_ATTRIB(SEXP to, SEXP from);

    /* TODO: a  Length(.) {say} which is length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.cpp
    */
    R_len_t Rf_length(SEXP s);

    R_xlen_t Rf_xlength(SEXP s);

    Rboolean Rf_isFrame(SEXP s);

    /** @brief Check to see if the arrays "x" and "y" have the identical extents
     */
    Rboolean Rf_conformable(SEXP x, SEXP y);

    /**
     * @note R's Rf_inherits() is based on inherits3() in ../main/objects.cpp
     * Here, use char / CHAR() instead of the slower more general Rf_translateChar()
     */
    Rboolean Rf_inherits(SEXP s, const char *name);

    void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);
} // extern "C"

#endif /* ROBJECT_HPP */
