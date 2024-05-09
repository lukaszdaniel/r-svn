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

#include <CXXR/GCNode.hpp>

namespace CXXR
{

struct vecsxp_struct {
    R_xlen_t m_length;
    R_xlen_t m_truelength;
    // void *m_data;
};

struct primsxp_struct {
    int m_offset;
};

struct symsxp_struct {
    RObject *m_pname;
    RObject *m_value;
    RObject *m_internal;
};

struct listsxp_struct {
    RObject *m_car;
    RObject *m_tail;
    RObject *m_tag;
};

struct envsxp_struct {
    RObject *m_frame;
    RObject *m_enclos;
    RObject *m_hashtab;
};

struct closxp_struct {
    RObject *m_formals;
    RObject *m_body;
    RObject *m_env;
};

struct promsxp_struct {
    RObject *m_value;
    RObject *m_expr;
    RObject *m_env;
};

struct bytecode_struct
{
    RObject *m_code;
    RObject *m_constants;
    RObject *m_expression;
};

struct altrep_struct
{
    RObject *m_data1;
    RObject *m_data2;
    RObject *m_altclass;
};

struct extptr_struct
{
    RObject *m_ptr;
    RObject *m_protege;
    RObject *m_tag;
};

struct s4ptr_struct
{
    RObject *m_car_dummy;
    RObject *m_tail_dummy;
    RObject *m_tag;
};

struct weakref_struct
{
    RObject *m_key;
    RObject *m_value;
    RObject *m_finalizer;
};

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

class RObject : public GCNode {
    public:
    RObject(SEXPTYPE stype = NILSXP) : GCNode(stype)
    {
        u.listsxp.m_car = nullptr;
        u.listsxp.m_tail = nullptr;
        u.listsxp.m_tag = nullptr;
    }
    // ~RObject() {}
    union {
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
	// struct vecsxp_struct vecsxp;
    } u;
};
} // namespace CXXR

#endif /* ROBJECT_HPP */
