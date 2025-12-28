/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 2003--2018  The R Foundation
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file names.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>
#include <string>
#include <Localization.h>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/RContext.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/String.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/StringVector.hpp>
#include <CXXR/SEXP_downcast.hpp>
#include <Defn.h>
#include <Internal.h>

#include <Print.h>
#include "arithmetic.h" /* for do_math[1234], do_cmathfuns */

#include <Rinterface.h>

using namespace R;
using namespace CXXR;

/* Table of  .Internal(.) and .Primitive(.)  R functions
 * =====     =========	      ==========
 *
 * Each entry is a line with
 *
 *  printname  c-entry  offset   eval       arity      pp-kind   precedence      rightassoc
 *  ---------  -------  ------   ----       -----      -------   ----------      ----------
 *2 name       cfun     code     eval       arity      gram.kind gram.precedence gram.rightassoc
 *3 PRIMNAME   PRIMFUN  PRIMVAL  PRIMPRINT  PRIMARITY  PPINFO    PPINFO          PPINFO
 *
 * where "2" are the component names of the FUNTAB struct (Defn.h)
 * and	 "3" are the accessor macros. [*]: PRIMPRINT(.) uses the eval component
 *
 * printname:	The function name in R
 *
 * c-entry:	The name of the corresponding C function,
 *		actually declared in ../include/Internal.h .
 *		Convention:
 *		 - all start with "do_",
 *		 - all return SEXP.
 *		 - all have argument list
 *			 (SEXP call, SEXP op, SEXP args, SEXP env)
 *
 * offset:	the 'op' (offset pointer) above; used for C functions
 *		which deal with more than one R function...
 *
 * eval:	= XYZ (three digits) --- where e.g. '1' means '001'
 *		X=1 says that we should force Evaluator::resultPrinted() off
 *		X=0 says that we should force Evaluator::resultPrinted() on
 *		X=2 says that we should switch Evaluator::resultPrinted() on but let the C
 *                  code update this.
 *		Y=1 says that this is an internal function which must
 *		    be accessed with a	.Internal(.) call, any other value is
 *		    accessible directly and printed in R as ".Primitive(..)".
 *		Z=1 says evaluate arguments before calling (BUILTINSXP) and
 *		Z=0 says don't evaluate (SPECIALSXP).
 *
 * arity:	How many arguments are required/allowed;  "-1"	meaning ``any''
 *
 * pp-kind:	Deparsing Info (-> PPkind in ../include/Defn.h )
 *
 * precedence: Operator precedence (-> PPprec in ../include/Defn.h )
 *
 * rightassoc: Right (1) or left (0) associative operator
 *
 */

std::vector<CXXR::BuiltInFunction::FUNTAB> R_FunTab =
{

/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc
 * ---------	-------		------	----	-----	-------      ----------	----------*/

/* Language Related Constructs */

/* Primitives */
{"if",		do_if,		0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"while",	do_while,	0,	100,	2,	{PP_WHILE,   PREC_FN,	  0}},
{"for",		do_for,		0,	100,	3,	{PP_FOR,     PREC_FN,	  0}},
{"repeat",	do_repeat,	0,	100,	1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break, CTXT_BREAK,	0,	0,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break, CTXT_NEXT,	0,	0,	{PP_NEXT,    PREC_FN,	  0}},
{"return",	do_return,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"{",		do_begin,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"(",		do_paren,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}},
{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"[",		do_subset,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"@",		do_AT,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"[<-",		do_subassign,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}, "EXPR"},
{"browser",	do_browser,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Internal",	do_internal,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"call",	do_call,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, "name"},
{"quote",	do_quote,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "expr"},
{"substitute",	do_substitute,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"nargs",	do_nargs,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"on.exit",	do_onexit,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"forceAndCall",do_forceAndCall,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"declare",	do_declare,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	do_gettext,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"ngettext",	do_ngettext,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"bindtextdomain",do_bindtextdomain,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addCondHands",do_addCondHands,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".addGlobHands",do_addGlobHands,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".resetCondHands",do_resetCondHands,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".signalCondition",do_signalCondition,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltStop",do_dfltStop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltWarn",do_dfltWarn,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addRestart",do_addRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".getRestart",do_getRestart,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".invokeRestart",do_invokeRestart,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addTryHandlers",do_addTryHandlers,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"geterrmessage",do_geterrmessage, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"seterrmessage",do_seterrmessage, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"printDeferredWarnings",do_printDeferredWarnings, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"interruptsSuspended",do_interruptsSuspended, 0,	11,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"as.function.default",do_asfunction,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}},
{"debug",	do_debug,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",do_delayed,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"makeLazy",	do_makelazy,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{"identical",	do_identical,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	  0}},
{"C_tryCatchHelper",do_tryCatchHelper,0,11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceValue",	do_getNSValue,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	  0}},


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
{"+",		do_arith,	PLUSOP,	1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, DispatchType::GROUP_OPS},
{"-",		do_arith,	MINUSOP,1,	-1,	{PP_BINARY,  PREC_SUM,	  0}, nullptr, DispatchType::GROUP_OPS},
{"*",		do_arith,	TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}, nullptr, DispatchType::GROUP_OPS},
{"/",		do_arith,	DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}, nullptr, DispatchType::GROUP_OPS},
{"^",		do_arith,	POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}, nullptr, DispatchType::GROUP_OPS},
{"%%",		do_arith,	MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, DispatchType::GROUP_OPS},
{"%/%",		do_arith,	IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}, nullptr, DispatchType::GROUP_OPS},
{"%*%",		do_matprod,	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}},

{"==",		do_relop,	EQOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{"!=",		do_relop,	NEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{"<",		do_relop,	LTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{"<=",		do_relop,	LEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{">=",		do_relop,	GEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{">",		do_relop,	GTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}, nullptr, DispatchType::GROUP_OPS},
{"&",		do_logic,	1,	1,	2,	{PP_BINARY,  PREC_AND,	  0}, nullptr, DispatchType::GROUP_OPS},
{"|",		do_logic,	2,	1,	2,	{PP_BINARY,  PREC_OR,	  0}, nullptr, DispatchType::GROUP_OPS},
{"!",		do_logic,	3,	1,	1,	{PP_UNARY,   PREC_NOT,	  0}, nullptr, DispatchType::GROUP_OPS},

/* specials as conditionally evaluate second arg */
{"&&",		do_logic2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_logic2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},
{":",		do_colon,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}},
/* does not evaluate */
{"~",		do_tilde,	0,	0,	-1,	{PP_BINARY,  PREC_TILDE,  0}},
{"::",		do_colon2,	0,	200,	2,	{PP_BINARY2, PREC_NS,	  0}},
{":::",		do_colon3,	0,	200,	2,	{PP_BINARY2, PREC_NS,	  0}},


/* Logic Related Functions */
/* these are group generic and so need to eval args */
{"all",		do_logic3,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, DispatchType::GROUP_SUMMARY},
{"any",		do_logic3,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}, nullptr, DispatchType::GROUP_SUMMARY},


/* Vectors, Matrices and Arrays */

/* Primitives */

{"...elt",      do_dotsElt,	0,    201,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"...length",   do_dotsLength,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"...names",    do_dotsNames,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"length",	do_length,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"length<-",	do_lengthgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", DispatchType::INTERNAL},
{"c",/* bind.c: */ do_c,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass",	do_class,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"oldClass<-",	do_classgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}, "x"},
{"class",	R_do_data_class,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",R_do_data_class,1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{".class2",	R_do_data_class,2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"unclass",	do_unclass,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"names",	do_names,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"names<-",	do_namesgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", DispatchType::INTERNAL},
{"dimnames",	do_dimnames,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"dimnames<-",	do_dimnamesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", DispatchType::INTERNAL},
{"dim",		do_dim,		0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"dim<-",	do_dimgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, nullptr, DispatchType::INTERNAL},
{"attributes",	do_attributes,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::INTERNAL},
{"attributes<-",do_attributesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"},
{"attr",	do_attr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"attr<-",	do_attrgets,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}},
{"@<-",		do_attrgets,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"levels<-",	do_levelsgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x", DispatchType::INTERNAL},

/* .Internals */

{"vector",	do_makevector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"complex",	do_complex,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"matrix",	do_matrix,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"array",	do_array,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"diag",	do_diag,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"backsolve",	do_backsolve,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"max.col",	do_maxcol,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"row",		do_rowscols,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"col",		do_rowscols,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::INTERNAL},
{"cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	do_drop,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"all.names",	do_allnames,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"comment",	do_comment,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"comment<-",	do_commentgets,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"get",		do_get,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"get0",	do_get,		2,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"assign",	do_assign,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"list2env",	do_list2env,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"remove",	do_remove,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"duplicated",	do_duplicated,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"anyNA",	do_anyNA,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	do_which,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.min",	do_first_min,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	do_pmatch,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"charmatch",	do_charmatch,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match.call",	do_matchcall,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"crossprod",	do_matprod,	1,	1,	1,	{PP_FUNCALL, PREC_FN,   0}},
{"tcrossprod",	do_matprod,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asplit",	do_asplit,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"lengths",	do_lengths,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::INTERNAL},
{"sequence",	do_sequence,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{"vhash",	do_vhash,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"attach",	do_attach,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"detach",	do_detach,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"search",	do_search,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setFileTime",	do_setFileTime,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},  // Special
{"signif",	do_Math2,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},  // Special
{"log",		do_log,		10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},  // Special
{"log10",	do_log1arg,	10010,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"log2",	do_log1arg,	10002,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"abs",		do_abs,		6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"floor",	do_math1,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"ceiling",	do_math1,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"sqrt",	do_math1,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"sign",	do_math1,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"trunc",	do_trunc,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},

{"exp",		do_math1,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"expm1",	do_math1,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"log1p",	do_math1,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},

{"cos",		do_math1,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"sin",		do_math1,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"tan",		do_math1,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"acos",	do_math1,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"asin",	do_math1,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"atan",	do_math1,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},

{"cosh",	do_math1,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"sinh",	do_math1,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"tanh",	do_math1,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"acosh",	do_math1,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"asinh",	do_math1,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"atanh",	do_math1,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},

{"lgamma",	do_math1,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"gamma",	do_math1,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},

{"digamma",	do_math1,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"trigamma",	do_math1,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
/* see "psigamma" below !*/

{"cospi",	do_math1,	47,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"sinpi",	do_math1,	48,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},
{"tanpi",	do_math1,	49,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::GROUP_MATH},

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/*
  Can remove all the [dpqr]xxx once the compiler knows how to optimize
  to .External.

  This is most of the do_math[23], and all of the do_math4, do_random[123]
*/
{"dchisq",	do_math2,	6,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2,	7,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2,	8,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2,	9,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2,	10,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2,	11,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2,	12,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2,	13,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2,	14,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2,	15,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2,	16,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2,	17,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2,	18,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2,	19,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2,	20,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2,	21,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2,	22,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2,	23,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of a Complex Argument:
 * These are group generic and so need to eval args --> ./complex.c */

{"Re",		do_cmathfuns,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z", DispatchType::GROUP_COMPLEX},
{"Im",		do_cmathfuns,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z", DispatchType::GROUP_COMPLEX},
{"Mod",		do_cmathfuns,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z", DispatchType::GROUP_COMPLEX},
{"Arg",		do_cmathfuns,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z", DispatchType::GROUP_COMPLEX},
{"Conj",	do_cmathfuns,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "z", DispatchType::GROUP_COMPLEX},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	1,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3,	2,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3,	3,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3,	4,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3,	5,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3,	6,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3,	7,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3,	8,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3,	9,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3,	10,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3,	11,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3,	12,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3,	13,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3,	14,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3,	15,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3,	16,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3,	17,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3,	18,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3,	19,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3,	20,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3,	21,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3,	22,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3,	23,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3,	24,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3,	25,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3,	26,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3,	27,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3,	28,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3,	29,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3,	30,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3,	31,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3,	32,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3,	33,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnchisq",	do_math3,	34,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnchisq",	do_math3,	35,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnchisq",	do_math3,	36,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnt",		do_math3,	37,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnt",		do_math3,	38,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnt",		do_math3,	39,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dwilcox",	do_math3,	40,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pwilcox",	do_math3,	41,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qwilcox",	do_math3,	42,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselI",	do_math3,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom_mu",	do_math3,	45,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3,	46,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom_mu",	do_math3,	47,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4,	1,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"phyper",	do_math4,	2,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qhyper",	do_math4,	3,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbeta",	do_math4,	4,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbeta",	do_math4,	5,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbeta",	do_math4,	6,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnf",		do_math4,	7,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnf",		do_math4,	8,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnf",		do_math4,	9,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dtukey",	do_math4,	10,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ptukey",	do_math4,	11,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qtukey",	do_math4,	12,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

/* Random Numbers */

{"rchisq",	do_random1,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rexp",	do_random1,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rgeom",	do_random1,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rpois",	do_random1,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rt",		do_random1,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rsignrank",	do_random1,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"rbeta",	do_random2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rbinom",	do_random2,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rcauchy",	do_random2,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rf",		do_random2,	3,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rgamma",	do_random2,	4,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlnorm",	do_random2,	5,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlogis",	do_random2,	6,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom",	do_random2,	7,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom_mu",	do_random2,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnchisq",	do_random2,	12,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnorm",	do_random2,	8,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"runif",	do_random2,	9,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rweibull",	do_random2,	10,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rwilcox",	do_random2,	11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"rhyper",	do_random3,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{"sample",	do_sample,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample2",	do_sample2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"RNGkind",	do_RNGkind,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"set.seed",	do_setseed,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},

/* Data Summaries */
/* these four are group generic and so need to eval args */
{"sum",		do_summary,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},
{"min",		do_summary,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},
{"max",		do_summary,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},
{"prod",	do_summary,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},

{"mean",	do_summary,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},
{"range",	do_range,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_SUMMARY},

/* Note that the number of arguments in this group only applies
   to the default method */
{"cumsum",	do_cum,		1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},
{"cumprod",	do_cum,		2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},
{"cummax",	do_cum,		3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},
{"cummin",	do_cum,		4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::GROUP_MATH},

/* Type coercion */

{"as.character",do_asatomic,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.integer",	do_asatomic,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.double",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.numeric",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.complex",	do_asatomic,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.logical",	do_asatomic,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.raw",	do_asatomic,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"as.call",	do_ascall,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"as.environment",do_as_environment,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "object", DispatchType::INTERNAL},
{"storage.mode<-",do_storage_mode,0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"asCharacterFactor",	do_asCharacterFactor,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"as.vector",	do_asvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::INTERNAL},
{"paste",	do_paste,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"paste0",	do_paste,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.path",	do_filepath,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format",	do_format,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"format.info",	do_formatinfo,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cat",		do_cat,		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"do.call",	do_docall,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"str2lang",	do_str2lang,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"str2expression", do_str2lang, 1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* String Manipulation */

{"nchar",	do_nchar,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"nzchar",	do_nzchar,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	do_substr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"startsWith",	do_startsWith,  0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"endsWith",	do_startsWith,  1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"substr<-",	do_substrgets,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"strsplit",	do_strsplit,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	do_abbrev,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.names",	do_makenames,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"pcre_config", do_pcre_config,	1,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"grep",	do_grep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepl",	do_grep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepRaw",	do_grepraw,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"sub",		do_gsub,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"gsub",	do_gsub,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"regexpr",	do_regexpr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gregexpr",	do_regexpr,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"regexec",	do_regexec,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"agrep",	do_agrep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"agrepl",	do_agrep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"adist",	do_adist,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"aregexec",	do_aregexec,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sprintf",	do_sprintf,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	do_makeunique,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"charToRaw",	do_charToRaw,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToChar",	do_rawToChar,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rawShift",	do_rawShift,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"intToBits",	do_intToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"numToBits",	do_numToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"numToInts",	do_numToInts,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToBits",	do_rawToBits,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"packBits",	do_packBits,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"utf8ToInt",	do_utf8ToInt,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"intToUtf8",	do_intToUtf8,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"validUTF8",	do_validUTF8,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"validEnc",	do_validEnc,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"encodeString",do_encodeString,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"iconv",	do_iconv,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	do_strtrim,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strtoi",	do_strtoi,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strrep",	do_strrep,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is,		NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.logical",	do_is,		LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.integer",	do_is,		INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.double",	do_is,		REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.complex",	do_is,		CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.character",do_is,		STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.symbol",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.name",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.environment",do_is,	ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.list",	do_is,		VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.pairlist",	do_is,		LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.expression",do_is,		EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.raw",	do_is,		RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.object",	do_is,		50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"isS4",	do_is,		51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.numeric",	do_is,		100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"is.matrix",	do_is,		101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"is.array",	do_is,		102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},

{"is.atomic",	do_is,		200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.recursive",do_is,		201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.call",	do_is,		300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.language",	do_is,		301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"is.function",	do_is,		302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.single",	do_is,		999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"is.na",	do_isna,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"is.nan",	do_isnan,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"is.finite",	do_isfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},
{"is.infinite",	do_isinfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x", DispatchType::INTERNAL},

{"is.vector",	do_isvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Miscellaneous */

/* Primitive */
{"proc.time",	do_proctime,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expression",	do_expression,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"interactive",	do_interactive,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"invisible",	do_invisible,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep.int",	do_rep_int,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rep_len",	do_rep_len,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"seq.int",	do_seq,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "length.out"},
{"seq_along",	do_seq_along,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "along.with"},
{"list",	do_makelist,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"xtfrm",	do_xtfrm,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"enc2native",	do_enc2,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},
{"enc2utf8",	do_enc2,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{"emptyenv",	do_emptyenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"baseenv",	do_baseenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"globalenv",	do_globalenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"environment<-",do_envirgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}, "x"},
{"pos.to.env",	do_pos2env,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}, "x"},

{".C",		do_dotCode,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External2",   do_External,   1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},

/* .Internal */
{"eapply",	do_eapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lapply",	do_lapply,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"vapply",	do_vapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mapply",	do_mapply,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"Version",	do_version,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"machine",	do_machine,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"commandArgs", do_commandArgs, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"internalsID",	do_internalsID,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}},

#ifdef Win32
{"system",	do_system,	0,	211,	7,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system,	0,	211,	4,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"shell.exec",	do_shellexec,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mkjunction", do_mkjunction,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"tzone_name", do_tzone_name,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif

{"parse",	do_parse,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
//{"parse_Rd",	do_parseRd,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
//{"deparseRd",	do_deparseRd,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
//{"parseLatex",  do_parseLatex,  0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"save",	do_save,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"saveToConn",	do_saveToConn,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"load",	do_load,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"loadFromConn2",    do_loadFromConn2,0, 111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"loadInfoFromConn2",do_loadFromConn2,1, 11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeToConn",	 do_serializeToConn,	0, 111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	 do_unserializeFromConn, 0, 11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeInfoFromConn",do_unserializeFromConn, 1, 11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"deparse",	do_deparse,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"dput",	do_dput,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"dump",	do_dump,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"quit",	do_quit,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"readline",	do_readln,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"print.default",do_printdefault,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	do_prmatrix,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc,		0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.maxVSize",do_maxVSize,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.maxNSize",do_maxNSize,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"split",	do_split,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, 0, 211,   3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	do_dynload,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dyn.unload",	do_dynunload,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		do_ls,		1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"typeof",	do_typeof,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"eval",	do_eval,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Exec",	do_tailcall,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"Tailcall",	do_tailcall,	1,	200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"returnValue",   do_returnValue,0,     11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"sys.parent",	do_sys,		1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.call",	do_sys,		2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frame",	do_sys,		3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.nframe",	do_sys,		4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.calls",	do_sys,		5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frames",	do_sys,		6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.on.exit",	do_sys,		7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parents",	do_sys,		8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.function",do_sys,		9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"traceback",	do_traceback,	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}},
{"browserText", do_sysbrowser,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserCondition", do_sysbrowser, 2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserSetDebug",  do_sysbrowser, 3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"parent.frame",do_parentframe,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sort",	do_sort,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.unsorted",	do_isunsorted,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, nullptr, DispatchType::INTERNAL},
{"sorted_fpass",do_sorted_fpass,0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"psort",	do_psort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	do_qsort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"radixsort",	do_radixsort,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"order",	do_order,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	do_rank,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"scan",	do_scan,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}},
{"t.default",	do_transpose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"aperm",	do_aperm,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"builtins",	do_builtins,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"args",	do_args,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"formals",	do_formals,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"body",	do_body,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bodyCode",	do_bodyCode,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environment",	do_envir,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environmentName",do_envirName,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"env2list",	do_env2list,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"reg.finalizer",do_regFinaliz,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"options",	do_options,	0,	211,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"getOption",	do_getOption,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sink",	do_sink,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sink.number",	do_sinknumber,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rapply",	do_rapply,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"islistfactor",do_islistfactor,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"colSums",	do_colsum,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"colMeans",	do_colsum,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowSums",	do_colsum,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowMeans",	do_colsum,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tracemem",    do_tracemem,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"},
{"retracemem",  do_retracemem,  0,      201,    -1,     {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}, "x"},
{"inspect",	do_inspect,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"address",     do_address,     0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"named",       do_named,       0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"refcnt",      do_refcnt,      0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"merge",	do_merge,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilities",do_capabilities,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilitiesX11",do_capabilitiesX11,0, 11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"new.env",	do_newenv,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env",  do_parentenv,   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env<-",do_parentenvgets, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}},
{"topenv",	do_topenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"l10n_info",	do_l10n_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Cstack_info", do_Cstack_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"mmap_file",	do_mmap_file,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"munmap_file",	do_munmap_file,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"wrap_meta",	do_wrap_meta,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tryWrap",	do_tryWrap,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"altrep_class",do_altrep_class, 0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	do_filecreate,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.remove",	do_fileremove,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.rename",	do_filerename,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.append",	do_fileappend,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.symlink",do_filesymlink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.link",	do_filelink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.copy",	do_filecopy,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", do_fileexists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.choose", do_filechoose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.info",	do_fileinfo,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.access",	do_fileaccess,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.exists",	do_direxists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.create",	do_dircreate,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tempfile",	do_tempfile,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tempdir",	do_tempdir,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"R.home",	do_Rhome,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"date",	do_date,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getenv",	do_getenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setenv",	do_setenv,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.unsetenv",do_unsetenv,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getwd",	do_getwd,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setwd",	do_setwd,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	do_basename,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dirname",	do_dirname,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.chmod",	do_syschmod,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.umask",	do_sysumask,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.readlink", do_readlink,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.info",	do_sysinfo,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",do_getlocale,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setlocale",do_setlocale,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.localeconv",do_localeconv,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"path.expand",	do_pathexpand,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getpid",	do_sysgetpid,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"normalizePath",do_normalizepath,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.glob",	do_glob,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}},
{"unlink",	do_unlink,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},

/* Complex Valued Functions */
{"polyroot",	do_polyroot,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Objects */
{"inherits",	do_inherits,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"UseMethod",	do_usemethod,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"NextMethod",	do_nextmethod,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"standardGeneric",do_standardGeneric,0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* date-time manipulations */
{"Sys.time",	do_systime,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXct",	do_asPOSIXct,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXlt",	do_asPOSIXlt,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format.POSIXlt",do_formatPOSIXlt,0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"strptime",	do_strptime,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Date2POSIXlt",do_D2POSIXlt,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"POSIXlt2Date",do_POSIXlt2D,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"balancePOSIXlt",do_balancePOSIXlt, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"unCfillPOSIXlt",do_balancePOSIXlt, 1,	 1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"compareNumericVersion",do_compareNumericVersion, 0, 11, 2, {PP_FUNCALL, PREC_FN,   0}},

{"mkCode",     do_mkcode,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
{"bcClose",    do_bcclose,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"is.builtin.internal", do_is_builtin_internal, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"disassemble", do_disassemble, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"bcVersion", do_bcversion,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}},
{"load.from.file", do_loadfile, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"save.to.file", do_savefile,   0,      11,     4,      {PP_FUNCALL, PREC_FN, 0}},
{"growconst", do_growconst,     0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"putconst", do_putconst,       0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"getconst", do_getconst,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
{"enableJIT",    do_enablejit,  0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"compilePKGS", do_compilepkgs, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

{"setNumMathThreads", do_setnumthreads,	      0, 11, 1, {PP_FUNCALL, PREC_FN, 0}},
{"setMaxNumMathThreads", do_setmaxnumthreads, 0, 11, 1, {PP_FUNCALL, PREC_FN, 0}},

/* Connections */
{"stdin",	do_stdin,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stdout",	do_stdout,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stderr",	do_stderr,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"isatty",	do_isatty,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"readLines",	do_readLines,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeLines",	do_writelines,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"readBin",	do_readbin,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeBin",	do_writebin,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"readChar",	do_readchar,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"writeChar",	do_writechar,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"open",	do_open,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"isOpen",	do_isopen,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"isIncomplete",do_isincomplete,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"isSeekable",	do_isseekable,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"close",	do_close,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	do_flush,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"file",	do_url,		1,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"url",		do_url,		0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"pipe",	do_pipe,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"fifo",	do_fifo,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"gzfile",	do_gzfile,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"bzfile",	do_gzfile,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"xzfile",	do_gzfile,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"zstdfile",	do_gzfile,	3,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"unz",		do_unz,		0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"seek",	do_seek,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"truncate",	do_truncate,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBack",	do_pushback,	0,     111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"clearPushBack",do_clearpushback,0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBackLength",do_pushbacklength,0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"rawConnection",do_rawconnection,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rawConnectionValue",do_rawconvalue,0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnection",do_textconnection,0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnectionValue",do_textconvalue,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"socketConnection",do_sockconn,0,	11,     8,      {PP_FUNCALL, PREC_FN,	0}},
{"socketAccept",do_sockconn,	1,	11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"sockSelect",do_sockselect,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"serverSocket",do_serversocket,0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"socketTimeout",do_socktimeout,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"getConnection",do_getconnection,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"getAllConnections",do_getallconnections,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}},
{"summary.connection",do_sumconnection,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"gzcon",	do_gzcon,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"memCompress",do_memCompress,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"memDecompress",do_memDecompress,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},


{"readDCF",	do_readDCF,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},


{"lockEnvironment", do_lockEnv,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}},
{"environmentIsLocked",	do_envIsLocked,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"lockBinding", do_lockBnd,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", do_bndIsLocked,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"makeActiveBinding", do_mkActiveBnd,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsActive", do_bndIsActive,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"activeBindingFunction", do_activeBndFun,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"mkUnbound",	do_mkUnbound,		0, 111,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"isNamespaceEnv",do_isNSEnv,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"registerNamespace",do_regNS,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"isRegisteredNamespace", do_getRegNS,	1, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",do_getNSRegistry, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}},
{"importIntoEnv",do_importIntoEnv, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"env.profile",  do_envprofile,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"Encoding",	do_encoding,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"setEncoding",	do_setencoding,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"setTimeLimit",do_setTimeLimit,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"setSessionTimeLimit",do_setSessionTimeLimit,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuGetCollate",do_ICUget,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"readRenviron",do_readEnviron,	0,      111,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"shortRowNames",do_shortRowNames,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"copyDFattr",do_copyDFattr,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredRoutines",do_getRegisteredRoutines,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getLoadedDLLs",do_getDllTable, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"getSymbolInfo",do_getSymbolInfo,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{".isMethodsDispatchOn",do_S4on,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",do_lazyLoadDBfetch,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBflush",do_lazyLoadDBflush,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getVarsFromFrame",do_getVarsFromFrame, 0, 11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"bincode",	do_bincode,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tabulate",	do_tabulate,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"findInterval",do_findinterval, 0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"pretty",	do_pretty,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"formatC",	do_formatC,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"crc64",	do_crc64,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseAnd",	do_bitwise,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseNot",	do_bitwise,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseOr",	do_bitwise,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseXor",	do_bitwise,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftL", do_bitwise,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftR",  do_bitwise,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serialize",	do_serialize,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeb",	do_serialize,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserialize",	do_serialize,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_matrix",do_rowsum,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_df",	do_rowsum,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"setS4Object",	do_setS4Object, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"traceOnOff",	do_traceOnOff,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"debugOnOff",	do_traceOnOff,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

// do_lapack : all in ---> ../modules/lapack/Lapack.c
{"La_qr_cmplx",	do_lapack,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack,	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",	do_lapack,	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack,	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack,	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon3",	do_lapack,	81,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zlange",	do_lapack,	61,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack,	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack,	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon3",	do_lapack,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve_cmplx",do_lapack,    11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve",	do_lapack,	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_qr",	do_lapack,	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack,	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack,	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack,	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack,	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack,	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack,	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmplx",	do_lapack,	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack,	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack,	401,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_version",	do_lapack,	1000,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"La_library",	do_lapack,	1001,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

{"bcprofcounts",do_bcprofcounts,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"bcprofstart",	do_bcprofstart,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"bcprofstop",	do_bcprofstop,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

{"eSoftVersion",do_eSoftVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"curlVersion", do_curlVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"curlGetHeaders",do_curlGetHeaders,0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"curlDownload",do_curlDownload, 0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"compilerVersion",do_compilerVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

// {NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}},
};


/* Table of special names.  These are marked as special with
   SET_SPECIAL_SYMBOL.  Environments on the function call stack that
   have never contained such a symbol are marked as such, so they can
   be quickly skipped when searching for a function named by such a
   special symbol.

   Any symbols can be put here, but ones that contain special
   characters, or are reserved words, are the ones unlikely to be
   defined in any environment other than base, and hence the ones
   where this is most likely to help.

   This is now also used for screening out syntactically special
   functions from use on the RHS of a pipe. If a
   non-syntactically-special symbol is added here it would neet to be
   explicitly allowed in the pipe code. */

const std::vector<std::string> Symbol::s_special_symbol_names = {
    "if", "while", "repeat", "for", "break", "next", "return", "function",
    "(", "{",
    "+", "-", "*", "/", "^", "%%", "%/%", "%*%", ":", "::", ":::", "?", "|>",
    "~", "@", "=>",
    "==", "!=", "<", ">", "<=", ">=",
    "&", "|", "&&", "||", "!",
    "<-", "<<-", "=",
    "$", "[", "[[",
    "$<-", "[<-", "[[<-"};


/* also used in eval.c */
attribute_hidden SEXP R::R_Primitive(const char *primname)
{
    unsigned int i = 0;
    for (const auto &el : R_FunTab)
    {
        if (streql(primname, el.name))
        { /* all names are ASCII */
            if ((el.m_eval % 100) / 10)
                return R_NilValue; /* it is a .Internal */
            else
                return mkPRIMSXP(i, el.m_eval % 10);
        }
        ++i;
    }
    return R_NilValue;
}

attribute_hidden SEXP do_primitive(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, prim;
    checkArity(op, args);
    name = CAR(args);
    if (!isString(name) || LENGTH(name) != 1 ||
	STRING_ELT(name, 0) == R_NilValue)
	errorcall(call, "%s", _("string argument required"));
    prim = R_Primitive(CHAR(STRING_ELT(name, 0)));
    if (prim == R_NilValue)
	errorcall(call, "%s", _("no such primitive function"));
    return prim;
}

attribute_hidden
int R::StrToInternal(const char *s)
{
    unsigned int i = 0;
    for (const auto &el : R_FunTab)
    {
        if (streql(s, el.name))
            return i;
        ++i;
    }
    return NA_INTEGER;
}

static void installFunTab()
{
    unsigned int i = 0;
    for (const auto &el : R_FunTab)
    {
        Symbol *sym = Symbol::obtain(el.name);
        /* mkPRIMSXP caches its results, thus prim does not need protection */
        SEXP prim = mkPRIMSXP(i, el.m_eval % 10);
        if ((el.m_eval % 100) / 10)
        {
            SET_INTERNAL(sym, prim);
        }
        else
        {
            SET_SYMVALUE(sym, prim);
        }
        ++i;
    }
}

static void SymbolShortcuts(void)
{  /* ../include/Rinternals.h : */
    R_Bracket2Symbol = install("[[");
    R_BracketSymbol = install("[");
    R_BraceSymbol = install("{");
    R_ClassSymbol = install("class");
    R_DeviceSymbol = install(".Device");
    R_DimNamesSymbol = install("dimnames");
    R_DimSymbol = install("dim");
    R_DollarSymbol = install("$");
    R_AtsignSymbol = install("@");
    R_DotsSymbol = install("...");
    R_DropSymbol = install("drop");
    R_EvalSymbol = install("eval");

    /* The last value symbol is used by the interpreter for recording
       the value of the most recently evaluated top level
       expression. To avoid creating an additional reference that
       would requires duplicating on modification this symbol does not
       increment reference counts on its symbol value.  This is safe
       since the symbol value corresponds to the base environment
       where complex assignments are not allowed.  */
    R_LastvalueSymbol = install(".Last.value");
    DISABLE_REFCNT(R_LastvalueSymbol);

    R_LevelsSymbol = install("levels");
    R_ModeSymbol = install("mode");
    R_NameSymbol  = install("name");
    R_NamesSymbol = install("names");
    R_NaRmSymbol = install("na.rm");
    R_PackageSymbol = install("package");
    R_PreviousSymbol = install("previous");
    R_QuoteSymbol = install("quote");
    R_RowNamesSymbol = install("row.names");
    R_SeedsSymbol = install(".Random.seed");
    R_SortListSymbol = install("sort.list");
    R_SourceSymbol = install("source");   /* Not used by R or core packages */
    R_TspSymbol = install("tsp");
    /* ../include/Defn.h , i.e. non-public : */
    R_CommentSymbol = install("comment");
    R_DotEnvSymbol = install(".Environment");
    R_ExactSymbol = install("exact");
    R_RecursiveSymbol = install("recursive");
    R_SrcfileSymbol = install("srcfile");
    R_SrcrefSymbol = install("srcref");
    R_WholeSrcrefSymbol = install("wholeSrcref");
    R_TmpvalSymbol = install("*tmp*");
    R_UseNamesSymbol = install("use.names");
    R_ColonSymbol = install(":");
    R_DoubleColonSymbol = install("::");
    R_TripleColonSymbol = install(":::");
    R_ConnIdSymbol = install("conn_id");
    R_DevicesSymbol = install(".Devices");
    // R_baseSymbol = // <- back compatible, "deprecated"
    R_BaseSymbol = install("base");
    R_SpecSymbol = install("spec");
    R_NamespaceEnvSymbol = install(".__NAMESPACE__.");
    R_AsCharacterSymbol = install("as.character");
    R_FunctionSymbol = install("function");

    R_dot_Generic = install(".Generic");
    R_dot_Method = install(".Method");
    R_dot_Methods = install(".Methods");
    R_dot_defined = install(".defined");
    R_dot_target = install(".target");
    R_dot_Group = install(".Group");
    R_dot_Class = install(".Class");
    R_dot_GenericCallEnv = install(".GenericCallEnv");
    R_dot_GenericDefEnv = install(".GenericDefEnv");
    R_dot_packageName = install(".packageName");
}

static void initializeDDVALSymbols(void)
{
    constexpr int N_DDVAL_SYMBOLS = 65;
    for (int i = 0; i < N_DDVAL_SYMBOLS; i++)
    {
        Symbol::obtainDotDotSymbol(i);
    }
}

attribute_hidden SEXP R::installDDVAL(int n)
{

    return Symbol::obtainDotDotSymbol(n);
}


static SEXP mkSymMarker(SEXP pname)
{
    SEXP ans = Symbol::create(pname, R_NilValue, R_NilValue);
    SET_SYMVALUE(ans, ans);

    return ans;
}

/* initialize the symbol table */
attribute_hidden void R::InitNames(void)
{
    /* String constants (CHARSXP values) */
    String::initialize(); // NA(), blank()

    /* Create marker values */
    R_UnboundValue = mkSymMarker(R_NilValue);
    R_MissingArg = mkSymMarker(String::obtain(""));
    R_InBCInterpreter = mkSymMarker(String::obtain("<in-bc-interp>"));
    R_RestartToken = mkSymMarker(String::obtain(""));
    R_CurrentExpression = mkSymMarker(String::obtain("<current-expression>"));

    R_print.na_string = NA_STRING;
    R_BlankScalarString = StringVector::createScalar(String::blank());
    MARK_NOT_MUTABLE(R_BlankScalarString);

    /* Set up a set of globals so that a symbol table search can be
       avoided when matching something like dim or dimnames. */
    SymbolShortcuts();

    /*  Builtin Functions */
    installFunTab();

    /* Special base functions */
    for (const auto &el : Symbol::s_special_symbol_names)
	SET_SPECIAL_SYMBOL(Symbol::obtain(el));

    R_initEvalSymbols();
    initializeDDVALSymbols();
    R_initialize_bcode();
    R_init_altrep();
}


/*  install - probe the symbol table */
/*  If "name" is not found, it is installed in the symbol table.
    The symbol corresponding to the string "name" is returned. */

Symbol *Symbol::obtain(/*const*/ String *charSXP)
{
    int len = LENGTH(charSXP);
    if (len == 0)
        error("%s", _("attempt to use zero-length variable name"));
    if (len > MAXIDSIZE)
        error(_("variable names are limited to %d bytes"), MAXIDSIZE);

    /* Check to see if the symbol is already present;  if it is, return it. */
    std::string name(CHAR(charSXP));
    auto found = Symbol::s_symbol_table.find(name);
    if (found != Symbol::s_symbol_table.end())
    {
        return found->second;
    }

    /* Create a new symbol node and link it into the table. */
    int hashcode = R_Newhashpjw(CHAR(charSXP));
    SET_HASHVALUE(charSXP, hashcode);
    SET_HASHASH(charSXP, 1);

    SEXP sym = R_NilValue;
    if (IS_ASCII(charSXP) || (IS_UTF8(charSXP) && utf8locale) ||
        (IS_LATIN1(charSXP) && latin1locale))
    {
        sym = mkSYMSXP(charSXP, R_UnboundValue);
    }
    else
    {
        /* This branch is to match behaviour of install (which is older):
           symbol C-string names are always interpreted as if
           in the native locale, even when they are not in the native locale */
        PROTECT(charSXP);
        sym = mkSYMSXP(String::obtain(name), R_UnboundValue);
        SET_HASHVALUE(PRINTNAME(sym), hashcode);
        SET_HASHASH(PRINTNAME(sym), 1);
        UNPROTECT(1);
    }

    auto [it, inserted] = Symbol::s_symbol_table.emplace(Symbol::Table::value_type(name, static_cast<Symbol *>(sym)));

    return it->second;
}

SEXP Rf_install(const char *name)
{
    if (!name)
        name = "";

    return Symbol::obtain(name);
}

/* This function is equivalent to install(CHAR(charSXP)), but faster.
   Like the equivalent code pattern, it discards the encoding information,
   hence in almost all cases installTrChar should be used, instead. */
attribute_hidden
SEXP R::installNoTrChar(SEXP charSXP)
{
    return Symbol::obtain(SEXP_downcast<String *>(charSXP));
}

attribute_hidden
SEXP R::installS3Signature(const char *className, const char *methodName) {

    return Symbol::obtainS3Signature(className, methodName);
}


/*  do_internal - This is the code for .Internal(). */

attribute_hidden SEXP do_internal(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, fun, ans;
    size_t save = R_PPStackTop;
    int flag;
    CXXR::RAllocStack::Scope rscope;

    checkArity(op, args);
    s = CAR(args);
    if (!isPairList(s))
	errorcall(call, "%s", _("invalid .Internal() argument"));
    fun = CAR(s);
    if (!isSymbol(fun))
	errorcall(call, "%s", _("invalid .Internal() argument"));
    if (INTERNAL(fun) == R_NilValue)
	errorcall(call, _("there is no .Internal function '%s'"),
		  CHAR(PRINTNAME(fun)));

#ifdef CHECK_INTERNALS
    if(R_Is_Running == RStatus::STARTED && getenv("_R_CHECK_INTERNALS2_")) {
	// find out if we were called from a namespace.
	// inlining by the compiler can defeat this.
	const char *ns = "";
	SEXP e = env;
	for (int i = 0; i < 10; i++) {
	    if(R_IsNamespaceEnv(e)) {
		ns = CHAR(STRING_ELT(R_NamespaceEnvSpec(e), 0));
		break;
	    }
	    e = ENCLOS(e);
	    if (isNull(e)) break;
	}
	const char *fn = CHAR(PRINTNAME(fun));
	// nspackloader.R contained a .Internal call, so need this
	// until all packages have been re-installed.
	if (!strlen(ns) && !streql(fn, "getRegisteredNamespace"))
	    errorcall(call,
		      _(".Internal(%s()) not called from a base namespace\n"), fn);
	if (strlen(ns)
	    && !streql(ns, "base") && !streql(ns, "tools")
	    && !streql(ns, "utils") && !streql(ns, "compiler"))
	    errorcall(call,
		      _(".Internal(%s()) called from namespace '%s'\n"), fn, ns);
    }
#endif


    args = CDR(s);
    if (TYPEOF(INTERNAL(fun)) == BUILTINSXP)
	args = evalList(args, env, call, 0);
    PROTECT(args);
    flag = PRIMPRINT(INTERNAL(fun).get());
    Evaluator::enableResultPrinting(flag != 1);
    ans = PRIMFUN(INTERNAL(fun).get()) (s, INTERNAL(fun), args, env);
    /* This resetting of Evaluator::enableResultPrinting(false) was to fix PR#7397,
       now fixed in GEText */
    if (flag < 2) Evaluator::enableResultPrinting(flag != 1);
#ifdef CHECK_VISIBILITY
    if(flag < 2 && Evaluator::resultPrinted() == flag) {
	char *nm = CHAR(PRINTNAME(fun));
	if(!streql(nm, "eval") && !streql(nm, "options") && !streql(nm, "Recall")
	   && !streql(nm, "do.call") && !streql(nm, "switch")
	   && !streql(nm, "recordGraphics") && !streql(nm, "writeBin")
	   && !streql(nm, "NextMethod"))
	    printf("vis: internal %s\n", nm);
    }
#endif
    UNPROTECT(1);
    check_stack_balance(INTERNAL(fun), save);

    return (ans);
}

	/* Internal code for the ~ operator */

attribute_hidden SEXP do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (isObject(call))
	return duplicate(call);
    else {
	SEXP klass;
	PROTECT(call = duplicate(call));
	PROTECT(klass = mkString("formula"));
	setAttrib(call, R_ClassSymbol, klass);
	setAttrib(call, R_DotEnvSymbol, rho);
	UNPROTECT(2);
	return call;
    }
}

/* For use in packages */
const char *R::getPRIMNAME(SEXP object)
{
    return PRIMNAME(object);
}
