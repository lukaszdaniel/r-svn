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
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.r-project.org/Licenses/
 */

/** @file CXXR.hpp
 * @brief CXXR API.
 */

#ifndef CXXR_HPP
#define CXXR_HPP

// Global configuration
#include <CXXR/config.hpp>

// Basic CR types
#include <CXXR/SEXPTYPE.hpp>
#include <CXXR/RTypes.hpp>

// Memory Management
#include <CXXR/CellPool.hpp>
#include <CXXR/MemoryBank.hpp>
#include <CXXR/Allocator.hpp>
#include <CXXR/RAllocStack.hpp>

// Garbage Collection
#include <CXXR/GCManager.hpp>
#include <CXXR/NodeStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/BadObject.hpp>

// CXXR extentions
#include <CXXR/Complex.hpp>
#include <CXXR/Logical.hpp>
#include <CXXR/String.hpp>

// Various utilities
#include <CXXR/SEXP_downcast.hpp>
#include <CXXR/Browser.hpp>
#include <CXXR/strutil.hpp>

// Representation of CR's SEXPREC
#include <CXXR/GCNode.hpp>
#include <CXXR/RObject.hpp>
#include <CXXR/Evaluator.hpp>
#include <CXXR/StackChecker.hpp>

// Context management
#include <CXXR/RContext.hpp>
#include <CXXR/CommandTerminated.hpp>
#include <CXXR/JMPException.hpp>

// Generic Vector class
#include <CXXR/VectorBase.hpp>

// Specialized derived classes
#include <CXXR/FixedVector.hpp>
#include <CXXR/RawVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/LogicalVector.hpp>
#include <CXXR/ComplexVector.hpp>

// Specialized derived classes
#include <CXXR/GCEdge.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/StringVector.hpp>

// Primitives, Specials and Closures
#include <CXXR/FunctionBase.hpp>
#include <CXXR/BuiltInFunction.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/DotInternal.hpp>

// Lists
#include <CXXR/ConsCell.hpp>
#include <CXXR/ByteCode.hpp>
#include <CXXR/DottedArgs.hpp>
#include <CXXR/PairList.hpp>
#include <CXXR/Expression.hpp>

// Other Derived classes
#include <CXXR/Environment.hpp>
#include <CXXR/Promise.hpp>
#include <CXXR/S4Object.hpp>
#include <CXXR/Symbol.hpp>
#include <CXXR/WeakRef.hpp>
#include <CXXR/ExternalPointer.hpp>

#endif // CXXR_HPP
