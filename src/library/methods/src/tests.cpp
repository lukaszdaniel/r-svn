/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2005   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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

#include <CXXR/ProtectStack.hpp>
#include <R.h>
#include "methods.h"

SEXP R_methods_test_MAKE_CLASS(SEXP className)
{
  SEXP classNameChar = PROTECT(asChar(className));
  const char *class_;
  class_ = CHAR(classNameChar);
  SEXP res = R_do_MAKE_CLASS(class_);
  UNPROTECT(1);
  return res;
}

SEXP R_methods_test_NEW(SEXP className)
{
  SEXP classNameChar = PROTECT(asChar(className));
  const char *class_;
  class_ = CHAR(classNameChar);
  SEXP clDef;
  PROTECT(clDef = R_do_MAKE_CLASS(class_));
  SEXP res = R_do_new_object(clDef);
  UNPROTECT(2);
  return res;
}
