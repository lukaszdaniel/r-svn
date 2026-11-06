/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2025   The R Core Team.
 *  Copyright (C) 2004-2017   The R Foundation
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *
 *
 *  Symbolic Differentiation
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/Closure.hpp>
#include <CXXR/ExpressionVector.hpp>
#include <Defn.h>  // for deparse1
#include "localization.h"
#include "statsErr.h"

using namespace R;
using namespace CXXR;

static SEXP ParenSymbol;
static SEXP PlusSymbol;
static SEXP MinusSymbol;
static SEXP TimesSymbol;
static SEXP DivideSymbol;
static SEXP PowerSymbol;
static SEXP ExpSymbol;
static SEXP LogSymbol;
static SEXP SinSymbol;
static SEXP CosSymbol;
static SEXP TanSymbol;
static SEXP SinhSymbol;
static SEXP CoshSymbol;
static SEXP TanhSymbol;
static SEXP SqrtSymbol;
static SEXP PnormSymbol;
static SEXP DnormSymbol;
static SEXP AsinSymbol;
static SEXP AcosSymbol;
static SEXP AtanSymbol;
static SEXP GammaSymbol;
static SEXP LGammaSymbol;
static SEXP DiGammaSymbol;
static SEXP TriGammaSymbol;
static SEXP PsiSymbol;
/* new symbols in R 3.4.0: */
static SEXP PiSymbol;
static SEXP ExpM1Symbol;
static SEXP Log1PSymbol;
static SEXP Log2Symbol;
static SEXP Log10Symbol;
static SEXP SinPiSymbol;
static SEXP CosPiSymbol;
static SEXP TanPiSymbol;
static SEXP FactorialSymbol;
static SEXP LFactorialSymbol;
/* possible future symbols
static SEXP Log1PExpSymbol;
static SEXP Log1MExpSymbol;
static SEXP Log1PMxSymbol;
*/

static void InitDerivSymbols(void)
{
    static bool s_Initialized = false;
    /* Called from doD() and deriv() */
    if(s_Initialized) return;
    ParenSymbol = install("(");
    PlusSymbol = install("+");
    MinusSymbol = install("-");
    TimesSymbol = install("*");
    DivideSymbol = install("/");
    PowerSymbol = install("^");
    ExpSymbol = install("exp");
    LogSymbol = install("log");
    SinSymbol = install("sin");
    CosSymbol = install("cos");
    TanSymbol = install("tan");
    SinhSymbol = install("sinh");
    CoshSymbol = install("cosh");
    TanhSymbol = install("tanh");
    SqrtSymbol = install("sqrt");
    PnormSymbol = install("pnorm");
    DnormSymbol = install("dnorm");
    AsinSymbol = install("asin");
    AcosSymbol = install("acos");
    AtanSymbol = install("atan");
    GammaSymbol = install("gamma");
    LGammaSymbol = install("lgamma");
    DiGammaSymbol = install("digamma");
    TriGammaSymbol = install("trigamma");
    PsiSymbol = install("psigamma");
/* new symbols */
    PiSymbol = install("pi");
    ExpM1Symbol = install("expm1");
    Log1PSymbol = install("log1p");
    Log2Symbol = install("log2");
    Log10Symbol = install("log10");
    SinPiSymbol = install("sinpi");
    CosPiSymbol = install("cospi");
    TanPiSymbol = install("tanpi");
    FactorialSymbol = install("factorial");
    LFactorialSymbol = install("lfactorial");
/* possible future symbols
    Log1PExpSymbol = install("log1pexp");    # log(1+exp(x))
    Log1MExpSymbol = install("log1mexp");    # log(1-exp(-x)), for x > 0
    Log1PMxSymbol = install("log1pmx");      # log1p(x)-x
*/

    s_Initialized = true;
}

static SEXP Constant(double x)
{
    return ScalarReal(x);
}

static int isZero(SEXP s)
{
    return asReal(s) == 0.0;
}

static int isOne(SEXP s)
{
    return asReal(s) == 1.0;
}

static int isUminus(SEXP s)
{
    if (TYPEOF(s) == LANGSXP && CAR(s) == MinusSymbol) {
	switch(length(s)) {
	case 2:
	    return 1;
	case 3:
	    if (CADDR(s) == R_MissingArg)
		return 1;
	    else return 0;
	default:
	    error("%s", _("invalid form in unary minus check"));
	    return -1;/* for -Wall */
	}
    }
    else return 0;
}

static SEXP simplify(SEXP fun, SEXP arg1, SEXP arg2)
{
    SEXP ans;
    if (fun == PlusSymbol) {
	if (isZero(arg1))
	    ans = arg2;
	else if (isZero(arg2))
	    ans = arg1;
	else if (isUminus(arg1))
	    ans = simplify(MinusSymbol, arg2, CADR(arg1));
	else if (isUminus(arg2))
	    ans = simplify(MinusSymbol, arg1, CADR(arg2));
	else
	    ans = lang3(PlusSymbol, arg1, arg2);
    }
    else if (fun == MinusSymbol) {
	if (arg2 == R_MissingArg) {
	    if (isZero(arg1))
		ans = Constant(0.);
	    else if (isUminus(arg1))
		ans = CADR(arg1);
	    else
		ans = lang2(MinusSymbol, arg1);
	}
	else {
	    if (isZero(arg2))
		ans = arg1;
	    else if (isZero(arg1))
		ans = simplify(MinusSymbol, arg2, R_MissingArg);
	    else if (isUminus(arg1)) {
		GCStackRoot<> tmp;
		tmp = simplify(PlusSymbol, CADR(arg1), arg2);
		ans = simplify(MinusSymbol, tmp, R_MissingArg);
	    }
	    else if (isUminus(arg2))
		ans = simplify(PlusSymbol, arg1, CADR(arg2));
	    else
		ans = lang3(MinusSymbol, arg1, arg2);
	}
    }
    else if (fun == TimesSymbol) {
	if (isZero(arg1) || isZero(arg2))
	    ans = Constant(0.);
	else if (isOne(arg1))
	    ans = arg2;
	else if (isOne(arg2))
	    ans = arg1;
	else if (isUminus(arg1)) {
	    GCStackRoot<> tmp;
	    tmp = simplify(TimesSymbol, CADR(arg1), arg2);
	    ans = simplify(MinusSymbol, tmp, R_MissingArg);
	}
	else if (isUminus(arg2)) {
	    GCStackRoot<> tmp;
	    tmp = simplify(TimesSymbol, arg1, CADR(arg2));
	    ans = simplify(MinusSymbol, tmp, R_MissingArg);
	}
	else
	    ans = lang3(TimesSymbol, arg1, arg2);
    }
    else if (fun == DivideSymbol) {
	if (isZero(arg1))
	    ans = Constant(0.);
	else if (isZero(arg2))
	    ans = Constant(NA_REAL);
	else if (isOne(arg2))
	    ans = arg1;
	else if (isUminus(arg1)) {
	    GCStackRoot<> tmp;
	    tmp = simplify(DivideSymbol, CADR(arg1), arg2);
	    ans = simplify(MinusSymbol, tmp, R_MissingArg);
	}
	else if (isUminus(arg2)) {
	    GCStackRoot<> tmp;
	    tmp = simplify(DivideSymbol, arg1, CADR(arg2));
	    ans = simplify(MinusSymbol, tmp, R_MissingArg);
	}
	else ans = lang3(DivideSymbol, arg1, arg2);
    }
    else if (fun == PowerSymbol) {
	if (isZero(arg2))
	    ans = Constant(1.);
	else if (isZero(arg1))
	    ans = Constant(0.);
	else if (isOne(arg1))
	    ans = Constant(1.);
	else if (isOne(arg2))
	    ans = arg1;
	else
	    ans = lang3(PowerSymbol, arg1, arg2);
    }
    else if (fun == ExpSymbol) {
        /* FIXME: simplify exp(lgamma( E )) = gamma( E ) */
        /* FIXME: simplify exp(lfactorial( E )) = factorial( E ) */
        ans = lang2(ExpSymbol, arg1);
    }
    else if (fun == LogSymbol) {
        /* FIXME: simplify log(gamma( E )) = lgamma( E ) */
        /* FIXME: simplify log(factorial( E )) = lfactorial( E ) */
        ans = lang2(LogSymbol, arg1);
    }
    else if (fun == CosSymbol)  ans = lang2(CosSymbol, arg1);
    else if (fun == SinSymbol)  ans = lang2(SinSymbol, arg1);
    else if (fun == TanSymbol)  ans = lang2(TanSymbol, arg1);
    else if (fun == CoshSymbol) ans = lang2(CoshSymbol, arg1);
    else if (fun == SinhSymbol) ans = lang2(SinhSymbol, arg1);
    else if (fun == TanhSymbol) ans = lang2(TanhSymbol, arg1);
    else if (fun == SqrtSymbol) ans = lang2(SqrtSymbol, arg1);
    else if (fun == PnormSymbol)ans = lang2(PnormSymbol, arg1);
    else if (fun == DnormSymbol)ans = lang2(DnormSymbol, arg1);
    else if (fun == AsinSymbol) ans = lang2(AsinSymbol, arg1);
    else if (fun == AcosSymbol) ans = lang2(AcosSymbol, arg1);
    else if (fun == AtanSymbol) ans = lang2(AtanSymbol, arg1);
    else if (fun == GammaSymbol)ans = lang2(GammaSymbol, arg1);
    else if (fun == LGammaSymbol)ans = lang2(LGammaSymbol, arg1);
    else if (fun == DiGammaSymbol) ans = lang2(DiGammaSymbol, arg1);
    else if (fun == TriGammaSymbol) ans = lang2(TriGammaSymbol, arg1);
    else if (fun == PsiSymbol){
       if (arg2 == R_MissingArg) ans = lang2(PsiSymbol, arg1);
       else ans = lang3(PsiSymbol, arg1, arg2);
    }
/* new symbols */
    else if (fun == ExpM1Symbol) {
        /* FIXME: simplify expm1(log1p( E )) = E */
        ans = lang2(ExpM1Symbol, arg1);
    }
    else if (fun == LogSymbol) {
        /* FIXME: simplify log1p(expm1( E )) = E */
        ans = lang2(Log1PSymbol, arg1);
    }
    else if (fun == Log2Symbol) ans = lang2(Log2Symbol, arg1);
    else if (fun == Log10Symbol) ans = lang2(Log10Symbol, arg1);
    else if (fun == CosPiSymbol) ans = lang2(CosPiSymbol, arg1);
    else if (fun == SinPiSymbol) ans = lang2(SinPiSymbol, arg1);
    else if (fun == TanPiSymbol) ans = lang2(TanPiSymbol, arg1);
    else if (fun == FactorialSymbol)ans = lang2(FactorialSymbol, arg1);
    else if (fun == LFactorialSymbol)ans = lang2(LFactorialSymbol, arg1);
/* possible future symbols
    else if (fun == Log1PExpSymbol) ans = lang2(Log1PExpSymbol, arg1);
    else if (fun == Log1MExpSymbol) ans = lang2(Log1MExpSymbol, arg1);
    else if (fun == Log1PMxSymbol) ans = lang2(Log1PMxSymbol, arg1);
*/

    else ans = Constant(NA_REAL);
    /* FIXME */
#ifdef NOTYET
    if (length(ans) == 2 && isAtomic(CADR(ans)) && CAR(ans) != MinusSymbol)
	c = eval(c, rho);
    if (length(c) == 3 && isAtomic(CADR(ans)) && isAtomic(CADDR(ans)))
	c = eval(c, rho);
#endif
    return ans;
}/* simplify() */


/* D() implements the "derivative table" : */
static SEXP D(SEXP expr, SEXP var)
{
    CXXR::RAllocStack::Scope rscope; // translateChar


    SEXP ans = R_NilValue;
    switch(TYPEOF(expr)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	ans = Constant(0);
	break;
    case SYMSXP:
	if (expr == var) ans = Constant(1.);
	else ans = Constant(0.);
	break;
    case LISTSXP:
	if (inherits(expr, "expression")) ans = D(CAR(expr), var);
	else ans = Constant(NA_REAL);
	break;
    case LANGSXP:
	if (CAR(expr) == ParenSymbol) {
	    ans = D(CADR(expr), var);
	}
	else if (CAR(expr) == PlusSymbol) {
	    if (length(expr) == 2)
		ans = D(CADR(expr), var);
	    else {
		GCStackRoot<> derivative_1(D(CADR(expr), var));
		GCStackRoot<> derivative_2(D(CADDR(expr), var));
		ans = simplify(PlusSymbol,
			       derivative_1,
			       derivative_2);
	    }
	}
	else if (CAR(expr) == MinusSymbol) {
	    if (length(expr) == 2) {
		GCStackRoot<> derivative(D(CADR(expr), var));
		ans = simplify(MinusSymbol,
			       derivative,
			       R_MissingArg);
	    }
	    else {
		GCStackRoot<> derivative_1(D(CADR(expr), var));
		GCStackRoot<> derivative_2(D(CADDR(expr), var));
		ans = simplify(MinusSymbol,
			       derivative_1,
			       derivative_2);
	    }
	}
	else if (CAR(expr) == TimesSymbol) {
	    GCStackRoot<> derivative_1(D(CADR(expr),var));
	    GCStackRoot<> tmp1(simplify(TimesSymbol, derivative_1, CADDR(expr)));
	    GCStackRoot<> derivative_2(D(CADDR(expr),var));
	    GCStackRoot<> tmp2(simplify(TimesSymbol, CADR(expr), derivative_2));
	    ans = simplify(PlusSymbol,
			   tmp1,
			   tmp2);
	}
	else if (CAR(expr) == DivideSymbol) {
	    GCStackRoot<> expr1, expr2;
	    expr1 = D(CADR(expr), var);
	    expr2 = D(CADDR(expr), var);
	    GCStackRoot<> tmp1(simplify(DivideSymbol, expr1, CADDR(expr)));
	    GCStackRoot<> times_2(simplify(TimesSymbol, CADR(expr), expr2));
	    GCStackRoot<> two(Constant(2.));
	    GCStackRoot<> power(simplify(PowerSymbol, CADDR(expr), two));
	    GCStackRoot<> tmp2(simplify(DivideSymbol, times_2, power));
	    ans = simplify(MinusSymbol,
			   tmp1,
			   tmp2);
	}
	else if (CAR(expr) == PowerSymbol) {
	    if (isLogical(CADDR(expr)) || isNumeric(CADDR(expr))) {
		GCStackRoot<> derivative(D(CADR(expr), var));
		GCStackRoot<> negative_one(Constant(asReal(CADDR(expr))-1.));
		GCStackRoot<> power(simplify(PowerSymbol, CADR(expr), negative_one));
		GCStackRoot<> tmp(simplify(TimesSymbol, derivative, power));
		ans = simplify(TimesSymbol,
			       CADDR(expr),
			       tmp);
	    }
	    else {
		GCStackRoot<> expr1, expr2;
		GCStackRoot<> one(Constant(1.0));
		GCStackRoot<> tmp1(simplify(MinusSymbol, CADDR(expr), one));
		GCStackRoot<> power(simplify(PowerSymbol, CADR(expr), tmp1));
		GCStackRoot<> derivative(D(CADR(expr), var));
		GCStackRoot<> tmp2(simplify(TimesSymbol, CADDR(expr), derivative));
		expr1 = simplify(TimesSymbol,
				 power,
				 tmp2);
		GCStackRoot<> power_2(simplify(PowerSymbol, CADR(expr), CADDR(expr)));
		GCStackRoot<> log(simplify(LogSymbol, CADR(expr), R_MissingArg));
		GCStackRoot<> tmp3(D(CADDR(expr), var));
		GCStackRoot<> tmp4(simplify(TimesSymbol, log, tmp3));
		expr2 = simplify(TimesSymbol,
				 power_2,
				 tmp4);
		ans = simplify(PlusSymbol, expr1, expr2);
	    }
	}
	else if (CAR(expr) == ExpSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(TimesSymbol,
			   expr,
			   derivative);
	}
	else if (CAR(expr) == LogSymbol) {
	    if (length(expr) != 2)
		error("%s", _("only single-argument calls to log() are supported;\n  maybe use log(x,a) = log(x)/log(a)"));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(DivideSymbol,
			   derivative,
			   CADR(expr));
	}
	else if (CAR(expr) == CosSymbol) {
	    GCStackRoot<> tmp1(simplify(SinSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> tmp2(simplify(MinusSymbol, derivative, R_MissingArg));
	    ans = simplify(TimesSymbol,
			   tmp1,
			   tmp2);
	}
	else if (CAR(expr) == SinSymbol) {
	    GCStackRoot<> cos(simplify(CosSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(TimesSymbol,
			   cos,
			   derivative);
	}
	else if (CAR(expr) == TanSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> cos(simplify(CosSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> two(Constant(2.0));
	    GCStackRoot<> power(simplify(PowerSymbol, cos, two));
	    ans = simplify(DivideSymbol,
			   derivative,
			   power);
	}
	else if (CAR(expr) == CoshSymbol) {
	    GCStackRoot<> sinh(simplify(SinhSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(TimesSymbol,
			   sinh,
			   derivative);
	}
	else if (CAR(expr) == SinhSymbol) {
	    GCStackRoot<> cosh(simplify(CoshSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(TimesSymbol,
			   cosh,
			   derivative);
	}
	else if (CAR(expr) == TanhSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> cosh(simplify(CoshSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> two(Constant(2.0));
	    GCStackRoot<> power(simplify(PowerSymbol, cosh, two));
	    ans = simplify(DivideSymbol,
			   derivative,
			   power);
	}
	else if (CAR(expr) == SqrtSymbol) {
	    GCStackRoot<> expr1;
	    expr1 = allocLang(3);
	    SETCAR(expr1, PowerSymbol);
	    SETCADR(expr1, CADR(expr));
	    SETCADDR(expr1, Constant(0.5));
	    ans = D(expr1, var);
	}
	else if (CAR(expr) == PnormSymbol) {
	    GCStackRoot<> norm(simplify(DnormSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    ans = simplify(TimesSymbol,
			   norm,
			   derivative);
	}
	else if (CAR(expr) == DnormSymbol) {
	    GCStackRoot<> tmp1(simplify(MinusSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> norm(simplify(DnormSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> tmp2(simplify(TimesSymbol, norm, derivative));
	    ans = simplify(TimesSymbol,
			   tmp1,
			   tmp2);
	}
	else if (CAR(expr) == AsinSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> one(Constant(1.));
	    GCStackRoot<> two(Constant(2.));
	    GCStackRoot<> power(simplify(PowerSymbol, CADR(expr), two));
	    GCStackRoot<> tmp1(simplify(MinusSymbol, one, power));
	    GCStackRoot<> tmp2(simplify(SqrtSymbol, tmp1, R_MissingArg));
	    ans = simplify(DivideSymbol,
			   derivative,
			   tmp2);
	}
	else if (CAR(expr) == AcosSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> one(Constant(1.));
	    GCStackRoot<> two(Constant(2.));
	    GCStackRoot<> power(simplify(PowerSymbol, CADR(expr), two));
	    GCStackRoot<> tmp1(simplify(MinusSymbol, one, power));
	    GCStackRoot<> sqrt(simplify(SqrtSymbol, tmp1, R_MissingArg));
	    GCStackRoot<> tmp2(simplify(DivideSymbol, derivative, sqrt));
	    ans = simplify(MinusSymbol,
			   tmp2, R_MissingArg);
	}
	else if (CAR(expr) == AtanSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> one(Constant(1.));
	    GCStackRoot<> two(Constant(2.));
	    GCStackRoot<> power(simplify(PowerSymbol, CADR(expr), two));
	    GCStackRoot<> denominator(simplify(PlusSymbol, one, power));
	    ans = simplify(DivideSymbol,
			   derivative,
			   denominator);
	}
	else if (CAR(expr) == LGammaSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> digamma(simplify(DiGammaSymbol, CADR(expr), R_MissingArg));
	    ans = simplify(TimesSymbol,
			   derivative,
			   digamma);
	}
	else if (CAR(expr) == GammaSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> digamma(simplify(DiGammaSymbol, CADR(expr), R_MissingArg));
	    GCStackRoot<> tmp(simplify(TimesSymbol, expr, digamma));
	    ans = simplify(TimesSymbol,
			   derivative,
			   tmp);
	}
	else if (CAR(expr) == DiGammaSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> trigamma(simplify(TriGammaSymbol, CADR(expr), R_MissingArg));
	    ans = simplify(TimesSymbol,
			   derivative,
			   trigamma);
	}
	else if (CAR(expr) == TriGammaSymbol) {
	    GCStackRoot<> derivative(D(CADR(expr), var));
	    GCStackRoot<> two(ScalarInteger(2));
	    GCStackRoot<> psi(simplify(PsiSymbol, CADR(expr), two));
	    ans = simplify(TimesSymbol,
			   derivative,
			   psi);
	}
	else if (CAR(expr) == PsiSymbol) {
	    if (length(expr) == 2){
		GCStackRoot<> derivative(D(CADR(expr), var));
		GCStackRoot<> one(ScalarInteger(1));
		GCStackRoot<> psi(simplify(PsiSymbol, CADR(expr), one));
		ans = simplify(TimesSymbol,
			       derivative,
			       psi);
	    } else if (TYPEOF(CADDR(expr)) == INTSXP ||
		       TYPEOF(CADDR(expr)) == REALSXP) {
		GCStackRoot<> derivative(D(CADR(expr), var));
		GCStackRoot<> tmp(ScalarInteger(asInteger(CADDR(expr))+1));
		GCStackRoot<> psi(simplify(PsiSymbol, CADR(expr), tmp));
		ans = simplify(TimesSymbol,
			       derivative,
			       psi);
	    } else {
		GCStackRoot<> derivative(D(CADR(expr), var));
		GCStackRoot<> one(ScalarInteger(1));
		GCStackRoot<> psi(simplify(PsiSymbol, CADR(expr), simplify(PlusSymbol, CADDR(expr),
					     one)));
		ans = simplify(TimesSymbol,
			       derivative,
			       psi);
	    }
	}
/* new in R 3.4.0 */
        else if (CAR(expr) == ExpM1Symbol) {
            GCStackRoot<> exp(simplify(ExpSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> derivative(D(CADR(expr), var));
            ans = simplify(TimesSymbol,
			   exp,
                           derivative);
        }
        else if (CAR(expr) == Log1PSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> one(Constant(1.));
            GCStackRoot<> tmp(simplify(PlusSymbol, one, CADR(expr)));
            ans = simplify(DivideSymbol,
                           derivative,
                           tmp);
        }
        else if (CAR(expr) == Log2Symbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> two(Constant(2.));
            GCStackRoot<> log(simplify(LogSymbol, two, R_MissingArg));
            GCStackRoot<> tmp(simplify(TimesSymbol, CADR(expr), log));
            ans = simplify(DivideSymbol,
                           derivative,
                           tmp);
        }
        else if (CAR(expr) == Log10Symbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> ten(Constant(10.));
            GCStackRoot<> log(simplify(LogSymbol, ten, R_MissingArg));
            GCStackRoot<> tmp(simplify(TimesSymbol, CADR(expr), log));
            ans = simplify(DivideSymbol,
                           derivative,
                           tmp);
        }
        else if (CAR(expr) == CosPiSymbol) {
            GCStackRoot<> sin(simplify(SinPiSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> tmp(simplify(MinusSymbol, PiSymbol, R_MissingArg));
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> tmp2(simplify(TimesSymbol, tmp, derivative));
            ans = simplify(TimesSymbol,
                           sin,
                           tmp2);
        }
        else if (CAR(expr) == SinPiSymbol) {
            GCStackRoot<> cos(simplify(CosPiSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> tmp(simplify(TimesSymbol, PiSymbol, derivative));
            ans = simplify(TimesSymbol,
                           cos,
                           tmp);
        }
        else if (CAR(expr) == TanPiSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> tmp(simplify(TimesSymbol, PiSymbol, derivative));
            GCStackRoot<> cos(simplify(CosPiSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> two(Constant(2.0));
            GCStackRoot<> power(simplify(PowerSymbol, cos, two));
            ans = simplify(DivideSymbol,
                           tmp,
			   power);
        }
        else if (CAR(expr) == LFactorialSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> one(ScalarInteger(1));
            GCStackRoot<> tmp(simplify(PlusSymbol, CADR(expr), one));
            GCStackRoot<> digamma(simplify(DiGammaSymbol, tmp, R_MissingArg));
            ans = simplify(TimesSymbol,
                           derivative,
                           digamma);
        }
        else if (CAR(expr) == FactorialSymbol) {
            GCStackRoot<> tmp1(D(CADR(expr), var));
            GCStackRoot<> one(ScalarInteger(1));
            GCStackRoot<> tmp2(simplify(PlusSymbol, CADR(expr), one));
            GCStackRoot<> digamma(simplify(DiGammaSymbol, tmp2, R_MissingArg));
            GCStackRoot<> tmp3(simplify(TimesSymbol, expr, digamma));
            ans = simplify(TimesSymbol,
                           tmp1,
                           tmp3);
        }
/* possible future symbols
        else if (CAR(expr) == Log1PExpSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> exp_x(simplify(ExpSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> numerator(simplify(TimesSymbol, derivative, exp_x));
            GCStackRoot<> one(Constant(1.));
            GCStackRoot<> denominator(simplify(PlusSymbol, one, exp_x));
            ans = simplify(DivideSymbol,
                           numerator,
                           denominator);
        }
        else if (CAR(expr) == Log1MExpSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> negative_derivative(simplify(MinusSymbol, derivative, R_MissingArg));
            GCStackRoot<> negative_x(simplify(MinusSymbol, CADR(expr), R_MissingArg));
            GCStackRoot<> exp_negative_x(simplify(ExpSymbol, negative_x, R_MissingArg));
            GCStackRoot<> numerator(simplify(TimesSymbol, negative_derivative, exp_negative_x));
            GCStackRoot<> denominator(simplify(ExpM1Symbol, negative_x, R_MissingArg));
            ans = simplify(DivideSymbol,
                           numerator,
                           denominator);
        }
        else if (CAR(expr) == Log1PMxSymbol) {
            GCStackRoot<> derivative(D(CADR(expr), var));
            GCStackRoot<> negative_derivative(simplify(MinusSymbol, derivative, R_MissingArg));
            GCStackRoot<> one(Constant(1.));
            GCStackRoot<> denominator(simplify(PlusSymbol, one, CADR(expr)));
            ans = simplify(DivideSymbol,
                           negative_derivative,
                           denominator);
        }
*/

	else {
	    SEXP u = deparse1(CAR(expr), FALSE, SIMPLEDEPARSE);
	    error(_("Function '%s' is not in the derivatives table"),
		  translateChar(STRING_ELT(u, 0)));
	}

	break;
    default:
	ans = Constant(NA_REAL);
    }
    return ans;


} /* D() */

static int isPlusForm(SEXP expr)
{
    return TYPEOF(expr) == LANGSXP
	&& length(expr) == 3
	&& CAR(expr) == PlusSymbol;
}

static int isMinusForm(SEXP expr)
{
    return TYPEOF(expr) == LANGSXP
	&& length(expr) == 3
	&& CAR(expr) == MinusSymbol;
}

static int isTimesForm(SEXP expr)
{
    return TYPEOF(expr) == LANGSXP
	&& length(expr) == 3
	&& CAR(expr) == TimesSymbol;
}

static int isDivideForm(SEXP expr)
{
    return TYPEOF(expr) == LANGSXP
	&& length(expr) == 3
	&& CAR(expr) == DivideSymbol;
}

static int isPowerForm(SEXP expr)
{
    return (TYPEOF(expr) == LANGSXP
	    && length(expr) == 3
	    && CAR(expr) == PowerSymbol);
}

static SEXP AddParens(SEXP expr)
{
    SEXP e;
    if (TYPEOF(expr) == LANGSXP) {
	e = CDR(expr);
	while(e != R_NilValue) {
	    SETCAR(e, AddParens(CAR(e)));
	    e = CDR(e);
	}
    }
    if (isPlusForm(expr)) {
	if (isPlusForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    else if (isMinusForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    else if (isTimesForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
	if (isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
    }
    else if (isDivideForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
	if (isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
    }
    else if (isPowerForm(expr)) {
	if (isPowerForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    return expr;
}

SEXP doD(SEXP args)
{
    args = CDR(args);
    SEXP expr;
    if (isExpression(CAR(args))) expr = XVECTOR_ELT(CAR(args), 0);
    else expr = CAR(args);
    if (!(isLanguage(expr) || isSymbol(expr) || isNumeric(expr) || isComplex(expr)))
        error(_("expression must not be type '%s'"), R_typeToChar(expr));
    SEXP var = CADR(args);
    if (!isString(var) || length(var) < 1)
	error("%s", _("variable must be a character string"));
    if (length(var) > 1)
	warning("%s", _("only the first element is used as variable name"));
    var = installTrChar(STRING_ELT(var, 0));
    InitDerivSymbols();
    PROTECT(expr = D(expr, var));
    expr = AddParens(expr);
    UNPROTECT(1);
    return expr;
}

/* ------ FindSubexprs ------ and ------ Accumulate ------ */

NORET static void InvalidExpression(const char *where)
{
    error(_("invalid expression in '%s'"), where);
}

static int equal(SEXP expr1, SEXP expr2)
{
    if (TYPEOF(expr1) == TYPEOF(expr2)) {
	switch(TYPEOF(expr1)) {
	case NILSXP:
	    return 1;
	case SYMSXP:
	    return expr1 == expr2;
	case LGLSXP:
	    return LOGICAL(expr1)[0] == LOGICAL(expr2)[0];
	case INTSXP:
	    return INTEGER(expr1)[0] == INTEGER(expr2)[0];
	case REALSXP:
	    return REAL(expr1)[0] == REAL(expr2)[0];
	case CPLXSXP:
	    return COMPLEX(expr1)[0].r == COMPLEX(expr2)[0].r
		&& COMPLEX(expr1)[0].i == COMPLEX(expr2)[0].i;
	case LANGSXP:
	case LISTSXP:
	    return equal(CAR(expr1), CAR(expr2))
		&& equal(CDR(expr1), CDR(expr2));
	default:
	    InvalidExpression("equal");
	}
    }
    return 0;
}

static int Accumulate(SEXP expr, SEXP exprlist)
{
    SEXP e = exprlist;
    int k = 0;
    while(CDR(e) != R_NilValue) {
	e = CDR(e);
	k = k + 1;
	if (equal(expr, CAR(e)))
	    return k;
    }
    SETCDR(e, CONS(expr, R_NilValue));
    return k + 1;
}

static int Accumulate2(SEXP expr, SEXP exprlist)
{
    SEXP e = exprlist;
    int k = 0;
    while(CDR(e) != R_NilValue) {
	e = CDR(e);
	k = k + 1;
    }
    SETCDR(e, CONS(expr, R_NilValue));
    return k + 1;
}

static SEXP MakeVariable(int k, const char *tag)
{
    char buf[64];
    int res = snprintf(buf, 64, "%s%d", tag, k);
    if (res >= 64)
	error("%s", _("too many variables"));
    return install(buf);
}

static int FindSubexprs(SEXP expr, SEXP exprlist, const char *tag)
{
    SEXP e;
    int k;
    switch(TYPEOF(expr)) {
    case SYMSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	return 0;
	break;
    case LISTSXP:
	if (inherits(expr, "expression"))
	    return FindSubexprs(CAR(expr), exprlist, tag);
	else { InvalidExpression("FindSubexprs"); return -1/*-Wall*/; }
	break;
    case LANGSXP:
	if (CAR(expr) == install("(")) {
	    return FindSubexprs(CADR(expr), exprlist, tag);
	}
	else {
	    e = CDR(expr);
	    while(e != R_NilValue) {
		if ((k = FindSubexprs(CAR(e), exprlist, tag)) != 0)
		    SETCAR(e, MakeVariable(k, tag));
		e = CDR(e);
	    }
	    return Accumulate(expr, exprlist);
	}
	break;
    default:
	InvalidExpression("FindSubexprs");
	return -1/*-Wall*/;
    }
}

static int CountOccurrences(SEXP sym, SEXP lst)
{
    switch(TYPEOF(lst)) {
    case SYMSXP:
	return lst == sym;
    case LISTSXP:
    case LANGSXP:
	return CountOccurrences(sym, CAR(lst))
	    + CountOccurrences(sym, CDR(lst));
    default:
	return 0;
    }
}

static SEXP Replace(SEXP sym, SEXP expr, SEXP lst)
{
    switch(TYPEOF(lst)) {
    case SYMSXP:
	if (lst == sym) return expr;
	else return lst;
    case LISTSXP:
    case LANGSXP:
	SETCAR(lst, Replace(sym, expr, CAR(lst)));
	SETCDR(lst, Replace(sym, expr, CDR(lst)));
	return lst;
    default:
	return lst;
    }
}

static SEXP CreateGrad(SEXP names)
{
    SEXP q, data, dim, dimnames;
    int n = length(names);
    PROTECT(dimnames = lang3(R_NilValue, R_NilValue, R_NilValue));
    SETCAR(dimnames, install("list"));
    SEXP p = install("c");
    PROTECT(q = allocList(n));
    SETCADDR(dimnames, LCONS(p, q));
    UNPROTECT(1);
    for (int i = 0 ; i < n ; i++) {
	SETCAR(q, ScalarString(STRING_ELT(names, i)));
	q = CDR(q);
    }
    PROTECT(dim = lang3(R_NilValue, R_NilValue, R_NilValue));
    SETCAR(dim, install("c"));
    SETCADR(dim, lang2(install("length"), install(".value")));
    SETCADDR(dim, ScalarInteger(length(names))); /* was real? */
    PROTECT(data = ScalarReal(0.));
    PROTECT(p = lang4(install("array"), data, dim, dimnames));
    p = lang3(install("<-"), install(".grad"), p);
    UNPROTECT(4);
    return p;
}

static SEXP CreateHess(SEXP names)
{
    SEXP q, data, dim, dimnames;
    int n = length(names);
    PROTECT(dimnames = lang4(R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    SETCAR(dimnames, install("list"));
    SEXP p = install("c");
    PROTECT(q = allocList(n));
    SETCADDR(dimnames, LCONS(p, q));
    UNPROTECT(1);
    for (int i = 0 ; i < n ; i++) {
	SETCAR(q, ScalarString(STRING_ELT(names, i)));
	q = CDR(q);
    }
    SETCADDDR(dimnames, duplicate(CADDR(dimnames)));
    PROTECT(dim = lang4(R_NilValue, R_NilValue, R_NilValue,R_NilValue));
    SETCAR(dim, install("c"));
    SETCADR(dim, lang2(install("length"), install(".value")));
    SETCADDR(dim, ScalarInteger(length(names)));
    SETCADDDR(dim, ScalarInteger(length(names)));
    PROTECT(data = ScalarReal(0.));
    PROTECT(p = lang4(install("array"), data, dim, dimnames));
    p = lang3(install("<-"), install(".hessian"), p);
    UNPROTECT(4);
    return p;
}

static SEXP DerivAssign(SEXP name, SEXP expr)
{
    SEXP ans, newname;
    PROTECT(ans = lang3(install("<-"), R_NilValue, expr));
    PROTECT(newname = ScalarString(name));
    SETCADR(ans, lang4(R_BracketSymbol, install(".grad"), R_MissingArg, newname));
    UNPROTECT(2);
    return ans;
}

static SEXP HessAssign1(SEXP name, SEXP expr)
{
    SEXP ans, newname;
    PROTECT(ans = lang3(install("<-"), R_NilValue, expr));
    PROTECT(newname = ScalarString(name));
    SETCADR(ans, lang5(R_BracketSymbol, install(".hessian"), R_MissingArg,
		       newname, newname));
    UNPROTECT(2);
    return ans;
}

static SEXP HessAssign2(SEXP name1, SEXP name2, SEXP expr)
{
    SEXP ans, newname1, newname2, tmp1, tmp2, tmp3;
    PROTECT(newname1 = ScalarString(name1));
    PROTECT(newname2 = ScalarString(name2));
    /* this is overkill, but PR#14772 found an issue */
    PROTECT(tmp1 = lang5(R_BracketSymbol, install(".hessian"), R_MissingArg,
			 newname1, newname2));
    PROTECT(tmp2 = lang5(R_BracketSymbol, install(".hessian"), R_MissingArg,
			 newname2, newname1));
    PROTECT(tmp3 = lang3(install("<-"), tmp2, expr));
    ans = lang3(install("<-"), tmp1, tmp3);
    UNPROTECT(5);
    return ans;
}

/* attr(.value, "gradient") <- .grad */

static SEXP AddGrad(void)
{
    SEXP ans;
    PROTECT(ans = mkString("gradient"));
    PROTECT(ans = lang3(install("attr"), install(".value"), ans));
    ans = lang3(install("<-"), ans, install(".grad"));
    UNPROTECT(2);
    return ans;
}

static SEXP AddHess(void)
{
    SEXP ans;
    PROTECT(ans = mkString("hessian"));
    PROTECT(ans = lang3(install("attr"), install(".value"), ans));
    ans = lang3(install("<-"), ans, install(".hessian"));
    UNPROTECT(2);
    return ans;
}

static SEXP Prune(SEXP lst)
{
    if (lst == R_NilValue)
	return lst;
    SETCDR(lst, Prune(CDR(lst)));
    if (CAR(lst) == R_MissingArg)
	return CDR(lst);
    else return lst ;
}

SEXP deriv(SEXP args)
{
/* deriv(expr, namevec, function.arg, tag, hessian) */
    SEXP ans, ans2, expr, funarg, names;
    int f_index, *d_index, *d2_index;
    int i, j, k, nexpr, nderiv=0;
    SEXP exprlist, stag;
    CXXR::RAllocStack::Scope rscope;
    const char *tag;

    args = CDR(args);
    InitDerivSymbols();
    PROTECT(exprlist = LCONS(R_BraceSymbol, R_NilValue));
    /* expr: */
    if (isExpression(CAR(args)))
	PROTECT(expr = XVECTOR_ELT(CAR(args), 0));
    else PROTECT(expr = CAR(args));
    args = CDR(args);
    /* namevec: */
    names = CAR(args);
    if (!isString(names) || (nderiv = length(names)) < 1)
	error("%s", _("invalid variable names"));
    args = CDR(args);
    /* function.arg: */
    funarg = CAR(args);
    args = CDR(args);
    /* tag: */
    stag = CAR(args);
    if (!isString(stag) || length(stag) < 1 || length(STRING_ELT(stag, 0)) < 1)
	error("%s", _("invalid tag"));
    tag = translateChar(STRING_ELT(stag, 0));
    if(strlen(tag) > 60) error("%s", _("invalid tag"));

    args = CDR(args);
    /* hessian: */
    bool hessian = asLogical(CAR(args));
    /* NOTE: FindSubexprs is destructive, hence the duplication.
       It can allocate, so protect the duplicate.
     */
    PROTECT(ans = duplicate(expr));
    f_index = FindSubexprs(ans, exprlist, tag);
    d_index = (int*)R_alloc((size_t) nderiv, sizeof(int));
    if (hessian)
	d2_index = (int*)R_alloc((size_t) ((nderiv * (1 + nderiv))/2),
				 sizeof(int));
    else d2_index = d_index;/*-Wall*/
    UNPROTECT(1);
    for(i=0, k=0; i<nderiv ; i++) {
	PROTECT(ans = duplicate(expr));
	PROTECT(ans = D(ans, installTrChar(STRING_ELT(names, i))));
	PROTECT(ans2 = duplicate(ans));	/* keep a temporary copy */
	d_index[i] = FindSubexprs(ans, exprlist, tag); /* examine the derivative first */
	PROTECT(ans = duplicate(ans2));	/* restore the copy */
	if (hessian) {
	    for(j = i; j < nderiv; j++) {
		PROTECT(ans2 = duplicate(ans)); /* install could allocate */
		PROTECT(ans2 = D(ans2, installTrChar(STRING_ELT(names, j))));
		d2_index[k] = FindSubexprs(ans2, exprlist, tag);
		k++;
		UNPROTECT(2);
	    }
	}
	UNPROTECT(4);
    }
    nexpr = length(exprlist) - 1;
    if (f_index) {
	Accumulate2(MakeVariable(f_index, tag), exprlist);
    }
    else {
	PROTECT(ans = duplicate(expr));
	Accumulate2(expr, exprlist);
	UNPROTECT(1);
    }
    Accumulate2(R_NilValue, exprlist);
    if (hessian) { Accumulate2(R_NilValue, exprlist); }
    for (i = 0, k = 0; i < nderiv ; i++) {
	if (d_index[i]) {
	    Accumulate2(MakeVariable(d_index[i], tag), exprlist);
	    if (hessian) {
		PROTECT(ans = duplicate(expr));
		PROTECT(ans = D(ans, installTrChar(STRING_ELT(names, i))));
		for (j = i; j < nderiv; j++) {
		    if (d2_index[k]) {
			Accumulate2(MakeVariable(d2_index[k], tag), exprlist);
		    } else {
			PROTECT(ans2 = duplicate(ans));
			PROTECT(ans2 = D(ans2, installTrChar(STRING_ELT(names, j))));
			Accumulate2(ans2, exprlist);
			UNPROTECT(2);
		    }
		    k++;
		}
		UNPROTECT(2);
	    }
	} else { /* the first derivative is constant or simple variable */
	    PROTECT(ans = duplicate(expr));
	    PROTECT(ans = D(ans, installTrChar(STRING_ELT(names, i))));
	    Accumulate2(ans, exprlist);
	    UNPROTECT(2);
	    if (hessian) {
		for (j = i; j < nderiv; j++) {
		    if (d2_index[k]) {
			Accumulate2(MakeVariable(d2_index[k], tag), exprlist);
		    } else {
			PROTECT(ans2 = duplicate(ans));
			PROTECT(ans2 = D(ans2, installTrChar(STRING_ELT(names, j))));
			if(isZero(ans2)) Accumulate2(R_MissingArg, exprlist);
			else Accumulate2(ans2, exprlist);
			UNPROTECT(2);
		    }
		    k++;
		}
	    }
	}
    }
    Accumulate2(R_NilValue, exprlist);
    Accumulate2(R_NilValue, exprlist);
    if (hessian) { Accumulate2(R_NilValue, exprlist); }

    i = 0;
    ans = CDR(exprlist);
    while (i < nexpr) {
	if (CountOccurrences(MakeVariable(i+1, tag), CDR(ans)) < 2) {
	    SETCDR(ans, Replace(MakeVariable(i+1, tag), CAR(ans), CDR(ans)));
	    SETCAR(ans, R_MissingArg);
	}
	else {
            SEXP var;
            PROTECT(var = MakeVariable(i+1, tag));
            SETCAR(ans, lang3(install("<-"), var, AddParens(CAR(ans))));
            UNPROTECT(1);
        }
	i = i + 1;
	ans = CDR(ans);
    }
    /* .value <- ... */
    SETCAR(ans, lang3(install("<-"), install(".value"), AddParens(CAR(ans))));
    ans = CDR(ans);
    /* .grad <- ... */
    SETCAR(ans, CreateGrad(names));
    ans = CDR(ans);
    /* .hessian <- ... */
    if (hessian) { SETCAR(ans, CreateHess(names)); ans = CDR(ans); }
    /* .grad[, "..."] <- ... */
    for (i = 0; i < nderiv ; i++) {
	SETCAR(ans, DerivAssign(STRING_ELT(names, i), AddParens(CAR(ans))));
	ans = CDR(ans);
	if (hessian) {
	    for (j = i; j < nderiv; j++) {
		if (CAR(ans) != R_MissingArg) {
		    if (i == j) {
			SETCAR(ans, HessAssign1(STRING_ELT(names, i),
						AddParens(CAR(ans))));
		    } else {
			SETCAR(ans, HessAssign2(STRING_ELT(names, i),
						STRING_ELT(names, j),
						AddParens(CAR(ans))));
		    }
		}
		ans = CDR(ans);
	    }
	}
    }
    /* attr(.value, "gradient") <- .grad */
    SETCAR(ans, AddGrad());
    ans = CDR(ans);
    if (hessian) { SETCAR(ans, AddHess()); ans = CDR(ans); }
    /* .value */
    SETCAR(ans, install(".value"));
    /* Prune the expression list removing eliminated sub-expressions */
    SETCDR(exprlist, Prune(CDR(exprlist)));

    if (TYPEOF(funarg) == LGLSXP && LOGICAL(funarg)[0]) { /* fun = TRUE */
	funarg = names;
    }

    if (TYPEOF(funarg) == CLOSXP)
    {
	SEXP formals = R_ClosureFormals(funarg);
	SEXP rho = R_ClosureEnv(funarg);
	    funarg = R_mkClosure(formals, exprlist, rho);
    }
    else if (isString(funarg)) {
	PROTECT(names = duplicate(funarg));
	PROTECT(ans = allocList(length(names)));
	SEXP a = ans;
	for(i = 0; i < length(names); i++) {
	    SET_TAG(a, installTrChar(STRING_ELT(names, i)));
	    SETCAR(a, R_MissingArg);
	    a = CDR(a);
	}
	funarg = R_mkClosure(ans, exprlist, R_GlobalEnv);
	UNPROTECT(2);
    }
    else {
	funarg = ExpressionVector::create(1);
	SET_XVECTOR_ELT(funarg, 0, exprlist);
	/* funarg = lang2(install("expression"), exprlist); */
    }
    UNPROTECT(2);
    return funarg;
}
