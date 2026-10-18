/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2025  The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <CXXR/GCStackRoot.hpp>
#include <CXXR/GCRoot.hpp>
#include <CXXR/ProtectStack.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/String.hpp>
#include <CXXR/IntVector.hpp>
#include <CXXR/RealVector.hpp>
#include <CXXR/ListVector.hpp>
#include <CXXR/StringVector.hpp>
#include <Defn.h> // for ENSURE_NAMEDMAX
#include <R_ext/Applic.h>

#include "statsR.h"
#include "localization.h"
#include "statsErr.h"

using namespace R;
using namespace CXXR;

SEXP getListElement(SEXP list, const char *str)
{
    if (!isNewList(list))
	error("%s", _("invalid argument type"));
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

    /* an unnamed list has no element called str; STRING_ELT() on the
       NULL names is unchecked here (Defn.h) and reads off R_NilValue */
    if (TYPEOF(names) != STRSXP)
	return R_NilValue;
    for (int i = 0; i < length(list); i++)
	if (streql(CHAR(STRING_ELT(names, i)), str)) { /* ASCII only */
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    return elmt;
}
// keep it aligned with src/appl/optim.cpp
struct opt_struct
{
    GCRoot<> R_fcall;    /* function */
    GCRoot<> R_gcall;    /* gradient */
    SEXP R_env;      /* where to evaluate the calls */
    RAllocStack::Vector<double> ndeps;   /* tolerances for numerical derivatives */
    double fnscale;  /* scaling for objective */
    RAllocStack::Vector<double> parscale;/* scaling for parameters */
    int usebounds;
    RAllocStack::Vector<double> lower, upper;
    SEXP names;	     /* names for par */
};
using OptStruct = struct opt_struct *;

static double fminfn(int n, double *p, void *ex)
{
    GCStackRoot<> s, x;
    OptStruct OS = (OptStruct) ex;

    x = RealVector::create(n);
    if(!isNull(OS->names)) setAttrib(x, R_NamesSymbol, OS->names);
    for (int i = 0; i < n; i++) {
	if (!R_FINITE(p[i])) error("%s", _("non-finite value supplied by optim"));
	REAL(x)[i] = p[i] * (OS->parscale[i]);
    }
    SETCADR(OS->R_fcall, x);
    s = eval(OS->R_fcall, OS->R_env);
    s = RealVector::coerce(s);
    if (LENGTH(s) != 1)
	error(_("objective function in optim evaluates to length %d not 1"),
	      LENGTH(s));
    return REAL(s)[0]/(OS->fnscale);
}

static void fmingr(int n, double *p, double *df, void *ex)
{
    GCStackRoot<> s, x;
    double val1, val2, eps, epsused, tmp;
    OptStruct OS = (OptStruct) ex;

    if (!isNull(OS->R_gcall)) { /* analytical derivatives */
	x = RealVector::create(n);
	if(!isNull(OS->names)) setAttrib(x, R_NamesSymbol, OS->names);
	for (int i = 0; i < n; i++) {
	    if (!R_FINITE(p[i]))
		error("%s", _("non-finite value supplied by optim"));
	    REAL(x)[i] = p[i] * (OS->parscale[i]);
	}
	SETCADR(OS->R_gcall, x);
	s = eval(OS->R_gcall, OS->R_env);
	s = RealVector::coerce(s);
	if(LENGTH(s) != n)
	    error(_("gradient in optim evaluated to length %d not %d"),
		  LENGTH(s), n);
	for (int i = 0; i < n; i++)
	    df[i] = REAL(s)[i] * (OS->parscale[i])/(OS->fnscale);
    } else { /* numerical derivatives */
        /* As discussed in PR#15958, the callback might save a copy of
           x, so we need to duplicate it before changes.  Currently this
           always duplicates; with true reference counting, it would
           only do so when necessary. */
	x = RealVector::create(n);
	setAttrib(x, R_NamesSymbol, OS->names);
	ENSURE_NAMEDMAX(x); // in case f tries to change it
	for (int i = 0; i < n; i++) REAL(x)[i] = p[i] * (OS->parscale[i]);
	SETCADR(OS->R_fcall, x);
	if(OS->usebounds == 0) {
	    for (int i = 0; i < n; i++) {
		eps = OS->ndeps[i];
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = (p[i] + eps) * (OS->parscale[i]);
		s = eval(OS->R_fcall, OS->R_env);
		s = RealVector::coerce(s);
		val1 = REAL(s)[0]/(OS->fnscale);
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = (p[i] - eps) * (OS->parscale[i]);
		s = eval(OS->R_fcall, OS->R_env);
		s = RealVector::coerce(s);
		val2 = REAL(s)[0]/(OS->fnscale);
		df[i] = (val1 - val2)/(2 * eps);
		if(!R_FINITE(df[i]))
		    error(_("non-finite finite-difference value [%d]"), i+1);
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = p[i] * (OS->parscale[i]);
	    }
	} else { /* usebounds */
	    for (int i = 0; i < n; i++) {
		epsused = eps = OS->ndeps[i];
		tmp = p[i] + eps;
		if (tmp > OS->upper[i]) {
		    tmp = OS->upper[i];
		    epsused = tmp - p[i];
		}
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = tmp * (OS->parscale[i]);
		s = eval(OS->R_fcall, OS->R_env);
		s = RealVector::coerce(s);
		val1 = REAL(s)[0]/(OS->fnscale);
		tmp = p[i] - eps;
		if (tmp < OS->lower[i]) {
		    tmp = OS->lower[i];
		    eps = p[i] - tmp;
		}
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = tmp * (OS->parscale[i]);
		s = eval(OS->R_fcall, OS->R_env);
		s = RealVector::coerce(s);
		val2 = REAL(s)[0]/(OS->fnscale);
		df[i] = (val1 - val2)/(epsused + eps);
		if(!R_FINITE(df[i]))
		    error(_("non-finite finite-difference value [%d]"), i+1);
                if (MAYBE_REFERENCED(x)) {
                    x = duplicate(x);
                    SETCADR(OS->R_fcall, x);
                }
		REAL(x)[i] = p[i] * (OS->parscale[i]);
	    }
	}
    }
}

/* par fn gr method options */
SEXP optim(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    GCStackRoot<> par; SEXP fn, gr, method, options, slower, supper;
    GCStackRoot<> tmp;
    GCStackRoot<> res,value, counts, conv;
    int npar=0, trace, maxit, fncount = 0, grcount = 0, nREPORT, tmax;
    int ifail = 0;
    double val = 0.0, abstol, reltol, temp;
    const char *tn;

    args = CDR(args);
    opt_struct OS_storage;
    OptStruct OS = &OS_storage;
    OS->usebounds = 0;
    OS->R_env = rho;
    par = CAR(args);
    OS->names = getAttrib(par, R_NamesSymbol);
    args = CDR(args); fn = CAR(args);
    if (!isFunction(fn)) error("%s", _("'fn' is not a function"));
    args = CDR(args); gr = CAR(args);
    args = CDR(args); method = CAR(args);
    if (!isString(method)|| LENGTH(method) != 1)
	error(_("invalid '%s' argument"), "method");
    tn = CHAR(STRING_ELT(method, 0));
    args = CDR(args); options = CAR(args);
    OS->R_fcall = lang2(fn, R_NilValue);
    par = RealVector::coerce(par);
    if (MAYBE_REFERENCED(par))
    	par = duplicate(par);
    npar = LENGTH(par);
    RAllocStack::Vector<double> dpar(npar);
    trace = asInteger(getListElement(options, "trace"));
    OS->fnscale = asReal(getListElement(options, "fnscale"));
    tmp = getListElement(options, "parscale");
    if (LENGTH(tmp) != npar)
	error("%s", _("'parscale' is of the wrong length"));
    tmp = RealVector::coerce(tmp);
    OS->parscale.resize(npar);
    for (int i = 0; i < npar; i++) OS->parscale[i] = REAL(tmp)[i];
    for (int i = 0; i < npar; i++)
	dpar[i] = REAL(par)[i] / (OS->parscale[i]);
    res = ListVector::create(5);
    GCStackRoot<> names;
    names = StringVector::create(5);
    SET_STRING_ELT(names, 0, mkChar("par"));
    SET_STRING_ELT(names, 1, mkChar("value"));
    SET_STRING_ELT(names, 2, mkChar("counts"));
    SET_STRING_ELT(names, 3, mkChar("convergence"));
    SET_STRING_ELT(names, 4, mkChar("message"));
    setAttrib(res, R_NamesSymbol, names);
    value = RealVector::create(1);
    counts = IntVector::create(2);
    GCStackRoot<> countnames;
    countnames = StringVector::create(2);
    SET_STRING_ELT(countnames, 0, mkChar("function"));
    SET_STRING_ELT(countnames, 1, mkChar("gradient"));
    setAttrib(counts, R_NamesSymbol, countnames);
    conv = IntVector::create(1);
    abstol = asReal(getListElement(options, "abstol"));
    reltol = asReal(getListElement(options, "reltol"));
    maxit = asInteger(getListElement(options, "maxit"));
    if (maxit == NA_INTEGER) error("%s", _("'maxit' is not an integer"));

    if (streql(tn, "Nelder-Mead")) {
	double alpha, beta, gamm;
	RAllocStack::Vector<double> opar(npar);

	alpha = asReal(getListElement(options, "alpha"));
	beta = asReal(getListElement(options, "beta"));
	gamm = asReal(getListElement(options, "gamma"));
	nmmin(npar, dpar.data(), opar.data(), &val, fminfn, &ifail, abstol, reltol,
	      (void *)OS, alpha, beta, gamm, trace, &fncount, maxit);
	for (int i = 0; i < npar; i++)
	    REAL(par)[i] = opar[i] * (OS->parscale[i]);
	grcount = NA_INTEGER;

    }
    else if (streql(tn, "SANN")) {
	tmax = asInteger(getListElement(options, "tmax"));
	temp = asReal(getListElement(options, "temp"));
	if (trace) trace = asInteger(getListElement(options, "REPORT"));
	if (tmax == NA_INTEGER || tmax < 1) // PR#15194
	    error("%s", _("'tmax' is not a positive integer"));
	if (!isNull(gr)) {
	    if (!isFunction(gr)) error("%s", _("'gr' is not a function"));
		OS->R_gcall = lang2(gr, R_NilValue);
	} else {
	    OS->R_gcall = R_NilValue; /* for balance */
	}
	samin(npar, dpar.data(), &val, fminfn, maxit, tmax, temp, trace, (void *)OS);
	for (int i = 0; i < npar; i++)
	    REAL(par)[i] = dpar[i] * (OS->parscale[i]);
	fncount = npar > 0 ? maxit : 1;
	grcount = NA_INTEGER;

    } else if (streql(tn, "BFGS")) {
	GCStackRoot<> ndeps;

	nREPORT = asInteger(getListElement(options, "REPORT"));
	if (!isNull(gr)) {
	    if (!isFunction(gr)) error("%s", _("'gr' is not a function"));
	    OS->R_gcall = lang2(gr, R_NilValue);
	} else {
	    OS->R_gcall = R_NilValue; /* for balance */
	    ndeps = getListElement(options, "ndeps");
	    if (LENGTH(ndeps) != npar)
		error("%s", _("'ndeps' is of the wrong length"));
	    OS->ndeps.resize(npar);
	    ndeps = RealVector::coerce(ndeps);
	    for (int i = 0; i < npar; i++) OS->ndeps[i] = REAL(ndeps)[i];
	}
	RAllocStack::Vector<int> mask(npar);
	for (int i = 0; i < npar; i++) mask[i] = 1;
	vmmin(npar, dpar.data(), &val, fminfn, fmingr, maxit, trace, mask.data(), abstol,
	      reltol, nREPORT, (void *)OS, &fncount, &grcount, &ifail);
	for (int i = 0; i < npar; i++)
	    REAL(par)[i] = dpar[i] * (OS->parscale[i]);
    } else if (streql(tn, "CG")) {
	int type;
	GCStackRoot<> ndeps;
	RAllocStack::Vector<double> opar(npar);

	type = asInteger(getListElement(options, "type"));
	if (!isNull(gr)) {
	    if (!isFunction(gr)) error("%s", _("'gr' is not a function"));
	    OS->R_gcall = lang2(gr, R_NilValue);
	} else {
	    OS->R_gcall = R_NilValue; /* for balance */
	    ndeps = getListElement(options, "ndeps");
	    if (LENGTH(ndeps) != npar)
		error("%s", _("'ndeps' is of the wrong length"));
	    OS->ndeps.resize(npar);
	    ndeps = RealVector::coerce(ndeps);
	    for (int i = 0; i < npar; i++) OS->ndeps[i] = REAL(ndeps)[i];
	}
	cgmin(npar, dpar.data(), opar.data(), &val, fminfn, fmingr, &ifail, abstol,
	      reltol, (void *)OS, type, trace, &fncount, &grcount, maxit);
	for (int i = 0; i < npar; i++)
	    REAL(par)[i] = opar[i] * (OS->parscale[i]);

    } else if (streql(tn, "L-BFGS-B")) {
	GCStackRoot<> ndeps, smsg;
	int lmm;
	RAllocStack::Vector<int> nbd(npar);
	double factr, pgtol;
	char msg[60];

	OS->lower.resize(npar);
	OS->upper.resize(npar);
	nREPORT = asInteger(getListElement(options, "REPORT"));
	factr = asReal(getListElement(options, "factr"));
	pgtol = asReal(getListElement(options, "pgtol"));
	lmm = asInteger(getListElement(options, "lmm"));
	if (!isNull(gr)) {
	    if (!isFunction(gr)) error("%s", _("'gr' is not a function"));
	    OS->R_gcall = lang2(gr, R_NilValue);
	} else {
	    OS->R_gcall = R_NilValue; /* for balance */
	    ndeps = getListElement(options, "ndeps");
	    if (LENGTH(ndeps) != npar)
		error("%s", _("'ndeps' is of the wrong length"));
	    OS->ndeps.resize(npar);
	    ndeps = RealVector::coerce(ndeps);
	    for (int i = 0; i < npar; i++) OS->ndeps[i] = REAL(ndeps)[i];
	}
	args = CDR(args); slower = CAR(args); /* coerce in calling code */
	args = CDR(args); supper = CAR(args);
	for (int i = 0; i < npar; i++) {
	    OS->lower[i] = REAL(slower)[i] / (OS->parscale[i]);
	    OS->upper[i] = REAL(supper)[i] / (OS->parscale[i]);
	    if (!R_FINITE(OS->lower[i])) {
		if (!R_FINITE(OS->upper[i])) nbd[i] = 0; else nbd[i] = 3;
	    } else {
		if (!R_FINITE(OS->upper[i])) nbd[i] = 1; else nbd[i] = 2;
	    }
	}
	OS->usebounds = 1;
	lbfgsb(npar, lmm, dpar.data(), OS->lower.data(), OS->upper.data(), nbd.data(), &val, fminfn, fmingr,
	       &ifail, (void *)OS, factr, pgtol, &fncount, &grcount,
	       maxit, msg, trace, nREPORT);
	for (int i = 0; i < npar; i++)
	    REAL(par)[i] = dpar[i] * (OS->parscale[i]);
	smsg = mkString(msg);
	SET_VECTOR_ELT(res, 4, smsg);
    } else
	error("%s", _("unknown 'method'"));

    if(!isNull(OS->names)) setAttrib(par, R_NamesSymbol, OS->names);
    REAL(value)[0] = val * (OS->fnscale);
    SET_VECTOR_ELT(res, 0, par); SET_VECTOR_ELT(res, 1, value);
    INTEGER(counts)[0] = fncount; INTEGER(counts)[1] = grcount;
    SET_VECTOR_ELT(res, 2, counts);
    INTEGER(conv)[0] = ifail;
    SET_VECTOR_ELT(res, 3, conv);

    return res;
}

/* par fn gr options */
SEXP optimhess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, gr, options;
    GCStackRoot<> par, tmp, ndeps, ans;
    int npar;
    double eps;

    args = CDR(args);
    opt_struct OS_storage;
    OptStruct OS = &OS_storage;
    OS->usebounds = 0;
    OS->R_env = rho;
    par = CAR(args);
    npar = LENGTH(par);
    OS->names = getAttrib(par, R_NamesSymbol);
    args = CDR(args); fn = CAR(args);
    if (!isFunction(fn)) error("%s", _("'fn' is not a function"));
    args = CDR(args); gr = CAR(args);
    args = CDR(args); options = CAR(args);
    OS->fnscale = asReal(getListElement(options, "fnscale"));
    tmp = getListElement(options, "parscale");
    if (LENGTH(tmp) != npar)
	error("%s", _("'parscale' is of the wrong length"));
    tmp = RealVector::coerce(tmp);
    OS->parscale.resize(npar);
    for (int i = 0; i < npar; i++) OS->parscale[i] = REAL(tmp)[i];
    OS->R_fcall = lang2(fn, R_NilValue);
    par = RealVector::coerce(par);
    if (!isNull(gr)) {
	if (!isFunction(gr)) error("%s", _("'gr' is not a function"));
	OS->R_gcall = lang2(gr, R_NilValue);
    } else {
	OS->R_gcall = R_NilValue; /* for balance */
    }
    ndeps = getListElement(options, "ndeps");
    if (LENGTH(ndeps) != npar) error("%s", _("'ndeps' is of the wrong length"));
    OS->ndeps.resize(npar);
    ndeps = RealVector::coerce(ndeps);
    for (int i = 0; i < npar; i++) OS->ndeps[i] = REAL(ndeps)[i];
    ans = allocMatrix(REALSXP, npar, npar);
    RAllocStack::Vector<double> dpar(npar);
    for (int i = 0; i < npar; i++)
	dpar[i] = REAL(par)[i] / (OS->parscale[i]);
    RAllocStack::Vector<double> df1(npar);
    RAllocStack::Vector<double> df2(npar);
    for (int i = 0; i < npar; i++) {
	eps = OS->ndeps[i]/(OS->parscale[i]);
	dpar[i] = dpar[i] + eps;
	fmingr(npar, dpar.data(), df1.data(), (void *)OS);
	dpar[i] = dpar[i] - 2 * eps;
	fmingr(npar, dpar.data(), df2.data(), (void *)OS);
	for (int j = 0; j < npar; j++)
	    REAL(ans)[i * npar + j] = (OS->fnscale) * (df1[j] - df2[j])/
		(2 * eps * (OS->parscale[i]) * (OS->parscale[j]));
	dpar[i] = dpar[i] + eps;
    }
    // now symmetrize
    for (int i = 0; i < npar; i++)
	for (int j = 0; j < i; j++) {
	    double tmp =
		0.5 * (REAL(ans)[i * npar + j] + REAL(ans)[j * npar + i]);
	    REAL(ans)[i * npar + j] = REAL(ans)[j * npar + i] = tmp;
	}
    SEXP nm = getAttrib(par, R_NamesSymbol);
    if(!isNull(nm)) {
	GCStackRoot<> dm;
	dm = ListVector::create(2);
	SET_VECTOR_ELT(dm, 0, duplicate(nm));
	SET_VECTOR_ELT(dm, 1, duplicate(nm));
	setAttrib(ans, R_DimNamesSymbol, dm);
    }

    return ans;
}
