/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2025	The R Core Team
 *  Copyright (C) 2001--2012	The R Foundation
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
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
 *
 *
 *  EXPORTS	printMatrix()
 *		printArray()
 *
 *  See ./printutils.c	 for general remarks on Printing
 *			 and the Encode.. utils.
 *
 *  See ./format.c	 for the  format_FOO_  functions used below.
 */

/** @file printarray.cpp
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib> /* for div() */
#include <CXXR/GCStackRoot.hpp>
#include <CXXR/RAllocStack.hpp>
#include <CXXR/ProtectStack.hpp>
#include <Localization.h>
#include <Defn.h>
#include <Print.h>

using namespace R;
using namespace CXXR;

/* We need display width of a string.
   Used only for row/column names found by GetMatrixDimnames,
   so in native encoding.  (NULL ones from do_prmatrix are skipped.)
*/
#define strwidth(x) R::Rstrwid(x, (int) strlen(x), CE_NATIVE, 0)

/* ceil_DIV(a,b) :=  ceil(a / b)  in _int_ arithmetic : */
static R_INLINE
int ceil_DIV(int a, int b)
{
    div_t div_res = div(a, b);
    return div_res.quot + ((div_res.rem != 0) ? 1 : 0);
}

/* moved from printutils.c */

static void MatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("%*s%s", w-l, "",
		EncodeString(tmp, l, 0, Rprt_adj_left));
    }
    else {
	Rprintf("%*s[,%ld]", w-IndexWidth(j+1)-3, "", (long)j+1);
    }
}

static void RightMatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	/* This does not work correctly at least on FC3
	Rprintf("%*s", R_print.gap+w,
		EncodeString(tmp, l, 0, Rprt_adj_right)); */
	Rprintf("%*s%s", R_print.gap+w-l, "",
		EncodeString(tmp, l, 0, Rprt_adj_right));
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", (long)j+1, w-IndexWidth(j+1)-3, "");
    }
}

static void LeftMatrixColumnLabel(SEXP cl, int j, int w)
{
    if (!isNull(cl)) {
	SEXP tmp = STRING_ELT(cl, j);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("%*s%s%*s", R_print.gap, "",
		EncodeString(tmp, l, 0, Rprt_adj_left), w-l, "");
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", (long)j+1, w-IndexWidth(j+1)-3, "");
    }
}

static void MatrixRowLabel(SEXP rl, int i, int rlabw, int lbloff)
{
    if (!isNull(rl)) {
	SEXP tmp = STRING_ELT(rl, i);
	int l = (tmp == NA_STRING) ? R_print.na_width_noquote : Rstrlen(tmp, 0);
	Rprintf("\n%*s%s%*s", lbloff, "",
		EncodeString(tmp, l, 0, Rprt_adj_left),
		rlabw-l-lbloff, "");
    }
    else {
	Rprintf("\n%*s[%ld,]", rlabw-3-IndexWidth(i + 1), "", (long)i+1);
    }
}



/* This is the first (of 6)  print<TYPE>Matrix()  functions.
 * We define macros that will be re-used in the other functions,
 * and comment the common code here (only):
*/
static void printLogicalMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       SEXP rl, SEXP cl, const char *rn, const char *cn,
			       bool print_ij)
{
/* initialization; particularly of row labels, rl= dimnames(.)[[1]] and
 * rn = names(dimnames(.))[1] : */
#define _PRINT_INIT_rl_rn				\
    int *w = (int *) R_alloc(c, sizeof(int));		\
    int width, rlabw = -1, clabw = -1; /* -Wall */	\
    int i, j, jmin = 0, jmax = 0, lbloff = 0;		\
							\
    if (!isNull(rl))					\
	formatString(STRING_PTR_RO(rl), r, &rlabw, 0);	\
    else						\
	rlabw = IndexWidth(r + 1) + 3;			\
							\
    if (rn) {						\
	int rnw = strwidth(rn);				\
	if ( rnw < rlabw + R_MIN_LBLOFF )		\
	    lbloff = R_MIN_LBLOFF;			\
	else						\
	    lbloff = rnw - rlabw;			\
							\
	rlabw += lbloff;				\
    }

#   define _COMPUTE_W2_(_FORMAT_j_, _LAST_j_)				\
    /* compute w[j] = column-width of j(+1)-th column : */		\
    for (j = 0; j < c; j++) {						\
	if(print_ij) { _FORMAT_j_; } else w[j] = 0;			\
									\
	if (!isNull(cl)) {						\
	    CXXR::RAllocStack::Scope rscope;				\
	    if(STRING_ELT(cl, j) == NA_STRING)				\
		clabw = R_print.na_width_noquote;			\
	    else clabw = strwidth(translateChar(STRING_ELT(cl, j)));	\
	} else								\
	    clabw = IndexWidth(j + 1) + 3;				\
									\
	if (w[j] < clabw)						\
	    w[j] = clabw;						\
	_LAST_j_;							\
    }

#   define _COMPUTE_W_(F_j) _COMPUTE_W2_(F_j, w[j] += R_print.gap)
    //                               _LAST_j  ------------------- for all but String

#   define _PRINT_ROW_LAB			\
						\
    if (cn != NULL)				\
	Rprintf("%*s%s\n", rlabw, "", cn);	\
    if (rn != NULL)				\
	Rprintf("%*s", -rlabw, rn);		\
    else					\
	Rprintf("%*s", rlabw, "")

#   define _PRINT_MATRIX_(_W_EXTRA_, DO_COLUMN_LABELS, ENCODE_I_J)	\
									\
    if (c == 0) {							\
	_PRINT_ROW_LAB;							\
	for (i = 0; i < r; i++)						\
	    MatrixRowLabel(rl, i, rlabw, lbloff);			\
	Rprintf("\n");							\
    }									\
    else while (jmin < c) {						\
	/* print columns  jmin:(jmax-1)	 where jmax has to be determined first */ \
									\
	width = rlabw;							\
	/* initially, jmax = jmin */					\
	do {								\
	    width += w[jmax] _W_EXTRA_;					\
	    jmax++;							\
	}								\
	while (jmax < c && width + w[jmax] _W_EXTRA_ < R_print.width);	\
									\
	_PRINT_ROW_LAB;							\
									\
	DO_COLUMN_LABELS;						\
									\
	for (i = 0; i < r_pr; i++) {					\
	    MatrixRowLabel(rl, i, rlabw, lbloff); /* starting with an "\n" */ \
	    if(print_ij) for (j = jmin; j < jmax; j++) {		\
		ENCODE_I_J;						\
	    }								\
	}								\
	Rprintf("\n");							\
	jmin = jmax;							\
    }

#   define STD_ColumnLabels			\
	for (j = jmin; j < jmax ; j++)		\
	    MatrixColumnLabel(cl, j, w[j])

    _PRINT_INIT_rl_rn;
    const int *x = LOGICAL_RO(sx) + offset;

    _COMPUTE_W_( formatLogical(&x[j * (R_xlen_t) r], (R_xlen_t) r, &w[j]) );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeLogical(x[i + j * (R_xlen_t) r], w[j])));
}

static void printIntegerMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       SEXP rl, SEXP cl, const char *rn, const char *cn,
			       bool print_ij)
{
    _PRINT_INIT_rl_rn;
    const int *x = INTEGER_RO(sx) + offset;

    _COMPUTE_W_( formatInteger(&x[j * (R_xlen_t) r], (R_xlen_t) r, &w[j]) );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeInteger(x[i + j * (R_xlen_t) r], w[j])));
}

static void printRealMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			    SEXP rl, SEXP cl, const char *rn, const char *cn,
			    bool print_ij)
{
    _PRINT_INIT_rl_rn;
    const double *x = REAL_RO(sx) + offset;
    int *d = (int *) R_alloc(c, sizeof(int)),
	*e = (int *) R_alloc(c, sizeof(int));

    _COMPUTE_W_( formatReal(&x[j * (R_xlen_t) r], (R_xlen_t) r, &w[j],
			    &d[j], &e[j], 0) );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%s", EncodeReal0(x[i + j * (R_xlen_t) r],
					     w[j], d[j], e[j], OutDec)) );
}

static void printComplexMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			       SEXP rl, SEXP cl, const char *rn, const char *cn,
			       bool print_ij)
{
    _PRINT_INIT_rl_rn;
    const Rcomplex *x = COMPLEX_RO(sx) + offset;
    int *dr = (int *) R_alloc(c, sizeof(int)),
	*er = (int *) R_alloc(c, sizeof(int)),
	*wr = (int *) R_alloc(c, sizeof(int)),
	*di = (int *) R_alloc(c, sizeof(int)),
	*ei = (int *) R_alloc(c, sizeof(int)),
	*wi = (int *) R_alloc(c, sizeof(int));

    /* Determine the column widths */
    _COMPUTE_W_( formatComplex(&x[j * (R_xlen_t) r], (R_xlen_t) r,
			       &wr[j], &dr[j], &er[j],
			       &wi[j], &di[j], &ei[j], 0);
		 w[j] = wr[j] + wi[j] + 2 );

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   if (ISNA(x[i + j * (R_xlen_t) r].r) ||
		       ISNA(x[i + j * (R_xlen_t) r].i))

		       Rprintf("%s", EncodeReal0(NA_REAL, w[j], 0, 0, OutDec));
		   else
		       /* Note that the label printing may modify w[j], so wr[j] is not 
		          necessarily still valid, and we use w[j] - wi[j] - 2  */
		       Rprintf("%s",
			       EncodeComplex(x[i + j * (R_xlen_t) r],
					     w[j] - wi[j] - 2, dr[j], er[j],
					     wi[j], di[j], ei[j], OutDec)) )
}

static void printStringMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			      int quote, int right, SEXP rl, SEXP cl,
			      const char *rn, const char *cn, bool print_ij)
{
    _PRINT_INIT_rl_rn;
    const SEXP *x = STRING_PTR_RO(sx)+offset;

    _COMPUTE_W2_( formatString(&x[j * (R_xlen_t) r], (R_xlen_t) r,
                               &w[j], quote), );

    _PRINT_MATRIX_( + R_print.gap,
	           /* DO_COLUMN_LABELS = */
		   if (right) {
		       for (j = jmin; j < jmax ; j++)
			   RightMatrixColumnLabel(cl, j, w[j]);
		   }
		   else {
		       for (j = jmin; j < jmax ; j++)
			   LeftMatrixColumnLabel(cl, j, w[j]);
		   },
		   /* ENCODE_I = */
		   Rprintf("%*s%s", R_print.gap, "",
			   EncodeString(x[i + j * (R_xlen_t) r],
		                        w[j], quote, (Rprt_adj) right)) );
}

static void printRawMatrix(SEXP sx, int offset, int r_pr, int r, int c,
			   SEXP rl, SEXP cl, const char *rn, const char *cn,
			   bool print_ij)
{
    _PRINT_INIT_rl_rn;
    const Rbyte *x = RAW_RO(sx) + offset;

    _COMPUTE_W_( formatRaw(&x[j * (R_xlen_t) r], (R_xlen_t) r, &w[j]) )

    _PRINT_MATRIX_( , STD_ColumnLabels,
		   Rprintf("%*s%s", w[j]-2, "",
		   EncodeRaw(x[i + j * (R_xlen_t) r], "")) );
}

/* rm and cn are found by GetMatrixDimnames so in native encoding */
attribute_hidden
void R::printMatrix(SEXP x, int offset, SEXP dim, int quote, int right,
		 SEXP rl, SEXP cl, const char *rn, const char *cn)
{
/* 'rl' and 'cl' are dimnames(.)[[1]] and dimnames(.)[[2]]  whereas
 * 'rn' and 'cn' are the  names(dimnames(.))
 */
    CXXR::RAllocStack::Scope rscope;
    const int *pdim = INTEGER_RO(dim);
    int r = pdim[0];
    int c = pdim[1];
    /* PR#850 */
    if ((rl != R_NilValue) && (r > length(rl)))
	error("%s", _("too few row labels"));
    if ((cl != R_NilValue) && (c > length(cl)))
	error("%s", _("too few column labels"));
    if (r == 0 && c == 0) { // FIXME?  names(dimnames(.)) :
	Rprintf("<0 x 0 matrix>\n");
	return;
    }

    int r_pr = r,
	/* Make sure we don't display more columns than max elements, see PR#15027 */
	c_pr = c > R_print.max ? R_print.max : c;

    if(c > 0 && R_print.max / c < r) /* avoid integer overflow */
	/* using floor(), not ceil(), since 'c' could be huge: */
	r_pr = R_print.max / c;
    /* Display at least one row in case of truncation */
    if (c > c_pr && r_pr < 1 && r > 0)
	r_pr = 1;
    switch (TYPEOF(x)) {
    case LGLSXP:
	printLogicalMatrix(x, offset, r_pr, r, c_pr, rl, cl, rn, cn, true);
	break;
    case INTSXP:
	printIntegerMatrix(x, offset, r_pr, r, c_pr, rl, cl, rn, cn, true);
	break;
    case REALSXP:
	printRealMatrix	  (x, offset, r_pr, r, c_pr, rl, cl, rn, cn, true);
	break;
    case CPLXSXP:
	printComplexMatrix(x, offset, r_pr, r, c_pr, rl, cl, rn, cn, true);
	break;
    case STRSXP:
	if (quote) quote = '"';
	printStringMatrix (x, offset, r_pr, r, c_pr, quote, right, rl, cl, rn, cn, true);
	break;
    case RAWSXP:
	printRawMatrix	  (x, offset, r_pr, r, c_pr, rl, cl, rn, cn, true);
	break;
    default:
	UNIMPLEMENTED_TYPE("printMatrix", x);
    }
    if (r_pr < r || c_pr < c) {
	Rprintf("%s", _(" [ reached 'max' / getOption(\"max.print\") -- omitted"));
	if (r_pr < r) {
    	    Rprintf(n_(" %d row", " %d rows", r - r_pr), r - r_pr);
	}
	if (c_pr < c) {
	    if (r_pr < r) Rprintf("%s", _(" and"));
	    Rprintf(n_(" %d column", " %d columns", c - c_pr), c - c_pr);
	}
	Rprintf(" ]\n");
    }
}

attribute_hidden
void R::printArray(SEXP x, SEXP dim, int quote, int right, SEXP dimnames)
{
/* == printArray(.) */
    CXXR::RAllocStack::Scope rscope;
    int ndim = LENGTH(dim);
    const char *rn = NULL, *cn = NULL;

    if (ndim == 1)
	printVector(x, 1, quote);
    else if (ndim == 2) {
	SEXP rl, cl;
	GetMatrixDimnames(x, &rl, &cl, &rn, &cn);
	printMatrix(x, 0, dim, quote, 0, rl, cl, rn, cn);
    }
    else { /* ndim >= 3 */
	GCStackRoot<> dnn;
	SEXP dn, dn0, dn1;
	const int *dims = INTEGER_RO(dim);
	int i, j, nb, nb_pr, ne_last, nc_last, nr_last,
	    nr = dims[0], nc = dims[1],
	    b = nr * nc;
	bool max_reached, has_dimnames = (dimnames != R_NilValue),
	    has_dnn = has_dimnames;

	if (!has_dimnames) {
	    dn0 = R_NilValue;
	    dn1 = R_NilValue;
	    dnn = R_NilValue; /* -Wall */
	}
	else {
	    dn0 = VECTOR_ELT(dimnames, 0);
	    dn1 = VECTOR_ELT(dimnames, 1);
	    dnn = getAttrib(dimnames, R_NamesSymbol);
	    has_dnn = !isNull(dnn);
	    if ( has_dnn ) {
		rn = (char *) translateChar(STRING_ELT(dnn, 0));
		cn = (char *) translateChar(STRING_ELT(dnn, 1));
	    }
	}
	/* nb := #{entries} in a slice such as x[1,1,..] or equivalently,
	 *       the number of matrix slices   x[ , , *, ..]  which
	 *       are printed as matrices -- if options("max.print") allows */
	for (i = 2, nb = 1; i < ndim; i++)
	    nb *= dims[i];
	max_reached = (b > 0 && R_print.max / b < nb);
	if (max_reached) { /* i.e., also  b > 0, nr > 0, nc > 0, nb > 0 */
	    /* nb_pr := the number of matrix slices to be printed */
	    nb_pr = ceil_DIV(R_print.max, b);
	    /* for the last, (nb_pr)th matrix slice, use only nr_last rows;
	     *  using floor(), not ceil(), since 'nc' could be huge: */
	    ne_last = R_print.max - b * (nb_pr - 1);
	    nc_last = (ne_last < nc) ? ne_last : nc;
	    nr_last = (ne_last < nc) ? 1 : ne_last / nc;
	    if(nr_last == 0) { nb_pr--; nc_last = nc; nr_last = nr;} 
	} else {
	    nb_pr = (nb > 0) ? nb : 1; // do print *something* when dim = c(a,b,0)
	    ne_last = b;
	    nc_last = nc;
	    nr_last = nr;
	}
	for (i = 0; i < nb_pr; i++) {
	    bool do_ij = (nb > 0),
		i_last = (i == nb_pr - 1); /* for the last slice */
	    int	use_nc = i_last ? nc_last : nc,
		use_nr = i_last ? nr_last : nr;
	    if(do_ij) {
		int k = 1;
		Rprintf(", ");
		for (j = 2 ; j < ndim; j++) {
		    int l = (i / k) % dims[j] + 1;
		    if (has_dimnames &&
			((dn = VECTOR_ELT(dimnames, j)) != R_NilValue)) {
			if ( has_dnn )
			    Rprintf(", %s = %s",
				    translateChar(STRING_ELT(dnn, j)),
				    translateChar(STRING_ELT(dn, l - 1)));
			else
			    Rprintf(", %s", translateChar(STRING_ELT(dn, l - 1)));
		    } else
			Rprintf(", %d", l);
		    k *= dims[j];
		}
		Rprintf("\n\n");
	    } else { // nb == 0 -- e.g. <2 x 3 x 0 array of logical>
		for (i = 0; i < ndim; i++)
		    Rprintf("%s%d", (i == 0) ? "<" : " x ", dims[i]);
		Rprintf(" array of %s>\n", CHAR(type2str_nowarn(TYPEOF(x))));
	    }
	    switch (TYPEOF(x)) {
	    case LGLSXP:
		printLogicalMatrix(x, i * b, use_nr, nr, use_nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case INTSXP:
		printIntegerMatrix(x, i * b, use_nr, nr, use_nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case REALSXP:
		printRealMatrix   (x, i * b, use_nr, nr, use_nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case CPLXSXP:
		printComplexMatrix(x, i * b, use_nr, nr, use_nc, dn0, dn1, rn, cn, do_ij);
		break;
	    case STRSXP:
		if (quote) quote = '"';
		printStringMatrix (x, i * b, use_nr, nr, use_nc,
				   quote, right, dn0, dn1, rn, cn, do_ij);
		break;
	    case RAWSXP:
		printRawMatrix    (x, i * b, use_nr, nr, use_nc, dn0, dn1, rn, cn, do_ij);
		break;
	    default:
		break;
	    }
	    Rprintf("\n");
	}

	if (max_reached) {
	    Rprintf("%s", _(" [ reached 'max' / getOption(\"max.print\") -- omitted"));
	    if (nb_pr < nb)
		Rprintf(n_(" %d slice", " %d slices", nb - nb_pr), nb - nb_pr);
	    else if (nb_pr == nb) {
		if ((nr -= nr_last) > 0) Rprintf(n_(" %d row",    " %d rows",    nr), nr);
		if ((nc -= nc_last) > 0) Rprintf(n_(" %d column", " %d columns", nc), nc);
	    }
	    Rprintf(" ] \n");
	}
    }
}
