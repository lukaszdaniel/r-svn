#  File src/library/base/R/eigen.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/


isSymmetric <- function(object, ...) UseMethod("isSymmetric")

isSymmetric.matrix <- function(object, tol = 100*.Machine$double.eps,
                               tol1 = 8*tol, trans = "C", ...)
{
    if(!is.matrix(object)) return(FALSE) ## we test for  symmetric *matrix*
    ## cheap pretest: is it square?
    d <- dim(object)
    if((n <- d[1L]) != d[2L]) return(FALSE)
    iCplx <- is.complex(object) && trans == "C"
    if(n > 1L && length(tol1)) {
	## initial pre-tests, fast for large non-symmetric:
	Cj <- if(iCplx) Conj else identity
	for(i in unique(c(1L, 2L, n-1L, n)))
	    if(is.character(all.equal(object[i, ], Cj(object[, i]), tolerance = tol1, ...)))
		return(FALSE)
    }
    test <-
        if(iCplx)
            all.equal.numeric(object, Conj(t(object)), tolerance = tol, ...)
        else # numeric, character, {complex, trans != "C"}, ..
            all.equal(object, t(object), tolerance = tol, ...)
    isTRUE(test)
}

eigen <- function(x, symmetric, only.values = FALSE, EISPACK = FALSE)
{
    x <- unname(as.matrix(x))
    n <- nrow(x)
    if (n != ncol(x)) stop("non-square matrix in 'eigen'")
    if (!n) stop("0 x 0 matrix")
    n <- as.integer(n)
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(x)"))

    complex.x <- is.complex(x)
    if (!all(is.finite(x))) stop("infinite or missing values in 'x'")

    if(missing(symmetric)) symmetric <- isSymmetric.matrix(x)

    if (symmetric) {
        z <- if(!complex.x) .Internal(La_rs(x, only.values))
        else .Internal(La_rs_cmplx(x, only.values))
        ord <- rev(seq_along(z$values))
    } else {
        z <- if(!complex.x) .Internal(La_rg(x, only.values))
        else .Internal(La_rg_cmplx(x, only.values))
        ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    if(only.values)
	list(values = z$values[ord], vectors = NULL)
    else
	structure(class = "eigen",
		  list(values = z$values[ord],
		       vectors = z$vectors[, ord, drop = FALSE]))
}

print.eigen <- function (x, ...) {
    cat("eigen() decomposition\n")
    print(unclass(x), ...)
    invisible(x)
}
