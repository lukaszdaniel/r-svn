#  File src/library/base/R/qr.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

## be fast rather than "complete":
is.qr <- function(x) is.list(x) && inherits(x, "qr")

qr <- function(x, ...) UseMethod("qr")

qr.default <- function(x, tol = 1e-07, LAPACK = FALSE, ...)
{
    x <- as.matrix(x)
    if(is.complex(x))
        return(structure(.Internal(La_qr_cmplx(x)), class = "qr"))
    ## otherwise :
    if(LAPACK)
        return(structure(.Internal(La_qr(x)), useLAPACK = TRUE, class = "qr"))
    ## else "Linpack" case:
    p <- as.integer(ncol(x))
    if(is.na(p)) stop(gettextf("invalid '%s' value", "ncol(x)"))
    n <- as.integer(nrow(x))
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(x)"))
    if(1.0 * n * p > 2147483647) stop("too large a matrix for LINPACK")
    storage.mode(x) <- "double"
    res <- .Fortran(.F_dqrdc2,
	     qr = x,
	     n,
	     n,
	     p,
	     as.double(tol),
	     rank = integer(1L),
	     qraux = double(p),
	     pivot = as.integer(seq_len(p)),
	     double(2L*p))[c(1,6,7,8)]# c("qr", "rank", "qraux", "pivot")
    if(!is.null(cn <- colnames(x)))
        colnames(res$qr) <- cn[res$pivot]
    class(res) <- "qr"
    res
}

## + qr.lm  method defined in ../../stats/R/lm.R


qr.coef <- function(qr, y)
{
    if( !is.qr(qr) ) stop("first argument must be a QR decomposition")
    n <- as.integer(nrow(qr$qr)); if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(qr$qr)"))
    p <- as.integer(ncol(qr$qr)); if(is.na(p)) stop(gettextf("invalid '%s' value", "ncol(qr$qr)"))
    k <- as.integer(qr$rank);     if(is.na(k)) stop(gettextf("invalid '%s' value", "ncol(qr$rank)"))
    im <- is.matrix(y)
    if (!im) y <- as.matrix(y)
    ny <- as.integer(ncol(y))
    if(is.na(ny)) stop(gettextf("invalid '%s' value", "ncol(y)"))
    if(nrow(y) != n) stop("'qr' and 'y' must have the same number of rows")
    isC <- is.complex(qr$qr)
    coef <- matrix(if(isC) NA_complex_ else NA_real_, p, ny)
    ix <- if (p > n) c(seq_len(n), rep(NA, p - n)) else seq_len(p)
    if(!is.null(nam <- colnames(qr$qr))) pivotted <- NA
    if (p == 0L) {
	pivotted <- FALSE
    } else if(isC) {
	coef[qr$pivot, ] <- .Internal(qr_coef_cmplx(qr, y))[ix, ]
    } else if(isTRUE(attr(qr, "useLAPACK"))) {
	coef[qr$pivot, ] <- .Internal(qr_coef_real(qr, y))[ix, ]
    } else if (k > 0L) { ## else "Linpack" case, k > 0 :
	storage.mode(y) <- "double"
	z <- .Fortran(.F_dqrcf,
		      as.double(qr$qr),
		      n, k,
		      as.double(qr$qraux),
		      y,
		      ny,
		      coef = matrix(0, nrow = k,ncol = ny),
		      info = integer(1L),
		      NAOK = TRUE)[c("coef","info")]
	if(z$info) stop("exact singularity in 'qr.coef'")
	pivotted <- k < p
	if(pivotted)
	    coef[qr$pivot[seq_len(k)], ] <- z$coef
	else coef                        <- z$coef
    }
    ## else k == 0
    ## In all cases, fixup dimnames (and drop to vector when y was):
    if(!is.null(nam)) {
	if(is.na(pivotted)) pivotted <- is.unsorted(qr$pivot)
	if(pivotted)
	    rownames(coef)[qr$pivot] <- nam
	else # faster
	    rownames(coef)           <- nam
    }
    if(im && !is.null(nam <- colnames(y)))
	colnames(coef) <- nam
    if(im) coef else drop(coef)
}

qr.qy <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr))
        return(.Internal(qr_qy_cmplx(qr, as.matrix(y), FALSE)))
    if(isTRUE(attr(qr, "useLAPACK")))
        return(.Internal(qr_qy_real(qr, as.matrix(y), FALSE)))

    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(qr$qr)"))
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop(gettextf("invalid '%s' value", "NCOL(y)"))
    storage.mode(y) <- "double"
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    .Fortran(.F_dqrqy,
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qy = y# incl. {dim}names
	     )$qy
}

qr.qty <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr))
        return(.Internal(qr_qy_cmplx(qr, as.matrix(y), TRUE)))
    if(isTRUE(attr(qr, "useLAPACK")))
        return(.Internal(qr_qy_real(qr, as.matrix(y), TRUE)))

    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(qr$qr)"))
    k <- as.integer(qr$rank)
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop(gettextf("invalid '%s' value", "NCOL(y)"))
    if(NROW(y) != n)
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrqty,
	     as.double(qr$qr),
	     n, k,
	     as.double(qr$qraux),
	     y,
	     ny,
	     qty = y# incl. {dim}names
             )$qty
}

qr.resid <- function(qr, y)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    if(isTRUE(attr(qr, "useLAPACK"))) stop("not supported for LAPACK QR")
    k <- as.integer(qr$rank)
    if (k==0) return(y)
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(qr$qr)"))
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop(gettextf("invalid '%s' value", "NCOL(y)"))
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrrsd,
	     as.double(qr$qr), n, k, as.double(qr$qraux), y, ny, rsd = y)$rsd
}

qr.fitted <- function(qr, y, k=qr$rank)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    if(is.complex(qr$qr)) stop("not implemented for complex 'qr'")
    if(isTRUE(attr(qr, "useLAPACK"))) stop("not supported for LAPACK QR")
    n <- as.integer(nrow(qr$qr))
    if(is.na(n)) stop(gettextf("invalid '%s' value", "nrow(qr$qr)"))
    k <- as.integer(k)
    if(k > qr$rank) stop("'k' is too large")
    ny <- as.integer(NCOL(y))
    if(is.na(ny)) stop(gettextf("invalid '%s' value", "NCOL(y)"))
    if( NROW(y) != n )
	stop("'qr' and 'y' must have the same number of rows")
    storage.mode(y) <- "double"
    .Fortran(.F_dqrxb,
	     as.double(qr$qr), n, k, as.double(qr$qraux), y, ny, xb = y)$xb
}

## qr.solve is defined in  ./solve.R

##---- The next three are from Doug Bates ('st849'):
qr.Q <- function (qr, complete = FALSE, Dvec)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    dqr <- dim(qr$qr)
    n <- dqr[1L]
    cmplx <- mode(qr$qr) == "complex"
    if(missing(Dvec))
	Dvec <- rep.int(if (cmplx) 1 + 0i else 1,
			if (complete) n else min(dqr))
    D <-
	if (complete) diag(Dvec, n)
	else {
	    ncols <- min(dqr)
	    diag(Dvec[seq_len(ncols)], nrow = n, ncol = ncols)
	}
    qr.qy(qr, D)
}

qr.R <- function (qr, complete = FALSE)
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    R <- qr$qr
    if (!complete)
	R <- R[seq.int(min(dim(R))), , drop = FALSE]
    R[row(R) > col(R)] <- 0
    R
}

qr.X <- function (qr, complete = FALSE,
		  ncol = if (complete) nrow(R) else min(dim(R)))
{
    if(!is.qr(qr)) stop("argument is not a QR decomposition")
    pivoted <- !identical(qr$pivot, ip <- seq_along(qr$pivot))
    R <- qr.R(qr, complete = TRUE)
    if(pivoted && ncol < length(qr$pivot))
        stop("need larger value of 'ncol' as pivoting occurred")
    cmplx <- mode(R) == "complex"
    p <- as.integer(dim(R)[2L])
    if(is.na(p)) stop(gettextf("invalid '%s' value", "NCOL(R)"))
    if (ncol < p)
	R <- R[, seq_len(ncol), drop = FALSE]
    else if (ncol > p) {
	tmp <- diag(if (!cmplx) 1 else 1 + 0i, nrow(R), ncol)
	tmp[, seq_len(p)] <- R
	R <- tmp
    }
    res <- qr.qy(qr, R)
    cn <- colnames(res)
    if(pivoted) {# res may have more columns than length(qr$pivot)
	res[, qr$pivot] <- res[, ip]
        if(!is.null(cn)) colnames(res)[qr$pivot] <- cn[ip]
    }
    res
}
