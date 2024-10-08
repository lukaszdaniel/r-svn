#  File src/library/base/R/mapply.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

mapply <- function(FUN,..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    dots <- list(...)

    answer <- .Internal(mapply(FUN, dots, MoreArgs))

    if (USE.NAMES && length(dots)) {
	if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]]))
            names(answer) <- if(length(answer)) dots[[1L]] else character()
	else if (!is.null(names1))
	    names(answer) <- names1
    }
    if(!isFALSE(SIMPLIFY))
	simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}

.mapply <- function(FUN, dots, MoreArgs)
    .Internal(mapply(match.fun(FUN), dots, MoreArgs))

Vectorize <- function(FUN, vectorize.args = arg.names, SIMPLIFY = TRUE,
                      USE.NAMES = TRUE)
{
    arg.names <- as.list(formals(FUN))
    arg.names[["..."]] <- NULL
    arg.names <- names(arg.names)

    vectorize.args <- as.character(vectorize.args)

    if (!length(vectorize.args)) return(FUN)

    if (!all(vectorize.args %in% arg.names))
    	stop("must specify names of formal arguments for 'vectorize'")

    collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES",
                                   "vectorize.args")
    if (any(collisions))
	stop(sprintf(ngettext(length(collisions), "%s function may not have argument named %s", "%s function may not have arguments named %s", domain = "R-base"),
	     sQuote("FUN"), paste(sQuote(arg.names[collisions]), collapse = ", ")), domain = NA)
    rm(arg.names, collisions)
    (function() {
    FUNV <- function() { ## will set the formals below
        args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
        names <- names(args) %||% character(length(args))
        dovec <- names %in% vectorize.args
        do.call("mapply", c(FUN = FUN,
                            args[dovec],
                            MoreArgs = list(args[!dovec]),
                            SIMPLIFY = SIMPLIFY,
                            USE.NAMES = USE.NAMES))
    }
    formals(FUNV) <- formals(FUN)
    environment(FUNV) <- parent.env(environment())
    FUNV
    })()
}
