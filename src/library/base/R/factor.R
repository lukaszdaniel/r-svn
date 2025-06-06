#  File src/library/base/R/factor.R
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

factor <- function(x = character(), levels, labels = levels,
                   exclude = NA, ordered = is.ordered(x), nmax = NA)
{
    if(is.null(x)) x <- character()
    nx <- names(x)
    matchAsChar <- is.object(x) ||
        !(is.character(x) || is.integer(x) || is.logical(x))
    if (missing(levels)) {
	y <- unique(x, nmax = nmax)
	ind <- order(y)
	if (matchAsChar)
	    y <- as.character(y)
	levels <- unique(y[ind])
    }
    force(ordered) # check if original x is an ordered factor
    if (matchAsChar)
	x <- as.character(x)
    ## levels could be a long vector, but match will not handle that.
    levels <- levels[is.na(match(levels, exclude))]
    f <- match(x, levels)
    if(!is.null(nx))
	names(f) <- nx
    if(missing(labels)) { ## default: labels := levels
	levels(f) <- as.character(levels)
    } else { ## labels specified explicitly
	nlab <- length(labels)
	if(nlab == length(levels)) { ## NB: duplicated labels should work
	    ## a version of  f <- `levels<-.factor`(f, as.character(labels))
	    ## ... but not dropping NA :
	    nlevs <- unique(xlevs <- as.character(labels))
	    at <- attributes(f)
	    at$levels <- nlevs
	    f <- match(xlevs, nlevs)[f]
	    attributes(f) <- at
	}
	else if(nlab == 1L)
	    levels(f) <- paste0(labels, seq_along(levels))
	else ## nlab is neither 1 nor length(levels)
	    stop(gettextf("invalid 'labels'; length %d should be 1 or %d",
			  nlab, length(levels)),
		 domain = NA)
    }
    class(f) <- c(if(ordered) "ordered", "factor")
    f
}


## Also used for methods::validObject(<factor>) :
.valid.factor <- function(object) {
    levs <- levels(object)
    if (!is.character(levs))
        return("factor levels must be \"character\"")
    if (d <- anyDuplicated(levs))
	return(sprintf("duplicated level [%d] in factor", d))
    ## 'else'	ok :
    TRUE
}

is.factor <- function(x) inherits(x, "factor")

as.factor <- function(x) {
    if (is.factor(x)) x
    else if (!is.object(x) && is.integer(x)) {
        ## optimization for calls from tapply via split.default
        levels <- sort.int(unique.default(x)) # avoid array methods
        f <- match(x, levels)
        levels(f) <- as.character(levels)
	if(!is.null(nx <- names(x))) names(f) <- nx
        class(f) <- "factor"
        f
    } else factor(x)
}

levels <- function(x) UseMethod("levels")
levels.default <- function(x) attr(x, "levels")
nlevels <- function(x) length(levels(x))

`levels<-.factor` <- function(x, value)
{
    xlevs <- levels(x)
    if (is.list(value)) {
        nlevs <- rep.int(names(value), lapply(value, length))
        value <- unlist(value)
        m <- match(value, xlevs, nomatch = 0L)
        xlevs[m] <- nlevs[m > 0L]
    } else {
        if (length(xlevs) > length(value))
            stop("number of levels differs")
        nlevs <- xlevs <- as.character(value)
        nlevs <- nlevs[!is.na(nlevs)]
    }
    ## take care here not to drop attributes, including class.
    ## factor(xlevs[x], levels = unique(nlevs))
    nlevs <- unique(nlevs)
    at <- attributes(x)
    at$levels <- nlevs
    y <- match(xlevs, nlevs)[x]
    attributes(y) <- at
    y
}

droplevels <- function(x, ...) UseMethod("droplevels")
## default 'exclude' matches `[.factor` (drop=TRUE)
droplevels.factor <- function(x, exclude = if(anyNA(levels(x))) NULL else NA, ...)
    factor(x, exclude = exclude)

droplevels.data.frame <- function(x, except = NULL, exclude, ...)
  {
    ix <- vapply(x, is.factor, NA)
    if (!is.null(except)) ix[except] <- FALSE
    x[ix] <- if(missing(exclude))
		  lapply(x[ix], droplevels)
	     else lapply(x[ix], droplevels, exclude=exclude)
    x
  }

as.vector.factor <- function(x, mode="any")
{
    if(mode=="list") as.list(x)
    else if(mode== "any" || mode== "character" || mode== "logical")
	as.vector(levels(x)[x], mode)
    else
	as.vector(unclass(x), mode)
}

as.character.factor <- function(x,...) .Internal(asCharacterFactor(x))

as.logical.factor <- function(x,...) as.logical(levels(x))[x]

as.list.factor <- function(x,...)
{
    res <- vector("list", length(x))
    for(i in seq_along(x)) res[[i]] <- x[[i]]
    if(is.null(names(x))) res else `names<-`(res, names(x))
}

## for `factor' *and* `ordered' :
print.factor <- function (x, quote = FALSE, max.levels = NULL,
                          width = getOption("width"), ...)
{
    ord <- is.ordered(x)
    if (length(x) == 0L)
        cat(if(ord)"ordered" else "factor", "()\n", sep = "")
    else {
        xx <- character(length(x))
        xx[] <- as.character(x)
        keepAttrs <- setdiff(names(attributes(x)), c("levels", "class"))
        attributes(xx)[keepAttrs] <- attributes(x)[keepAttrs]
        print(xx, quote = quote, ...)
    }
    maxl <- max.levels %||% TRUE
    if (maxl) {
        n <- length(lev <- encodeString(levels(x), quote=ifelse(quote, '"', '')))
        colsep <- if(ord) " < " else " "
        T0 <- "Levels: "
        if(is.logical(maxl))
            maxl <- { ## smart default
                width <- width - (nchar(T0, "w") + 3L + 1L + 3L)
                                        # 3='...', 3=#lev, 1=extra
                lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))
                if(n <= 1L || lenl[n] <= width) n
		else max(1L, which.max(lenl > width) - 1L)
            }
        drop <- n > maxl
        cat(if(drop) paste(format(n), ""), T0,
            paste(if(drop)c(lev[1L:max(1,maxl-1)],"...",if(maxl > 1) lev[n])
                      else lev, collapse = colsep),
            "\n", sep = "")
    }
    if(!isTRUE(val <- .valid.factor(x)))
	warning(val) # stop() in the future
    invisible(x)
}


Math.factor <- function(x, ...)
    stop(gettextf("%s not meaningful for factors", sQuote(.Generic)))

## The next two have an .ordered method:
Summary.factor <- function(..., na.rm)
    stop(gettextf("%s not meaningful for factors", sQuote(.Generic)))

Ops.factor <- function(e1, e2)
{
    ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
    if(!ok) {
	warning(gettextf("%s not meaningful for factors", sQuote(.Generic)))
	return(rep.int(NA, max(length(e1), if(!missing(e2)) length(e2))))
    }
    ## Need this for NA *levels* as opposed to missing
    noNA.levels <- function(f) {
	r <- levels(f)
	if(any(ina <- is.na(r))) {
	    n <- "  NA "
	    while(n %in% r) n <- paste(n, ".")
	    r[ina] <- n
	}
	r
    }
    if (nzchar(.Method[1L])) { # e1 *is* a factor
        ## fastpath for factor w/ no NA levels vs scalar character
        if(!anyNA(levels(e1)) && is.character(e2) && length(e2) == 1L) {
            if(.Generic == "==") {
                ## if e1[i] OR e2 is NA then (leq[e1])[i] is NA
                ## as desired
                leq <- (levels(e1) == e2)
                return(leq[e1])
            } else { ## != case
                leq <- (levels(e1) != e2)
                return(leq[e1])
            }
        }
	l1 <- noNA.levels(e1)
	e1 <- l1[e1]
    }
    if (nzchar(.Method[2L])) { # e2 *is* a factor
        ## fastpath for factor w/ no NA levels vs scalar character
        if(!anyNA(levels(e2)) && is.character(e1) && length(e1) == 1L){
            if(.Generic == "==") {
                leq <- (levels(e2) == e1)
                return(leq[e2])
            } else {  ## != case
                leq <- (levels(e2) != e1)
                return(leq[e2])
            }
        }
	l2 <- noNA.levels(e2)
	e2 <- l2[e2]
    }
    if (all(nzchar(.Method)) &&
	(length(l1) != length(l2) || !all(sort.int(l2) == sort.int(l1))))
	stop("level sets of factors are different")
    value <- NextMethod(.Generic)
    nas <- is.na(e1) | is.na(e2)
    value[nas] <- NA
    value
}

## NB for next four:
## a factor has levels before class in attribute list (PR#6799)
`[.factor` <- function(x, ..., drop = FALSE)
{
    y <- NextMethod("[")
    attr(y,"contrasts") <- attr(x,"contrasts")
    attr(y,"levels") <- attr(x,"levels")
    class(y) <- oldClass(x)
    if (drop)
        factor(y, exclude = if(anyNA(levels(x))) NULL else NA ) else y
}

`[<-.factor` <- function(x, ..., value)
{
    lx <- levels(x)
    cx <- oldClass(x)
    if (is.factor(value)) value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value)))
	warning("invalid factor level, NA generated")
    class(x) <- NULL
    x[...] <- m
    attr(x,"levels") <- lx
    class(x) <- cx
    x
}

`[[.factor` <- function(x, ...)
{
    y <- NextMethod("[[")
    attr(y,"contrasts") <- attr(x,"contrasts")
    attr(y,"levels") <- attr(x,"levels")
    class(y) <- oldClass(x)
    y
}

## added for 2.12.0
`[[<-.factor` <- function(x, ..., value)
{
    lx <- levels(x)
    cx <- oldClass(x)
    if (is.factor(value)) value <- levels(value)[value]
    m <- match(value, lx)
    if (any(is.na(m) & !is.na(value)))
	warning("invalid factor level, NA generated")
    class(x) <- NULL
    x[[...]] <- m
    attr(x,"levels") <- lx
    class(x) <- cx
    x
}


## ordered factors ...

ordered <- function(x = character(), ...) factor(x, ..., ordered=TRUE)

is.ordered <- function(x) inherits(x, "ordered")
as.ordered <- function(x) if(is.ordered(x)) x else ordered(x)

Ops.ordered <- function (e1, e2)
{
    ok <- switch(.Generic,
		 "<" = , ">" = , "<=" = , ">=" = ,"=="=, "!=" =TRUE,
		 FALSE)
    if(!ok) {
	warning(sprintf("'%s' is not meaningful for ordered factors",
                        .Generic))
	return(rep.int(NA, max(length(e1), if(!missing(e2)) length(e2))))
    }
    if (.Generic %in% c("==", "!="))
      return(NextMethod(.Generic))  ##not S-PLUS compatible, but saner
    nas <- is.na(e1) | is.na(e2)
    ord1 <- FALSE
    ord2 <- FALSE
    if (nzchar(.Method[1L])) {
	l1 <- levels(e1)
	ord1 <- TRUE
    }
    if (nzchar(.Method[2L])) {
	l2 <- levels(e2)
	ord2 <- TRUE
    }
    if (all(nzchar(.Method)) &&
        (length(l1) != length(l2) || !all(l2 == l1)))
	stop("level sets of factors are different")
    if (ord1 && ord2) {
	e1 <- as.integer(e1) # was codes, but same thing for ordered factor.
	e2 <- as.integer(e2)
    }
    else if (!ord1) {
	e1 <- match(e1, l2)
	e2 <- as.integer(e2)
    }
    else if (!ord2) {
	e2 <- match(e2, l1)
	e1 <- as.integer(e1)
    }
    value <- get(.Generic, mode = "function")(e1, e2)
    value[nas] <- NA
    value
}

Summary.ordered <- function(..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE,
		 FALSE)
    if (!ok)
	stop(gettextf("'%s' not defined for ordered factors", .Generic),
	     domain = NA)
    args <- list(...)
    levl <- lapply(args, levels)
    levset <- levl[[1]]
    if (!all(vapply(args, is.ordered, NA)) ||
	!all(vapply(levl, identical, NA, levset)))
	stop(gettextf("'%s' is only meaningful for ordered factors if all arguments have the same level sets",
		      .Generic))
    codes <- lapply(args, as.integer)
    ind <- do.call(.Generic, c(codes, na.rm = na.rm))
    ordered(levset[ind], levels = levset)
}

`is.na<-.factor` <- function(x, value)
{
    lx <- levels(x)
    cx <- oldClass(x)
    class(x) <- NULL
    x[value] <- NA
    structure(x, levels = lx, class = cx)
}

`length<-.factor` <- function(x, value)
{
    cl <- class(x)
    levs <- levels(x)
    x <- NextMethod()
    structure(x, levels=levs, class=cl)
}

addNA <- function(x, ifany=FALSE)
{
    if (!is.factor(x)) x <- factor(x)
    if (ifany && !anyNA(x)) return(x)
    ll <- levels(x)
    if (!anyNA(ll)) ll <- c(ll, NA)
    else if (!ifany && !anyNA(x)) return(x)
    factor(x, levels=ll, exclude=NULL)
}

c.factor <- function(..., recursive=TRUE) {
    x <- list(...)
    y <- unlist(x, recursive = recursive)
    if(inherits(y, "factor") &&
       all(vapply(x, inherits, NA, "ordered")) &&
       (length(unique(lapply(x, levels))) == 1L))
        class(y) <- c("ordered", "factor")
    y
}
