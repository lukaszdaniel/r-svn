#  File src/library/base/R/warnings.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

warnings <- function(...)
{
    structure(baseenv()[["last.warning"]] %||% list(),
              dots = list(...), class = "warnings")
}

`[.warnings` <- function(x, ...)
    structure(NextMethod("["), class = "warnings")

c.warnings <- function (..., recursive = FALSE)
    structure(NextMethod("c"), class = "warnings")

duplicated.warnings <- function(x, incomparables = FALSE, ...)
    duplicated(paste(names(x), as.character(x)), incomparables, ...)
unique.warnings <- function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

print.warnings <- function(x, tags,
                           header = ngettext(n, "Warning message:\n", "Warning messages:\n", domain = "R-base"),
                           ...)
{
    if(n <- length(x)) {
        if(length(header)) cat(header)
        if(missing(tags) || length(tags) == 0)
            tags <- if(n == 1L) "" else paste0(seq_len(n), ": ")
        else if(length(tags <- as.character(tags)) != n)
            stop("'tags' must be a character vector of the same length as 'x'")
        msgs <- names(x)
        for(i in seq_len(n)) {
            out <- if(length(x[[i]])) { ## the 'call' iff (call. = TRUE) as by default
                ## deparse can overshoot cutoff
                temp <- deparse(x[[i]], width.cutoff = 50L, nlines = 2L)
                ## Put on one line if narrow enough.
                sm <- strsplit(msgs[i], "\n")[[1L]]
                nl <- if(nchar(tags[i], "w") + nchar(temp[1L], "w") +
                         nchar(sm[1L], "w") <= (options("width")[[1]] - 5))
                    " " else "\n  "
                cmd <- paste(temp[1L], if(length(temp) > 1L) " ...", sep="", collapse="")
                paste(tags[i], gettextf("In %s", sQuote(cmd), domain = "R-base"),
                      ":", nl, msgs[i], sep = "")
            } else paste0(tags[i], msgs[i])
            do.call("cat", c(list(out), attr(x, "dots"), fill=TRUE))
        }
    }
    invisible(x)
}

summary.warnings <- function(object, ...) {
    msgs <- names(object)
    calls <- as.character(object) ## alternatively (in line with print() method above):
    ## lapply(object, deparse, width.cutoff = 50L * 2L, backtick=FALSE, control=NULL)
    ss <- " |<:>| " # << should not ever appear in 'calls'
    c.m. <- paste(calls, msgs, sep = ss)
    if(length(i.no.call <- which(calls == "NULL")))
        c.m.[i.no.call] <- substr(c.m.[i.no.call],
				  nchar(paste0("NULL", ss))+1L, 100000L)
    i.uniq <- which(!duplicated(object, incomparables=FALSE))
    tm <- table(factor(c.m., levels=c.m.[i.uniq]), deparse.level=0L)
    structure(object[i.uniq], counts = as.vector(tm), class = "summary.warnings")
}

print.summary.warnings <- function(x, ...) {
    n <- length(x)
    cn <- attr(x, "counts")
    if(n == 0)
	cat("No warnings\n")
    else if(n == 1)
	print.warnings(x, header = sprintf(ngettext(sum(cn), "%d identical warning:\n",
					 "%d identical warnings:\n", domain = "R-base"), sum(cn)))
    else ## n >= 2
	print.warnings(x, tags = paste0(format(cn), "x : "),
		       header = gettextf("Summary of (a total of %d) warning messages:\n",
					 sum(cn)))
    invisible(x)
}




##' @title Warn about extraneous arguments in the "..."	 (of its caller).
##' @author Martin Maechler, June 2012, May 2014
##' @param ...
##' @param which.call passed to sys.call().  A caller may use -2 if the message should
##' mention *its* caller
##' @param allowed not yet implemented: character vector of *named* elements in '...'
##' which are \dQuote{allowed} and hence not warned about
chkDots <- function(..., which.call = -1, allowed = character(0)) {
    if(nx <- ...length())
	warning(sprintf(ngettext(nx,
				 "In '%s':\n extra argument %s will be disregarded",
				 "In '%s':\n extra arguments %s will be disregarded", domain = "R-base"),
			paste(deparse(sys.call(which.call), control=c()), collapse="\n"),
			## sub(")$", '', sub("^list\\(", '', deparse(list(...), control=c())))
			paste(sQuote(...names()), collapse = ", ")),
		call. = FALSE, domain=NA)
}
