#  File src/library/utils/R/edit.R
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

## the default message is used by grDevices' X11() and quartz()
## (and in the "warn" case captured by R CMD check's run_examples)
check_screen_device <- function (where = "screen devices")
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- sprintf("%s should not be used in examples etc", where)
    if (identical(check, "stop"))
        stop(msg, domain = NA)
    else if (identical(check, "warn"))
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
}

dataentry <- function (data, modes)
{
    check_screen_device("dataentry()")

    if(!is.list(data) || !length(data) || !all(vapply(data, is.vector, NA)))
        stop(gettextf("invalid '%s' argument", "data"))
    if(!is.list(modes) ||
       (length(modes) && !all(vapply(modes, is.character, NA))))
        stop(gettextf("invalid '%s' argument", "modes"))
    if (grepl("darwin", R.version$os))
        check_for_XQuartz(file.path(R.home("modules"), "R_de.so"))
    .External2(C_dataentry, data, modes)
}

View <- function (x, title)
{
    check_screen_device("View()")

    ## could multi-line deparse with maliciously-designed inputs
    if(missing(title)) title <- paste("Data:", deparse(substitute(x))[1])
    x0 <- as.data.frame(x)
    x <- as.list(format.data.frame(x0))
    rn <- row.names(x0)
    if(any(rn != seq_along(rn))) x <- c(list(row.names = rn), x)
    if(!is.list(x) || !length(x) || !all(vapply(x, is.atomic, NA)) ||
       !max(lengths(x)))
        stop(gettextf("invalid '%s' argument", "x"))
    if (grepl("darwin", R.version$os))
        check_for_XQuartz(file.path(R.home("modules"), "R_de.so"))
    invisible(.External2(C_dataviewer, x, title))
}

edit <- function(name,...)UseMethod("edit")

edit.default <-
    function (name = NULL, file = "", title = NULL,
              editor = getOption("editor"), ...)
{
    ## editor could be set to "cat" or similar, or a function handling the
    ## non-interactive case, so we don't check_screen_device("edit()") here

    if (is.null(title)) title <- deparse1(substitute(name))
    if (is.function(editor)) invisible(editor(name = name, file = file, title = title))
    else .External2(C_edit, name, file, title, editor)
}

edit.data.frame <-
    function(name, factor.mode = c("character", "numeric"),
             edit.row.names =  any(row.names(name) != 1L:nrow(name)), ...)
{
    if (.Platform$OS.type == "unix"  && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY") == "" )
            return (edit.default(name, ...))

    check_screen_device("edit()")

    is.vector.unclass <- function(x) is.vector(unclass(x))
    if (length(name) && !all(vapply(name, is.vector.unclass, NA)
                                 | vapply(name, is.factor, NA)))
        stop("can only handle vector and factor elements")

    if (grepl("darwin", R.version$os))
        check_for_XQuartz(file.path(R.home("modules"), "R_de.so"))

    factor.mode <- match.arg(factor.mode)

    as.num.or.char <- function(x)
    {
        if (is.numeric(x)) x
        else if (is.factor(x) && factor.mode == "numeric") as.numeric(x)
        else as.character(x)
    }

    attrlist <- lapply(name, attributes)
    datalist <- lapply(name, as.num.or.char)
    factors  <- which(vapply(name, is.factor, NA))
    logicals <- which(vapply(name, is.logical, NA))

    if(length(name)) {
        has_class <-
            vapply(name, function(x) (is.object(x) || isS4(x)) && !is.factor(x), NA)
        if(any(has_class))
            warning(sprintf(ngettext(sum(has_class),
                                    "class discarded from column %s",
                                    "classes discarded from columns %s"),
                            paste(sQuote(names(name)[has_class]),
                                  collapse=", ")),
                    domain = NA, call. = FALSE, immediate. = TRUE)
    }

    modes <- lapply(datalist, mode)
    if (edit.row.names) {
        datalist <- c(list(row.names = row.names(name)), datalist)
        modes <- c(list(row.names = "character"), modes)
    }
    rn <- attr(name, "row.names")

    out <- .External2(C_dataentry, datalist, modes)
    if(length(out) == 0L) {
        ## e.g. started with 0-col data frame or NULL, and created no cols
        return (name)
    }
    lengths <- lengths(out)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste0("row", (ln+1):maxlength))
    } else if(length(rn) != maxlength) rn <- seq_len(maxlength)
    for (i in factors) {
        if(factor.mode != mode(out[[i]])) next # user might have switched mode
        a <- attrlist[[i]]
        if (factor.mode == "numeric") {
            o <- as.integer(out[[i]])
            ok <- is.na(o) | (o > 0 & o <= length(a$levels))
            if (any(!ok)) {
                warning(gettextf("invalid factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o[!ok] <- NA
            }
	    attributes(o) <- a
        } else {
            o <- out[[i]]
            if (any(new <- is.na(match(o, c(a$levels, NA_integer_))))) {
                new <- unique(o[new])
                warning(gettextf("added factor levels in '%s'", names(out)[i]),
                        domain = NA)
                o <- factor(o, levels=c(a$levels, new),
                            ordered = is.ordered(o))
            } else {
                o <- match(o, a$levels)
                attributes(o) <- a
            }
        }
        out[[i]] <- o
    }
    for (i in logicals) out[[i]] <- as.logical(out[[i]])

    attr(out, "row.names") <- rn
    attr(out, "class") <- "data.frame"
    if (edit.row.names) {
        if(anyDuplicated(rn)) {
            warning("edited row names contain duplicates and will be ignored")
            attr(out, "row.names") <- seq_len(maxlength)
        }
    }
    out
}

edit.matrix <-
    function(name, edit.row.names = !is.null(dn[[1L]]), ...)
{
    if (.Platform$OS.type == "unix" && .Platform$GUI != "AQUA")
        if(.Platform$GUI == "unknown" || Sys.getenv("DISPLAY")=="" )
            return (edit.default(name, ...))

    check_screen_device("edit()")

    if(!is.matrix(name) ||
       ! mode(name) %in% c("numeric", "character", "logical") ||
       any(dim(name) < 1))
        stop("invalid input matrix")

    if (grepl("darwin", R.version$os))
        check_for_XQuartz(file.path(R.home("modules"), "R_de.so"))

    ## logical matrices will be edited as character
    logicals <- is.logical(name)
    if (logicals) mode(name) <- "character"
    if(is.object(name) || isS4(name))
        warning("class of 'name' will be discarded",
                call. = FALSE, immediate. = TRUE)

    dn <- dimnames(name)
    ## <FIXME split.matrix>
    datalist <- split(c(name), col(name))
    if(!is.null(dn[[2L]])) names(datalist) <- dn[[2L]]
    else names(datalist) <- paste0("col", 1L:ncol(name))
    modes <- as.list(rep.int(mode(name), ncol(name)))
    ## guard against user error (PR#10500)
    if(edit.row.names && is.null(dn[[1L]]))
        stop("cannot edit NULL row names")
    if (edit.row.names) {
        datalist <- c(list(row.names = dn[[1L]]), datalist)
        modes <- c(list(row.names = "character"), modes)
    }

    out <- .External2(C_dataentry, datalist, modes)

    lengths <- lengths(out)
    maxlength <- max(lengths)
    if (edit.row.names) rn <- out[[1L]]
    for (i in which(lengths != maxlength))
         out[[i]] <- c(out[[i]], rep.int(NA, maxlength - lengths[i]))
    if (edit.row.names) {
        out <- out[-1L]
        if((ln <- length(rn)) < maxlength)
            rn <- c(rn, paste0("row", (ln+1L):maxlength))
    }
    out <- do.call("cbind", out)
    if (edit.row.names)
        rownames(out) <- rn
    else if(!is.null(dn[[1L]]) && length(dn[[1L]]) == maxlength)
        rownames(out) <- dn[[1L]]
    if (logicals) mode(out) <- "logical"
    out
}

file.edit <-
  function (..., title = file, editor = getOption("editor"), fileEncoding = "")
{
    file <- path.expand(c(...))
    title <- rep_len(as.character(title), length(file))
    if(nzchar(fileEncoding) && fileEncoding != "native.enc") {
        tfile <- file
        for(i in seq_along(file)) {
            ## We won't know when that is done with
            ## so leave around for the R session.
            tfile <- tempfile()
            con <- file(file[i], encoding = fileEncoding)
            writeLines(readLines(con), tfile)
            close(con)
            file[i] <- tfile
        }
    }
    if (is.function(editor))
        ## FIXME: the docs say this is called with name, file and title args
        invisible(editor(file = file, title = title))
    else
        invisible(.External2(C_fileedit, file, title, editor))
}

vi <- function(name = NULL, file = "")
    edit.default(name, file, editor = "vi")

emacs <- function(name = NULL, file = "")
    edit.default(name, file, editor = "emacs")

xemacs <- function(name = NULL, file = "")
    edit.default(name, file, editor = "xemacs")

xedit <- function(name = NULL, file = "")
    edit.default(name, file, editor = "xedit")

pico <- function(name = NULL, file = "")
    edit.default(name, file, editor = "pico")

