#  File src/library/base/R/connections.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

stdin <- function() .Internal(stdin())
stdout <- function() .Internal(stdout())
stderr <- function() .Internal(stderr())

nullfile <- function()
    if (.Platform$OS.type == "windows") "nul:" else "/dev/null"

isatty <- function(con) {
    if (!inherits(con, "terminal")) FALSE
    else .Internal(isatty(con))
}

readLines <- function(con = stdin(), n = -1L, ok = TRUE, warn = TRUE,
                      encoding = "unknown", skipNul = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    .Internal(readLines(con, n, ok, warn, encoding, skipNul))
}


writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE)
{
    if(!is.character(text))
        stop("can only write character objects")
    if(is.character(con)) {
        con <- file(con, "w")
        on.exit(close(con))
    }
    .Internal(writeLines(text, con, sep, useBytes))
}

open <- function(con, ...)
    UseMethod("open")

open.connection <- function(con, open = "r", blocking = TRUE, ...)
    .Internal(open(con, open, blocking))

isOpen <- function(con, rw = "")
{
    rw <- pmatch(rw, c("read", "write"), 0L)
    .Internal(isOpen(con, rw))
}

isIncomplete <- function(con)
    .Internal(isIncomplete(con))

isSeekable <- function(con)
    .Internal(isSeekable(con))

close <- function(con, ...)
    UseMethod("close")

close.connection <- function (con, type = "rw", ...)
    .Internal(close(con, type))

flush <- function(con) UseMethod("flush")

flush.connection <- function (con)
    .Internal(flush(con))

file <- function(description = "", open = "", blocking = TRUE,
                 encoding = getOption("encoding"), raw = FALSE,
                 method = getOption("url.method", "default")) {
    .Internal(file(description, open, blocking, encoding, method, raw))
}
pipe <- function(description, open = "", encoding = getOption("encoding"))
    .Internal(pipe(description, open, encoding))

fifo <- function(description, open = "", blocking = FALSE,
                 encoding = getOption("encoding"))
    .Internal(fifo(description, open, blocking, encoding))

url <- function(description, open = "", blocking = TRUE,
                encoding = getOption("encoding"),
                method = getOption("url.method", "default"), headers = NULL)
{
    method <- match.arg(method, c("default", "internal", "libcurl", "wininet"))
    if(!is.null(headers)) {
        nh <- names(headers)
        if(length(nh) != length(headers) || any(nh == "") || anyNA(headers) || anyNA(nh))
            stop("'headers' must have names and must not be NA")
        headers <- paste0(nh, ": ", headers)
        headers <- list(headers, paste0(headers, "\r\n", collapse = ""))
    }
    .Internal(url(description, open, blocking, encoding, method, headers))
}

gzfile <- function(description, open = "",
                   encoding = getOption("encoding"), compression = 6)
    .Internal(gzfile(description, open, encoding, compression))

unz <- function(description, filename, open = "",
                encoding = getOption("encoding"))
    .Internal(unz(paste(description, filename, sep=":"), open, encoding))

bzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 9)
    .Internal(bzfile(description, open, encoding, compression))

xzfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 6)
    .Internal(xzfile(description, open, encoding, compression))

zstdfile <- function(description, open = "", encoding = getOption("encoding"),
                   compression = 9)
    .Internal(zstdfile(description, open, encoding, compression))

socketConnection <- function(host = "localhost", port, server = FALSE,
                             blocking = FALSE, open = "a+",
                             encoding = getOption("encoding"),
                             timeout = getOption("timeout"),
                             options = getOption("socketOptions"))
    .Internal(socketConnection(host, port, server, blocking, open, encoding,
                               timeout, options))

socketAccept <- function(socket, blocking = FALSE, open = "a+",
                         encoding = getOption("encoding"),
                         timeout = getOption("timeout"),
                         options = getOption("socketOptions"))
    .Internal(socketAccept(socket, blocking, open, encoding, timeout, options))

serverSocket <- function(port)
    .Internal(serverSocket(port))

socketTimeout <- function(socket, timeout = -1)
    .Internal(socketTimeout(socket, timeout))

rawConnection <- function(object, open = "r") {
    .Internal(rawConnection(deparse(substitute(object)), object, open))
}

rawConnectionValue <- function(con) .Internal(rawConnectionValue(con))

textConnection <- function(object, open = "r", local = FALSE,
                           name = deparse1(substitute(object)),
                           encoding = c("", "bytes", "UTF-8"))
{
    env <- if (local) parent.frame() else .GlobalEnv
    type <- match(match.arg(encoding), c("", "bytes", "UTF-8"))
    if(!(is.character(name) && length(name) == 1))
        stop(gettextf("'%s' must be a character string", "name"), domain = NA)
    .Internal(textConnection(name, object, open, env, type))
}

textConnectionValue <- function(con) .Internal(textConnectionValue(con))

seek <- function(con, ...)
    UseMethod("seek")

seek.connection <- function(con, where = NA, origin = "start", rw = "", ...)
{
    origin <- pmatch(origin, c("start", "current", "end"))
    rw <- pmatch(rw, c("read", "write"), 0L)
    if(is.na(origin))
        stop("'origin' must be one of 'start', 'current' or 'end'")
    .Internal(seek(con, as.double(where), origin, rw))
}

truncate <- function(con, ...)
    UseMethod("truncate")

truncate.connection <- function(con, ...)
{
    if(!isOpen(con)) stop("can only truncate an open connection")
    .Internal(truncate(con))
}

pushBack <- function(data, connection, newLine = TRUE,
                     encoding = c("", "bytes", "UTF-8"))
{
    # match.arg doesn't work on "" default
    if (length(encoding) > 1L) encoding <- encoding[1]
    if (nzchar(encoding)) encoding <- match.arg(encoding)
    type <- match(encoding, c("", "bytes", "UTF-8"))
    .Internal(pushBack(data, connection, newLine, type))
}

pushBackLength <- function(connection)
    .Internal(pushBackLength(connection))

clearPushBack <- function(connection)
    .Internal(clearPushBack(connection))

print.connection <- function(x, ...)
{
    usumm <- tryCatch(unlist(summary(x)), error = function(e) {})
    ## could also show  as.numeric(x) {as str() currently does}
    if(is.null(usumm)) {
	cl <- oldClass(x); cl <- cl[cl != "connection"]
	cat("A connection, ",
	    if(length(cl)) paste0("specifically, ",
				  paste(sQuote(cl), collapse=", "), ", "),
	    "but invalid.\n", sep = "")
    } else {
	cat("A connection with") # {newline from print() below}
	print(cbind(` ` = usumm), ...)
    }
    invisible(x)
}

summary.connection <- function(object, ...)
    .Internal(summary.connection(object))

showConnections <- function(all = FALSE)
{
    gc() # to run finalizers
    set <- getAllConnections()
    if(!all) set <- set[set > 2L]
    ans <- matrix("", length(set), 7L)
    for(i in seq_along(set)) ans[i, ] <- unlist(summary.connection(set[i]))
    rownames(ans) <- set
    colnames(ans) <- c("description", "class", "mode", "text", "isopen",
                       "can read", "can write")
    if(!all) ans[ans[, 5L] == "opened", , drop = FALSE]
    else ans[, , drop = FALSE]
}

## undocumented
getAllConnections <- function() .Internal(getAllConnections())

getConnection <- function(what) .Internal(getConnection(what))

closeAllConnections <- function()
{
    ## first re-divert any diversion of stderr.
    i <- sink.number(type = "message")
    if(i != 2L) sink(stderr(), type = "message")
    ## now unwind the sink diversion stack.
    n <- sink.number()
    if(n > 0L) for(i in seq_len(n)) sink()
    gc() # to run finalizers
    ## get all the open connections.
    set <- getAllConnections()
    set <- set[set > 2L]
    ## and close all user connections.
    for(i in seq_along(set)) close(getConnection(set[i]))
    invisible()
}

readBin <- function(con, what, n = 1L, size = NA_integer_, signed = TRUE,
                    endian = .Platform$endian)
{
    if (!endian %in% c("big", "little", "swap"))
        stop(gettextf("invalid '%s' argument", "endian"))
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    swap <- endian != .Platform$endian
    if(!is.character(what) || is.na(what) ||
       length(what) != 1L || ## hence length(what) == 1:
       !any(what == c("numeric", "double", "integer", "int", "logical",
	    "complex", "character", "raw")))
	what <- typeof(what)
    .Internal(readBin(con, what, n, size, signed, swap))
}

writeBin <-
    function(object, con, size = NA_integer_, endian = .Platform$endian,
             useBytes = FALSE)
{
    if (!endian %in% c("big", "little", "swap"))
        stop(gettextf("invalid '%s' argument", "endian"))
    swap <- endian != .Platform$endian
    if(!is.vector(object) || mode(object) == "list")
        stop("can only write vector objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeBin(object, con, size, swap, useBytes))
}

readChar <- function(con, nchars, useBytes = FALSE)
{
    if(is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    .Internal(readChar(con, as.integer(nchars), useBytes))
}

writeChar <- function(object, con, nchars = nchar(object, type="chars"),
                      eos = "", useBytes = FALSE)
{
    if(!is.character(object))
        stop("can only write character objects")
    if(is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeChar(object, con, as.integer(nchars), eos, useBytes))
}

gzcon <- function(con, level = 6, allowNonCompressed = TRUE, text = FALSE)
    .Internal(gzcon(con, level, allowNonCompressed, text))

socketSelect <- function(socklist, write = FALSE, timeout = NULL) {
    if (is.null(timeout))
        timeout <- -1
    else if (timeout < 0)
        stop("'timeout' must be NULL or a non-negative number")
    if (length(write) < length(socklist))
        write <- rep_len(write, length(socklist))
    .Internal(sockSelect(socklist, write, timeout))
}

memCompress <-
    function(from, type = c("gzip", "bzip2", "xz", "zstd", "none"))
{
    if(is.character(from))
        from <- charToRaw(paste(from, collapse = "\n"))
    else if(!is.raw(from)) stop("'from' must be raw or character")
    type <- match(match.arg(type), c("none", "gzip", "bzip2", "xz", "zstd"))
    .Internal(memCompress(from, type))
}

memDecompress <-
    function(from,
             type = c("unknown", "gzip", "bzip2", "xz", "zstd", "none"),
             asChar = FALSE)
{
    type <- match(match.arg(type),
                  c("none", "gzip", "bzip2", "xz", "unknown", "zstd"))
    ans <- .Internal(memDecompress(from, type))
    if(asChar) rawToChar(ans) else ans
}
