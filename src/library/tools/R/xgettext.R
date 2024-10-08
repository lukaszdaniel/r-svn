#  File src/library/tools/R/xgettext.R
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

xgettext <-
function(dir, verbose = FALSE, asCall = TRUE)
{
    dir <- file_path_as_absolute(dir)
    bn <- basename(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for(d in c("unix", "windows")) {
        OSdir <- file.path(dir, d)
        if(dir.exists(OSdir))
            R_files <- c(R_files, list_files_with_exts(OSdir, exts))
    }
    if(bn == "base") {
        ## include loader files in R_HOME/share/R
        shdir <- file.path(dir, "../../../../share/R")
        R_files <- c(R_files, list_files_with_exts(shdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files

    find_strings <- function(e) {
        find_strings2 <- function(e, cmd_e, suppress) {
            if(is.character(e)) {
                e <- sub("^[ \t\n]*", "", e)
                e <- sub("[ \t\n]*$", "", e)
                if(!suppress) strings <<- c(strings, list(c(msg = e, cmd = cmd_e)))
            } else if(is.call(e)) {
                cmd_e <- paste(deparse(e), sep = "", collapse = "")
                if(is.name(e[[1L]])) {
                    fname <- as.character(e[[1L]])
                    if(fname %in% c("warningCondition", "errorCondition")) {
                        e <- match.call(baseenv()[[fname]], e)
                        e <- e["message"] # ignore condition class etc
                    } else if(fname %in% c("gettext", "gettextf")) {
                        domain <- e[["domain"]]
                        suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
                        if(fname == "gettextf") {
                            e <- match.call(gettextf, e)
                            e <- e["fmt"] # just look at fmt arg
                        } else if(fname == "gettext" &&
                                  !is.null(names(e))) {
                            e <- e[!(names(e) == "domain")] # remove domain arg
                        }
                    } else if(fname == "ngettext")
                        return()
                }
                for(i in seq_along(e)) find_strings2(e[[i]], cmd_e, suppress)
            }
        }
        if(is.call(e)
           && is.name(e[[1L]])
           ## FIXME: this skips `base::`-prefixed calls
           && (as.character(e[[1L]])
               %in% c("warning", "stop", "message", "packageStartupMessage",
                      "gettext", "gettextf"))) {
             cmd_e <- paste(deparse(e), sep = "", collapse = "")
             domain <- e[["domain"]]
             suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
             ## remove named args
             if(!is.null(names(e)))
                 e <- e[names(e) %notin% c("call.", "immediate.", "domain")]
             if(asCall) {
                 e <- sub("^[ \t\n]*", "", as.character(e)[-1L])
                 e <- sub("[ \t\n]*$", "", e)
                 if(!suppress) strings <<- c(strings, list(c(msg = e, cmd = cmd_e)))
             } else {
                 if(as.character(e[[1L]]) == "gettextf") {
                     e <- match.call(gettextf, e)
                     e <- e["fmt"] # just look at fmt arg
                 }
                 for(i in seq_along(e)) find_strings2(e[[i]], cmd_e, suppress)
             }
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = NA)
        strings <- list()
        for(e in parse(file = f)) find_strings(e)
        ## strip leading and trailing white space
        out[[f]] <- structure(strings, class="xgettext")
    }

    out[lengths(out) > 0L]
}

print.xgettext <- function(x, ...)
{
    lapply(x, function(x)
           cat("\nmsgid        = ", encodeString(x[1L]),
               "\ncommand      = ", encodeString(x[2L]),
               "\n", sep = ""))
    invisible(x)
}

print.xngettext <- function(x, ...)
{
    lapply(x, function(x) {
        e <- encodeString(x)
        cat("\nmsgid        = ", e[1L],
            "\nmsgid_plural = ", e[2L],
            "\ncommand      = ", e[3L],
            "\n", sep = "")
    })
    invisible(x)
}

xngettext <-
function(dir, verbose = FALSE)
{
    dir <- file_path_as_absolute(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for(d in c("unix", "windows", "aqua")) {
        OSdir <- file.path(dir, d)
       if(dir.exists(OSdir))
            R_files <- c(R_files, list_files_with_exts(OSdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files

    find_strings <- function(e) {
        if(is.call(e) && is.name(e[[1L]])
           && as.character(e[[1L]]) %in% "ngettext") {
	    cmd_e <- paste(deparse(e), sep = "", collapse = "")
	    e <- match.call(ngettext, e)
            domain <- e[["domain"]]
            suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
	    if (!suppress &&
                is.character(e[["msg1"]]) && is.character(e[["msg2"]]))
	    	strings <<- c(strings, list(c(msg1 = e[["msg1"]],
	    				      msg2 = e[["msg2"]],
	    				      cmd  = cmd_e)))
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = NA)
        strings <- list()
        for(e in parse(file = f)) find_strings(e)
        out[[f]] <- structure(strings, class="xngettext")
    }

    out[lengths(out) > 0L]
}

xgettext2pot <-
function(dir, potFile, name = "R", version, bugs)
{
    dir <- file_path_as_absolute(dir)
    if(missing(potFile))
        potFile <- paste0("R-", basename(dir), ".pot")
    msgid <- unlist(xgettext(dir, asCall = FALSE))
    ind <- 2*seq_len(length(msgid)/2)-1 # traverse through every odd record
    msgid <- data.frame(msg = msgid[ind], Cmd = msgid[ind+1], location = names(msgid[ind]), stringsAsFactors=FALSE, row.names = NULL)
    msgid <- msgid[order(msgid[, "msg"]), ] # order wrt translatable msg
    regpth <- paste(dir, "/", sep = "", collapse = "")
    # trim command
    msgid[,"Cmd"] <- paste(sub("\\.msg[0-9]*$", ": ", sub(regpth, "#. ", msgid[,"location"])), msgid[,"Cmd"], "\n", sep = "")
    # trim location
    msgid[,"location"] <- sub("\\.msg[0-9]*$", ": 0", sub(regpth, "\n#: ", msgid[,"location"]))

    # group all locations and commands wrt translatable msg
    for(i in seq_len(nrow(msgid)-1)) 
      if(msgid[i,"msg"] == msgid[i+1,"msg"]) {
        if(msgid[i,"location"] != msgid[i+1,"location"])
		 msgid[i+1,"location"] <- paste(msgid[i,"location"], msgid[i+1,"location"], sep = "", collapse = "")
        msgid[i+1,"Cmd"] <- paste(msgid[i,"Cmd"], msgid[i+1,"Cmd"], sep = "", collapse = "")
        msgid[i, ] <- "" # mark record as empty ""
      }
    msgid <- msgid[nzchar(msgid[,"msg"]), ]
    if(length(msgid) > 0L)
	msgid[,"msg"] <- shQuote(encodeString(msgid[,"msg"]), type="cmd")  # need to quote \n, \t etc

    msgid_plural <- unlist(xngettext(dir))
    if(!is.null(msgid_plural)) {
    ind <- 3*seq_len(length(msgid_plural)/3)-2 # traverse through every 3rd record starting from 1
    msgid_plural <- data.frame(Smsg = msgid_plural[ind], Pmsg = msgid_plural[ind + 1], Cmd = msgid_plural[ind + 2], location = names(msgid_plural[ind]), stringsAsFactors=FALSE, row.names = NULL)
    msgid_plural <- msgid_plural[order(msgid_plural[, "Smsg"]), ] # order wrt translatable msg
    regpth <- paste(dir, "/", sep = "", collapse = "")
    # trim command
    msgid_plural[,"Cmd"] <- paste(sub("\\.msg[0-9]*$", ": ", sub(regpth, "#. ", msgid_plural[,"location"])), msgid_plural[,"Cmd"], "\n", sep = "")
    # trim location
    msgid_plural[,"location"] <- sub("\\.msg[0-9]*$", ": 0", sub(regpth, "\n#: ", msgid_plural[,"location"]))

    # group all locations and commands wrt translatable msg
    for(i in seq_len(nrow(msgid_plural)-1)) 
      if(msgid_plural[i,"Smsg"] == msgid_plural[i+1,"Smsg"]) {
        if(msgid_plural[i,"location"] != msgid_plural[i+1,"location"]) 
		 msgid_plural[i+1,"location"] <- paste(msgid_plural[i,"location"], msgid_plural[i+1,"location"], sep = "", collapse = "")
        msgid_plural[i+1,"Cmd"] <- paste(msgid_plural[i,"Cmd"], msgid_plural[i+1,"Cmd"], sep = "", collapse = "")
        msgid_plural[i, ] <- "" # mark record as empty ""
      }
    msgid_plural <- msgid_plural[nzchar(msgid_plural[,"location"]), ]
    }
    con <- file(potFile, "wt")
    on.exit(close(con))
    if(missing(version))
        version <- paste(R.version$major, R.version$minor, sep = ".")
    if(missing(bugs)) bugs <- "bugs.r-project.org"
    writeLines(con = con,
               c('msgid ""',
                 'msgstr ""',
                 sprintf('"Project-Id-Version: %s %s\\n"', name, version),
                 sprintf('"Report-Msgid-Bugs-To: %s\\n"', bugs),
                 paste0('"POT-Creation-Date: ',
                        format(Sys.time(), "%Y-%m-%d %H:%M"), # %z is not portable
                        '\\n"'),
                 '"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"',
                 '"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"',
                 '"Language-Team: LANGUAGE <LL@li.org>\\n"',
                 '"Language: \\n"',
                 '"MIME-Version: 1.0\\n"',
                 '"Content-Type: text/plain; charset=CHARSET\\n"',
                 '"Content-Transfer-Encoding: 8bit\\n"',
                 if(!is.null(msgid_plural)) '"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"'))
    for(i in seq_len(nrow(msgid)))
        writeLines(con=con, c('', 
				msgid[i, "location"],
				msgid[i, "Cmd"],
				paste('msgid', msgid[i, "msg"]),
				'msgstr ""'))

    if(!is.null(msgid_plural)) {
    for(i in seq_len(nrow(msgid_plural))) {
                writeLines(
                    con=con,
                    c('',
                      msgid_plural[i, "location"],
                      msgid_plural[i, "Cmd"],
                      paste('msgid       ', shQuote(encodeString(msgid_plural[i, "Smsg"]), type="cmd")),
                      paste('msgid_plural', shQuote(encodeString(msgid_plural[i, "Pmsg"]), type="cmd")),
                      'msgstr[0]    ""',
                      'msgstr[1]    ""')
                )
            }
    }
}


checkPoFile <- function(f, strictPlural = FALSE)
{
    getfmts <- function(s) .Call(C_getfmts, s)

    lines <- readLines(f, encoding = "bytes")
    i <- 0
    noCformat <- FALSE
    f1_plural <- NULL
    ref <- NA
    fuzzy <- FALSE

    result <- matrix(character(), ncol = 5L, nrow = 0L)
    while (i < length(lines)) {
	i <- i + 1L

	if (startsWith(lines[i], "#,")) { # useBytes=TRUE (speedup ?)
	    noCformat <- noCformat || grepl("no-c-format", lines[i], useBytes = TRUE)
	    fuzzy <- fuzzy || grepl("fuzzy", lines[i], useBytes = TRUE)
	} else if (startsWith(lines[i], "#:")) {
	    if (!is.na(ref))
		ref <- paste(ref, "etc.")
	    else
		ref <- sub("^#:[[:blank:]]*", "", lines[i])
	} else if (startsWith(lines[i], "msgid ")) {
	    s1 <- sub('^msgid[[:blank:]]+["](.*)["][[:blank:]]*$', "\\1", lines[i])
	    while (startsWith(lines[i+1L], '"')) {
		i <- i + 1L
		s1 <- paste0(s1, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[i]))
	    }
	    f1 <- tryCatch(getfmts(s1), error = identity)
	    j <- i + 1L

	    if (noCformat || inherits(f1, "error")) {
		noCformat <- FALSE
		next
	    }

	    while (j <= length(lines)) {
		if (grepl("^msgid_plural[[:blank:]]", lines[j], useBytes = TRUE))
		    statement <- "msgid_plural"
		else if (grepl("^msgstr[[:blank:]]", lines[j], useBytes = TRUE))
		    statement <- "msgstr"
		else if (grepl("^msgstr\\[[[:digit:]]+\\][[:blank:]]", lines[j], useBytes = TRUE))
		    statement <- sub("^(msgstr)\\[([[:digit:]]+)\\].*$", "\\1\\\\[\\2\\\\]", lines[j])
		else
		    break

		s2 <- sub( paste0("^", statement, "[[:blank:]]+[\"](.*)[\"][[:blank:]]*$"),
		                 "\\1", lines[j])
		while (!is.na(lines[j+1L]) && startsWith(lines[j+1L], '"')) {
		    j <- j+1L
		    s2 <- paste0(s2, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[j]))
		}

		if (s1 == "") { # The header
		    encoding <- sub(".*Content-Type:[^\\]*charset=([^\\[:space:]]*)[[:space:]]*\\\\n.*", "\\1", s2)
		    lines <- iconv(lines, encoding, "UTF-8")
		    break
		}

		f2 <- tryCatch(getfmts(s2), error = identity)

		if (statement == "msgid_plural") {
		    if (!strictPlural) {
			f1_plural <- f2
			j <- j+1L
			next
		    }
		}

		if (nzchar(s2) &&
		     !(identical(f1, f2) || identical(f1_plural, f2))) {
		    location <- paste0(f, ":", j)
		    if (inherits(f2, "error"))
			diff <- conditionMessage(f2)
		    else {
		    	if (length(f1) < length(f2)) {
			    diff <- "too many entries"
			    length(f2) <- length(f1)
		    	} else if (length(f1) > length(f2)) {
			    diff <- "too few entries"
			    length(f1) <- length(f2)
			} else
			    diff <- ""
			diffs <- which(f1 != f2)
			if (length(diffs)) {
			    if (nzchar(diff))
			    	diff <- paste0(diff, ", ")
			    if (length(diffs) > 1)
				diff <- paste(paste0(diff, "differences in entries"),
			                      paste(diffs, collapse = ","))
			    else
				diff <- paste(paste0(diff, "difference in entry"),
				              diffs)
			}
			if (grepl("\u066A", s2, fixed=TRUE))
			    diff <- paste0(diff, ", translation contains arabic percent sign U+066A")
			if (grepl("\uFE6A", s2, fixed=TRUE))
			    diff <- paste0(diff, ", translation contains small percent sign U+FE6A")
			if (grepl("\uFF05", s2, fixed=TRUE))
			    diff <- paste0(diff, ", translation contains wide percent sign U+FF05")
		    }
                    if (!fuzzy)
                        result <- rbind(result, c(location, ref, diff, s1, s2))
		}
		j <- j+1L
	    }
	    i <- j-1L
	    noCformat <- FALSE
	    f1_plural <- NULL
	    ref <- NA
            fuzzy <- FALSE
	}
    }
    structure(result, class = "check_po_files")
}

checkPoFiles <- function(language, dir=".")
{
    files <- list.files(path = dir, pattern = paste0(language, "[.]po$"),
                        full.names = TRUE, recursive = TRUE)
    result <- matrix(character(), ncol = 5L, nrow = 0L)
    for (f in files) {
	errs <- checkPoFile(f, strictPlural = startsWith(basename(f), "R-"))
	if (nrow(errs)) result <- rbind(result, errs)
    }
    structure(result, class = "check_po_files")
}

print.check_po_files <- function(x, ...)
{
    if (!nrow(x))
	cat("No errors\n")
    else
	for (i in 1:nrow(x)) {
	    cols <- if(is.na(x[i, 2L])) c(1L, 3:5) else 1:5
	    cat(x[i, cols], sep = "\n")
	    cat("\n")
	}
}
