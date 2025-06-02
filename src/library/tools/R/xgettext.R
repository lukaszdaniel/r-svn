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

    find_strings <- function(e, srcref = NULL) {
        find_strings2 <- function(e, cmd_e, suppress, srcref = NULL) {
            if(is.character(e)) {
                e <- sub("^[ \t\n]*", "", e)
                e <- sub("[ \t\n]*$", "", e)
                if(!suppress) {
                    line <- if (!is.null(srcref)) srcref[1L] else 0
                    strings <<- c(strings, list(c(msg = e, cmd = cmd_e, line = line)))
                }
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
                for(i in seq_along(e)) find_strings2(e[[i]], cmd_e, suppress, attr(e, "srcref"))
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

            line <- if (!is.null(srcref)) srcref[1L] else 0

             if(asCall) {
                 e <- sub("^[ \t\n]*", "", as.character(e)[-1L])
                 e <- sub("[ \t\n]*$", "", e)
                 if(!suppress) strings <<- c(strings, list(c(msg = e, cmd = cmd_e, line = line)))
             } else {
                 if(as.character(e[[1L]]) == "gettextf") {
                     e <- match.call(gettextf, e)
                     e <- e["fmt"] # just look at fmt arg
                 }
                 for(i in seq_along(e)) find_strings2(e[[i]], cmd_e, suppress, attr(e, "srcref"))
             }
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]], attr(e[[i]], "srcref"))
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = "R-tools")
        strings <- list()
        exprs <- parse(file = f, keep.source = TRUE)
        for (e in exprs) find_strings(e, attr(e, "srcref"))
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
               "\nline         = ", encodeString(x[3L]),
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
            "\nline         = ", e[4L],
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

    find_strings <- function(e, srcref = NULL) {
        if(is.call(e) && is.name(e[[1L]])
           && as.character(e[[1L]]) %in% "ngettext") {
	    cmd_e <- paste(deparse(e), sep = "", collapse = "")
	    e <- match.call(ngettext, e)
            domain <- e[["domain"]]
            suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
	    if (!suppress &&
                is.character(e[["msg1"]]) && is.character(e[["msg2"]])) {
	    	line <- if (!is.null(srcref)) srcref[1L] else 0
	    	strings <<- c(strings, list(c(msg1 = e[["msg1"]],
	    				      msg2 = e[["msg2"]],
	    				      cmd  = cmd_e,
	    				      line = line)))
            }
        } else if(is.recursive(e))
            for(i in seq_along(e)) Recall(e[[i]], attr(e[[i]], "srcref"))
    }

    for(f in R_files) {
        if(verbose) message(gettextf("parsing '%s'", f), domain = "R-tools")
        strings <- list()
        exprs <- parse(file = f, keep.source = TRUE)
        for(e in exprs) find_strings(e, attr(e, "srcref"))
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

    regpth <- paste(dir, "/", sep = "", collapse = "")

    # --- Extract singular messages from xgettext ---
    convert_singular_to_df <- function(out) {
        result <- lapply(names(out), function(fname) {
        entries <- out[[fname]]
        lapply(entries, function(entry) {
          data.frame(
            msg = entry[["msg"]],
            command = entry[["cmd"]],
            line = entry[["line"]],
            file = sub(regpth, "", fname),
            stringsAsFactors = FALSE
          )
        })
        })

        # Flatten list and filter NULLs
        result <- Filter(Negate(is.null), unlist(result, recursive = FALSE))

        # Combine into data frame
        do.call(rbind, result)
    }
    #raw <- unlist(xgettext(dir, asCall = FALSE))
    msg_list <- xgettext(dir, asCall = FALSE)
    msg_df <- convert_singular_to_df(msg_list)
    msg_df <- msg_df[order(msg_df[["msg"]]), ]

    # --- Combine entries with same msg ---
    msg_df$loc_str <- sprintf("\n#: %s: %s", msg_df$file, msg_df$line)
    msg_df$Cmd_str <- sprintf("#. %s: %s\n", msg_df$file, msg_df$command)

    for(i in seq_len(nrow(msg_df) - 1)) {
        if(msg_df$msg[i] == msg_df$msg[i + 1]) {
            if(msg_df$loc_str[i] != msg_df$loc_str[i + 1])
                msg_df$loc_str[i + 1] <- paste0(msg_df$loc_str[i], msg_df$loc_str[i + 1])
            msg_df$Cmd_str[i + 1] <- paste0(msg_df$Cmd_str[i], msg_df$Cmd_str[i + 1])
            msg_df[i, ] <- "" # mark record as empty ""
        }
    }
    msg_df <- msg_df[nzchar(msg_df$msg), ]
    msg_df$msg <- shQuote(encodeString(msg_df$msg), type = "cmd")  # need to quote \n, \t etc

    # --- Extract plural messages from xngettext ---
    convert_plural_to_df <- function(out) {
        result <- lapply(names(out), function(fname) {
        entries <- out[[fname]]
        lapply(entries, function(entry) {
            data.frame(
            Smsg = entry[["msg1"]],
            Pmsg = entry[["msg2"]],
            command =entry[["cmd"]],
            line = entry[["line"]],
            file = sub(regpth, "", fname),
            stringsAsFactors = FALSE
          )
        })
        })

        # Flatten list and filter NULLs
        result <- Filter(Negate(is.null), unlist(result, recursive = FALSE))

        # Combine into data frame
        do.call(rbind, result)
    }

    msgp_list <- xngettext(dir)
    msgp_df <- data.frame()
    if(length(msgp_list) > 0) {
    msgp_df <- convert_plural_to_df(msgp_list)
    msgp_df <- msgp_df[order(msgp_df$Smsg), ]

    # --- Combine entries with same msg ---
    msgp_df$loc_str <- sprintf("\n#: %s: %s", msgp_df$file, msgp_df$line)
    msgp_df$Cmd_str <- sprintf("#. %s: %s\n", msgp_df$file, msgp_df$command)

    for(i in seq_len(nrow(msgp_df) - 1)) {
        if(msgp_df$Smsg[i] == msgp_df$Smsg[i + 1]) {
            if(msgp_df$loc_str[i] != msgp_df$loc_str[i + 1])
                msgp_df$loc_str[i + 1] <- paste0(msgp_df$loc_str[i], msgp_df$loc_str[i + 1])
            msgp_df$Cmd_str[i + 1] <- paste0(msgp_df$Cmd_str[i], msgp_df$Cmd_str[i + 1])
            msgp_df[i, ] <- "" # mark record as empty ""
        }
    }
    msgp_df <- msgp_df[nzchar(msgp_df$Smsg), ]
    msgp_df$Smsg <- shQuote(encodeString(msgp_df$Smsg), type = "cmd")
    msgp_df$Pmsg <- shQuote(encodeString(msgp_df$Pmsg), type = "cmd")
    }

    # --- Write POT file ---
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
                 sprintf('"Content-Type: text/plain; charset=%s\\n"', "UTF-8"),
                 '"Content-Transfer-Encoding: 8bit\\n"',
                 if(nrow(msgp_df) > 0) '"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"'))

    # --- Write singular messages ---
    for(i in seq_len(nrow(msg_df)))
        writeLines(con=con, c('', 
				msg_df[i, "loc_str"],
				msg_df[i, "Cmd_str"],
				sprintf("msgid %s", msg_df[i, "msg"]),
				'msgstr ""'))

    # --- Write plural messages ---
    if(nrow(msgp_df) > 0) {
    for(i in seq_len(nrow(msgp_df))) {
            writeLines(con = con, c("",
                      msgp_df[i, "loc_str"],
                      msgp_df[i, "Cmd_str"],
                      sprintf("msgid        %s", msgp_df[i, "Smsg"]),
                      sprintf("msgid_plural %s", msgp_df[i, "Pmsg"]),
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
