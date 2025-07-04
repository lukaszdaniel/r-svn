#  File src/library/utils/R/help.R
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

help <-
function(topic, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         try.all.packages = getOption("help.try.all.packages"),
         help_type = getOption("help_type"))
{
    types <- c("text", "html", "pdf")
    help_type <- if(!length(help_type)) "text"
		 else match.arg(tolower(help_type), types)
    if(!missing(package)) # Don't check for NULL; may be nonstandard eval
        if(is.name(y <- substitute(package)))
            package <- as.character(y)
    ## If no topic was given ...
    if(missing(topic)) {
        if(!is.null(package)) {	# "Help" on package.
            ## Carter Butts and others misuse 'help(package=)' in startup
            if (interactive() && help_type == "html") {
                port <- tools::startDynamicHelp(NA)
                if (port <= 0L) # fallback to text help
                    return(library(help = package, lib.loc = lib.loc,
                                   character.only = TRUE))
                browser <- if (.Platform$GUI == "AQUA") {
                    get("aqua.browser", envir = as.environment("tools:RGUI"))
                } else getOption("browser")
 		browseURL(paste0("http://127.0.0.1:", port,
                                 "/library/", package, "/html/00Index.html"),
                          browser)
                return(invisible())
            } else return(library(help = package, lib.loc = lib.loc,
                                  character.only = TRUE))
        }
        if(!is.null(lib.loc))           # text "Help" on library.
            return(library(lib.loc = lib.loc))
        ## ultimate default is to give help on help()
        topic <- "help"; package <- "utils"; lib.loc <- .Library
    }

    ischar <- tryCatch(is.character(topic) && length(topic) == 1L,
                       error = function(e) FALSE)
    ## if this was not a length-one character vector, try for the name.
    if(!ischar) {
        ## the reserved words that could be parsed as a help arg:
        reserved <-
            c("TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
              "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse1(substitute(topic))
        if(!is.name(substitute(topic)) && ! stopic %in% reserved)
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }

    paths <- index.search(topic,
                          find.package(if (is.null(package)) loadedNamespaces() else package,
			               lib.loc, verbose = verbose))
    try.all.packages <- !length(paths) && is.logical(try.all.packages) &&
        !is.na(try.all.packages) && try.all.packages && is.null(package) && is.null(lib.loc)
    if(try.all.packages) {
        ## Try all the remaining packages.
        for(lib in .libPaths()) {
            packages <- .packages(TRUE, lib)
            packages <- packages[is.na(match(packages, .packages()))]
            paths <- c(paths, index.search(topic, file.path(lib, packages)))
        }
        paths <- paths[nzchar(paths)]
    }

    structure(unique(paths),
	      call = match.call(), topic = topic,
	      tried_all_packages = try.all.packages, type = help_type,
	      class = "help_files_with_topic")
}

print.help_files_with_topic <- function(x, ...) # ...  may contain  msg=FALSE
{
    topic <- attr(x, "topic")
    type  <- attr(x, "type")
    if(type == "html")
        browser <- 
            if (.Platform$GUI == "AQUA" && type == "html")
                get("aqua.browser", envir = as.environment("tools:RGUI"))
            else getOption("browser")
    paths <- as.character(x)
    if(!length(paths)) {
        writeLines(c(gettextf("No documentation for %s in specified packages and libraries:",
                              sQuote(topic)),
                     gettextf("you could try %s",
                              sQuote(paste0("??", topic)))))
        return(invisible(x))
    }

    if(type == "html") port <- tools::startDynamicHelp(NA)

    if(attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic %s is not in any loaded package but can be found in the following packages:",
                        sQuote(topic))
        if (type == "html" && port > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste0('<!DOCTYPE html>\n',
                          '<html>\n',
                          '<head>\n<title>R: help</title>\n',
                          '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />\n',
                          '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />\n',
                          '<link rel="stylesheet" type="text/css" href="/doc/html/R.css" />\n',
                          '</head>\n<body>\n<div class="container">\n\n<hr>\n')
            out <- c(out, '<p>', msg, '</p><br>')
            out <- c(out, '<table style="width: 100%;">\n',
                     '<tr style="text-align: left; vertical-align: top;">\n',
                     '<td style="width: 25%;">Package</td><td>Library</td></tr>\n')
            pkgs <- basename(paths)
            links <- paste0('<a href="http://127.0.0.1:', port,
                            '/library/', pkgs, '/help/', topic, '">',
                            pkgs, '</a>')
            out <- c(out, paste0('<tr style="text-align: left; vertical-align: top;">\n',
                                '<td>', links, '</td><td>',
                                dirname(paths), '</td></tr>\n'))
            out <- c(out, "</table>\n<hr>\n</div>\n</body>\n</html>")
            writeLines(out, file.path(path, "all.available.html"))
            browseURL(paste0("http://127.0.0.1:", port,
                             "/doc/html/all.available.html"),
                      browser)
        } else {
            writeLines(c(strwrap(msg), "",
                         paste0("  ",
                                formatDL(c(gettext("Package"), basename(paths)),
                                         c(gettext("Library"), dirname(paths)),
                                         indent = 22))))
        }
    } else {
        if(length(paths) > 1L) {
            if (type == "html" && port > 0L) { # Redo the search if dynamic help is running
		browseURL(paste0("http://127.0.0.1:", port,
                                 "/library/NULL/help/",
                                 URLencode(topic, reserved = TRUE)),
                          browser)
		return(invisible(x))
	    }
            file <- paths[1L]
            p <- paths
            msg <- gettextf("Help on topic %s was found in the following packages:",
                            sQuote(topic))
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)),
                            c("Library", dirname(paths)),
                            indent = 22L)
            writeLines(c(strwrap(msg), "", paste0("  ", txt), ""))
            if(interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if(type == "html" || type == "latex")
                    tp <- tools::file_path_sans_ext(tp)
                for (i in seq_along(fp)) {
                    tmp <- try(readRDS(fp[i]))
                    titles[i] <- if(inherits(tmp, "try-error"))
                        "unknown title" else
                    tmp[tools::file_path_sans_ext(tmp$File) == tp[i], "Title"]
                }
                txt <- paste0(titles, " {", basename(paths), "}")
                ## the default on menu() is currently graphics = FALSE
                res <- menu(txt, title = gettext("Choose one"),
                            graphics = getOption("menu.graphics"))
                if(res > 0) file <- p[res]
            } else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else
            file <- paths

        if(type == "html") {
            if (port > 0L) {
		path <- dirname(file)
		dirpath <- dirname(path)
		pkgname <- basename(dirpath)
		browseURL(paste0("http://127.0.0.1:", port,
                                 "/library/", pkgname, "/html/", basename(file),
                                 ".html"),
                          browser)
            } else {
                warning("HTML help is unavailable", call. = FALSE)
                att <- attributes(x)
                xx <- sub("/html/([^/]*)\\.html$", "/help/\\1", x)
                attributes(xx) <- att
                attr(xx, "type") <- "text"
                print(xx)
            }
        } else if(type == "text") {
            pkgname <- basename(dirname(dirname(file)))
            temp <- tools::Rd2txt(.getHelpFile(file), out = tempfile("Rtxt"),
                                  package = pkgname)
            file.show(temp, title = gettextf("R Help on %s", sQuote(topic)),
                      delete.file = TRUE)
        }
        else if(type %in% "pdf") {
            path <- dirname(file)
            dirpath <- dirname(path)
            texinputs <- file.path(dirpath, "help", "figures")
            tf2 <- tempfile("Rlatex")
            tools::Rd2latex(.getHelpFile(file), out = tf2)
            .show_help_on_topic_offline(tf2, topic, type, texinputs, ...)
            unlink(tf2)
        }
    }
    invisible(x)
}

.help_topic_latex <- function(file, topic) { # Side effect: creates file  <topic>.tex in working directory
    lines <- readLines(file)
    encpatt <- "^\\\\inputencoding\\{(.*)\\}$"
    encoding <- if(length(res <- grep(encpatt, lines,
                                      perl = TRUE, useBytes = TRUE, value = TRUE)))
                    sub(encpatt, "\\1", res, perl = TRUE, useBytes = TRUE)
                else ""
    texfile <- paste0(topic, ".tex")
    if(nzchar(opt <- Sys.getenv("R_RD4PDF"))) opt else "times,inconsolata"
    cat("\\documentclass[", getOption("papersize"), "paper]{article}\n",
        "\\usepackage[", opt, "]{Rd}\n",
        if(nzchar(encoding)) sprintf("\\usepackage[%s]{inputenc}\n", encoding),
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\usepackage{graphicx}\n",
        "\\begin{document}\n",
        file = texfile, sep = "")
    file.append(texfile, file)
    cat("\\end{document}\n", file = texfile, append = TRUE)
    structure(texfile, has_figure = any(grepl("\\Figure", lines)))
}

## "static": _only_ called once above for type == "pdf" : currently  "offline" <==> {latex -> pdf}
.show_help_on_topic_offline <-
    function(file, topic, type = "pdf", texinputs = NULL, msg = TRUE)
{
    texfile <- .help_topic_latex(file, topic)
    on.exit(unlink(texfile)) ## ? leave to helper
    helper <- get0("offline_help_helper", envir = .GlobalEnv) %||% offline_help_helper # <-> below
    if (attr(texfile, "has_figure"))
         helper(texfile, type, msg=msg, texinputs=texinputs)
    else helper(texfile, type, msg=msg)
    invisible()
}


.getHelpFile <- function(file)
{
    path <- dirname(file)
    dirpath <- dirname(path)
    if(!file.exists(dirpath))
        stop(gettextf("invalid '%s' argument", "file"), domain = NA)
    pkgname <- basename(dirpath)
    RdDB <- file.path(path, pkgname)
    if(!file.exists(paste0(RdDB, ".rdx")))
        stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", sQuote(pkgname)), domain = NA)
    tools:::fetchRdDB(RdDB, basename(file))
}


offline_help_helper <- function(texfile, type, msg = TRUE, texinputs = NULL)
{
    ## Some systems have problems with texfile names like ".C.tex"
    tf <- tempfile("tex", tmpdir = ".", fileext = ".tex"); on.exit(unlink(tf))
    file.copy(texfile, tf)
    tools::texi2pdf(tf, clean = TRUE, texinputs = texinputs)
    ofile <- sub("tex$", "pdf", tf)
    ofile2 <- sub("tex$", "pdf", texfile)
    if(!file.exists(ofile))
        stop(gettextf("creation of %s failed", sQuote(ofile2)), domain = NA)
    if(file.copy(ofile, ofile2, overwrite = TRUE)) {
        unlink(ofile)
        if(msg) ofile <- basename(ofile2)
    }
    if(msg)
        message(gettextf("Saving help page to %s", sQuote(ofile)), domain = NA)
    invisible()
}

