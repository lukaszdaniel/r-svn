#  File src/library/tools/R/CRANtools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2014-2025 The R Core Team
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

summarize_CRAN_check_status <-
function(packages, results = NULL, details = NULL, issues = NULL)
{
    if(is.null(results))
        results <- CRAN_check_results()
    results <-
        results[!is.na(match(results$Package, packages)) & !is.na(results$Status), ]

    if(!NROW(results)) {
        s <- character(length(packages))
        names(s) <- packages
        return(s)
    }

    if(any(results$Status != "OK")) {
        if(is.null(details))
            details <- CRAN_check_details()
        details <- details[!is.na(match(details$Package, packages)), ]
        ## Remove all ok stubs.
        details <- details[details$Check != "*", ]
        ## Remove trailing white space from outputs ... remove eventually
        ## when this is done on CRAN.
        details$Output <- sub("[[:space:]]+$", "", details$Output)

    } else {
        ## Create empty details directly to avoid the cost of reading
        ## and subscripting the actual details db.
        details <- as.data.frame(matrix(character(), ncol = 7L),
                                 stringsAsFactors = FALSE)
        names(details) <-
            c("Package", "Version", "Flavor", "Check", "Status", "Output",
              "Flags")
    }

    if(is.null(issues))
        issues <- CRAN_check_issues()

    summarize_results <- function(p, r) {
        if(!NROW(r)) return(character())
        tab <- table(r$Status)[c("OK", "NOTE", "WARNING", "ERROR", "FAILURE")]
        tab <- tab[!is.na(tab) & (tab > 0)]
        paste(c(sprintf("Current CRAN status: %s",
                        paste(sprintf("%s: %s", names(tab), tab),
                              collapse = ", ")),
                sprintf("See: <https://CRAN.R-project.org/web/checks/check_results_%s.html>",
                        p)),
              collapse = "\n")
    }

    summarize_details <- function(p, d) {
        if(!NROW(d)) return(character())

        pof <- which(names(d) == "Flavor")
        poo <- which(names(d) == "Output")
        ## Outputs from checking "whether package can be installed" will
        ## have a machine-dependent final line
        ##    See ....... for details.
        ind <- d$Check == "whether package can be installed"
        if(any(ind)) {
            d[ind, poo] <-
                sub("\nSee[^\n]*for details[.]$", "", d[ind, poo])
        }
        txt <- apply(d[-pof], 1L, paste, collapse = "\r")
        ## Outputs from checking "installed package size" will vary
        ## according to system.
        ind <- d$Check == "installed package size"
        if(any(ind)) {
            txt[ind] <-
                apply(d[ind, - c(pof, poo)],
                      1L, paste, collapse = "\r")
        }

        ## Canonicalize fancy quotes.
        ## Could also try using iconv(to = "ASCII//TRANSLIT"))
        txt <- .canonicalize_quotes(txt)
        out <-
            lapply(split(seq_len(NROW(d)), match(txt, unique(txt))),
                   function(e) {
                       tmp <- d[e[1L], ]
                       flags <- tmp$Flags
                       flavors <- d$Flavor[e]
                       c(sprintf("Version: %s", tmp$Version),
                         if(nzchar(flags)) sprintf("Flags: %s", flags),
                         sprintf("Check: %s, Result: %s", tmp$Check, tmp$Status),
                         sprintf("  %s",
                                 gsub("\n", "\n  ", tmp$Output,
                                      perl = TRUE)),
                         sprintf("See: %s",
                                 paste(sprintf("<https://www.r-project.org/nosvn/R.check/%s/%s-00check.html>",
                                               flavors,
                                               p),
                                       collapse = ",\n     ")))
                   })
        paste(unlist(lapply(out, paste, collapse = "\n")),
              collapse = "\n\n")
    }

    summarize_issues <- function(i) {
        if(!length(i)) return(character())
        ## In principle the hyperrefs can be obtained from the package
        ## check results page already pointed to by summarize_results(),
        ## but this is not convenient for plain text processing ...
        paste(c("Additional issues:",
                sprintf("  %s <%s>", i$kind, i$href)),
              collapse = "\n")
    }

    summarize <- function(p, r, d, i) {
        paste(c(summarize_results(p, r),
                summarize_issues(i),
                summarize_details(p, d)),
              collapse = "\n\n")
    }

    ## Split according to package.
    issues <- split(issues[-1L], issues[[1L]])

    s <- if(length(packages) == 1L) {
        summarize(packages, results, details, issues[[packages]])
    } else {
        results <- split(results, factor(results$Package, packages))
        details <- split(details, factor(details$Package, packages))
        unlist(lapply(packages,
                      function(p) {
                          summarize(p,
                                    results[[p]],
                                    details[[p]],
                                    issues[[p]])
                      }))
    }

    names(s) <- packages
    class(s) <- "summarize_CRAN_check_status"
    s
}

format.summarize_CRAN_check_status <-
function(x, header = NA, ...)
{
    if(is.na(header)) header <- (length(x) > 1L)
    if(header) {
        s <- sprintf("Package: %s", names(x))
        x <- sprintf("%s\n%s\n\n%s", s, gsub(".", "*", s), x)
    }
    x
}

print.summarize_CRAN_check_status <-
function(x, ...)
{
    writeLines(paste(format(x, ...), collapse = "\n\n"))
    invisible(x)
}

## Summarize complete CRAN check status according to maintainer.

summarize_CRAN_check_status_according_to_maintainer <-
function(db = CRAN_package_db(),
         results = CRAN_check_results(),
         details = CRAN_check_details(),
         issues  = CRAN_check_issues())
{
    ind <- !duplicated(db[, "Package"])

    maintainer <- db[, "Maintainer"]
    maintainer <- tolower(sub(".*<(.*)>.*", "\\1", maintainer))

    split(format(summarize_CRAN_check_status(db[ind, "Package"],
                                             results,
                                             details,
                                             issues),
                 header = TRUE),
          maintainer[ind])
}

CRAN_baseurl_for_src_area <-
function()
    Sys.getenv("R_CRAN_SRC", .get_CRAN_repository_URL())

## This allows for partial local mirrors, or to look at a
## more-frequently-updated mirror.  Exposed as utils::findCRANmirror
CRAN_baseurl_for_web_area <-
function()
    Sys.getenv("R_CRAN_WEB", .get_CRAN_repository_URL())

read_CRAN_object <-
function(cran, path)
{
    con <- gzcon(url(sprintf("%s/%s", cran, path),
                     open = "rb"))
    on.exit(close(con))
    readRDS(con)
}

CRAN_check_results <-
function(flavors = NULL)
{
    db <- read_CRAN_object(CRAN_baseurl_for_web_area(),
                           "web/checks/check_results.rds")
    if(!is.null(flavors))
        db <- db[!is.na(match(db$Flavor, flavors)), ]
    db
}

CRAN_check_details <-
function(flavors = NULL)
{
    db <- read_CRAN_object(CRAN_baseurl_for_web_area(),
                           "web/checks/check_details.rds")
    if(!is.null(flavors))
        db <- db[!is.na(match(db$Flavor, flavors)), ]
    db
}

## Deprecated in 3.4.1, removed in 4.3.0
## CRAN_memtest_notes <-
## function()
## {
##     .Deprecated("CRAN_check_issues")
##     read_CRAN_object(CRAN_baseurl_for_web_area(),
##                      "web/checks/memtest_notes.rds")
## }

CRAN_check_issues <-
function()
    read_CRAN_object(CRAN_baseurl_for_web_area(),
                     "web/checks/check_issues.rds")

CRAN_package_db <-
function()
    as.data.frame(read_CRAN_object(CRAN_baseurl_for_web_area(),
                                   "web/packages/packages.rds"),
                  stringsAsFactors = FALSE)

CRAN_aliases_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/aliases.rds")

CRAN_archive_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/archive.rds")

CRAN_authors_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/authors.rds")

CRAN_current_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/current.rds")

CRAN_rdxrefs_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/rdxrefs.rds")

check_CRAN_mirrors <-
function(mirrors = NULL, verbose = FALSE)
{
    retry_upon_error <- function(expr, n = 3L) {
        i <- 1L
        repeat {
            y <- tryCatch(expr, error = identity)
            if(!inherits(y, "error") || (i >= n))
                break
            i <- i + 1L
        }
        y
    }

    read_package_db <- function(baseurl) {
        path <- sprintf("%ssrc/contrib/PACKAGES.gz", baseurl)
        db <- retry_upon_error({
            con <- gzcon(url(path, "rb"))
            on.exit(close(con))
            readLines(con)
        })
        if(inherits(db, "error")) {
            msg <- sprintf("Reading %s failed with message: %s",
                           path, conditionMessage(db))
            return(simpleError(msg))
        }
        db
    }

    read_timestamp <- function(baseurl, path) {
        path <- sprintf("%s%s", baseurl, path)
        ts <- retry_upon_error(readLines(path))
        if(inherits(ts, "error")) {
            msg <- sprintf("Reading %s failed with message: %s",
                           path, conditionMessage(ts))
            return(simpleError(msg))
        }
        as.POSIXct(as.numeric(ts), origin = "1970-01-01")
    }

    if_ok <- function(u, v) if(inherits(u, "error")) u else v

    check_mirror <- function(mirror) {
        mirror_packages <- read_package_db(mirror)
        mirror_ts1 <- read_timestamp(mirror, path_ts1)
        mirror_ts2 <- read_timestamp(mirror, path_ts2)
        mirror_ts3 <- read_timestamp(mirror, path_ts3)

        list("PACKAGES" =
             if_ok(mirror_packages,
                   c("Delta_master_mirror" =
                         sprintf("%d/%d",
                                 length(setdiff(master_packages,
                                                mirror_packages)),
                                 length(master_packages)),
                     "Delta_mirror_master" =
                         sprintf("%d/%d",
                                 length(setdiff(mirror_packages,
                                                master_packages)),
                                 length(mirror_packages)))),
             "TIME" =
             if_ok(mirror_ts1, difftime(master_ts1, mirror_ts1)),
             "TIME_r-release" =
             if_ok(mirror_ts2, difftime(master_ts2, mirror_ts2)),
             "TIME_r-old-release" =
             if_ok(mirror_ts3, difftime(master_ts3, mirror_ts3))
             )
    }

    master <- "https://CRAN.R-project.org/"
    path_ts1 <- "TIME"
    path_ts2 <- "bin/windows/contrib/r-release/TIME_r-release"
    path_ts3 <- "bin/windows/contrib/r-old-release/TIME_r-old-release"

    master_packages <- read_package_db(master)
    master_ts1 <- read_timestamp(master, path_ts1)
    master_ts2 <- read_timestamp(master, path_ts2)
    master_ts3 <- read_timestamp(master, path_ts3)

    if(is.null(mirrors)) {
        mirrors <- as.character(utils::getCRANmirrors(all = TRUE)$URL)
    }

    results <- lapply(mirrors,
                      function(m) {
                          if(verbose)
                              message(sprintf("Checking %s", m))
                          suppressWarnings(tryCatch(check_mirror(m),
                                                    error = identity))
                      })
    names(results) <- mirrors

    results
}

CRAN_mirror_maintainers_info <-
function(mirrors, db = NULL, collapse = TRUE)
{
    if(is.null(db))
        db <- utils::getCRANmirrors(all = TRUE)
    mirrors <- sort(unique(mirrors))
    ind <- match(mirrors, as.character(db$URL))
    addresses <- db[ind, "Maintainer"]
    addresses <- gsub("[[:space:]]*#[[:space:]]*", "@", addresses)
    to <- unique(unlist(strsplit(addresses,
                                 "[[:space:]]*,[[:space:]]*")))
    head <- list("To" = "CRAN@R-project.org",
                 "Bcc" = to,
                 "Subject" = "CRAN mirrors maintained by you",
                 "Reply-To" = "CRAN@R-project.org")
    if(collapse) {
        head$Bcc <- paste(head$Bcc, collapse = ",\n    ")
        head <- sprintf("%s: %s", names(head), unlist(head))
    }
    len <- length(addresses)
    body <- c(if(len > 1L) {
                  "Dear maintainers,"
              } else {
                  "Dear maintainer,"
              },
              "",
              strwrap(paste(if(length(mirrors) > 1L) {
                                "This concerns the following CRAN mirrors"
                            } else {
                                "This concerns the following CRAN mirror"
                            },
                            "maintained by",
                            if(len > 1L) "one of",
                            "you:")),
              "",
              paste0("  ", formatDL(mirrors, addresses, style = "list"))
              )
    list(head = head, body = body)
}

CRAN_mirror_mirmon_status <-
function()
{
    ## See
    ## <http://www.projects.science.uu.nl/csg/mirmon/mirmon.html#state_file_format>.

    fields <-
        c("url",
          "age",
          "status_last_probe",
          "time_last_successful_probe",
          "probe_history",
          "state_history",
          "last_probe")
    ts_to_POSIXct <- function(ts) {
        suppressWarnings(as.POSIXct(as.numeric(as.character(ts)),
                                    origin = "1970-01-01"))
    }
    read_mirmon_state_file <- function(con) {
        db <- utils::read.table(con, header = FALSE, col.names = fields)
        db$url <- as.character(db$url)
        db$age <- ts_to_POSIXct(db$age)
        db$time_last_successful_probe <-
            ts_to_POSIXct(db$time_last_successful_probe)
        db$last_probe <- ts_to_POSIXct(db$last_probe)
        db$delta <- difftime(Sys.time(), db$age, units = "days")
        db
    }
    state_files <-
        c("TIME" = "mirror.state",
          "TIME_r-release" = "mirror_release.state",
          "TIME_r-old-release" = "mirror_old_release.state")

    ## Need to always use master for now (the mirrors do not have the
    ## state files).
    do.call(rbind,
            c(Map(function(u, v) {
                      u <- paste0("https://cran.r-project.org/mirmon/state/", u)
                      cbind(read_mirmon_state_file(u),
                            timestamp = v,
                            stringsAsFactors = FALSE)
                  },
                  state_files,
                  names(state_files)),
              list(make.row.names = FALSE)))
}


CRAN_Rd_xref_db_with_expansions <-
function()
{
    db <- CRAN_rdxrefs_db()
    ## Flatten:
    db <- cbind(do.call(rbind, db),
                rep.int(names(db), vapply(db, NROW, 0L)))
    colnames(db) <- c(colnames(db)[1L : 2L], "S_File", "S_Package")
    unique(cbind(db, .expand_anchored_Rd_xrefs(db)))
}

CRAN_Rd_xref_available_target_ids <-
function()
{
    targets <- lapply(CRAN_aliases_db(), .Rd_available_xref_targets)
    .Rd_object_id(rep.int(names(targets), lengths(targets)),
                  unlist(targets, use.names = FALSE))
}

CRAN_Rd_xref_reverse_dependencies <-
function(packages, db = NULL, details = FALSE)
{
    if(is.null(db))
        db <- CRAN_Rd_xref_db_with_expansions()
    y <- split.data.frame(db, db[, "T_Package"])[packages]
    if(!details)
        y <- lapply(y, function(e) unique(e[, "S_Package"]))
    y
}

CRAN_Rd_xref_problems <-
function()
{
    y <- list()

    db <- CRAN_Rd_xref_db_with_expansions()
    db <- db[nzchar(db[, "T_Package"]), , drop = FALSE]
    ## Add ids:
    db <- cbind(db,
                T_ID = .Rd_object_id(db[, "T_Package"], db[, "T_File"]))

    ## Do we have Rd xrefs to current CRAN packages which no longer work?
    current <- sub("_.*", "", rownames(CRAN_current_db()))
    db1 <- db[!is.na(match(db[, "T_Package"], current)), , drop = FALSE]
    y$broken_xrefs_to_current_CRAN_packages <-
        db1[is.na(match(db1[, "T_ID"],
                        CRAN_Rd_xref_available_target_ids())), ,
            drop = FALSE]

    ## Do we have Rd xrefs "likely" to archived CRAN packages?
    ## This is a bit tricky because packages could have been archived on
    ## CRAN but still be available from somewhere else.  The code below
    ## catches availability in standard repositories, but not in
    ## additional repositories.
    repos <- .get_standard_repository_URLs() # CRAN and BioC
    ## Previous versions used getOption("repos").
    archived <-
        setdiff(names(CRAN_archive_db()),
                c(rownames(utils::available.packages(filters = list(),
                                                     repos = repos)),
                  unlist(.get_standard_package_names(),
                         use.names = FALSE)))
    y$xrefs_likely_to_archived_CRAN_packages <-
        db[!is.na(match(db[, "T_Package"], archived)), , drop = FALSE]

    y
}

.Rd_available_xref_targets <-
function(aliases)
{
    ## Argument aliases as obtained from Rd_aliases(), or directly by
    ## calling
    ##   lapply(rddb, .Rd_get_metadata, "alias")
    ## on an Rd db.
    unique(c(unlist(aliases, use.names = FALSE),
             sub("\\.[Rr]d", "", basename(names(aliases)))))
}

.Rd_object_id <-
function(package, nora)
{
    ## Name OR Alias: nora.
    sprintf("%s::%s", package, nora)
}

CRAN_package_maintainers_db <-
function(db = CRAN_package_db())
{
    maintainer <- db[, "Maintainer"]
    address <- tolower(sub(".*<(.*)>.*", "\\1", maintainer))
    maintainer <- gsub("\n", " ", maintainer, fixed=TRUE)
    list2DF(list(Package = db[, "Package"],
                 Address = address,
                 Maintainer = maintainer))
}

CRAN_package_maintainers_info <-
function(packages, db = NULL, collapse = TRUE)
{
    if(is.null(db))
        db <- CRAN_package_maintainers_db()
    ind <- match(packages, db[, "Package"])
    addresses <- db[ind, "Address"]
    to <- sort(unique(addresses))
    head <- list("To" = "CRAN@R-project.org",
                 "Bcc" = to,
                 "Subject" = "CRAN packages maintained by you",
                 "Reply-To" = "CRAN@R-project.org")
    if(collapse) {
        head$Bcc <- paste(head$Bcc, collapse = ",\n    ")
        head <- sprintf("%s: %s", names(head), unlist(head))
    }
    lst <- split(db[ind, "Package"], db[ind, "Maintainer"])
    len <- length(addresses)
    body <- c(if(len > 1L) {
                  "Dear maintainers,"
              } else {
                  "Dear maintainer,"
              },
              "",
              if(length(packages) > 1L) {
                  "This concerns the CRAN packages"
              } else {
                  "This concerns the CRAN package"
              },
              "",
              paste(strwrap(paste(sort(packages), collapse = " "),
                            indent = 2L, exdent = 2L),
                    collapse = "\n"),
              "",
              paste("maintained by",
                    if(len > 1L) "one of",
                    "you:"),
              "",
              paste0("  ",
                     formatDL(vapply(lst, paste, "", collapse = " "),
                              style = "list"))
              )
    list(head = head, body = body)
}

CRAN_package_reverse_dependencies_and_views <-
function(packages)
{
    repos <- getOption("repos")
    ## Alternatively, use .get_standard_repository_URLs()

    a <- utils::available.packages(filters = list(), repos = repos)

    v <- read_CRAN_object(CRAN_baseurl_for_src_area(),
                          "src/contrib/Views.rds")
    v <- do.call(rbind,
                 mapply(cbind,
                        Package =
                        lapply(v, function(e) e$packagelist$name),
                        View = vapply(v, `[[`, "name", FUN.VALUE = "")))
    v <- split(v[, 2L], v[, 1L])

    r <- package_dependencies(packages, a, reverse = TRUE)
    rr <- package_dependencies(packages, a,
                               reverse = TRUE, recursive = TRUE)
    rrs <- package_dependencies(packages, a, "Suggests",
                                reverse = TRUE, recursive = "strong")

    ## For formatting reverse dependencies, for now indicate non-CRAN
    ## ones by adding a '*'.
    expansions <- unique(c(unlist(r, use.names = FALSE),
                           unlist(rr, use.names = FALSE),
                           unlist(rrs, use.names = FALSE)))
    names(expansions) <- expansions
    if("CRAN" %in% names(repos)) {
        ind <- !startsWith(a[match(expansions, a[, "Package"]),
                             "Repository"],
                           repos["CRAN"])
        expansions[ind] <- paste0(expansions[ind], "*")
    }

    rxrefs <- CRAN_Rd_xref_reverse_dependencies(packages)

    fmt <- function(x) {
        if(length(x)) paste(sort(x), collapse = " ") else NA_character_
    }

    y <- lapply(packages,
                function(p) {
                    c(Package = p,
                      "Reverse depends" =
                          fmt(expansions[r[[p]]]),
                      "Additional recursive reverse depends" =
                          fmt(expansions[setdiff(rr[[p]], r[[p]])]),
                      "Additional recursive reverse depends of suggests" =
                          fmt(expansions[setdiff(rrs[[p]], rr[[p]])]),
                      "Reverse Rd xref depends" =
                          fmt(rxrefs[[p]]),
                      "Views" =
                          fmt(v[[p]]))
                })
    y <- as.data.frame(do.call(rbind, y), stringsAsFactors = FALSE)
    class(y) <- c("CRAN_package_reverse_dependencies_and_views",
                  class(y))
    y
}

format.CRAN_package_reverse_dependencies_and_views <-
function(x, ...)
{
    apply(x, 1L,
          function(e) {
              paste(formatDL(e[!is.na(e)],
                             style = "list", indent = 2L),
                    collapse = "\n")
          })
}

print.CRAN_package_reverse_dependencies_and_views <-
function(x, ...)
{
    writeLines(paste(format(x, ...), collapse = "\n\n"))
    invisible(x)
}

CRAN_package_reverse_dependencies_with_maintainers <-
function(packages, which = "strong", recursive = FALSE,
         db = CRAN_package_db())
{
    rdepends <- package_dependencies(packages, db, which,
                                     recursive = recursive,
                                     reverse = TRUE)
    rdepends <- sort(unique(unlist(rdepends)))
    pos <- match(rdepends, db[, "Package"], nomatch = 0L)

    db[pos, c("Package", "Version", "Maintainer")]
}

CRAN_package_dependencies_with_dates <-
function(packages, which = "most", recursive = FALSE,
         db = CRAN_package_db())
{
    repos <- .get_standard_repository_URLs() # CRAN and BioC
    a <- utils::available.packages(filters = list(), repos = repos)

    pb <- NULL                          # Compute if necessary ...
    d <- package_dependencies(packages, a, which = which,
                              recursive = recursive)
    ## We currently keep the base packages dependencies, which have no
    ## date.  Hence, filter these out ...
    base_packages <- .get_standard_package_names()[["base"]]
    lapply(d,
           function(e) {
               e <- setdiff(as.character(e), base_packages)
               i <- match(e, db[, "Package"])
               d <- db[i, "Published"]
               if(any(j <- is.na(i))) {
                   eb <- e[j]
                   if(is.null(pb))
                       pb <<- BioC_package_db()
                   ib <- match(eb, pb[, "Package"])
                   d[j] <- pb[ib, "Date/Publication"]
                   e[j] <- paste0(eb, "*")
               }
               d <- as.Date(d)
               o <- order(d, decreasing = TRUE)
               list2DF(list(Package = e[o], Date = d[o]))
           })
}

CRAN_packages_with_maintainer_matching <-
function(pattern, db = CRAN_package_db(), ...)
{
    ind <- grep(pattern, db[, "Maintainer"], ...)
    db[ind, "Package"]
}

write_texts_to_dir <-
function(lst, dir, verbose = FALSE)
{
    dir.create(dir, showWarnings = FALSE, recursive = FALSE)

    Map(function(m, s) {
        if(verbose)
            message(sprintf("Processing %s ...", m))
        writeLines(paste(s, collapse = "\n\n"),
                   file.path(dir, sprintf("%s.txt", m)))
    },
        names(lst),
        lst)

    invisible()
}

CRAN_package_URL <- function(p)
    paste0("https://CRAN.R-project.org/package=", p)

CRAN_package_check_URL <- function(p)
    sprintf("https://CRAN.R-project.org/web/checks/check_results_%s.html",
            p)

BioC_package_db <-
function(remap = TRUE)
{
    urls <- .get_standard_repository_URLs()
    urls <- urls[startsWith(names(urls), "BioC")]
    if(!length(urls)) return(NULL)
    info <- lapply(urls, function(u) {
                       con <- url(paste0(u, "/VIEWS"))
                       on.exit(close(con))
                       read.dcf(con)
                   })
    db <- Reduce(function(u, v) merge(u, v, all = TRUE),
                 lapply(info,
                        as.data.frame,
                        stringsAsFactors = FALSE))
    if(remap) {
        ## Map BioC reverse dependency names to CRAN ones.
        biocrevnames <- c(dependsOnMe = "Reverse depends",
                          importsMe = "Reverse imports",
                          linksToMe = "Reverse linking to",
                          suggestsMe = "Reverse suggests")
        pos <- match(colnames(db), names(biocrevnames), nomatch = 0L)
        colnames(db)[pos > 0] <- biocrevnames[pos]
    }
    db
}

.get_BioC_repository_URL <-
function(which = "BioCsoft")
{
    which <- match.arg(which)
    repos <- getOption("repos")
    if(!is.null(repos) && !is.na(u <- repos[which]))
        return(u)
    utils:::.get_repositories()[which, "URL"]
}

BioC_aliases_db <-
function()
    read_CRAN_object(.get_BioC_repository_URL(),
                     "src/contrib/Meta/aliases.rds")

BioC_rdxrefs_db <- 
function()
    read_CRAN_object(.get_BioC_repository_URL(),
                     "src/contrib/Meta/rdxrefs.rds")
