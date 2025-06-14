#  File src/library/utils/R/summRprof.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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

# The profile file always starts with a single header line followed by stack lines
#   If the header contains "memory profiling", the stack lines have memory info
#     The memory info is a fixed width prefix on each line of the form :[0-9]+:[0-9]+:[0-9]+:[0-9]+:
#   If the header contains "line profiling", there will be filename lines and stack lines will contain
#     line number info of the form [0-9]+#[0-9]+
#   The filename lines will start #File [0-9]+:

summaryRprof <-
    function(filename = "Rprof.out", chunksize = 5000,
             memory = c("none", "both", "tseries", "stats"),
             lines = c("hide", "show", "both"),
             index = 2, diff = TRUE, exclude = NULL, basenames = 1)
{
    con <- file(filename, "rt")
    on.exit(close(con))
    firstline <- readLines(con, n = 1L)
    if(!length(firstline))
        stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA)
    sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L]) / 1e6
    memory.profiling <- startsWith(firstline, "memory")
    line.profiling <- grepl("line profiling", firstline)
    if (line.profiling)
    	filenames <- character(0)

    memory <- match.arg(memory)
    if(memory != "none" && !memory.profiling)
        stop("profile does not contain memory information")
    if (memory == "tseries")
        return(Rprof_memory_summary(con, chunksize = chunksize,
                                    label = index, diff = diff, exclude = exclude,
                                    sample.interval = sample.interval))
    else if (memory == "stats")
        return(Rprof_memory_summary(con, chunksize = chunksize,
                                    aggregate = index, diff = diff, exclude = exclude,
                                    sample.interval = sample.interval))

    lines <- match.arg(lines)
    if (lines != "hide" && !line.profiling)
    	stop("profile does not contain line information")

    fnames <- NULL
    ucounts <- NULL
    fcounts <- NULL
    memcounts <- NULL
    umem <- NULL

    repeat {
        chunk <- readLines(con, n = chunksize)

        if (line.profiling) {
            filenamelines <- grep("^#File [0-9]+: ", chunk)
            if (length(filenamelines)) {
       	   	fnum <- as.integer(sub("^#File ([0-9]+): .*", "\\1", chunk[filenamelines]))
       	   	filenames[fnum] <- sub("^#File [0-9]+: ", "", chunk[filenamelines])
       	   	if (basenames) {
       		    dirnames <- dirname(filenames[fnum])
       	   	    filenames[fnum] <- basename(filenames[fnum])
       	   	    for (i in seq_len(basenames - 1)) {
       	   	        tail <- basename(dirnames)
       	   	    	filenames[fnum] <- ifelse(tail == ".", filenames[fnum],
       	   	    	                          paste0(tail, "/", filenames[fnum]))
                                        # May have Windows-style names here where dirname("c:/") == "c:/"
       	   	    	parent <- dirname(dirnames)
       	   	    	dirnames <- ifelse(dirnames == parent, ".", parent)
       	   	    }
       	   	}
       	   	chunk <- chunk[-filenamelines]
            }
        }
        if (length(chunk) == 0L)
            break

        if (memory.profiling) {
            memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", chunk), "match.length")
            if (memory == "both") {
                memstuff <- substr(chunk, 2L, memprefix-1L)
                memcounts <- pmax(apply(vapply(strsplit(memstuff, ":"), as.numeric, numeric(4L)),
                                        1L, diff), 0)
                if (!is.matrix(memcounts)) # Need a matrix result (PR#16395)
                    memcounts <- matrix(memcounts, nrow = 1)
                memcounts <- c(0, rowSums(cbind(memcounts[, 1L:2L, drop = FALSE] * 8, ## convert to bytes
                                                memcounts[, 3L,    drop = FALSE])))
            }
            chunk <- substr(chunk, memprefix+1L, nchar(chunk,  "c"))
            if(any((nc <- nchar(chunk, "c")) == 0L)) {
                chunk     <- chunk    [nc > 0L]
                memcounts <- memcounts[nc > 0L]
            }
            if(!length(chunk))
                next # chunk
        }

        chunk <- strsplit(chunk, " ")
        if(line.profiling)
            chunk <- lapply(chunk, function(x) {
           	locations <- !startsWith(x, '"')
           	if (lines != "hide") {
           	    fnum <- sub("#.*", "", x[locations])
           	    lnum <- sub(".*#", "", x[locations])
           	    x[locations] <- paste0(filenames[as.integer(fnum)], "#", lnum)
                }
           	if(lines != "both")
                    x <- x[switch(lines,
                                  "hide" = !locations,
                                  "show" =  locations)]
       	      	if(length(x)) x else "<no location>"
            })
        newfirsts  <- vapply(chunk, `[[`, "char", 1L)
        newuniques <- lapply(chunk,  unique)
        ulen <- lengths(newuniques)
        newuniques <- unlist(newuniques)

        new.utable <- table(newuniques)
        new.ftable <- table(factor(newfirsts, levels = names(new.utable)))
        if (memory == "both")
            new.umem <- rowsum(memcounts[rep.int(seq_along(memcounts), ulen)], newuniques)

        fcounts <- rowsum(c(as.vector(new.ftable), fcounts),
                          c(names(new.ftable), fnames) )
        ucounts <- rowsum(c(as.vector(new.utable), ucounts),
                          c(names(new.utable), fnames) )
        if(memory == "both")
            umem <- rowsum(c(new.umem, umem), c(names(new.utable), fnames))

        fnames <- sort(unique(c(fnames, names(new.utable))))

    } # end{repeat}


    firstnum  <- fcounts*sample.interval
    uniquenum <- ucounts*sample.interval

    ## sort and form % on unrounded numbers
    index1 <- order(-firstnum, -uniquenum)
    index2 <- order(-uniquenum, -firstnum)

    if (lines == "show") {
    	filename <- sub("#.*$", "", fnames)
    	linenum <- rep.int(0, length(filename))
    	hasline <- filename != fnames
    	linenum[hasline] <- as.numeric(sub("^.*#", "", fnames[hasline]))
    	index3 <- order(filename, linenum)
    }

    firstpct  <- round(100*firstnum /sum(firstnum), 2)
    uniquepct <- round(100*uniquenum/sum(firstnum), 2)

    digits <- ifelse(sample.interval < 0.01,  3L, 2L)
    firstnum  <- round(firstnum,  digits)
    uniquenum <- round(uniquenum, digits)

    if (memory == "both") memtotal <-  round(umem/1048576, 1)     ## 0.1MB

    rval <- data.frame(firstnum, firstpct, uniquenum, uniquepct)
    names(rval) <- c("self.time", "self.pct", "total.time", "total.pct")
    rownames(rval) <- fnames
    if (memory == "both") rval$mem.total <- memtotal

    by.self <- rval[index1, ]
    by.self <- by.self[by.self[,1L] > 0, ]
    by.total <- rval[index2, c(3L, 4L,  if(memory == "both") 5L, 1L, 2L)]

    result <- list(by.self = by.self, by.total = by.total)

    if (lines == "show")
    	result <- c(result, list(by.line = rval[index3,]))

    c(result,
      sample.interval = sample.interval,
      sampling.time = sum(fcounts)*sample.interval)
}

Rprof_memory_summary <- function(con, chunksize = 5000,
                                 label = c(1, -1), aggregate = 0, diff = FALSE,
                                 exclude = NULL, sample.interval)
{
    memcounts <- NULL
    firsts <- NULL
    labels <- vector("list", length(label))
    index <- NULL

    repeat {
       chunk <- readLines(con, n = chunksize)
       if(!length(chunk))
           break # finished reading
       memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", chunk),
                         "match.length")
       memstuff <- substr(chunk, 2L, memprefix-1L)# drop boundary ":"
       mcnt <- rbind(t(sapply(strsplit(memstuff, ":"), as.numeric)))
       ## convert to bytes
       mcnt <- cbind(mcnt[, 1L:2L, drop = FALSE] * 8,
                     mcnt[, 3L:4L, drop = FALSE])
       chunk <- substr(chunk, memprefix+1, nchar(chunk,  "c"))
       if(any(c0 <- nchar(chunk,  "c") == 0L)) {
           mcnt  <- mcnt [!c0, ]
           chunk <- chunk[!c0]
       }
       if(!length(chunk))
           next # chunk

       memcounts <- rbind(memcounts, mcnt)
       chunk <- strsplit(chunk, " ")

       if (length(exclude))
           chunk <- lapply(chunk, function(l) l[!(l %in% exclude)])

       len.pos <- lengths(chunk) > 0L
       newfirsts <- sapply(chunk[len.pos], `[[`,  1L)
       firsts <- c(firsts, newfirsts)

       if (!aggregate && length(label)) {
           for(i in seq_along(label)) {
               labels[[i]] <- c(labels[[i]],
                                if (label[i] == 1)
                                    newfirsts
                                else if(label[i] > 1)
                                    vapply(chunk,
                                           function(line)
                                               paste(rev(line)[1L:min(label[i], length(line))],
                                                     collapse = ":"),
                                           "")
                                else # label[i] < 1
                                    vapply(chunk,
                                           function(line)
                                               paste(line[1L:min(-label[i], length(line))],
                                                     collapse = ":"),
                                           "")
                                )
           }
       } else if (aggregate) {
           index <- c(index,
                      vapply(chunk,
                             if(aggregate > 0)
                                 function(line)
                                     paste(rev(line)[1L:min(aggregate, length(line))], collapse = ":")

                             else # aggregate < 0
                                 function(line)
                                     paste(line[1L:min(-aggregate, length(line))],     collapse = ":"),
                             ""))
       }

       if (length(chunk) < chunksize)
           break
    }

    if (length(memcounts) == 0L) stop("no events were recorded")

    memcounts <- as.data.frame(memcounts)
    names(memcounts) <- c("vsize.small", "vsize.large", "nodes", "duplications")
    if (!aggregate) {
        rownames(memcounts) <- (1L:nrow(memcounts))*sample.interval
        names(labels) <- paste0("stack:", label)
        memcounts <- cbind(memcounts, labels)
    }

    if (diff)
        memcounts[-1L, 1L:3L] <- pmax(0L, apply(memcounts[, 1L:3L], 2L, diff))

    if (aggregate)
        by(memcounts, index,
           function(these)
               with(these,
                    round(c(vsize.small = mean(vsize.small),
                            max.vsize.small = max(vsize.small),
                            vsize.large = mean(vsize.large),
                            max.vsize.large = max(vsize.large),
                            nodes = mean(nodes),
                            max.nodes = max(nodes),
                            duplications = mean(duplications),
                            tot.duplications = sum(duplications),
                            samples = nrow(these)
                            ))
                    )
           )
    else
        memcounts
}
