#  File src/library/grDevices/R/unix/dev2bitmap.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

dev2bitmap <- function(file, type = "png16m", height = 7, width = 7, res = 72,
                       units = "in", pointsize, ...,
                       method = c("postscript", "pdf"), taa = NA, gaa = NA)
{
    if(missing(file)) stop("'file' is missing with no default")
    if(!is.character(file) || length(file) != 1L || !nzchar(file))
        stop("'file' must be a non-empty character string")
    method <- match.arg(method)
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * height
    width <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * width
    gsexe <- tools::find_gs_cmd()
    if(!nzchar(gsexe)) stop("GhostScript was not found")
    check_gs_type(gsexe, type)
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    tmp <- tempfile("Rbit")
    on.exit(unlink(tmp))
    din <- graphics::par("din"); w <- din[1L]; h <- din[2L]
    if(missing(width) && !missing(height)) width <- w/h * height
    if(missing(height) && !missing(width)) height <- h/w * width

    current.device <- dev.cur()
    if(method == "pdf")
        dev.off(dev.copy(device = pdf, file = tmp, width = width,
                         height = height,
                         pointsize = pointsize, paper = "special", ...))
    else
        dev.off(dev.copy(device = postscript, file = tmp, width = width,
                         height = height,
                         pointsize = pointsize, paper = "special",
                         horizontal = FALSE, ...))
    dev.set(current.device)
    extra <- ""
    if (!is.na(taa)) extra <- paste0(" -dTextAlphaBits=", taa)
    if (!is.na(gaa)) extra <- paste0(extra, " -dGraphicsAlphaBits=", gaa)
    cmd <- paste0(shQuote(gsexe), " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                  " -r", res,
                  " -dAutoRotatePages=/None",
                  " -g", ceiling(res*width), "x", ceiling(res*height),
                  extra,
                  " -sOutputFile=", shQuote(file), " ", tmp)
    system(cmd)
    invisible()
}

bitmap <- function(file, type = "png16m", height = 7, width = 7, res = 72,
                   units = "in", pointsize, taa = NA, gaa = NA, ...)
{
    if(missing(file)) stop("'file' is missing with no default")
    if(!is.character(file) || length(file) != 1L || !nzchar(file))
        stop("'file' must be a non-empty character string")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * height
    width <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * width
    gsexe <- tools::find_gs_cmd()
    if(!nzchar(gsexe)) stop("GhostScript was not found")
    check_gs_type(gsexe, type)
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    extra <- ""
    if (!is.na(taa)) extra <- paste0(" -dTextAlphaBits=", taa)
    if (!is.na(gaa)) extra <- paste0(extra, " -dGraphicsAlphaBits=", gaa)
    cmd <- paste0("|", shQuote(gsexe),
                  " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                  " -r", res,
                  " -dAutoRotatePages=/None",
                  " -g", ceiling(res*width), "x", ceiling(res*height),
                  extra,
                  " -sOutputFile=", shQuote(file), " -")
    postscript(file = cmd, width = width, height = height,
               pointsize = pointsize, paper = "special", horizontal = FALSE, ...)
    invisible()
}


## unexported
check_gs_type <- function(gsexe, type)
{
    gshelp <- system(paste(gsexe, "-help"), intern = TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    if(!length(st) || !length(en))
        warning("unrecognized format of gs -help")
    else {
        gsdevs <- gshelp[(st+1L):(en-1L)]
        devs <- c(strsplit(gsdevs, " "), recursive = TRUE)
        if(match(type, devs, 0L) == 0L) {
            op <- options(warning.length = 8000L)
            on.exit(options(op))
            stop(gettextf("device '%s' is not available\n", type),
                 gettextf("Available devices are:\n%s",
                          paste(gsdevs, collapse = "\n")),
                 domain = "R-grDevices")
        }
    }
}
