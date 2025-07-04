#  File src/library/grDevices/R/windows/png.R
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

.geometry <- function(width, height, units, res)
{
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    width <- switch(units,
                    "in" = res,
                    "cm" = res/2.54,
                    "mm" = res/25.4,
                    "px" = 1) * width
    height <- switch(units,
                     "in" = res,
                     "cm" = res/2.54,
                     "mm" = res/25.4,
                     "px" = 1) * height
    list(width = width, height = height)
}

png <-
    function(filename = "Rplot%03d.png",
             width = 480, height = 480, units = "px", pointsize = 12,
             bg = "white", res = NA, family = "sans",
             restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"),
             antialias = c("default", "none", "cleartype", "gray", "subpixel"),
             symbolfamily="default")
{
    if(!checkIntFormat(filename)) stop(gettextf("invalid '%s' argument", "filename"))
    g <- .geometry(width, height, units, res)
    if(match.arg(type) == "cairo") {
        antialias <- match(match.arg(antialias), aa.cairo)
        invisible(.External(C_devCairo, filename, 2L,
                            g$width, g$height, pointsize,
                            bg, res, antialias, 100L,
                            if(nzchar(family)) family else "sans", 300,
                            chooseSymbolFont(symbolfamily)))
    } else if(match.arg(type) == "cairo-png") {
        antialias <- match(match.arg(antialias), aa.cairo)
        invisible(.External(C_devCairo, filename, 5L,
                            g$width, g$height, pointsize,
                            bg, res, antialias, 100L,
                            if(nzchar(family)) family else "sans", 300,
                            chooseSymbolFont(symbolfamily)))
    } else {
        new <- if (!missing(antialias)) {
            list(bitmap.aa.win = match.arg(antialias, aa.win))
        } else list()
        antialias <-
            check.options(new = new, envir = .WindowsEnv,
                          name.opt = ".Windows.Options",
                          reset = FALSE, assign.opt = FALSE)$bitmap.aa.win
        invisible(.External(C_devga, paste0("png:", filename),
                            g$width, g$height, pointsize, FALSE, 1L,
                            NA_real_, NA_real_, bg, 1,
                            as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                            restoreConsole, "", FALSE, TRUE,
                            family, match(antialias, aa.win)))
    }
}

bmp <-
    function(filename = "Rplot%03d.bmp",
             width = 480, height = 480, units = "px", pointsize = 12,
             bg = "white", res = NA, family = "sans",
             restoreConsole = TRUE, type = c("windows", "cairo"),
             antialias = c("default", "none", "cleartype", "gray", "subpixel"),
             symbolfamily="default")
{
    if(!checkIntFormat(filename)) stop(gettextf("invalid '%s' argument", "filename"))
    g <- .geometry(width, height, units, res)
    if(match.arg(type) == "cairo") {
        antialias <- match(match.arg(antialias), aa.cairo)
        invisible(.External(C_devCairo, filename,
                            9L, g$width, g$height, pointsize,
                            bg, res, antialias, 100L,
                            if(nzchar(family)) family else "sans", 300,
                            chooseSymbolFont(symbolfamily)))
    } else {
        new <- if (!missing(antialias)) {
            list(bitmap.aa.win = match.arg(antialias, aa.win))
        } else list()
        antialias <-
            check.options(new = new, envir = .WindowsEnv,
                          name.opt = ".Windows.Options",
                          reset = FALSE, assign.opt = FALSE)$bitmap.aa.win
        invisible(.External(C_devga, paste0("bmp:", filename),
                            g$width, g$height, pointsize, FALSE, 1L,
                            NA_real_, NA_real_, bg, 1,
                            as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                            restoreConsole, "", FALSE, TRUE,
                            family, match(antialias, aa.win)))
    }
}

jpeg <-
    function(filename = "Rplot%03d.jpg",
             width = 480, height = 480, units = "px", pointsize = 12,
             quality = 75, bg = "white", res = NA, family = "sans",
             restoreConsole = TRUE, type = c("windows", "cairo"),
             antialias = c("default", "none", "cleartype", "gray", "subpixel"),
             symbolfamily="default")
{
    if(!checkIntFormat(filename)) stop(gettextf("invalid '%s' argument", "filename"))
    g <- .geometry(width, height, units, res)
    if(match.arg(type) == "cairo") {
        antialias <- match(match.arg(antialias), aa.cairo)
        invisible(.External(C_devCairo, filename, 3L, g$width, g$height, pointsize,
                            bg, res, antialias, quality,
                            if(nzchar(family)) family else "sans", 300,
                            chooseSymbolFont(symbolfamily)))
    } else {
        new <- if (!missing(antialias)) {
            list(bitmap.aa.win = match.arg(antialias, aa.win))
        } else list()
        antialias <-
            check.options(new = new, envir = .WindowsEnv,
                          name.opt = ".Windows.Options",
                          reset = FALSE, assign.opt = FALSE)$bitmap.aa.win
        invisible(.External(C_devga,
                            paste0("jpeg:", quality, ":",filename),
                            g$width, g$height, pointsize, FALSE, 1L,
                            NA_real_, NA_real_, bg, 1,
                            as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                            restoreConsole, "", FALSE, TRUE,
                            family, match(antialias, aa.win)))
    }
}

tiff <-
    function(filename = "Rplot%03d.tif",
             width = 480, height = 480, units = "px", pointsize = 12,
             compression = c("none", "rle", "lzw", "jpeg", "zip",
                             "lzw+p", "zip+p",
                             "lerc", "lzma", "zstd", "webp"),
             bg = "white", res = NA, family = "sans",
             restoreConsole = TRUE, type = c("windows", "cairo"),
             antialias = c("default", "none", "cleartype", "gray", "subpixel"),
             symbolfamily="default")
{
    if(!checkIntFormat(filename)) stop(gettextf("invalid '%s' argument", "filename"))
    g <- .geometry(width, height, units, res)
    comp <- if(is.numeric(compression)) compression
            else
                switch(match.arg(compression),
                       "none" = 1L, "rle" = 2L, "lzw" = 5L, "jpeg" = 7L,
                       "zip" = 8L, "lzw+p" = 15L, "zip+p" = 18L,
                       "lerc" = 34887L, "lzma" = 34925L,
                       "zstd" = 50000L, "webp" = 50001L)
   if(match.arg(type) == "cairo") {
        antialias <- match(match.arg(antialias), aa.cairo)
        invisible(.External(C_devCairo, filename, 8L,
                            g$width, g$height, pointsize,
                            bg, res, antialias, comp,
                            if(nzchar(family)) family else "sans", 300,
                            chooseSymbolFont(symbolfamily)))
    } else {
        new <- if (!missing(antialias)) {
            list(bitmap.aa.win = match.arg(antialias, aa.win))
        } else list()
        antialias <-
            check.options(new = new, envir = .WindowsEnv,
                          name.opt = ".Windows.Options",
                          reset = FALSE, assign.opt = FALSE)$bitmap.aa.win
        invisible(.External(C_devga,
                            paste0("tiff:", comp, ":", filename),
                            g$width, g$height, pointsize, FALSE, 1L,
                            NA_real_, NA_real_, bg, 1,
                            as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                            restoreConsole, "", FALSE, TRUE,
                            family, match(antialias, aa.win)))
    }
}

grSoftVersion <- function() {
    bm <- .Call(C_bmVersion)
    if(nzchar(bm[3L])) bm[3L] <- strsplit(bm[3L], "\n")[[1L]][1L]
    c(cairo = cairoVersion(), cairoFT = cairoFT(), pango = pangoVersion(), bm)
}

chooseSymbolFont <- function(family) {
    if (family == "default") {
        if (grSoftVersion()["cairoFT"] == "yes") {
            cairoSymbolFont("Standard Symbols L")
        } else {
            cairoSymbolFont("Symbol")
        }
    } else {
        checkSymbolFont(family)
    }
}
