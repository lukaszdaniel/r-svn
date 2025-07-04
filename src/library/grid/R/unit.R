#  File src/library/grid/R/unit.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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


# Create an object of class "unit"
# Simple units are of the form 'unit(1, "cm")' or 'unit(1L:3, "cm")' or
# 'unit(c(1,3,6), c("cm", "inch", "npc"))'
# More complicated units are of the form 'unit(1, "string", "a string")'
# or 'unit(1, "grob", a.grob)'
unit <- function(x, units, data = NULL) {
    # Old units passed to a grob will get passed on to unit() and need to be 
    # upgraded instead of converted to the default unit
    if (is.unit(x)) return(upgradeUnit(x))

    x <- as.numeric(x)
    units <- as.character(units)
    if (length(x) == 0 || length(units) == 0)
        stop("'x' and 'units' must have length > 0")
    if (is.null(data)) {
        data <- list(NULL)
    } else if (is.language(data)) {
        data <- list(as.expression(data))
    } else if (is.character(data) || is.grob(data) || inherits(data, "gPath")) {
    	data <- list(data)
    }
    .Call(C_constructUnits, x, data, units)
}

single_unit <- function(x, data, valid_units) {
    `class<-`(list(
        list(
            x,
            data,
            valid_units
        )
    ), c("unit", "unit_v2"))
}
grid.convert <- function(x, unitTo, axisFrom="x", typeFrom="location",
                         axisTo=axisFrom, typeTo=typeFrom,
                         valueOnly=FALSE) {
    .Defunct("convertUnit")
}

convertUnit <- function(x, unitTo, axisFrom="x", typeFrom="location",
                        axisTo=axisFrom, typeTo=typeFrom,
                        valueOnly=FALSE) {
  whatfrom <- match(axisFrom, c("x", "y")) - 1L +
    2L*(match(typeFrom, c("location", "dimension")) - 1L)
  whatto <- match(axisTo, c("x", "y")) - 1L +
    2L*(match(typeTo, c("location", "dimension")) - 1L)
  x <- upgradeUnit(x)
  if (is.na(whatfrom) || is.na(whatto))
    stop("invalid 'axis' or 'type'")
  value <- grid.Call(C_convert, x, as.integer(whatfrom),
                 as.integer(whatto), valid.units(unitTo))
  if (!valueOnly)
    unit(value, unitTo)
  else
    value
}

grid.convertX <- function(x, unitTo, valueOnly=FALSE) {
  .Defunct("convertX")
}

convertX <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "x", "location", "x", "location",
              valueOnly=valueOnly)
}

grid.convertY <- function(x, unitTo, valueOnly=FALSE) {
  .Defunct("convertY")
}

convertY <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "y", "location", "y", "location",
              valueOnly=valueOnly)
}

grid.convertWidth <- function(x, unitTo, valueOnly=FALSE) {
  .Defunct("convertWidth")
}

convertWidth <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "x", "dimension", "x", "dimension",
              valueOnly=valueOnly)
}

grid.convertHeight <- function(x, unitTo, valueOnly=FALSE) {
  .Defunct("convertHeight")
}

convertHeight <- function(x, unitTo, valueOnly=FALSE) {
  convertUnit(x, unitTo, "y", "dimension", "y", "dimension",
              valueOnly=valueOnly)
}

convertNative <- function(unit, dimension="x", type="location") {
  .Defunct("convertUnit")
}

deviceLoc <- function(x, y, valueOnly=FALSE, device=FALSE) {
    if (!(is.unit(x) && length(x) > 0 &&
          is.unit(y) && length(y) > 0 &&
          is.logical(device) && length(device) == 1 && !is.na(device)))
        stop("'x' and 'y' must be units and 'device' must be logical")
    result <- grid.Call(C_devLoc, x, y, as.logical(device))
    names(result) <- c("x", "y")
    if (valueOnly) {
        result
    } else {
        if (device) {
            list(x=unit(result$x, "native"), y=unit(result$y, "native"))
        } else {
            list(x=unit(result$x, "in"), y=unit(result$y, "in"))
        }
    }
}

deviceDim <- function(w, h, valueOnly=FALSE, device=FALSE) {
    if (!(is.unit(w) && length(w) > 0 &&
          is.unit(h) && length(h) > 0 &&
          is.logical(device) && length(device) == 1 && !is.na(device)))
        stop("'w' and 'h' must be units and 'device' must be logical")
    result <- grid.Call(C_devDim, w, h, as.logical(device))
    names(result) <- c("w", "h")
    if (valueOnly) {
        result
    } else {
        if (device) {
            list(w=unit(result$w, "native"), h=unit(result$h, "native"))
        } else {
            list(w=unit(result$w, "in"), h=unit(result$h, "in"))
        }
    }
}

# This is like the "convert" functions:  it evaluates units (immediately)
# in the current context
calcStringMetric <- function(text) {
    # .Call rather than .Call.graphics because it is a one-off calculation
    metric <- grid.Call(C_stringMetric, text)
    names(metric) <- c("ascent", "descent", "width")
    metric
}

# NOTE: the order of the strings in these conversion functions must
# match the order of the enums in ../src/grid.h
# AND in ../src/unit.c (see UnitTable)
# NOTE: ../src/unit.c also allows some pseudonyms (e.g., "in" for "inches")
.grid.unit.list <- c("npc", "cm", "inches", "lines",
                     "native", "null", "snpc", "mm",
                     "points", "picas", "bigpts",
                     "dida", "cicero", "scaledpts",
                     "strwidth", "strheight",
                     "strascent", "strdescent",
                     "vplayoutwidth", "vplayoutheight", "char",
                     "grobx", "groby", "grobwidth", "grobheight",
                     "grobascent", "grobdescent",
                     "mylines", "mychar", "mystrwidth", "mystrheight",
                     "sum", "min", "max")

valid.units <- function(units) {
  .Call(C_validUnits, units)
}

# Printing, formating, and coercion
unitDesc <- function(x, format = FALSE, ...) {
    amount <- if (format) format(x[[1]], ...) else x[[1]]
    unit <- units[as.character(x[[3]])]
    if (unit %in% c('sum', 'min', 'max')) {
        paste0(if (amount == 1) '' else paste0(amount, '*'),
               unit, '(',
               paste(lapply(unclass(x[[2]]), unitDesc, format = format, ...),
                     collapse = ', '),
               ')')
    } else {
        paste0(amount, unit)
    }
}

unitType <- function(x, recurse=FALSE) {
    x <- upgradeUnit(x)
    if (is.simpleUnit(x)) {
        names <- rep_len(units[[as.character(attr(x, "unit"))]], length(x))
        if (recurse) {
            unit <- as.list(names)
            names(unit) <- names
            unit
        } else {
            names
        }
    } else {
        unit <- lapply(unclass(x), `[[`, 3)
        names <- unlist(units[as.character(unit)], use.names=FALSE)
        if (recurse) {
            sub <- names %in% c("sum", "min", "max")
            if (any(sub)) {
                unit[sub] <- lapply(unclass(x)[sub],
                                    function(u) unitType(u[[2]], recurse))
            }
            if (any(!sub)) {
                unit[!sub] <- names[!sub]
            }
            names(unit) <- names
            unit
        } else {
            names
        }
    }
}

as.character.unit <- function(x, ...) {
    x <- upgradeUnit(x) # guard against old unit
    vapply(unclass(as.unit(x)), unitDesc, character(1))
}
as.double.unit <- function(x, ...) {
    x <- upgradeUnit(x) # guard against old unit
    vapply(unclass(x), `[[`, numeric(1), 1L)
}
as.vector.unit <- as.double.unit
format.unit <- function(x, ...) {
    x <- upgradeUnit(x) # guard against old unit
    vapply(unclass(as.unit(x)), unitDesc, character(1), format = TRUE, ...)
}
print.unit <- function(x, ...) {
    print(as.character(x), quote = FALSE, ...)
    invisible(x)
}
as.double.simpleUnit <- function(x, ...) as.double(unclass(x), ...)
as.vector.simpleUnit <- function(x, ...) as.double(unclass(x), ...)

upgradeUnit <- function(x) {
    if (is.newUnit(x)) return(x)
    UseMethod("upgradeUnit")
}
upgradeUnit.unit <- function(x) {
    unit(unclass(x), attr(x, "unit"), attr(x, 'data'))
}
upgradeUnit.unit.list <- function(x) {
    do.call(unit.c, lapply(unclass(x), upgradeUnit))
}
upgradeUnit.unit.arithmetic <- function(x) {
    fun <- .subset2(x, "fname")
    arg1 <- .subset2(x, "arg1")
    if (inherits(arg1, "unit")) arg1 <- upgradeUnit(arg1)
    arg2 <- .subset2(x, "arg2")
    if (inherits(arg2, "unit")) arg2 <- upgradeUnit(arg2)
    
    do.call(fun, list(arg1, arg2))
}
upgradeUnit.default <- function(x) {
    stop("Not a unit object")
}
is.unit <- function(x) {
    inherits(x, 'unit')
}
is.newUnit <- function(x) {
    inherits(x, 'unit_v2')
}
is.simpleUnit <- function(x) inherits(x, 'simpleUnit')
identicalUnits <- function(x) .Call(C_conformingUnits, x)

as.unit <- function(x) {
	.Call(C_asUnit, x)
}

str.unit <- function(object, ...) {
    object <- upgradeUnit(object)
    object <- unclass(as.unit(object))
    for (i in seq_along(object)) {
      unit <- object[[i]]
      cat('[[', i, ']] Amount: ', unit[[1]], '; Unit: ', 
          units[[as.character(unit[[3]])]], '; Data: ', 
          if (is.null(unit[[2]])) 
              'none' 
          else 
              paste(as.character(unit[[2]]), collapse = ", "), '\n', sep = '')
    }
}
#########################
# UNIT ARITHMETIC STUFF
#########################

Summary.unit <- function(..., na.rm=FALSE) {
    units <- list(...)
    units <- units[!vapply(units, is.null, logical(1))]
    ok <- switch(.Generic, "sum" = 201L, "min" = 202L, "max" = 203L, 0L)
    if (ok == 0)
        stop(gettextf("'Summary' function '%s' not meaningful for units",
                      .Generic), domain = NA)
    # Optimise for simple units
    identicalSimple <- identicalUnits(units)
    if (!is.null(identicalSimple)) {
        res <- switch(
            .Generic,
            "sum" = sum(unlist(units)),
            "min" = min(unlist(units)),
            "max" = max(unlist(units)),
        )
        return(`attributes<-`(res, list(
            class = c("simpleUnit", "unit", "unit_v2"), 
            unit = identicalSimple
        )))
    }
    # NOTE that this call to unit.c makes sure that arg1 is
    # a single unit object
    x <- unlist(lapply(units, as.unit), recursive = FALSE)
    class(x) <- c('unit', 'unit_v2')
    matchUnits <- .Call(C_matchUnit, x, ok)
    x <- unclass(x)
    nMatches <- length(matchUnits)
    
    if (nMatches != 0) {
        data <- lapply(x, .subset2, 2L)
        amount <- vapply(x, .subset2, numeric(1), 1L)[matchUnits]
        amount <- rep(amount, lengths(data[matchUnits]))
        matchData <- unclass(unlist(data[matchUnits], recursive = FALSE))
        for (i in seq_along(amount)) {
            if (amount[i] != 1) 
                matchData[[i]][[1]] <- matchData[[i]][[1]] * amount[i]
        }
        if (nMatches == length(x)) {
            data <- matchData
        } else {
            data <- c(x[-matchUnits], matchData)
        }
    } else {
        data <- x
    }
    single_unit(1, `class<-`(data, c('unit', 'unit_v2')), ok)
}
Ops.unit <- function(e1, e2) {
    ok <- switch(.Generic, "+"=TRUE, "-"=TRUE, "*"=TRUE, "/"=TRUE, FALSE)
    if (!ok)
        stop(gettextf("operator '%s' not meaningful for units", .Generic),
             domain = NA)
    # Unary
    if (missing(e2)) {
        if (.Generic %in% c('*', '/')) 
            stop("'*' or '/' cannot be used as a unary operator")
        if (.Generic == '-') {
            if (is.simpleUnit(e1)) {
                attr <- attributes(e1)
                e1 <- -as.vector(e1)
                attributes(e1) <- attr
            } else {
                e1 <- upgradeUnit(e1)  # guard against old unit
                e1 <- .Call(C_flipUnits, e1)
            }
        }
        return(e1)
    }
    # Multiply
    if (.Generic %in% c("*", "/")) {
        # can only multiply a unit by a scalar
        if (nzchar(.Method[1L])) {
            if (nzchar(.Method[2L])) stop("only one operand may be a unit")
            if (!is.numeric(e2)) stop("non-unit operand must be numeric")
            unit <- e1
            value <- e2
        } else {
            if (!is.numeric(e1)) stop("non-unit operand must be numeric")
            if (.Generic == "/") stop("can't divide with a unit")
            unit <- e2
            value <- e1
        }
        if (.Generic == "/") value <- 1 / value
        if (is.simpleUnit(unit)) {
            attr <- attributes(unit)
            unit <- value * as.vector(unit)
            attributes(unit) <- attr
        } else {
            unit <- upgradeUnit(unit)  # guard against old unit
            unit <- .Call(C_multUnits, unit, value)
        }
        return(unit)
    }
    # Add and sub remains
    if (!nzchar(.Method[1L]) && !nzchar(.Method[2L])) {
        stop("both operands must be units")
    }
    if ((is.simpleUnit(e1) && is.simpleUnit(e2)) && (attr(e1, 'unit') == attr(e2, 'unit'))) {
        attr <- attributes(e1)
        unit <- switch(
            .Generic, 
            "-" = as.vector(e1) - as.vector(e2), 
            "+" = as.vector(e1) + as.vector(e2)
        )
        return(`attributes<-`(unit, attr))
    }
    # Convert subtraction to addition
    if (.Generic == '-') {
        e2 <- -e2
    }
    .Call(C_addUnits, as.unit(e1), as.unit(e2))
}

unit.pmin <- function(...) {
    pSummary(..., op = 'min')
}

unit.pmax <- function(...) {
    pSummary(..., op = 'max')
}

unit.psum <- function(...) {
    pSummary(..., op = 'sum')
}

pSummary <- function(..., op) {
    units <- list(...)
    units <- units[lengths(units) != 0]
    # optimisation for simple units
    identicalSimple <- identicalUnits(units)
    if (!is.null(identicalSimple)) {
        res <- switch(
            op,
            "sum" = Reduce(`+`, lapply(units, unclass)),
            "min" = do.call(pmin, lapply(units, unclass)),
            "max" = do.call(pmax, lapply(units, unclass))
        )
        return(`attributes<-`(res, list(
            class = c("simpleUnit", "unit", "unit_v2"), 
            unit = identicalSimple
        )))
    }
    op <- switch(op, sum = 201L, min = 202L, max = 203L)
    .Call(C_summaryUnits, units, op)
}

#########################
# Unit subsetting
#########################

## 'top' argument retained to avoid breaking any uses from it
## that are hang-overs from old unit implementation

`[.unit` <- function(x, index, ..., top = TRUE) {
    x <- upgradeUnit(x) # guard against old unit
    attr <- attributes(x)
    x <- unclass(x)
    n <- length(x)
    if (is.numeric(index) && any(index > n)) {
        if (top) stop('index out of bounds ("unit" subsetting)', call. = FALSE)
        index <- (seq_len(n)[index] - 1L) %% n + 1L
    }
    x <- x[index]
    if (length(x) == 0) {
        stop('Cannot create zero-length unit vector ("unit" subsetting)', call. = FALSE)
    }
    if (!is.null(attr$names)) attr$names <- attr$names[index]
    `attributes<-`(x, attr)
}
`[[.unit` <- function(x, index, ...) {
    if (length(index) != 1) {
        stop("index must be of length 1", call. = FALSE)
    }
    x[index]
}
`[<-.unit` <- function(x, i, value) {
    x <- upgradeUnit(x) # guard against old unit
    attr <- attributes(x)
    simpleResult <- FALSE
    if (is.simpleUnit(x)) {
        if (!(is.simpleUnit(value) && attr(x, 'unit') == attr(value, 'unit'))) {
            x <- as.unit(x)
            value <- as.unit(value)
        } else {
            simpleResult <- TRUE
        }
    } else {
        value <- as.unit(value)
    }
    x <- unclass(x)
    x[i] <- value
    if (simpleResult) {
        attributes(x) <- attr
    } else {
        class(x) <- c("unit", "unit_v2")
    }
    x
}
`[[<-.unit` <- function(x, i, value) {
    if (length(i) != 1) {
        stop("index must be of length 1", call. = FALSE)
    }
    if (length(value) != 1) {
        stop("replacement must be of length 1", call. = FALSE)
    }
    x[i] <- value
    x
}
#########################
# "c"ombining unit objects
#########################

# NOTE that I have not written methods for c()
# because method dispatch occurs on the first argument to
# "c" so c(unit(...), ...) would come here, but c(whatever, unit(...), ...)
# would go who-knows-where.
# A particularly nasty example is:  c(1, unit(1, "npc")) which will
# produce the same result as c(1, 1)
# Same problem for trying to control c(<unit>, <unit.arithmetic>)
# versus c(<unit.arithmetic>, <unit>), etc ...

# If any arguments are unit.arithmetic or unit.list, then the result will be
# unit.list

unit.c <- function(..., check = TRUE) {
    x <- list(...)
    identicalSimple <- identicalUnits(x)
    if (!is.null(identicalSimple)) {
        `attributes<-`(unlist(x), list(class = c('simpleUnit', 'unit', 'unit_v2'), unit = identicalSimple))
    } else {
        `class<-`(unlist(lapply(x, as.unit), recursive = FALSE), c('unit', 'unit_v2'))
    }
}

#########################
# rep'ing unit objects
#########################

rep.unit <- function(x, times = 1, length.out = NA, each = 1, ...) {
    index <- rep(seq_along(x), times = times, length.out = length.out, each = each)
    x[index]
}

# Vestige from when rep() was not generic
unit.rep <- function (x, ...)
{
  warning("'unit.rep' has been deprecated in favour of a unit method for the generic rep function", domain = NA)
  rep(x, ...)
}

#########################
# Length of unit objects
#########################

# Vestige of when length was not generic and a custom length method was needed
unit.length <- function(unit) {
   warning("'unit.length' has been deprecated in favour of a unit method for the generic length function", domain = NA)
   length(unit)
}

#########################
# Convenience functions
#########################

stringWidth <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        string <- as.expression(string)
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strwidth", data=data)
}

stringHeight <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        string <- as.expression(string)
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strheight", data=data)
}

stringAscent <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        string <- as.expression(string)
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strascent", data=data)
}

stringDescent <- function(string) {
    n <- length(string)
    if (is.language(string)) {
        string <- as.expression(string)
        data <- vector("list", n)
        for (i in 1L:n)
            data[[i]] <- string[i]
    } else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strdescent", data=data)
}

convertTheta <- function(theta) {
    if (is.character(theta))
        # Allow some aliases for common angles
        switch(theta,
               east=0,
               north=90,
               west=180,
               south=270,
               stop(gettextf("invalid '%s' argument", "theta")))
    else
        # Ensure theta in [0, 360)
        theta <- as.numeric(theta) %% 360
}

# grobX
grobX <- function(x, theta) {
    UseMethod("grobX", x)
}

grobX.grob <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=x)
}

grobX.gList <- function(x, theta) {
  unit(rep(convertTheta(theta), length(x)), "grobx", data=x)
}

grobX.gPath <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=x)
}

grobX.default <- function(x, theta) {
  unit(convertTheta(theta), "grobx", data=gPath(as.character(x)))
}

# grobY
grobY <- function(x, theta) {
    UseMethod("grobY", x)
}

grobY.grob <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=x)
}

grobY.gList <- function(x, theta) {
  unit(rep(convertTheta(theta), length(x)), "groby", data=x)
}

grobY.gPath <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=x)
}

grobY.default <- function(x, theta) {
  unit(convertTheta(theta), "groby", data=gPath(as.character(x)))
}

# grobWidth
grobWidth <- function(x) {
  UseMethod("grobWidth")
}

grobWidth.grob <- function(x) {
  unit(1, "grobwidth", data=x)
}

grobWidth.gList <- function(x) {
  unit(rep_len(1, length(x)), "grobwidth", data=x)
}

grobWidth.gPath <- function(x) {
  unit(1, "grobwidth", data=x)
}

grobWidth.default <- function(x) {
  unit(1, "grobwidth", data=gPath(as.character(x)))
}

# grobHeight
grobHeight <- function(x) {
  UseMethod("grobHeight")
}

grobHeight.grob <- function(x) {
  unit(1, "grobheight", data=x)
}

grobHeight.gList <- function(x) {
  unit(rep_len(1, length(x)), "grobheight", data=x)
}

grobHeight.gPath <- function(x) {
  unit(1, "grobheight", data=x)
}

grobHeight.default <- function(x) {
  unit(1, "grobheight", data=gPath(as.character(x)))
}

# grobAscent
grobAscent <- function(x) {
  UseMethod("grobAscent")
}

grobAscent.grob <- function(x) {
  unit(1, "grobascent", data=x)
}

grobAscent.gList <- function(x) {
  unit(rep_len(1, length(x)), "grobascent", data=x)
}

grobAscent.gPath <- function(x) {
  unit(1, "grobascent", data=x)
}

grobAscent.default <- function(x) {
  unit(1, "grobascent", data=gPath(as.character(x)))
}

# grobDescent
grobDescent <- function(x) {
  UseMethod("grobDescent")
}

grobDescent.grob <- function(x) {
  unit(1, "grobdescent", data=x)
}

grobDescent.gList <- function(x) {
  unit(rep_len(1, length(x)), "grobdescent", data=x)
}

grobDescent.gPath <- function(x) {
  unit(1, "grobdescent", data=x)
}

grobDescent.default <- function(x) {
  unit(1, "grobdescent", data=gPath(as.character(x)))
}

#########################
# Function to decide which values in a unit are "absolute" (do not depend
# on parent's drawing context or size)
#########################

absolute.units <- function(unit) {
    .Call(C_absoluteUnits, unit)
}

# Lookup table for unit ids
# This table MUST correspond to the enumeration in grid.h
units <- list(
    '0' = "npc",
    '1' = "cm",
    '2' = "inches",
    '3' = "lines",
    '4' = "native",
    '5' = "null",
    '6' = "snpc",
    '7' = "mm",
    '8' = "points",
    '9' = "picas",
    '10' = "bigpts",
    '11' = "dida",
    '12' = "cicero",
    '13' = "scaledpts",
    '14' = "strwidth",
    '15' = "strheight",
    '16' = "strascent",
    '17' = "strdescent",
    '18' = "char",
    '19' = "grobx",
    '20' = "groby",
    '21' = "grobwidth",
    '22' = "grobheight",
    '23' = "grobascent",
    '24' = "grobdescent",

    '103' = "mylines",
    '104' = "mychar",
    '105' = "mystrwidth",
    '106' = "mystrheight",

    '201' = "sum",
    '202' = "min",
    '203' = "max",

    '1001' = "centimetre",
    '1001' = "centimetres",
    '1001' = "centimeter",
    '1001' = "centimeters",
    '1002' = "in",
    '1002' = "inch",
    '1003' = "line",
    '1007' = "millimetre",
    '1007' = "millimetres",
    '1007' = "millimeter",
    '1007' = "millimeters",
    '1008' = "point",
    '1008' = "pt"
)

