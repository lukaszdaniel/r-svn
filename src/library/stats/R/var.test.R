#  File src/library/stats/R/var.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

var.test <- function(x, ...) UseMethod("var.test")

var.test.default <-
function(x, y, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, ...)
{
    if (!((length(ratio) == 1L) && is.finite(ratio) && (ratio > 0)))
        stop("'ratio' must be a single positive number")

    alternative <- match.arg(alternative)

    if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
          (conf.level > 0) && (conf.level < 1)))
        stop("'conf.level' must be a single number between 0 and 1")

    DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))

    if (inherits(x, "lm") && inherits(y, "lm")) {
        DF.x <- x$df.residual
        DF.y <- y$df.residual
        V.x <- sum(x$residuals^2) / DF.x
        V.y <- sum(y$residuals^2) / DF.y
    } else {
        x <- x[is.finite(x)]
        DF.x <- length(x) - 1L
        if (DF.x < 1L)
            stop(gettextf("not enough '%s' observations", "x"))
        y <- y[is.finite(y)]
        DF.y <- length(y) - 1L
        if (DF.y < 1L)
            stop(gettextf("not enough '%s' observations", "y"))
        V.x <- var(x)
        V.y <- var(y)
    }
    ESTIMATE <- V.x / V.y
    STATISTIC <- ESTIMATE / ratio
    PARAMETER <- c("num df" = DF.x, "denom df" = DF.y)
    PVAL <- pf(STATISTIC, DF.x, DF.y)
    if (alternative == "two.sided") {
        PVAL <- 2 * min(PVAL, 1 - PVAL)
        BETA <- (1 - conf.level) / 2
        CINT <- c(ESTIMATE / qf(1 - BETA, DF.x, DF.y),
                  ESTIMATE / qf(BETA, DF.x, DF.y))
    }
    else if (alternative == "greater") {
        PVAL <- 1 - PVAL
        CINT <- c(ESTIMATE / qf(conf.level, DF.x, DF.y), Inf)
    }
    else
        CINT <- c(0, ESTIMATE / qf(1 - conf.level, DF.x, DF.y))
    names(STATISTIC) <- "F"
    names(ESTIMATE) <- names(ratio) <- "ratio of variances"
    attr(CINT, "conf.level") <- conf.level
    METHOD <- "F test to compare two variances"
    alt.name <- switch(alternative,
                           two.sided = gettextf("true ratio of variances is not equal to %s", ratio, domain = "R-stats"),
                           less = gettextf("true ratio of variances is less than %s", ratio, domain = "R-stats"),
                           greater = gettextf("true ratio of variances is greater than %s", ratio, domain = "R-stats"))

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 conf.int = CINT,
                 estimate = ESTIMATE,
                 null.value = ratio,
                 alternative = alternative,
                 alt.name = alt.name,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

var.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    ## Call the default method.
    y <- var.test(x = DATA[[1L]], y = DATA[[2L]], ...)
    y$data.name <- DNAME
    y
}
