#  File src/library/stats/R/cor.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
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

cor.test <- function(x, ...) UseMethod("cor.test")

cor.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"), exact = NULL,
         conf.level = 0.95, continuity = FALSE, ...)
{
    alternative <- match.arg(alternative)
    method <- match.arg(method)
    DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))

    if(!is.numeric(x)) stop("'x' must be a numeric vector")
    if(!is.numeric(y)) stop("'y' must be a numeric vector")
    if(length(x) != length(y))
	stop("'x' and 'y' must have the same length")
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    n <- length(x)

    NVAL <- 0
    conf.int <- FALSE

    if(method == "pearson") {
	if(n < 3L)
	    stop("not enough finite observations")
	method <- "Pearson's product-moment correlation"
	names(NVAL) <- "correlation"
	r <- cor(x, y)
        df <- n - 2L
	ESTIMATE <- c(cor = r)
	PARAMETER <- c(df = df)
	STATISTIC <- c(t = sqrt(df) * r / sqrt(1 - r^2))
        if(n > 3) { ## confidence int.
            if(!missing(conf.level) &&
               (length(conf.level) != 1 || !is.finite(conf.level) ||
                conf.level < 0 || conf.level > 1))
                stop("'conf.level' must be a single number between 0 and 1")
            conf.int <- TRUE
            z <- atanh(r)
            sigma <- 1 / sqrt(n - 3)
            cint <-
                switch(alternative,
                       less = c(-Inf, z + sigma * qnorm(conf.level)),
                       greater = c(z - sigma * qnorm(conf.level), Inf),
                       two.sided = z +
                       c(-1, 1) * sigma * qnorm((1 + conf.level) / 2))
            cint <- tanh(cint)
            attr(cint, "conf.level") <- conf.level
        }
	PVAL <- switch(alternative,
		       "less" = pt(STATISTIC, df),
		       "greater" = pt(STATISTIC, df, lower.tail=FALSE),
		       "two.sided" = 2 * min(pt(STATISTIC, df),
					     pt(STATISTIC, df, lower.tail=FALSE)))
    }
    else {
	if(n < 2)
	    stop("not enough finite observations")
	PARAMETER <- NULL
	TIES <- (min(length(unique(x)), length(unique(y))) < n)
	if(method == "kendall") {
	    method <- "Kendall's rank correlation tau"
	    names(NVAL) <- "tau"
	    r <- cor(x,y, method = "kendall")
            ESTIMATE <- c(tau = r)

            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(T = NA)
                PVAL <- NA
            }
            else {
                if(is.null(exact))
                    exact <- (n < 50)
                if(exact && !TIES) {
                    q <- round((r + 1) * n * (n - 1) / 4)
                    STATISTIC <- c(T = q)
                    pkendall <- function(q, n) .Call(C_pKendall, q, n)
                    PVAL <-
                        switch(alternative,
                               "two.sided" = {
                                   if(q > n * (n - 1) / 4)
                                       p <- 1 - pkendall(q - 1, n)
                                   else
                                       p <- pkendall(q, n)
                                   min(2 * p, 1)
                               },
                               "greater" = 1 - pkendall(q - 1, n),
                               "less" = pkendall(q, n))
                } else {
                    xties <- table(x[duplicated(x)]) + 1
                    yties <- table(y[duplicated(y)]) + 1
                    T0 <- n * (n - 1)/2
                    T1 <- sum(xties * (xties - 1))/2
                    T2 <- sum(yties * (yties - 1))/2
                    S <- r * sqrt((T0 - T1) * (T0 - T2))
                    v0 <- n * (n - 1) * (2 * n + 5)
                    vt <- sum(xties * (xties - 1) * (2 * xties + 5))
                    vu <- sum(yties * (yties - 1) * (2 * yties + 5))
                    v1 <- sum(xties * (xties - 1)) * sum(yties * (yties - 1))
                    v2 <- sum(xties * (xties - 1) * (xties - 2)) *
                          sum(yties * (yties - 1) * (yties - 2))

                    var_S <- (v0 - vt - vu) / 18 +
                        v1 / (2 * n * (n - 1)) +
                            v2 / (9 * n * (n - 1) * (n - 2))

                    if(exact && TIES)
                        warning("Cannot compute exact p-value with ties")
                    if (continuity) S <- sign(S) * (abs(S) - 1)
                    STATISTIC <- c(z = S / sqrt(var_S))
		    PVAL <- switch(alternative,
				   "less" = pnorm(STATISTIC),
				   "greater" = pnorm(STATISTIC, lower.tail=FALSE),
				   "two.sided" = 2 * min(pnorm(STATISTIC),
							 pnorm(STATISTIC, lower.tail=FALSE)))
                }
            }
	} else {
	    method <- "Spearman's rank correlation rho"
            if (is.null(exact))
                exact <- TRUE
	    names(NVAL) <- "rho"
	    r <- cor(rank(x), rank(y))
	    ESTIMATE <- c(rho = r)
            if(!is.finite(ESTIMATE)) {  # all x or all y the same
                ESTIMATE[] <- NA
                STATISTIC <- c(S = NA)
                PVAL <- NA
            }
            else {
                ## Use the test statistic S = sum(rank(x) - rank(y))^2
                ## and AS 89 for obtaining better p-values than via the
                ## simple normal approximation.
                ## In the case of no ties, S = (1-rho) * (n^3-n)/6.
                pspearman <- function(q, n, lower.tail = TRUE) {
                    if(n <= 1290 && exact) # n*(n^2 - 1) does not overflow
                        .Call(C_pRho, round(q) + 2*lower.tail, n, lower.tail)
		    else { # for large n: asymptotic t_{n-2}
                        den <- (n*(n^2-1))/6 # careful for overflow
                        ## Kendall et all (1939) p. 260
                        if (continuity) den <- den + 1
			r <- 1 - q/den
			pt(r / sqrt((1 - r^2)/(n-2)), df = n-2,
			   lower.tail = !lower.tail)
		    }
                }
                q <- (n^3 - n) * (1 - r) / 6
                STATISTIC <- c(S = q)
                if(TIES && exact){
                    exact <- FALSE
                    warning("Cannot compute exact p-value with ties")
                }
                PVAL <-
                    switch(alternative,
                           "two.sided" = {
                               p <- if(q > (n^3 - n) / 6)
                                   pspearman(q, n, lower.tail = FALSE)
                               else
				   pspearman(q, n, lower.tail = TRUE)
			       min(2 * p, 1)
			   },
			   "greater" = pspearman(q, n, lower.tail = TRUE),
			   "less" = pspearman(q, n, lower.tail = FALSE))
            }
        }
    }

    if(names(NVAL) == "correlation") {
	    alt.name <- switch(alternative,
                           two.sided = gettextf("true correlation is not equal to %s", NVAL, domain = "R-stats"),
                           less = gettextf("true correlation is less than %s", NVAL, domain = "R-stats"),
                           greater = gettextf("true correlation is greater than %s", NVAL, domain = "R-stats"))
    } else if(names(NVAL) == "tau") {
	    alt.name <- switch(alternative,
                           two.sided = gettextf("true tau is not equal to %s", NVAL, domain = "R-stats"),
                           less = gettextf("true tau is less than %s", NVAL, domain = "R-stats"),
                           greater = gettextf("true tau is greater than %s", NVAL, domain = "R-stats"))
   } else {
	    alt.name <- switch(alternative,
                           two.sided = gettextf("true rho is not equal to %s", NVAL, domain = "R-stats"),
                           less = gettextf("true rho is less than %s", NVAL, domain = "R-stats"),
                           greater = gettextf("true rho is greater than %s", NVAL, domain = "R-stats"))

   }

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = as.numeric(PVAL),
                 estimate = ESTIMATE,
                 null.value = NVAL,
                 alternative = alternative,
                 alt.name = alt.name,
                 method = method,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL, list(conf.int = cint))
    class(RVAL) <- "htest"
    RVAL
}

cor.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || !inherits(formula, "formula")
       || length(formula) != 2L)
        stop("'formula' missing or invalid")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    if(length(mf) != 2L)
        stop("invalid formula")
    DNAME <- paste(names(mf), collapse = " and ")
    ## Call the default method.    
    y <- cor.test(x = mf[[1L]], y = mf[[2L]], ...)
    y$data.name <- DNAME
    y
}
