#  File src/library/stats/R/t.test.R
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

t.test <- function(x, ...) UseMethod("t.test")

t.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
         ...)
{
    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
    if( !is.null(y) ) {
	dname <- paste(deparse1(substitute(x)),"and",
		       deparse1(substitute(y)))
	if(paired)
	    xok <- yok <- complete.cases(x,y)
	else {
	    yok <- !is.na(y)
	    xok <- !is.na(x)
	}
	y <- y[yok]
    }
    else {
	dname <- deparse1(substitute(x))
	if (paired) stop("'y' is missing for paired test")
	xok <- !is.na(x)
	yok <- NULL
    }
    x <- x[xok]
    if (paired) {
	x <- x-y
	y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    if(is.null(y)) {
        if(nx < 2) stop("not enough 'x' observations")
	df <- nx-1
	stderr <- sqrt(vx/nx)
        if(!is.na(stderr) && stderr < 10 *.Machine$double.eps * abs(mx))
            stop("data are essentially constant")
	tstat <- (mx-mu)/stderr
	method <- if(paired) "Paired t-test" else "One Sample t-test"
	estimate <-
	    setNames(mx, if(paired)"mean difference" else "mean of x")
    } else {
	ny <- length(y)
        if(nx < 1 || (!var.equal && nx < 2))
            stop("not enough 'x' observations")
	if(ny < 1 || (!var.equal && ny < 2))
            stop("not enough 'y' observations")
        if(var.equal && nx+ny < 3) stop("not enough observations")
	my <- mean(y)
	vy <- var(y)
	method <- paste(if(!var.equal)"Welch", "Two Sample t-test")
	estimate <- c(mx,my)
	names(estimate) <- c("mean of x","mean of y")
	if(var.equal) {
	    df <- nx+ny-2
            v <- 0
            if(nx > 1) v <- v + (nx-1)*vx
            if(ny > 1) v <- v + (ny-1)*vy
	    v <- v/df
	    stderr <- sqrt(v*(1/nx+1/ny))
	} else {
	    stderrx <- sqrt(vx/nx)
	    stderry <- sqrt(vy/ny)
	    stderr <- sqrt(stderrx^2 + stderry^2)
	    df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
	}
        if(!is.na(stderr) && stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
            stop("data are essentially constant")
        tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
	pval <- pt(tstat, df)
	cint <- c(-Inf, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
	pval <- pt(tstat, df, lower.tail = FALSE)
	cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
	pval <- 2 * pt(-abs(tstat), df)
	alpha <- 1 - conf.level
        cint <- qt(1 - alpha/2, df)
	cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired) "mean difference"
                 else if(!is.null(y)) "difference in means"
                 else "mean"
    attr(cint,"conf.level") <- conf.level
    if(paired) {
	alt.name <- switch(alternative,
                           two.sided = gettextf("true mean difference is not equal to %s", mu, domain = "R-stats"),
                           less = gettextf("true mean difference is less than %s", mu, domain = "R-stats"),
                           greater = gettextf("true mean difference is greater than %s", mu, domain = "R-stats"))
   } else if (!is.null(y)) {
	alt.name <- switch(alternative,
                           two.sided = gettextf("true difference in means is not equal to %s", mu, domain = "R-stats"),
                           less = gettextf("true difference in means is less than %s", mu, domain = "R-stats"),
                           greater = gettextf("true difference in means is greater than %s", mu, domain = "R-stats"))
   } else {
	alt.name <- switch(alternative,
                           two.sided = gettextf("true mean is not equal to %s", mu, domain = "R-stats"),
                           less = gettextf("true mean is less than %s", mu, domain = "R-stats"),
                           greater = gettextf("true mean is greater than %s", mu, domain = "R-stats"))
   }

    rval <- list(statistic = tstat, parameter = df, p.value = pval,
	       conf.int = cint, estimate = estimate, null.value = mu,
	       stderr = stderr,
	       alternative = alternative,
	       alt.name = alt.name,
	       method = method, data.name = dname)
    class(rval) <- "htest"
    rval
}

t.test.formula <-
function (formula, data, subset, na.action = na.pass, ...) 
{
    if (missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    if ("paired" %in% ...names())
        stop("cannot use 'paired' in formula method")    
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) 
        if (formula[[3L]] == 1L)
            oneSampleOrPaired <- TRUE
        else
            stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ") # works in all cases
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    if (! oneSampleOrPaired) {
        g <- factor(mf[[-response]])
        if (nlevels(g) != 2L) 
            stop("grouping factor must have exactly 2 levels")
        DATA <- split(mf[[response]], g)
        ## Call the default method.
        y <- t.test(x = DATA[[1L]], y = DATA[[2L]], ...)
        if (length(y$estimate) == 2L) {
            names(y$estimate) <- paste("mean in group", levels(g))
            names(y$null.value) <-
                paste("difference in means between",
                      paste("group", levels(g), collapse = " and "))
        }
    }
    else { # 1-sample and paired tests
        respVar <- mf[[response]]
        if (inherits(respVar, "Pair")) {
            ## Call the default method.
            y <- t.test(x = respVar[, 1L], y = respVar[, 2L],
                        paired = TRUE, ...)
        }
        else {
            ## Call the default method.            
            y <- t.test(x = respVar, ...)
        }
    }
    y$data.name <- DNAME
    y
}
