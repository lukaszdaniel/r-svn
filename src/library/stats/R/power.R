#  File src/library/stats/R/power.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

power.t.test <-
    function(n=NULL, delta=NULL, sd=1, sig.level=0.05, power=NULL,
	     type=c("two.sample", "one.sample", "paired"),
	     alternative=c("two.sided", "one.sided"), strict=FALSE,
	     tol = .Machine$double.eps^0.25)
{
    if ( sum(vapply(list(n, delta, sd, power, sig.level), is.null, NA)) != 1 )
	stop("exactly one of 'n', 'delta', 'sd', 'power', and 'sig.level' must be NULL")
    assert_NULL_or_prob(sig.level)
    assert_NULL_or_prob(power)

    type <- match.arg(type)
    alternative <- match.arg(alternative)

    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    force(tsample)# codetools
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    if (tside == 2 && !is.null(delta)) delta <- abs(delta)

    p.body <-
        if (strict && tside == 2) # count rejections in opposite tail
            quote({
                nu <- pmax(1e-7, n - 1) * tsample
                qu <- qt(sig.level/tside, nu, lower.tail = FALSE)
                pt( qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE) +
                pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = TRUE)
            })
        else ## normal case:
            quote({nu <- pmax(1e-7, n - 1) * tsample
                   pt(qt(sig.level/tside, nu, lower.tail = FALSE),
                      nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE)})

    if (is.null(power))
	power <- eval(p.body)
    else if (is.null(n))
	n <- uniroot(function(n) eval(p.body) - power,
		     c(2, 1e7), tol=tol, extendInt = "upX")$root
    else if (is.null(sd))
	sd <- uniroot(function(sd) eval(p.body) - power,
		      delta * c(1e-7, 1e+7), tol=tol, extendInt = "downX")$root
    else if (is.null(delta))
	delta <- uniroot(function(delta) eval(p.body) - power,
		      sd * c(1e-7, 1e+7), tol=tol, extendInt = "upX")$root
    else if (is.null(sig.level))
	sig.level <- uniroot(function(sig.level) eval(p.body) - power,
		      c(1e-10, 1-1e-10), tol=tol, extendInt = "yes")$root
    else # Shouldn't happen
	stop("internal error", domain = NA)
    NOTE <- switch(type,
		   paired = "n is number of *pairs*, sd is std.dev. of *differences* within pairs",
		   two.sample = "n is number in *each* group", NULL)

    METHOD <- switch(type,
			   one.sample = "One-sample t test power calculation",
			   two.sample = "Two-sample t test power calculation",
			   paired = "Paired t test power calculation")

    structure(list(n=n, delta=delta, sd=sd,
		   sig.level=sig.level, power=power,
		   alternative=alternative, note=NOTE, method=METHOD),
	      class="power.htest")
}

power.prop.test <-
    function(n=NULL, p1=NULL, p2=NULL, sig.level=0.05, power=NULL,
	     alternative=c("two.sided", "one.sided"), strict=FALSE,
	     tol = .Machine$double.eps^0.25)
{
    if ( sum(vapply(list(n, p1, p2, power, sig.level), is.null, NA)) != 1 )
	stop("exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
    assert_NULL_or_prob(sig.level)
    assert_NULL_or_prob(power)

    alternative <- match.arg(alternative)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)

    p.body <-
        if (strict && tside == 2) # count rejections in opposite tail
            quote({
                qu <- qnorm(sig.level/tside, lower.tail = FALSE)
                d <- abs(p1 - p2)
                q1 <- 1 - p1
                q2 <- 1 - p2
                pbar <- (p1 + p2)/2
                qbar <- 1 - pbar
                v1 <- p1 * q1
                v2 <- p2 * q2
                vbar <- pbar * qbar
                pnorm((sqrt(n)*d - qu * sqrt(2 * vbar) ) / sqrt(v1 + v2)) +
                pnorm((sqrt(n)*d + qu * sqrt(2 * vbar) ) / sqrt(v1 + v2),
                      lower.tail=FALSE)
            })
        else ## normal case:
            quote(pnorm((sqrt(n) * abs(p1 - p2)
                          - (qnorm(sig.level/tside, lower.tail = FALSE)
                             * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))
                         / sqrt(p1 * (1 - p1) + p2 * (1 - p2))))

    if (is.null(power))
	power <- eval(p.body)
    else if (is.null(n))
	n <- uniroot(function(n) eval(p.body) - power,
		     c(1,1e7), tol=tol, extendInt = "upX")$root
    else if (is.null(p1)) {
	p1 <- uniroot(function(p1) eval(p.body) - power,
		      c(0,p2), tol=tol, extendInt = "yes")$root
        if(p1 < 0) warning("No p1 in [0, p2] can be found to achieve the desired power")
    }
    else if (is.null(p2)) {
	p2 <- uniroot(function(p2) eval(p.body) - power,
		      c(p1,1), tol=tol, extendInt = "yes")$root
        if(p2 > 1) warning("No p2 in [p1, 1] can be found to achieve the desired power")
    }
    else if (is.null(sig.level)) {
	sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                             c(1e-10, 1-1e-10), tol=tol, extendInt = "upX")$root
        if(sig.level < 0 || sig.level > 1)
            warning("No significance level [0, 1] can be found to achieve the desired power")
    }
    else # Shouldn't happen
	stop("internal error", domain = NA)

    structure(list(n=n, p1=p1, p2=p2,
		   sig.level=sig.level, power=power,
		   alternative=alternative,
                   note = "n is number in *each* group",
                   method = "Two-sample comparison of proportions power calculation"),
	      class="power.htest")
}

print.power.htest <- function(x, digits = getOption("digits"), ...)
{
    cat("\n    ", x$method, "\n\n")
    note <- x$note
    x[c("method", "note")] <- NULL
    cat(paste(format(names(x), width = 15L, justify = "right"),
	      format(x, digits=digits), sep = " = "), sep = "\n")
    if(!is.null(note)) cat("\n", "NOTE: ", note, "\n\n", sep = "") else cat("\n")
    invisible(x)
}

assert_NULL_or_prob <- function (x)
{
    if(!is.null(x) && (!is.numeric(x) || any(0 > x | x > 1)))
        stop(gettextf("'%s' must be numeric in [0, 1]",
                      deparse1(substitute(x))),
             call. = FALSE)
    invisible(x)
}
