#  File src/library/stats/R/models.R
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

formula <- function(x, ...) UseMethod("formula")
formula.default <- function (x = NULL, env = parent.frame(), ...)
{
    notAtomic <- !is.atomic(x)
    notnull <- function(z) notAtomic && !is.null(z)

    if (notnull(x$formula)) eval(x$formula)
    else if (notnull(x$terms)) {z <- x$terms; oldClass(z) <- "formula"; z}
    else if (notnull(x$call$formula))	eval(x$call$formula)
    else attr(x, "formula") %||% {
        form <- switch(mode(x),
                       NULL = structure(list(), class = "formula"),
                       character = eval(str2expression(x)), # ever used?  formula.character!
                       call = eval(x),
                       stop("invalid formula"))
        environment(form) <- env
        form
    }
}
formula.formula <- function(x, ...) x
formula.terms <- function(x, ...) {
    env <- environment(x)
    attributes(x) <- list(class = "formula") # dropping all attr. incl ".Environment"
    environment(x) <- env %||% globalenv()
    x
}

DF2formula <- function(x, env = parent.frame()) {
    nm <- lapply(names(x), as.name)
    mkRHS <- function(nms) Reduce(function(x, y) call("+", x, y), nms)
    ff <- if (length(nm) > 1L)
              call("~", nm[[1L]], mkRHS(nm[-1L]))
          else if (length(nm) == 1L)
              call("~", nm[[1L]])
          else stop("cannot create a formula from a zero-column data frame")
    class(ff) <- "formula" # was ff <- eval(ff)
    environment(ff) <- env
    ff
}

formula.data.frame <- function (x, ...)
{
    if(length(tx <- attr(x, "terms")) && length(ff <- formula.terms(tx)))
	ff
    else DF2formula(x, parent.frame())
}

## Future version {w/o .Deprecated etc}:
formula.character <- function(x, env = parent.frame(), ...)
{
    ff <- str2lang(x)
    if(!(is.call(ff) && is.symbol(c. <- ff[[1L]]) && c. == quote(`~`)))
        stop(gettextf("invalid formula: %s", deparse2(x)), domain=NA)
    class(ff) <- "formula"
    environment(ff) <- env
    ff
}

## Active version helping to move towards future version:
formula.character <- function(x, env = parent.frame(), ...)
{
    ff <- if(length(x) > 1L) {
              .Deprecated(msg=
 gettext("Using formula(x) is deprecated when x is a character vector of length > 1.
  Consider formula(paste(x, collapse = \" \")) instead."))
              str2expression(x)[[1L]]
          } else {
              str2lang(x)
          }
    if(!is.call(ff))
        stop(gettextf("invalid formula %s: not a call", deparse2(x)), domain=NA)
    ## else
    if(is.symbol(c. <- ff[[1L]]) && c. == quote(`~`)) {
        ## perfect
    } else {
        if(is.symbol(c.)) { ## back compatibility
            ff <- if(c. == quote(`=`)) {
                      .Deprecated(msg = gettextf(
				"invalid formula %s: assignment is deprecated",
				deparse2(x)))
                      ff[[3L]] # RHS of "v = <form>" (pkgs 'GeNetIt', 'KMgene')
                  } else if(c. == quote(`(`) || c. == quote(`{`)) {
                      .Deprecated(msg = gettextf(
			"invalid formula %s: extraneous call to '%s' is deprecated",
			deparse2(x), as.character(c.)))
                      eval(ff)
                  }
        } else
            stop(gettextf("invalid formula %s", deparse2(x)), domain=NA)
    }
    class(ff) <- "formula"
    environment(ff) <- env
    ff
}

print.formula <- function(x, showEnv = !identical(e, .GlobalEnv), ...)
{
    e <- environment(.x <- x) ## return(.) original x
    attr(x, ".Environment") <- NULL
    print.default(unclass(x), ...)
    if (showEnv) print(e)
    invisible(.x)
}

`[.formula` <- function(x,i) {
    ans <- NextMethod("[")
    if(!length(ans) || is.symbol(a1 <- ans[[1L]]) && as.character(a1) == "~") {
        if(is.null(ans)) ans <- list()
        class(ans) <- "formula"
        environment(ans) <- environment(x)
    }
    ans
}

as.formula <- function(object, env = parent.frame())
{
    if(inherits(object, "formula"))
        object
    else {
        rval <- formula(object, env = baseenv())
        if (identical(environment(rval), baseenv()) || !missing(env))
            environment(rval) <- env
        rval
    }
}

terms <- function(x, ...) UseMethod("terms")
terms.default <- function(x, ...) {
    x$terms %||% attr(x, "terms") %||% stop("no terms component nor attribute")
}

terms.terms <- function(x, ...) x
print.terms <- function(x, ...) {
    print.default(unclass(x), ...)
    invisible(x)
}

## moved from base/R/labels.R
labels.terms <- function(object, ...) attr(object, "term.labels")

### do this `by hand' as previous approach was vulnerable to re-ordering.
delete.response <- function (termobj)
{
    a <- attributes(termobj)
    y <- a$response
    if(!is.null(y) && y) {
        termobj[[2L]] <- NULL
        a$response <- 0
        a$variables <- a$variables[-(1+y)]
        a$predvars <- a$predvars[-(1+y)]
        if(length(a$factors))
            a$factors <- a$factors[-y, , drop = FALSE]
        if(length(a$offset))
            a$offset <- ifelse(a$offset > y, a$offset-1, a$offset)
        if(length(a$specials))
            for(i in seq_along(a$specials)) {
                b <- a$specials[[i]]
                a$specials[[i]] <- ifelse(b > y, b-1, b)
            }
        attributes(termobj) <- a
    }
    termobj
}

reformulate <- function (termlabels, response=NULL, intercept = TRUE, env = parent.frame())
{
    ## an extension of formula.character()
    if(!is.character(termlabels))
        stop("'termlabels' must be a character vector")
    if(intercept && !length(termlabels)) termlabels <- "1"
    termtext <- paste(termlabels, collapse = "+")
    if(!intercept) termtext <- paste(termtext, "- 1")
    terms <- str2lang(termtext)
    fexpr <-
	if(is.null(response))
	    call("~", terms)
	else
	    call("~",
		 ## response can be a symbol or call as  Surv(ftime, case)
		 if(is.character(response)) {
		     if(length(response) != 1)
			 stop(gettextf("'%s' must be a character string", "response"), domain=NA)
                     tryCatch(str2lang(response),
                              error = function(e) {
                                  sc <- sys.calls()
                                  sc1 <- lapply(sc, `[[`, 1L)
                                  isF <- function(cl) is.symbol(cl) && cl == quote(reformulate)
                                  reformCall <- sc[[match(TRUE, vapply(sc1, isF, NA))]]
                                  warning(warningCondition(message = paste(sprintf(
		"Unparseable 'response' \"%s\"; use is deprecated.  Use as.name(.) or `..`!",
									response),
						conditionMessage(e), sep="\n"),
                                      class = c("reformulate", "deprecatedWarning"),
                                      call = reformCall)) # , domain=NA
                                  as.symbol(response)
                              })
		 }
                 else response,
		 terms)
    formula(fexpr, env)
}

drop.terms <- function(termobj, dropx = NULL, keep.response = FALSE)
{
    if (!length(dropx))
	if(keep.response) termobj else delete.response(termobj)
    else {
        if(!inherits(termobj, "terms"))
            stop(gettextf("'termobj' must be a object of class %s",
                          dQuote("terms")),
                 domain = NA)
	response <- attr(termobj, "response")
	newformula <- attr(termobj, "term.labels")[-dropx]
	if (!is.null(off <- attr(termobj, "offset")))
	    newformula <- c(newformula,
			    as.character(attr(termobj, "variables")[off + 1L]))
	newformula <-
	    reformulate(newformula,
			response = if(response && keep.response) termobj[[2L]],
			intercept = attr(termobj, "intercept"),
			env = environment(termobj))
	result <- terms(newformula, specials=names(attr(termobj, "specials")))

	# Edit the optional attributes
	dropOpt <- if(response && !keep.response) # we have a response in termobj, but not in the result
		       c(response, dropx + length(response))
		   else
		       dropx + max(response)

	if (!is.null(predvars <- attr(termobj, "predvars"))) {
	    # predvars is a language expression giving a list of
	    # values corresponding to terms in the model
            # so add 1 for the name "list"
	    attr(result, "predvars") <- predvars[-(dropOpt+1)]
	}
	if (!is.null(dataClasses <- attr(termobj, "dataClasses"))) {
	    # dataClasses is a character vector of
	    # values corresponding to terms in the model
	    attr(result, "dataClasses") <- dataClasses[-dropOpt]
	}
	result
    }
}


`[.terms` <- function (termobj, i)
{
    resp <- if (attr(termobj, "response")) termobj[[2L]]
    newformula <- attr(termobj, "term.labels")[i]
    if (!is.null(off <- attr(termobj, "offset")))
	newformula <- c(newformula,
			as.character(attr(termobj, "variables")[off + 1L]))
    if (length(newformula) == 0L) newformula <- "1"
    newformula <- reformulate(newformula, resp, attr(termobj, "intercept"), environment(termobj))
    result <- terms(newformula, specials = names(attr(termobj, "specials")))

    # Edit the optional attributes

    addindex <- function(index, offset)
        # add a non-negative offset to a possibly negative index
    	ifelse(index < 0, index - offset,
    	       ifelse(index == 0, 0, index + offset))

    if (is.logical(i))
    	i <- which(rep_len(i, length.out = length(attr(termobj, "term.labels"))))

    response <- attr(termobj, "response")
    if (response)
	iOpt <- c(if (max(i) > 0) response, # inclusive indexing
	          addindex(i, max(response)))
    else
	iOpt <- i

    if (!is.null(predvars <- attr(termobj, "predvars")))
	attr(result, "predvars") <- predvars[c(if (max(iOpt) > 0) 1,
	                                     addindex(iOpt, 1))]

    if (!is.null(dataClasses <- attr(termobj, "dataClasses")))
	attr(result, "dataClasses") <- dataClasses[iOpt]

    result
}


## Arguments abb and neg.out are a legacy from S
## simplify=TRUE was the default in R < 1.7.0
terms.formula <- function(x, specials = NULL, abb = NULL, data = NULL,
			  neg.out = TRUE, keep.order = FALSE,
                          simplify = FALSE, ..., allowDotAsName = FALSE)
{
    if(!missing(abb))    .Deprecated(msg=gettextf("setting '%s' in terms.formula() is deprecated", "abb"))
    if(!missing(neg.out)).Deprecated(msg=gettextf("setting '%s' in terms.formula() is deprecated", "neg.out"))
    if(simplify)
    fixFormulaObject <- function(object) {
        Terms <- terms(object)
	tmp <- attr(Terms, "term.labels")
        ## fix up terms involving | : PR#8462
        ind <- grep("|", tmp, fixed = TRUE)
        if(length(ind)) tmp[ind] <- paste("(", tmp[ind], ")")
        ## need to add back any offsets
        if(length(ind <- attr(Terms, "offset"))) {
            ## can't look at rownames of factors, as not there for y ~ offset(x)
            tmp2 <- as.character(attr(Terms, "variables"))[-1L]
            tmp <- c(tmp, tmp2[ind])
        }
	rhs <- if(length(tmp)) paste(tmp, collapse = " + ") else "1"
	if(!attr(Terms, "intercept")) rhs <- paste(rhs, "- 1")
        if(length(form <- formula(object)) > 2L) {
            res <- formula(paste("lhs ~", rhs))
            res[[2L]] <- form[[2L]]
            res
        } else formula(paste("~", rhs))
    }

    if (!is.null(data) && !is.environment(data) && !is.data.frame(data))
	data <- as.data.frame(data, optional = TRUE)
    terms <-
        .External(C_termsform, x, specials, data, keep.order, allowDotAsName)
    if (simplify) {
        a <- attributes(terms)
        terms <- fixFormulaObject(terms)
        attributes(terms) <- a
    }
    environment(terms) <- environment(x)
    if(!inherits(terms, "formula"))
        class(terms) <- c(oldClass(terms), "formula")
    terms
}

coef <- function(object, ...) UseMethod("coef")
## 'complete': be compatible with vcov()
coef.default <- function(object, complete=TRUE, ...) {
    cf <- object$coefficients
    if(complete) cf else cf[!is.na(cf)]
}
coef.aov <- coef.default; formals(coef.aov)[["complete"]] <- FALSE
coefficients <- coef

residuals <- function(object, ...) UseMethod("residuals")
residuals.default <- function(object, ...)
    naresid(object$na.action, object$residuals)
resid <- residuals

deviance <- function(object, ...) UseMethod("deviance")
deviance.default <- function(object, ...) object$deviance

fitted <- function(object, ...) UseMethod("fitted")
## we really do need partial matching here
fitted.default <- function(object, ...)
{
    xx <- if("fitted.values" %in% names(object))
        object$fitted.values else object$fitted
    napredict(object$na.action, xx)
}
fitted.values <- fitted

anova <- function(object, ...)UseMethod("anova")

effects <- function(object, ...)UseMethod("effects")

weights <- function(object, ...)UseMethod("weights")
## used for class "lm", e.g. in drop1.
weights.default <- function(object, ...)
{
    wts <-  object$weights
    if (is.null(wts)) wts else napredict(object$na.action, wts)
}

df.residual <- function(object, ...)UseMethod("df.residual")
df.residual.default <- function(object, ...) object$df.residual

variable.names <- function(object, ...) UseMethod("variable.names")
variable.names.default <- function(object, ...) colnames(object)

case.names <- function(object, ...) UseMethod("case.names")
case.names.default <- function(object, ...) rownames(object)

simulate <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulate")

offset <- function(object) object
## ?

.checkMFClasses <- function(cl, m, ordNotOK = FALSE)
{
    ## when called from predict.nls, vars not match.
    new <- vapply(m, .MFclass, "")
    new <- new[names(new) %in% names(cl)]
    if(length(new) == 0L) return(invisible())
    ## else
    old <- cl[names(new)]
    if(!ordNotOK) {
        old[old == "ordered"] <- "factor"
        new[new == "ordered"] <- "factor"
    }
    ## ordered is OK as a substitute for factor, but not v.v.
    new[new == "ordered" & old == "factor"] <- "factor"
    ## factor is OK as a substitute for character
    ## This probably means the original character got auto-converted to
    ## factor, setting xlevels and causing the conversion of the new
    new[new == "factor" & old == "character"] <- "character"
    if(!identical(old, new)) {
        wrong <- old != new
        if(sum(wrong) == 1)
            stop(gettextf(
    "variable '%s' was fitted with type \"%s\" but type \"%s\" was supplied",
                          names(old)[wrong], old[wrong], new[wrong]),
                 call. = FALSE, domain = NA)
        else
            stop(gettextf(
    "variables %s were specified with different types from the fit",
                 paste(sQuote(names(old)[wrong]), collapse=", ")),
                 call. = FALSE, domain = NA)
    }
    else invisible()
}

##' Model Frame Class
.MFclass <- function(x)
{
    ## the idea is to identify the relevant classes that model.matrix
    ## will handle differently
    ## logical, factor, ordered vs numeric, and other for future proofing
    if(is.logical(x)) return("logical")
    if(is.ordered(x)) return("ordered")
    if(is.factor(x)) return("factor")
    ## Character vectors may be auto-converted to factors, but keep them separate for now
    if(is.character(x)) return("character")
    if(is.matrix(x) && is.numeric(x))
        return(paste0("nmatrix.", ncol(x)))
    ## this is unclear.  Prior to 2.6.0 we assumed numeric with attributes
    ## meant something, but at least for now model.matrix does not
    ## treat it differently.
##    if(is.vector(x) && is.numeric(x)) return("numeric")
    if(is.numeric(x)) return("numeric")
    return("other")
}

##' A complete deparse for "models", i.e. for formula and variable names (PR#15377)
##' @param width.cutoff = 500L: Some people have generated longer variable names
##' https://stat.ethz.ch/pipermail/r-devel/2010-October/058756.html
deparse2 <- function(x)
    paste(deparse(x, width.cutoff = 500L, backtick = !is.symbol(x) && is.language(x)),
          collapse = " ")

model.frame <- function(formula, ...) UseMethod("model.frame")
model.frame.default <-
    function(formula, data = NULL, subset = NULL, na.action,
	     drop.unused.levels = FALSE, xlev = NULL,...)
{
    ## first off, establish if we were passed a data frame 'newdata'
    ## and note the number of rows.
    possible_newdata <-
        !missing(data) && is.data.frame(data) &&
        identical(substitute(data), quote(newdata)) &&
        (nr <- nrow(data)) > 0

    ## were we passed just a fitted model object?
    ## the fit might have a saved model object
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && !is.null(m <- formula$model)) return(m)
    ## if not use the saved call (if there is one).
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && all(c("terms", "call") %in% names(formula))) {
        fcall <- formula$call
        m <- match(c("formula", "data", "subset", "weights", "na.action"),
                   names(fcall), 0)
        fcall <- fcall[c(1, m)]
        ## need stats:: for non-standard evaluation
        fcall[[1L]] <- quote(stats::model.frame)
        env <- environment(formula$terms) %||% parent.frame()
        return(eval(fcall, env)) # 2-arg form as env is an environment
    }
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") && length(attr(data, "terms")))
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    } else
        formula <- as.formula(formula)
    if(missing(na.action)) {
	na.action <-
            if(!is.null(naa <- attr(data, "na.action")) && mode(naa)!="numeric")
                naa
            else getOption("na.action") %||%
                     na.fail # rarely happens (option historically unset in S, see FAQ 3.3.2)
    }

    ## The following logic is quite ancient and should possibly be revised
    ## In particular it lets data=1 slip through and subsequent eval()
    ## would interpret it as a sys.frame() index (PR#17879).
    ## For now, insert explicit check below

    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")

    ## Explicitly check "data"
    if (!is.data.frame(data) && !is.environment(data) && !is.list(data)
        && !is.null(data))
        stop("'data' must be a data.frame, environment, or list")

    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars") %||% vars
    varnames <- vapply(vars, deparse2, " ")[-1L]
    variables <- eval(predvars, data, env)
    resp <- attr(formula, "response")
    if(is.null(rownames) && resp > 0L) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    if(possible_newdata && length(variables)) {
        ## need to do this before subsetting and na.action
        nr2 <- max(vapply(variables, NROW, 0L))
        if(nr2 != nr)
            warning(sprintf(paste0(ngettext(nr,
                                            "'newdata' had %d row",
                                            "'newdata' had %d rows", domain = "R-stats"),
                                   " ",
                                  ngettext(nr2,
                                           "but variable found had %d row",
                                           "but variables found have %d rows", domain = "R-stats")),
                            nr, nr2),
                    call. = FALSE, domain = NA)
    }
    if(is.null(attr(formula, "predvars"))) {
        for (i in seq_along(varnames))
            predvars[[i+1L]] <- makepredictcall(variables[[i]], vars[[i+1L]])
        attr(formula, "predvars") <- predvars
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    data <- .External2(C_modelframe, formula, rownames, variables, varnames,
                       extras, extranames, subset, na.action)
    ## fix up the levels
    if(length(xlev)) {
	for(nm in names(xlev))
	    if(!is.null(xl <- xlev[[nm]])) {
		xi <- data[[nm]]
                if(is.character(xi))
                    xi <- as.factor(xi)
		if(!is.factor(xi) || is.null(nxl <- levels(xi)))
		    warning(gettextf("variable '%s' is not a factor", nm),
                            domain = NA)
		else {
		    ctr <- attr(xi, "contrasts")
		    xi <- xi[, drop = TRUE] # drop unused levels
                    nxl <- levels(xi)
		    if(any(m <- is.na(match(nxl, xl))))
                        stop(sprintf(ngettext(length(m),
                                              "factor %s has new level %s",
                                              "factor %s has new levels %s", domain = "R-stats"),
                                     nm, paste(nxl[m], collapse=", ")),
                             domain = NA)
		    data[[nm]] <- factor(xi, levels=xl, exclude=NULL)
		    if (!identical(attr(data[[nm]], "contrasts"), ctr))
		    	warning(gettextf("contrasts dropped from factor %s", nm),
		    	        call. = FALSE, domain = NA)
		}
	    }
    } else if(drop.unused.levels) {
	for(nm in names(data)) {
	    x <- data[[nm]]
	    if(is.factor(x) &&
	       length(unique(x[!is.na(x)])) < length(levels(x))) {
	        ctr <- attr(x, "contrasts")
		data[[nm]] <- x[, drop = TRUE]
		if (!identical(attr(data[[nm]], "contrasts"), ctr))
		    warning(gettextf(
				"contrasts dropped from factor %s due to missing levels",
					    nm), domain = NA, call. = FALSE)
	    }
	}
    }
    attr(formula, "dataClasses") <- vapply(data, .MFclass, "")
    attr(data, "terms") <- formula
    data
}

## we don't assume weights are numeric or a vector, leaving this to the
## calling application
model.weights <- function(x) .subset2(x, "(weights)")

## we do check that offsets are numeric.
model.offset <- function(x) {
    offsets <- attr(attr(x, "terms"),"offset")
    if(length(offsets)) {
	ans <- .subset2(x, "(offset)") %||% 0
	for(i in offsets) ans <- ans+x[[i]]
    }
    else ans <- .subset2(x, "(offset)")
    if(!is.null(ans) && !is.numeric(ans)) stop("'offset' must be numeric")
    ans
}

model.matrix <- function(object, ...) UseMethod("model.matrix")

model.matrix.default <- function(object, data = environment(object),
				 contrasts.arg = NULL, xlev = NULL, ...)
{
    t <- if(missing(data)) terms(object) else terms(object, data=data)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
	reorder <- match(vapply(attr(t, "variables"), deparse2, "")[-1L],
                         names(data))
	if (anyNA(reorder))
	    stop("model frame and formula mismatch in model.matrix()")
	if(!identical(reorder, seq_len(ncol(data))))
	    data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) {
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        ## turn any character columns into factors
        for(i in namD)
            if(is.character(data[[i]]))
                data[[i]] <- factor(data[[i]])
        isF <- vapply(data, function(x) is.factor(x) || is.logical(x), NA)
        isF[int] <- FALSE
        isOF <- vapply(data, is.ordered, NA)
        for(nn in namD[isF])            # drop response
            if(is.null(attr(data[[nn]], "contrasts")))
                contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        ## it might be safer to have numerical contrasts:
        ##	  get(contr.funs[1 + isOF[nn]])(nlevels(data[[nn]]))
        if (!is.null(contrasts.arg)) {
          if (!is.list(contrasts.arg))
              warning("non-list contrasts argument ignored")
          else {  ## contrasts.arg is a list
            if (is.null(namC <- names(contrasts.arg)))
                stop("'contrasts.arg' argument must be named")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD)))
                    warning(gettextf("variable '%s' is absent, its contrast will be ignored", nn),
                            domain = NA)
                else {
                    ca <- contrasts.arg[[nn]]
                    if(is.matrix(ca)) contrasts(data[[ni]], ncol(ca)) <- ca
                    else contrasts(data[[ni]]) <- ca
                }
            }
          }
        } ## non-null contrasts.arg
    } else { #  no rhs terms ('~1', or '~0'): internal model.matrix needs some variable
	isF <- FALSE
	data[["x"]] <- raw(nrow(data))
    }
    ans <- .External2(C_modelmatrix, t, data) # modelmatrix() in ../src/model.c
    if(any(isF))
	attr(ans, "contrasts") <- lapply(data[isF], attr, "contrasts")
    ans
}

model.response <- function (data, type = "any")
{
    if (attr(attr(data, "terms"), "response")) {
	if (is.list(data) || is.data.frame(data)) {
	    v <- data[[1L]]
	    if (type == "numeric" && is.factor(v)) {
		warning("using type = \"numeric\" with a factor response will be ignored")
	    } else if (type == "numeric" || type == "double")
		storage.mode(v) <- "double"
	    else if (type != "any") stop("invalid response type")
	    if (is.matrix(v) && ncol(v) == 1L) dim(v) <- NULL
	    if(is.object(v) && inherits(v, "AsIs"))
		v <- unclass(v)
	    rows <- attr(data, "row.names")
	    if (nrows <- length(rows)) {
		if (length(v) == nrows) names(v) <- rows
		else if (length(dd <- dim(v)) == 2L)
		    if (dd[1L] == nrows && !length((dn <- dimnames(v))[[1L]]))
			dimnames(v) <- list(rows, dn[[2L]])
	    }
	    return(v)
	} else stop(gettextf("invalid '%s' argument", "data"))
    } else return(NULL)
}

model.extract <- function (frame, component)
{
    component <- as.character(substitute(component))
    rval <- switch(component,
		   response = model.response(frame),
		   offset   = model.offset  (frame),
                   ## otherwise :
                   frame[[paste0("(", component, ")")]]
                   )
    if(!is.null(rval)){
	if (length(rval) == nrow(frame))
	    names(rval) <- attr(frame, "row.names")
	else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
	    t1 <- dimnames(rval)
	    dimnames(rval) <- list(attr(frame, "row.names"), t1[[2L]])
	}
    }
    rval
}

preplot <- function(object, ...) UseMethod("preplot")
update <- function(object, ...) UseMethod("update")

is.empty.model <- function (x)
{
    tt <- terms(x)
    (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}

makepredictcall <- function(var, call) UseMethod("makepredictcall")

makepredictcall.default  <- function(var, call)
{
    if(as.character(call)[1L] != "scale") return(call)
    if(!is.null(z <- attr(var, "scaled:center"))) call$center <- z
    if(!is.null(z <- attr(var, "scaled:scale"))) call$scale <- z
    call
}

.getXlevels <- function(Terms, m)
{
    xvars <- vapply(attr(Terms, "variables"), deparse2, "")[-1L]
    if((yvar <- attr(Terms, "response")) > 0) xvars <- xvars[-yvar]
    if(length(xvars)) {
	xlev <- lapply(m[xvars], function(x)
	    if(is.factor(x)) levels(x)
	    else if(is.character(x)) levels(as.factor(x))) # else NULL
	xlev[!vapply(xlev, is.null, NA)]
    }
}

get_all_vars <- function(formula, data = NULL, ...)
{
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")) )
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")

    ## Explicitly check "data" -- see comment in model.frame.default
    if (!is.data.frame(data) && !is.environment(data) && !is.list(data)
        && !is.null(data))
        stop("'data' must be a data.frame, environment, or list")

    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    varnames <- all.vars(formula)
    variables <- lapply(lapply(varnames, as.name), eval, data, env)
    if(is.null(rownames) && (resp <- attr(formula, "response")) > 0) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
	rownames <- if(!is.null(d <- dim(lhs)) && length(d) == 2L) {
			if(is.data.frame(lhs)) .row_names_info(lhs, 0L) else rownames(lhs)
		    } else names(lhs)
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    x <- c(variables, extras)
    ## protect the unprotected matrices:
    if(anyM <- any(isM <- vapply(x, function(o) is.matrix(o) && !inherits(o,"AsIs"), NA)))
        x[isM] <- lapply(x[isM], I)
    nms.x <- c(varnames, extranames)
    if(any(vapply(x, is.data.frame, NA)))
        nms.x <- unlist(lapply(seq_along(x), function(i)
            if(is.list(x[[i]])) names(x[[i]]) else nms.x[[i]]))
    x <- as.data.frame(x, optional=TRUE)
    names(x) <- nms.x
    if(anyM)
        x[isM] <- lapply(x[isM], function(o) `class<-`(o, class(o)[class(o) != "AsIs"]))
    attr(x, "row.names") <- rownames %||%  # might be short form
        .set_row_names(max(vapply(x, NROW, integer(1))))
    x
}
