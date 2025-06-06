% File src/library/base/man/tapply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{tapply}
\alias{tapply}
\title{Apply a Function Over a Ragged Array}
\description{
  Apply a function to each cell of a ragged array, that is to each
  (non-empty) group of values or data rows given by a unique
  combination of the levels of certain factors.
}
\usage{
tapply(X, INDEX, FUN = NULL, \dots, default = NA, simplify = TRUE)
}
\arguments{
  \item{X}{an \R object for which a \code{\link{split}} method
    exists.  Typically vector-like, allowing subsetting with
    \code{\link{[}}, or a data frame. }
  \item{INDEX}{a \code{\link{list}} of one or more \code{\link{factor}}s,
    each of same length as \code{X}.  The elements are coerced to
    factors by \code{\link{as.factor}}. Can also be a formula, which is
    useful if \code{X} is a data frame; see the \code{f} argument in
    \code{\link{split}} for interpretation. }
  \item{FUN}{a function (or name of a function) to be applied, or \code{NULL}.
    In the case of functions like \code{+}, \code{\%*\%}, etc.,
    the function name must be backquoted or quoted.  If \code{FUN} is
    \code{NULL}, \code{tapply} returns a vector which can be used to subscript
    the multi-way array \code{tapply} normally produces.}
  \item{\dots}{optional arguments to \code{FUN}: the Note section.}
  \item{default}{(only in the case of simplification to an array) the
    value with which the array is initialized as
    \code{\link{array}(default, dim = ..)}.  Before \R 3.4.0, this
    was hard coded to \code{\link{array}()}'s default \code{NA}.  If it
    is \code{NA} (the default), the missing value of the answer type,
    e.g. \code{\link{NA_real_}}, is chosen (\code{\link{as.raw}(0)} for
    \code{"raw"}).  In a numerical case, it may be set, e.g., to
    \code{FUN(integer(0))}, e.g., in the case of \code{FUN = sum} to
    \code{0} or \code{0L}.}
  \item{simplify}{logical; if \code{FALSE}, \code{tapply} always returns
    an array of mode \code{"list"}; in other words, a \code{\link{list}}
    with a \code{\link{dim}} attribute.  If \code{TRUE} (the default), then if
    \code{FUN} always returns a scalar, \code{tapply} returns an array
    with the mode of the scalar.}
}

\details{
  If \code{FUN} is not \code{NULL}, it is passed to
  \code{\link{match.fun}}, and hence it can be a function or a symbol or
  character string naming a function.
}

\value{
  When \code{FUN} is present, \code{tapply} calls \code{FUN} for each
  cell that has any data in it.  If \code{FUN} returns a single atomic
  value for each such cell (e.g., functions \code{mean} or \code{var})
  and when \code{simplify} is \code{TRUE}, \code{tapply} returns a
  multi-way \link{array} containing the values, and \code{NA} for the
  empty cells.  The array has the same number of dimensions as
  \code{INDEX} has components; the number of levels in a dimension is
  the number of levels (\code{nlevels()}) in the corresponding component
  of \code{INDEX}.  Note that if the return value has a class (e.g., an
  object of class \code{"\link{Date}"}) the class is discarded.

  \code{simplify = TRUE} always returns an array, possibly 1-dimensional.

  If \code{FUN} does not return a single atomic value, \code{tapply}
  returns an array of mode \code{\link{list}} whose components are the
  values of the individual calls to \code{FUN}, i.e., the result is a
  list with a \code{\link{dim}} attribute.

  When there is an array answer, its \code{\link{dimnames}} are named by
  the names of \code{INDEX} and are based on the levels of the grouping
  factors (possibly after coercion).

  For a list result, the elements corresponding to empty cells are
  \code{NULL}.

  The \code{\link{array2DF}} function can be used to convert the array
  returned by \code{tapply} into a data frame, which may be more
  convenient for further analysis.
}
\note{
  Optional arguments to \code{FUN} supplied by the \code{...} argument
  are not divided into cells.  It is therefore inappropriate for
  \code{FUN} to expect additional arguments with the same length as
  \code{X}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  the convenience functions \code{\link{by}} and
  \code{\link{aggregate}} (using \code{tapply});
  \code{\link{apply}},
  \code{\link{lapply}} with its versions
  \code{\link{sapply}} and \code{\link{mapply}}.

  \code{\link{array2DF}} to convert the result into a data frame.
}
\examples{
require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)

n <- 17; fac <- factor(rep_len(1:3, n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, default = 0) # maybe more desirable
tapply(1:n, fac, sum, simplify = FALSE)
tapply(1:n, fac, range)
tapply(1:n, fac, quantile)
tapply(1:n, fac, length) ## NA's
tapply(1:n, fac, length, default = 0) # == table(fac)
\dontshow{stopifnot(all.equal(
  unname(unclass(table(fac))),
  unname(        tapply(1:n, fac, length, default = 0))))}
## example of ... argument: find quarterly means
tapply(presidents, cycle(presidents), mean, na.rm = TRUE)

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)

## Some assertions (not held by all patch proposals):
nq <- names(quantile(1:5))
stopifnot(
  identical(tapply(1:3, ind), c(1L, 2L, 4L)),
  identical(tapply(1:3, ind, sum),
            matrix(c(1L, 2L, NA, 3L), 2, dimnames = list(c("1", "2"), c("A", "B")))),
  identical(tapply(1:n, fac, quantile)[-1],
            array(list(`2` = structure(c(2, 5.75, 9.5, 13.25, 17), names = nq),
                 `3` = structure(c(3, 6, 9, 12, 15), names = nq),
                 `4` = NULL, `5` = NULL), dim=4, dimnames=list(as.character(2:5)))))
}
\keyword{iteration}
\keyword{category}
