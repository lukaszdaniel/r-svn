% File src/library/base/man/get.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{get}
\title{Return the Value of a Named Object}
\alias{get}
\alias{mget}
\alias{dynGet}
\description{
  Search by name for an object (\code{get}) or zero or more objects
  (\code{mget}).
}
\usage{
get(x, pos = -1, envir = as.environment(pos), mode = "any",
    inherits = TRUE)

mget(x, envir = as.environment(-1), mode = "any", ifnotfound,
     inherits = FALSE)

dynGet(x, ifnotfound = , minframe = 1L, inherits = FALSE)
}
\arguments{
  \item{x}{For \code{get}, an object name (given as a character
    string or a symbol).\cr
    For \code{mget}, a character vector of object names.
  }
  \item{pos, envir}{where to look for the object (see \sQuote{Details}); if
    omitted search as if the name of the object appeared unquoted in an
    expression.}
  \item{mode}{the mode or type of object sought: see the
    \sQuote{Details} section.}
  \item{inherits}{should the enclosing frames of the environment be
    searched?}
  \item{ifnotfound}{For \code{mget}, a \code{\link{list}} of values to
    be used if the item is not found: it will be coerced to a list if
    necessary.\cr
    For \code{dynGet} any \R object, e.g., a call to
    \code{\link{stop}()}.}
  \item{minframe}{integer specifying the minimal frame number to look
    into.}
}
\details{
  The \code{pos} argument can specify the environment in which to look
  for the object in any of several ways: as a positive integer (the
  position in the \code{\link{search}} list); as the character string
  name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}}
  to access the currently active function calls).  The default of
  \code{-1} indicates the current environment of the call to
  \code{get}. The \code{envir} argument is an alternative way to
  specify an environment.

  These functions look to see if each of the name(s) \code{x} have a
  value bound to it in the specified environment.  If \code{inherits} is
  \code{TRUE} and a value is not found for \code{x} in the specified
  environment, the enclosing frames of the environment are searched
  until the name \code{x} is encountered.  See \code{\link{environment}}
  and the \sQuote{R Language Definition} manual for details about the
  structure of environments and their enclosures.

  If \code{mode} is specified then only objects of that type are sought.
  \code{mode} here is a mixture of the meanings of \code{\link{typeof}}
  and \code{\link{mode}}: \code{"function"} covers primitive functions
  and operators, \code{"numeric"}, \code{"integer"} and \code{"double"}
  all refer to any numeric type, \code{"symbol"} and \code{"name"} are
  equivalent \emph{but} \code{"language"} must be used (and not
  \code{"call"} or \code{"("}).
  Currently, \code{mode = "S4"} and \code{mode = "object"} are equivalent.

  For \code{mget}, the values of \code{mode} and \code{ifnotfound} can
  be either the same length as \code{x} or of length 1.  The argument
  \code{ifnotfound} must be a list containing either the value to use if
  the requested item is not found or a function of one argument which
  will be called if the item is not found, with argument the name of the
  item being requested.

  \code{dynGet()} is somewhat experimental and to be used \emph{inside}
  another function.  It looks for an object in the callers, i.e.,
  the \code{\link{sys.frame}()}s of the function.  Use with caution.
}
\value{
  For \code{get}, the object found.  If no object is found an error results.
  If the object is the internal missing argument (aka \code{R_MissingArg}
  in C), a \emph{classed} error, class \code{"getMissingError"} is
  signalled.

  For \code{mget}, a named list of objects (found or specified \emph{via}
  \code{ifnotfound}).
}
\note{
  The reverse (or \dQuote{inverse}) of \code{a <- get(nam)} is
  \code{\link{assign}(nam, a)}, assigning \code{a} to name \code{nam}.

  \code{inherits = TRUE} is the default for \code{get} in \R
  but not for S where it had a different meaning.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{exists}} for checking whether an object exists;
  \code{\link{get0}} for an efficient way of both checking existence and
  getting an object.

  \code{\link{assign}}, the inverse of \code{get()}, see above.

  Use \code{\link[utils]{getAnywhere}} for searching for an object
  anywhere, including in other namespaces, and
  \code{\link[utils]{getFromNamespace}} to find an object in a specific
  namespace.
}
\examples{
get("\%o\%")

## test mget
e1 <- new.env()
mget(letters, e1, ifnotfound = as.list(LETTERS))

## very low-level: get()ing the "missing argument", e.g., inside browser()
ls.str(E <- environment((\(m) \(){})()))  # m : <missing>
str(E$m) # (empty) symbol
ee <- tryCatch(get("m",E), error = function(e) e)
str(ee)
ee
stopifnot(exprs = {
    inherits(ee, "missingArgError") # and
    inherits(ee, "getMissingError")
    ##
    inherits(tryCatch(get0("m", E), error=identity), "getMissingError")
    is.symbol(E$m) # => valid argument to get(), and *also* gets 'missing arg':
    inherits(tryCatch(get ( E$m ) , error=identity), "getMissingError")
})
}
\keyword{data}
