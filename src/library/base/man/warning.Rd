% File src/library/base/man/warning.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{warning}
\alias{warning}
\alias{suppressWarnings}
\title{Warning Messages}
\description{
  Generates a warning message that corresponds to its argument(s) and
  (optionally) the expression or function from which it was called.
}
\usage{
warning(\dots, call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)
suppressWarnings(expr, classes = "warning")
}
\arguments{
  \item{\dots}{\emph{either} zero or more objects which can be coerced
    to character (and which are pasted together with no separator)
    \emph{or} a single \link{condition} object.}
  \item{call.}{logical, indicating if the call should become part of the
    warning message.}
  \item{immediate.}{logical, indicating if the warning should be output
    immediately, even if \code{\link{getOption}("warn") <=
      0}. \strong{NB:} this is not respected for condition objects.}
  \item{noBreaks.}{logical, indicating as far as possible the message should
    be output as a single line when \code{\link{options}(warn = 1)}.}
  \item{expr}{expression to evaluate.}
  \item{domain}{see \code{\link{gettext}}.  If \code{NA}, messages will
    not be translated, see also the note in \code{\link{stop}}.}
  \item{classes}{character, indicating which classes of warnings should
    be suppressed.}
}
\details{
  The result \emph{depends} on the value of
  \code{\link{options}("warn")} and on handlers established in the
  executing code.

  If a \link{condition} object is supplied it should be the only
  argument, and further arguments will be ignored, with a message.
  \code{\link{options}(warn = 1)} can be used to request an immediate
  report.

  \code{warning} signals a warning condition by (effectively) calling
  \code{signalCondition}.  If there are no handlers or if all handlers
  return, then the value of \code{warn = \link{getOption}("warn")} is
  used to determine the appropriate action.  If \code{warn} is negative
  warnings are ignored; if it is zero they are stored and printed after
  the top--level function has completed; if it is one they are printed
  as they occur and if it is 2 (or larger) warnings are turned into
  errors. Calling \code{warning(immediate. = TRUE)} turns \code{warn <=
    0} into \code{warn = 1} for this call only.


  If \code{warn} is zero (the default), a read-only variable
  \code{last.warning} is created.  It contains the warnings which can be
  printed via a call to \code{\link{warnings}}.

  Warnings will be truncated to \code{\link{getOption}("warning.length")}
  characters, default 1000, indicated by \code{[... truncated]}.

  While the warning is being processed, a \code{muffleWarning} restart
  is available.  If this restart is invoked with \code{invokeRestart},
  then \code{warning} returns immediately.

  An attempt is made to coerce other types of inputs to \code{warning}
  to character vectors.

  \code{suppressWarnings} evaluates its expression in a context that
  ignores all warnings.
}
\value{
  The warning message as \code{\link{character}} string, invisibly.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{stop}} for fatal errors,
  \code{\link{message}} for diagnostic messages,
  \code{\link{warnings}},
  and \code{\link{options}} with argument \code{warn=}.

  \code{\link{gettext}} for the mechanisms for the automated translation
  of messages.
}
\examples{
\dontshow{oldopt <- options(warn = 1)}
testit <- function() warning("testit")
testit() ## shows call
testit <- function() warning("problem in testit", call. = FALSE)
testit() ## no call
suppressWarnings(warning("testit"))
\dontshow{eigenval <- 10 ^ -stats::rnorm(1, mean = 6)
if(eigenval < 1.e-7) warning("system near singular")
options(oldopt)}
}
\keyword{programming}
\keyword{error}
