% File src/library/tools/man/check_packages_in_dir.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2013-2023 R Core Team
% Distributed under GPL 2 or later

\name{check_packages_in_dir}
\alias{check_packages_in_dir}
\alias{summarize_check_packages_in_dir_depends}
\alias{summarize_check_packages_in_dir_results}
\alias{summarize_check_packages_in_dir_timings}
\alias{check_packages_in_dir_changes}
\alias{check_packages_in_dir_details}
\title{Check Source Packages and Their Reverse Dependencies}
\description{
  Check source packages in a given directory, optionally with their
  reverse dependencies.
}
\usage{
check_packages_in_dir(dir,
                      pfiles = Sys.glob("*.tar.gz"),
                      check_args = character(),
                      check_args_db = list(),
                      reverse = NULL,
                      check_env = character(),
                      xvfb = FALSE,
                      Ncpus = getOption("Ncpus", 1L),
                      clean = TRUE,
                      install_args = list(),
                      parallel_args = list(),
                      ...)

summarize_check_packages_in_dir_results(dir, all = TRUE,
                                        full = FALSE, ...)
summarize_check_packages_in_dir_timings(dir, all = FALSE,
                                        full = FALSE)
summarize_check_packages_in_dir_depends(dir, all = FALSE,
                                        which = c("Depends",
                                                  "Imports",
                                                  "LinkingTo"))

check_packages_in_dir_changes(dir, old,
                              outputs = FALSE, sources = FALSE, ...)
check_packages_in_dir_details(dir, logs = NULL, drop_ok = TRUE, ...)
}
\arguments{
  \item{dir}{a character string giving the path to the directory with
    the source \file{.tar.gz} files to be checked.}
  \item{pfiles}{(optional) character vector of tarball files to be
    checked.  Useful for choosing a subset of the \file{*.tar.gz} files in
    \code{dir}.}
  \item{check_args}{a character vector with arguments to be passed to
    \command{R CMD check}, or a list of length two of such character
    vectors to be used for checking packages and reverse dependencies,
    respectively.}
  \item{check_args_db}{a named list of character vectors with arguments
    to be passed to \command{R CMD check}, with names the respective
    package names.}
  \item{reverse}{a list with names partially matching \code{"repos"},
    \code{"which"}, or \code{"recursive"}, giving the repositories to
    use for locating reverse dependencies (a subset of
    \code{getOption("repos")}, the default), the types of reverse dependencies
    (default: \code{c("Depends", "Imports", "LinkingTo")}, with
    shorthands \code{"most"} and \code{"all"} as for
    \code{\link{package_dependencies}}), and indicating whether to also
    check reverse dependencies of reverse dependencies and so on
    (default: \code{FALSE}), or \code{NULL} (default), in which case no
    reverse dependencies are checked.} 
  \item{check_env}{a character vector of name=value strings to set
    environment variables for checking, or a list of length two of such
    character vectors to be used for checking packages and reverse
    dependencies, respectively.}
  \item{xvfb}{a logical indicating whether to perform checking inside a
    virtual framebuffer X server (Unix only), or a character vector of
    \command{Xvfb} options for doing so.}
  \item{Ncpus}{the number of parallel processes to use for parallel
    installation and checking.}
  \item{clean}{a logical indicating whether to remove the downloaded
    reverse dependency sources.}
  \item{install_args}{list of arguments to be passed to underlying 
    \code{\link{install.packages}} call.}
  \item{parallel_args}{list of arguments to be passed to underlying calls 
    of \code{\link[parallel]{parLapply}} (on Windows) or 
    \code{\link[parallel]{mclapply}} (on other OS).}
  \item{...}{passed to \code{\link{readLines}}, e.g. for reading log files 
    produced in a different encoding; currently not used by 
    \code{check_packages_in_dir}.}
  \item{all}{a logical indicating whether to also summarize the reverse
    dependencies checked.}
  \item{full}{a logical indicating whether to also give details for
    checks with non-ok results, or summarize check example timings (if
    available).}
  \item{which}{see \code{\link{package_dependencies}}.}
  \item{old}{a character string giving the path to the directory of a
    previous \code{check_packages_in_dir} run.}
  \item{outputs}{a logical indicating whether to analyze changes in
    the outputs of the checks performed, or only (default) the status of
    the checks.}
  \item{sources}{a logical indicating whether to also investigate the
    changes in the source files checked (default: \code{FALSE}).}
  \item{logs}{a character vector with the paths of \file{00check.log} to
    analyze.  Only used if \code{dir} was not given.}
  \item{drop_ok}{a logical indicating whether to drop checks with
    \sQuote{ok} status, or a character vector with the \sQuote{ok}
    status tags to drop.  The default corresponds to tags \samp{OK},
    \samp{NONE} and \samp{SKIPPED}.}
}
\details{
  \code{check_packages_in_dir} allows to conveniently check source
  package \file{.tar.gz} files in the given directory \code{dir}, along
  with their reverse dependencies as controlled by \code{reverse}.

  The \code{"which"} component of \code{reverse} can also be a list, in
  which case reverse dependencies are obtained for each element of the
  list and the corresponding element of the \code{"recursive"} component
  of \code{reverse} (which is recycled as needed).

  If needed, the source \file{.tar.gz} files of the reverse dependencies
  to be checked as well are downloaded into \code{dir} (and removed at
  the end if \code{clean} is true).  Next, all packages (additionally)
  needed for checking are installed to the \file{Library} subdirectory
  of \code{dir}.  Then, all \file{.tar.gz} files are checked using the
  given arguments and environment variables, with outputs and messages
  to files in the \file{Outputs} subdirectory of \code{dir}.  The
  \file{*.Rcheck} directories with the check results of the reverse
  dependencies are renamed by prefixing their base names with
  \samp{rdepends_}.

  Results and timings can conveniently be summarized using
  \code{summarize_check_packages_in_dir_results} and
  \code{summarize_check_packages_in_dir_timings}, respectively.

  Installation and checking is performed in parallel if \code{Ncpus} is
  greater than one: this will use \code{\link[parallel]{mclapply}} on
  Unix and \code{\link[parallel]{parLapply}} on Windows.

  \code{check_packages_in_dir} returns an object inheriting from class
  \code{"check_packages_in_dir"} which has \code{\link{print}} and
  \code{\link{summary}} methods.

  \code{check_packages_in_dir_changes} allows to analyze the effect of
  changing (some of) the sources.  With \code{dir} and \code{old} the
  paths to the directories with the new and old sources, respectively,
  and the corresponding check results, possible changes in the check
  results can conveniently be analyzed as controlled via options
  \code{outputs} and \code{sources}.  The changes object returned can be
  subscripted according to change in severity from the old to the new
  results by using one of \code{"=="}, \code{"!="}, \code{"<"},
  \code{"<="}, \code{">"} or \code{">="} as row index.

  \code{check_packages_in_dir_details} analyzes check log files to
  obtain check details as a data frame which can be used for further
  processing, providing check name, status and output for every check
  performed and not dropped according to status tag (via variables
  \code{Check}, \code{Status} and \code{Output}, respectively).
  
  Environment variable \env{_R_CHECK_ELAPSED_TIMEOUT_} can be used to
  set a limit on the elapsed time of each \command{check} run.  See the
  \sQuote{R Internals} manual for how the value is interpreted and for
  other environment variables which can be used for finer-grained
  control on timeouts within a \command{check} run.
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions. 
}
\keyword{utilities}
\examples{
\dontrun{
## Check packages in dir without reverse dependencies:
check_packages_in_dir(dir)
## Check packages in dir and their reverse dependencies using the
## defaults (all repositories in getOption("repos"), all "strong"
## reverse dependencies, no recursive reverse dependencies):
check_packages_in_dir(dir, reverse = list())
## Check packages in dir with their reverse dependencies from CRAN,
## using all strong reverse dependencies and reverse suggests:
check_packages_in_dir(dir,
                      reverse = list(repos = getOption("repos")["CRAN"],
                                     which = "most"))                   
## Check packages in dir with their reverse dependencies from CRAN,
## using '--as-cran' for the former but not the latter:
check_packages_in_dir(dir,
                      check_args = c("--as-cran", ""),
                      reverse = list(repos = getOption("repos")["CRAN"]))
}
}
