% File src/library/utils/man/PkgUtils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{PkgUtils}
\alias{build}
\alias{check}
\title{Utilities for Building and Checking Add-on Packages}
\description{
  Utilities for checking whether the sources of an \R add-on package
  work correctly, and for building a source package from them.
}
\usage{
\special{R CMD check [options] pkgdirs}
\special{R CMD build [options] pkgdirs}
}
\arguments{
  \item{pkgdirs}{a list of names of directories with sources of \R
    add-on packages.  For \code{check} these can also be the filepaths of
    (possibly compressed) \command{tar} archives.}
  \item{options}{further options to control the processing, or for
    obtaining information about usage and version of the utility.}
}
\details{
  \command{R CMD check}
  checks \R add-on packages from their sources, performing a wide
  variety of diagnostic checks.

  \command{R CMD build} builds \R source tarballs.  The name(s) of the
  packages are taken from the \file{DESCRIPTION} files and not from the
  directory names.  This works entirely on a copy of the supplied source
  directories.

  Use \command{R CMD \var{foo} --help} to obtain usage information on
  utility \code{\var{foo}}, notably the possible \command{options}.

  The defaults for some of the options to \command{R CMD build} can be
  set by environment variables \env{_R_BUILD_RESAVE_DATA_} and
  \env{_R_BUILD_COMPACT_VIGNETTES_}: see \sQuote{Writing R Extensions}.
  Many of the checks in \command{R CMD check} can be turned off or on by
  environment variables: see Chapter \sQuote{Tools} of the
  \sQuote{R Internals} manual.

  By default \command{R CMD build} uses the \code{"internal"} option to
  \code{\link{tar}} to prepare the tarball.  An external \command{tar}
  program can be specified by the \env{R_BUILD_TAR} environment
  variable.  This may be substantially faster for very large packages,
  and can be needed for packages with long path names (over 100 bytes)
  or very large files (over 8GB): however, the resulting tarball may not
  be portable.

  \command{R CMD check} by default unpacks tarballs by the internal
  \code{\link{untar}} function: if needed an external \command{tar}
  command can be specified by the environment variable
  \env{R_INSTALL_TAR}: please ensure that it can handle the type of
  compression used on the tarball.  (This is sometimes needed for
  tarballs containing invalid or unsupported sections, and can be faster
  on very large tarballs.  Setting \env{R_INSTALL_TAR} to \samp{tar.exe}
  has been needed to overcome permissions issues on some Windows
  systems.  For further details see the help for \code{\link{INSTALL}}.)
}
\note{
  Only on Windows:
  They make use of a temporary directory specified by the environment
  variable \code{TMPDIR} and defaulting to \samp{c:/TEMP}.  Do ensure
  that if set forward slashes are used.
}
\seealso{
  The sections on \sQuote{Checking and building packages} and
  \sQuote{Processing documentation files} in \sQuote{Writing R Extensions}:
  \code{\link{RShowDoc}("R-exts")}.
}
\keyword{utilities}
