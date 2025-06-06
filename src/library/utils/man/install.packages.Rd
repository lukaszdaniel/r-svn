% File src/library/utils/man/install.packages.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{install.packages}
\alias{install.packages}
\title{Install Packages from Repositories or Local Files}
\description{
  Download and install packages from CRAN-like repositories or from
  local files.
}
\usage{
install.packages(pkgs, lib, repos = getOption("repos"),
                 contriburl = contrib.url(repos, type),
                 method, available = NULL, destdir = NULL,
                 dependencies = NA, type = getOption("pkgType"),
                 configure.args = getOption("configure.args"),
                 configure.vars = getOption("configure.vars"),
                 clean = FALSE, Ncpus = getOption("Ncpus", 1L),
                 verbose = getOption("verbose"),
                 libs_only = FALSE, INSTALL_opts, quiet = FALSE,
                 keep_outputs = FALSE, \dots)
}
\arguments{
  \item{pkgs}{character vector of the names of packages whose
    current versions should be downloaded from the repositories.

    If \code{repos = NULL}, a character vector of file paths,
    \describe{
      \item{on Windows,}{
    file paths of \file{.zip} files containing binary builds of
    packages.  (\samp{http://} and \samp{file://} URLs are also accepted
    and the files will be downloaded and installed from local copies.)
    Source directories or file paths or URLs of archives may be
    specified with \code{type = "source"}, but some packages need
    suitable tools installed (see the \sQuote{Details} section).
      }
      \item{On Unix-alikes,}{these file paths can be source directories or archives
    or binary package archive files (as created by \command{R CMD build
      --binary}).  (\samp{http://} and \samp{file://} URLs are also
    accepted and the files will be downloaded and installed from local
    copies.)  On a CRAN build of \R for macOS these can be \file{.tgz}
    files containing binary package archives.
    Tilde-expansion will be done on file paths.
      }
    }

    If this is missing, a listbox of
    available packages is presented where possible in an interactive \R
    session.
  }
  \item{lib}{
    character vector giving the library directories where to
    install the packages.  Recycled as needed.  If missing, defaults to
    the first element of \code{\link{.libPaths}()}.
  }
  \item{repos}{
    character vector, the base URL(s) of the repositories
    to use, e.g., the URL of a CRAN mirror such as
    \code{"https://cloud.r-project.org"}.  For more details on
    supported URL schemes see \code{\link{url}}.

    Can be \code{NULL} to install from local files, directories or URLs:
    this will be inferred by extension from \code{pkgs} if of length one.
  }
  \item{contriburl}{
    URL(s) of the contrib sections of the repositories.  Use this
    argument if your repository mirror is incomplete, e.g., because
    you mirrored only the \file{contrib} section, or only have
    binary packages.  Overrides argument \code{repos}.
    Incompatible with \code{type = "both"}.
  }
  \item{method}{
    download method, see \code{\link{download.file}}.
  }
  \item{available}{
    a matrix as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} when
    the function makes an internal call to \code{available.packages}.
    Incompatible with \code{type = "both"}.
  }
  \item{destdir}{
    directory where downloaded packages are stored.  If it is
    \code{NULL} (the default) a subdirectory
    \code{downloaded_packages} of the session temporary
    directory will be used (and the files will be deleted
    at the end of the session).
  }
  \item{dependencies}{logical indicating whether to also install
    uninstalled packages which these packages depend on/link
    to/import/suggest (and so on recursively).
    Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.

    Only supported if \code{lib} is of length one (or missing),
    so it is unambiguous where to install the dependent packages.  If
    this is not the case it is ignored, with a warning.

    The default, \code{NA}, means
    \code{c("Depends", "Imports", "LinkingTo")}.

    \code{TRUE} means to use
    \code{c("Depends", "Imports", "LinkingTo", "Suggests")} for
    \code{pkgs} and
    \code{c("Depends", "Imports", "LinkingTo")} for added dependencies:
    this installs all the packages needed to run \code{pkgs}, their
    examples, tests and vignettes (if the package author specified them
    correctly).

    In all of these, \code{"LinkingTo"} is omitted for binary packages.
  }
  \item{type}{character, indicating the type of package to download and
    install.  Will be \code{"source"} except on Windows and some macOS
    builds: see the section on \sQuote{Binary packages} for those.
  }
  \item{configure.args}{
    (Used only for source installs.) A character vector or a named list.
    If a character vector with no names is supplied, the elements are
    concatenated into a single string (separated by a space) and used as
    the value for the \option{--configure-args} flag in the call to
    \command{R CMD INSTALL}.  If the character vector has names these
    are assumed to identify values for \option{--configure-args} for
    individual packages.  This allows one to specify settings for an
    entire collection of packages which will be used if any of those
    packages are to be installed.  (These settings can therefore be
    re-used and act as default settings.)

    A named list can be used also to the same effect, and that
    allows multi-element character strings for each package
    which are concatenated to a single string to be used as the
    value for \option{--configure-args}.
  }
  \item{configure.vars}{
    (Used only for source installs.) Analogous to \code{configure.args}
    for flag \option{--configure-vars}, which is used to set environment
    variables for the \command{configure} run.
  }
  \item{clean}{a logical value indicating whether to add the
    \option{--clean} flag to the call to \command{R CMD INSTALL}.
    This is sometimes used to perform additional operations at the end
    of the package installation in addition to removing intermediate files.
  }
  \item{Ncpus}{the number of parallel processes to use for a parallel
    install of more than one source package.  Values greater than one
    are supported if the \command{make} command specified by
    \code{Sys.getenv("MAKE", "make")} accepts argument
    \option{-k -j <Ncpus>}.
  }
  \item{verbose}{
    a logical indicating if some \dQuote{progress report} should be given.
  }
  \item{libs_only}{
    a logical value: should the \option{--libs-only} option be used to
    install only additional sub-architectures for source installs?  (See also
    \code{INSTALL_opts}.)  This can also be used on Windows to install
    just the DLL(s) from a binary package, e.g.\sspace{}to add 64-bit
    DLLs to a 32-bit install.
  }
  \item{INSTALL_opts}{
    an optional character vector of additional option(s) to be passed to
    \command{R CMD INSTALL} for a source package install.  E.g.,
    \code{c("--html", "--no-multiarch", "--no-test-load")} or, for
    macOS, \code{"--dsym"}.

    Can also be a named list of character vectors to be used as
    additional options, with names the respective package names.
  }
  \item{quiet}{
    logical: if true, reduce the amount of output.  This is \emph{not}
    passed to \code{\link{available.packages}()} in case that is called, on
    purpose.
  }
  \item{keep_outputs}{
    a logical: if true, keep the outputs from installing source packages
    in the current working directory, with the names of the output files
    the package names with \file{.out} appended (overwriting existing
    files, possibly from previous installation attempts).  Alternatively, a
    character string giving the directory in which to save the outputs.
    Ignored when installing from local files.
  }
  \item{\dots}{
    further arguments to be passed to \code{\link{download.file}},
    \code{\link{available.packages}}, or to the functions for binary
    installs on macOS and Windows (which accept an argument \code{"lock"}:
    see the section on \sQuote{Locking}).
  }
}
\details{
  This is the main function to install packages.  It takes a vector of
  names and a destination library, downloads the packages from the
  repositories and installs them.  (If the library is omitted it
  defaults to the first directory in \code{.libPaths()}, with a message
  if there is more than one.)  If \code{lib} is omitted or is of length
  one and is not a (group) writable directory, in interactive use the
  code offers to create a personal library tree (the first element of
  \code{Sys.getenv("R_LIBS_USER")}) and install there.

  Detection of a writable directory is problematic on Windows: see the
  \sQuote{Note} section.

  For installs from a repository an attempt is made to install the
  packages in an order that respects their dependencies.  This does
  assume that all the entries in \code{lib} are on the default library
  path for installs (set by environment variable \env{R_LIBS}).

  You are advised to run \code{update.packages} before
  \code{install.packages} to ensure that any already installed
  dependencies have their latest versions.
}
\value{
  Invisible \code{NULL}.
}
\section{Binary packages}{
  This section applies only to platforms where binary packages are
  available: Windows and CRAN builds for macOS.

  \R packages are primarily distributed as \emph{source} packages, but
  \emph{binary} packages (a packaging up of the installed package) are
  also supported, and the type most commonly used on Windows and by the
  CRAN builds for macOS.  This function can install either type, either by
  downloading a file from a repository or from a local file.

  Possible values of \code{type} for binary packages are either simply
  \code{"binary"} to denote the binaries matching the current R, or
  a string consisting of two or three parts separated by periods: the
  operating system (\code{"win"} or \code{"mac"}), the string
  \code{"binary"} and optional build name (e.g., \code{"big-sur-arm64"}).
  The last part is optional and currently only used on macOS to
  disambiguate builds targeting different macOS versions or
  architectures. Example values:
  \code{"win.binary"} for Windows binaries and
  \code{"mac.binary.big-sur-arm64"} for macOS 11 (\I{Big Sur}) arm64 binaries.
  The corresponding binary type for the running R can be obtained via
  \code{.Platform$pkgType}, however, it may be \code{"source"} if
  the build does not support package binaries.

  For a binary install from a repository, the function checks for the
  availability of a source package on the same repository, and reports
  if the source package has a later version, or is available but no
  binary version is.  This check can be suppressed by using
\preformatted{    options(install.packages.check.source = "no")}
  and should be if there is a partial repository containing only binary
  files.

  An alternative (and the current default) is \code{"both"} which means
  \sQuote{use binary if available and current, otherwise try
    source}. The action if there are source packages which are preferred
  but may contain code which needs to be compiled is controlled by
  \code{\link{getOption}("install.packages.compile.from.source")}.
  \code{type = "both"} will be silently changed to \code{"binary"} if
  either \code{contriburl} or \code{available} is specified.

  Using packages with \code{type = "source"} always works provided the
  package contains no C/C++/Fortran code that needs compilation.
  Otherwise,
  \describe{
    \item{on Windows,}{you will need to have installed the \I{Rtools}
  collection as described in the \sQuote{R for Windows FAQ} \emph{and}
  you must have the \env{PATH} environment variable set up as required
  by \I{Rtools}.

  For a 32/64-bit installation of \R on Windows, a small minority of
  packages with compiled code need either \code{INSTALL_opts =
  "--force-biarch"} or \code{INSTALL_opts = "--merge-multiarch"} for a
  source installation.  (It is safe to always set the latter when
  installing from a repository or tarballs, although it will be a little
  slower.)

  When installing a package on Windows, \code{install.packages} will abort
  the install if it detects that the package is already installed and is
  currently in use.  In some circumstances (e.g., multiple instances of
  \R running at the same time and sharing a library) it will not detect a
  problem, but the installation may fail as Windows locks files in use.
    }
    \item{On Unix-alikes,}{when the package contains C/C++/Fortran code
      that needs compilation, suitable compilers and related tools need
      to be installed.  On macOS you need to have installed the
      \sQuote{Command-line tools for \I{Xcode}} (see the
      \sQuote{R Installation and Administration} manual) and if needed
      by the package a Fortran compiler, and have them in your path.
    }
  }
}

\section{Locking}{
  There are various options for locking: these differ between source and
  binary installs.

  By default for a source install, the library directory is
  \sQuote{locked} by creating a directory \file{00LOCK} within it.  This
  has two purposes: it prevents any other process installing into that
  library concurrently, and is used to store any previous version of the
  package to restore on error.  A finer-grained locking is provided by
  the option \option{--pkglock} which creates a separate lock for each
  package: this allows enough freedom for parallel
  installation.  Per-package locking is the default when installing a
  single package, and for multiple packages when \code{Ncpus > 1L}.
  Finally locking (and restoration on error) can be suppressed by
  \option{--no-lock}.
  % and also options(install.lock = FALSE) in an \R startup file.

  For a macOS binary install, no locking is done by default.  Setting
  argument \code{lock} to \code{TRUE} (it defaults to the value of
  \code{\link{getOption}("install.lock", FALSE)}) will use per-directory
  locking as described for source installs.  For Windows binary install,
  per-directory locking is used by default (\code{lock} defaults to the
  value of \code{\link{getOption}("install.lock", TRUE)}).  If the value is
  \code{"pkglock"} per-package locking will be used.

  If package locking is used on Windows with \code{libs_only = TRUE} and
  the installation fails, the package will be restored to its previous
  state.

  Note that it is possible for the package installation to fail so badly
  that the lock directory is not removed: this inhibits any further
  installs to the library directory (or for \option{--pkglock}, of the
  package) until the lock directory is removed manually.
}

\section{Parallel installs}{
  Parallel installs are attempted if \code{pkgs} has length greater than
  one and \code{Ncpus > 1}.  It makes use of a parallel \command{make},
  so the \code{make} specified (default \command{make}) when \R was
  built must be capable of supporting \command{make -j \var{N}}: GNU make,
  \command{dmake} and \command{pmake} do, but Solaris \command{make} and
  older FreeBSD \command{make} do not: if necessary environment variable
  \env{MAKE} can be set for the current session to select a suitable
  \command{make}.

  \code{install.packages} needs to be able to compute all the
  dependencies of \code{pkgs} from \code{available}, including if one
  element of \code{pkgs} depends indirectly on another.  This means that
  if for example you are installing \abbr{CRAN} packages which depend
  on Bioconductor packages which in turn depend on \abbr{CRAN}
  packages, \code{available} needs to cover both \abbr{CRAN} and
  Bioconductor packages.
}

\section{Timeouts}{
  A limit on the elapsed time for each call to \command{R CMD INSTALL}
  (so for source installs) can be set \emph{via} environment variable
  \env{_R_INSTALL_PACKAGES_ELAPSED_TIMEOUT_}: in seconds (or in minutes
  or hours with optional suffix \samp{m} or \samp{h}, suffix \samp{s}
  being allowed for the default seconds) with \code{0} meaning no limit.

  For non-parallel installs this is implemented \emph{via} the
  \code{timeout} argument of \code{\link{system2}}: for parallel
  installs \emph{via} the OS's \command{timeout} command.  (The one
  tested is from GNU \I{coreutils}, commonly available on Linux but
  not other Unix-alikes.  If no such command is available the timeout
  request is ignored, with a warning. On Windows, one needs to specify
  a suitable \command{timeout} command via environment variable
  \env{R_TIMEOUT}, because \file{c:/Windows/system32/timeout.exe} is
  not.)  For parallel installs a
  \samp{Error 124} message from \command{make} indicates that timeout
  occurred.

  Timeouts during installation might leave lock directories behind and
  not restore previous versions.
}
\section{Version requirements on source installs}{
  If you are not running an up-to-date version of \R you may see a
  message like
\preformatted{
   package 'RODBC' is not available (for R version 3.5.3)
}
  One possibility is that the package is not available in any of the
  selected repositories; another is that is available but only for
  current or recent versions of \R{}.  For \abbr{CRAN} packages take
  a look at the package's CRAN page (e.g.,
  \url{https://cran.r-project.org/package=RODBC}). If that indicates in
  the \samp{Depends} field a dependence on a later version of \R you
  will need to look in the \samp{Old sources} section and select the URL
  of a version of comparable age to your \R.  Then you can supply that
  URL as the first argument of \code{install.packages()}: you may
  need to first manually install its dependencies.

  For other repositories, using \code{available.packages(filters =
    "OS_type")[\var{pkgname}, ]} will show if the package is available
  for any \R version (for your OS).
}
\note{
  \describe{
    \item{On Unix-alikes:}{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g.\sspace{}an \code{R-devel} RPM.  \code{install.packages} will
  give an error if called with \code{type = "source"} on such a system.

  Some binary Linux distributions of \R can be installed on a machine
  without the tools needed to install packages: a possible remedy is to
  do a complete install of \R which should bring in all those tools as
  dependencies.
    }
    \item{On Windows:}{
  \code{install.packages} tries to detect if you have write permission
  on the library directories specified, but Windows reports unreliably.
  If there is only one library directory (the default), \R tries to
  find out by creating a test directory, but even this need not be the
  whole story: you may have permission to write in a library directory
  but lack permission to write binary files (such as \file{.dll} files)
  there.  See the \sQuote{R for Windows FAQ} for workarounds.
    }
  }
}
\seealso{
  \code{\link{update.packages}},
  \code{\link{available.packages}},
  \code{\link{download.packages}},
  \code{\link{installed.packages}},
  \code{\link{contrib.url}}.

  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{untar}} for manually unpacking source package tarballs.

  \code{\link{INSTALL}}, \code{\link{REMOVE}}, \code{\link{remove.packages}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}

  The \sQuote{R Installation and Administration} manual for how to
  set up a repository.
}

\examples{\dontrun{
## A Linux example for Fedora's layout of udunits2 headers.
install.packages(c("ncdf4", "RNetCDF"),
  configure.args = c(RNetCDF = "--with-netcdf-include=/usr/include/udunits2"))
}}
\keyword{utilities}
