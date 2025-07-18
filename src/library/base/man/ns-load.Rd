% File src/library/base/man/ns-load.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{ns-load}
\title{Loading and Unloading Name Spaces}
%% Currently considered "main" help page on namespaces
%%                       --------------- -> \references{}, \seealso{}
\alias{attachNamespace}
\alias{loadNamespace}
\alias{requireNamespace}
\alias{loadedNamespaces}
\alias{unloadNamespace}
\alias{isNamespaceLoaded}

\description{
  Functions to load and unload name spaces explicitly.  Loading namespaces typically
  happens automatically, often from \code{\link{library}()} or also when
  other packages import their dependent package namespaces.
}
\usage{
attachNamespace(ns, pos = 2L, depends = NULL, exclude, include.only)
loadNamespace(package, lib.loc = NULL,
              keep.source = getOption("keep.source.pkgs"),
              partial = FALSE, versionCheck = NULL,
              keep.parse.data = getOption("keep.parse.data.pkgs"))
requireNamespace(package, ..., quietly = FALSE)
loadedNamespaces()
unloadNamespace(ns)
isNamespaceLoaded(name)
}
\arguments{
  \item{ns}{string or name space object.}
  \item{pos}{integer specifying position to attach.}
  \item{depends}{\code{NULL} or a character vector of dependencies to be
    recorded in object \code{.Depends} in the package.}
  \item{package}{string naming the package/name space to load.}
  \item{lib.loc}{character vector specifying library search path (the location
    of \R library trees to search through.}
  \item{keep.source}{now ignored except during package installation.}
  \item{keep.parse.data}{ignored except during package installation.}
  \item{partial}{logical; if true, stop just after loading code.}
  \item{versionCheck}{\code{NULL} or a version specification, i.e., a
    \code{\link{list}} with components \code{op} and \code{version} where
    \code{op} is a string denoting a \code{\link{Comparison}} operator such
    as \code{">="}, \code{"=="}, \code{"<"}, etc.}
  \item{quietly}{logical: should progress and error messages be suppressed?}
  \item{name}{string or \sQuote{name}, see \code{\link{as.symbol}},
    of a package, e.g., \code{"stats"}.}
  \item{exclude, include.only}{character vectors; see \code{\link{library}}.}
  \item{\dots}{further arguments to be passed to \code{loadNamespace}.}
}
\details{
  The functions \code{loadNamespace} and \code{attachNamespace} are
  usually called implicitly when \code{\link{library}} is used to load a name
  space and any imports needed.  However it may be useful at times to
  call these functions directly.

  \code{loadNamespace} loads the specified name space and registers it in
  an internal data base.  A request to load a name space when one of that
  name is already loaded has no effect.  \code{versionCheck} allows to
  require a namespace with restricted (e.g., minimal or maximal) package
  version (\code{\link{packageVersion}()}).  If the version check fails,
  \code{loadNamespace()} signals an error, whereas \code{requireNamespace()}
  returns \code{FALSE} after showing \code{loadNamespace}'s error message.

  Further arguments have the same
  meaning as the corresponding arguments to \code{\link{library}}, whose
  help page explains the details of how a particular installed package
  comes to be chosen.  After loading, \code{loadNamespace} looks for a
  hook function named \code{\link{.onLoad}} as an internal variable in
  the name space (it should not be exported).  Partial loading is used
  to support installation with lazy-loading.

  Optionally the package licence is checked during loading: see section
  \sQuote{Licenses} in the help for \code{\link{library}}.

  \code{loadNamespace} does not attach the name space it loads to the
  search path.  \code{attachNamespace} can be used to attach a frame
  containing the exported values of a name space to the search path (but
  this is almost always done \emph{via} \code{\link{library}}).  The
  hook function \code{\link{.onAttach}} is run after the name space
  exports are attached.

  \code{requireNamespace} is a wrapper for \code{loadNamespace}
  analogous to \code{\link{require}} that returns a logical value.

  \code{loadedNamespaces} returns a character vector of the names of
  the loaded name spaces.

  \code{isNamespaceLoaded(pkg)} is equivalent to but more efficient than
  \code{pkg \%in\% loadedNamespaces()}.

  \code{unloadNamespace} can be used to attempt to force a name space to
  be unloaded.  If the name space is attached, it is first
  \code{\link{detach}}ed, thereby running a \code{\link{.onDetach}} or
  \code{.Last.lib} function in the name space if one is exported.  An
  error is signaled and the name space is not unloaded if the name space
  is imported by other loaded name spaces.  If defined, a hook function
  \code{\link{.onUnload}} is run before removing the name space from the
  internal registry.

  See the comments in the help for \code{\link{detach}} about some
  issues with unloading and reloading name spaces.
}
\value{
  \code{attachNamespace} returns invisibly the package environment it
  adds to the search path.

  \code{loadNamespace} returns the name space environment, either one
  already loaded or the one the function causes to be loaded.

  \code{requireNamespace} returns \code{TRUE} if it succeeds or
  \code{FALSE}.

  \code{loadedNamespaces} returns a \code{\link{character}} vector.

  \code{unloadNamespace} returns \code{NULL}, invisibly.
}
\section{Tracing}{
  As from \R 4.1.0 the operation of \code{loadNamespace} can be traced,
  which can help track down the causes of unexpected messages (including
  which package(s) they come from since \code{loadNamespace} is called in
  many ways including from itself and by \code{::} and can be called by
  \code{load}). Setting the environment variable
  \env{_R_TRACE_LOADNAMESPACE_} to a numerical value will generate
  additional messages on progress.  Non-zero values,
  e.g.\sspace{}\code{1}, report which namespace is being loaded and when
  loading completes: values \code{2} to \code{4} report in increasing
  detail.  Negative values are reserved for tracing specific features and
  their current meanings are documented in source-code comments.

  Loading standard packages is never traced.
}
\seealso{
  \code{\link{getNamespace}}, \code{\link{asNamespace}},
  \code{\link{topenv}}, \code{\link{.onLoad}} (etc);
  further \code{\link{environment}}.
}
\references{
  The \sQuote{Writing R Extensions} manual, section \dQuote{Package namespaces}.
}
\author{Luke Tierney and R-core}
\examples{
 (lns <- loadedNamespaces())
 statL <- isNamespaceLoaded("stats")% checks work also when it is *not* loaded
 stopifnot( identical(statL, "stats" \%in\% lns) )

 ## The string "foo" and the symbol 'foo' can be used interchangeably here:
 stopifnot( identical(isNamespaceLoaded(  "foo"   ), FALSE),
            identical(isNamespaceLoaded(quote(foo)), FALSE),
            identical(isNamespaceLoaded(quote(stats)), statL))

hasS <- isNamespaceLoaded("splines") # (to restore if needed)
Sns <- asNamespace("splines") # loads it if not already
stopifnot(   isNamespaceLoaded("splines"))
if (is.null(try(unloadNamespace(Sns)))) # try unloading the NS 'object'
stopifnot( ! isNamespaceLoaded("splines"))
if (hasS) loadNamespace("splines") # (restoring previous state)
}
\keyword{data}
\keyword{utilities}
