% File src/library/base/man/grep.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{grep}
\title{Pattern Matching and Replacement}
\alias{grep}
\alias{grepv}
\alias{grepl}
\alias{sub}
\alias{gsub}
\alias{regexpr}
\alias{gregexpr}
\alias{regexec}
\alias{gregexec}
\description{
  \code{grep}, \code{grepv}, \code{grepl}, \code{regexpr}, \code{gregexpr}, 
  \code{regexec} and \code{gregexec} search for matches to argument
  \code{pattern} within each element of a character vector: they differ in
  the format of and amount of detail in the results.

  \code{sub} and \code{gsub} perform replacement of the first and all
  matches respectively within each element of a character vector.
}
\usage{
grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
     fixed = FALSE, useBytes = FALSE, invert = FALSE)

grepv(pattern, x, ignore.case = FALSE, perl = FALSE, value = TRUE,
      fixed = FALSE, useBytes = FALSE, invert = FALSE)

grepl(pattern, x, ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE)

sub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE)

gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
     fixed = FALSE, useBytes = FALSE)

regexpr(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)

gregexpr(pattern, text, ignore.case = FALSE, perl = FALSE,
         fixed = FALSE, useBytes = FALSE)

regexec(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)

gregexec(pattern, text, ignore.case = FALSE, perl = FALSE,
        fixed = FALSE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{character string containing a \link{regular expression}
    (or character string for \code{fixed = TRUE}) to be matched
    in the given character vector.  Coerced by
    \code{\link{as.character}} to a character string if possible.  If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.  Missing values are allowed except for
    \code{regexpr}, \code{gregexpr} and \code{regexec}.}
  \item{x, text}{a character vector where matches are sought, or an
    object which can be coerced by \code{as.character} to a character
    vector.  \link{Long vectors} are supported.}
  \item{ignore.case}{logical. if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{perl}{logical.  Should Perl-compatible regexps be used?}
  \item{value}{logical. If \code{FALSE}, a vector containing the (\code{integer})
    indices of the matches determined by \code{grep} is returned, and if
    \code{TRUE}, a vector containing the matching elements themselves is
    returned.}
  \item{fixed}{logical. If \code{TRUE}, \code{pattern} is a string to be
    matched as is.  Overrides all conflicting arguments.}
  \item{useBytes}{logical.  If \code{TRUE} the matching is done
    byte-by-byte rather than character-by-character.  See
    \sQuote{Details}.}
  \item{invert}{logical. If \code{TRUE} return indices or values for
    elements that do \emph{not} match.}
  \item{replacement}{a replacement for the matched pattern in \code{sub} and
    \code{gsub}.  Coerced to character if possible.  For
    \code{fixed = FALSE} this can include backreferences \code{"\\1"} to
    \code{"\\9"} to parenthesized subexpressions of \code{pattern}.  For
    \code{perl = TRUE} only, it can also contain \code{"\\U"} or
    \code{"\\L"} to convert the rest of the replacement to upper or
    lower case and \code{"\\E"} to end case conversion.  If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.  If \code{NA}, all elements in the result
    corresponding to matches will be set to \code{NA}.
  }
}
\details{
  Arguments which should be character strings or character vectors are
  coerced to character if possible.

  Each of these functions operates in one of three modes:
  \enumerate{
    \item \code{fixed = TRUE}: use exact matching.
    \item \code{perl = TRUE}: use Perl-style regular expressions.
    \item \code{fixed = FALSE, perl = FALSE}: use POSIX 1003.2
    extended regular expressions (the default).
  }
  See the help pages on \link{regular expression} for details of the
  different types of regular expressions.

  The two \code{*sub} functions differ only in that \code{sub} replaces
  only the first occurrence of a \code{pattern} whereas \code{gsub}
  replaces all occurrences.  If \code{replacement} contains
  backreferences which are not defined in \code{pattern} the result is
  undefined (but most often the backreference is taken to be \code{""}).

  For \code{regexpr}, \code{gregexpr}, \code{regexec} and \code{gregexec}
  it is an error for \code{pattern} to be \code{NA}, otherwise \code{NA}
  is permitted and gives an \code{NA} match.

  \code{grep} and \code{grepv} only differ in the default of the
  \code{value} argument.
  
  Both \code{grep} and \code{grepl} take missing values in \code{x} as
  not matching a non-missing \code{pattern}.

  The main effect of \code{useBytes = TRUE} is to avoid errors/warnings
  about invalid inputs and spurious matches in multibyte locales, but
  for \code{regexpr} it changes the interpretation of the output.  It
  inhibits the conversion of inputs with marked encodings, and is forced
  if any input is found which is marked as \code{"bytes"} (see
  \code{\link{Encoding}}).

  Caseless matching does not make much sense for bytes in a multibyte
  locale, and you should expect it only to work for ASCII characters if
  \code{useBytes = TRUE}.

  \code{regexpr} and \code{gregexpr} with \code{perl = TRUE} allow
  Python-style named captures, but not for \emph{long vector} inputs.

  Invalid inputs in the current locale are warned about up to 5 times.

  Caseless matching with \code{perl = TRUE} for non-ASCII characters
  depends on the PCRE library being compiled with \sQuote{Unicode
  property support}, which PCRE2 is by default.
}

\value{
  \code{grep(value = FALSE)} returns a vector of the indices
  of the elements of \code{x} that yielded a match (or not, for
  \code{invert = TRUE}).  This will be an integer vector unless the input
  is a \emph{\link{long vector}}, when it will be a double vector.

  \code{grep(value = TRUE)} returns a character vector containing the
  selected elements of \code{x} (after coercion, preserving names but no
  other attributes).

  \code{grepl} returns a logical vector (match or not for each element of
  \code{x}).

  \code{sub} and \code{gsub} return a character vector of the same length
  and with the same attributes as \code{x} (after possible coercion to
  character).  Elements of character vectors \code{x} which are not
  substituted will be returned unchanged (including any declared encoding if
  \code{useBytes = FALSE}).  If \code{useBytes = FALSE} a non-ASCII
  substituted result will often be in UTF-8 with a marked encoding (e.g., if
  there is a UTF-8 input, and in a multibyte locale unless \code{fixed =
  TRUE}).  Such strings can be re-encoded by \code{\link{enc2native}}.  If
  any of the inputs is marked as \code{"bytes"}, elements of character
  vectors \code{x} which are substituted will be returned marked as
  \code{"bytes"}, but the encoding flag on elements not substituted is
  unspecified (it may be the original or "bytes").  If none of the inputs is
  marked as \code{"bytes"}, but \code{useBytes = TRUE} is given explicitly,
  the encoding flag is unspecified even on the substituted elements (it may
  be \code{"bytes"} or \code{"unknown"}, possibly invalid in the current
  encoding).  Mixed use of \code{"bytes"} and other marked encodings is
  discouraged, but if still desired one may use \code{\link{iconv}} to
  re-encode the result e.g.  to UTF-8 with suitably substituted invalid
  bytes.

  \code{regexpr} returns an integer vector of the same length as
  \code{text} giving the starting position of the first match or
  \eqn{-1} if there is none, with attribute \code{"match.length"}, an
  integer vector giving the length of the matched text (or \eqn{-1} for
  no match).  The match positions and lengths are in characters unless
  \code{useBytes = TRUE} is used, when they are in bytes (as they are
  for ASCII-only matching: in either case an attribute
  \code{useBytes} with value \code{TRUE} is set on the result).  If
  named capture is used there are further attributes
  \code{"capture.start"}, \code{"capture.length"} and
  \code{"capture.names"}.

  \code{gregexpr} returns a list of the same length as \code{text} each
  element of which is of the same form as the return value for
  \code{regexpr}, except that the starting positions of every (disjoint)
  match are given.

  \code{regexec} returns a list of the same length as \code{text} each
  element of which is either \eqn{-1} if there is no match, or a
  sequence of integers with the starting positions of the match and all
  substrings corresponding to parenthesized subexpressions of
  \code{pattern}, with attribute \code{"match.length"} a vector
  giving the lengths of the matches (or \eqn{-1} for no match).  The
  interpretation of positions and length and the attributes follows
  \code{regexpr}.

  \code{gregexec} returns the same as \code{regexec}, except that to
  accommodate multiple matches per element of \code{text}, the integer
  sequences for each match are made into columns of a matrix, with one
  matrix per element of \code{text} with matches.

  Where matching failed because of resource limits (especially for
  \code{perl = TRUE}) this is regarded as a non-match, usually with a
  warning.
}

\section{Warning}{
  The POSIX 1003.2 mode of \code{gsub} and \code{gregexpr} does not
  work correctly with repeated word-boundaries (e.g.,
  \code{pattern = "\\b"}).
  Use \code{perl = TRUE} for such matches (but that may not
  work as expected with non-ASCII inputs, as the meaning of
  \sQuote{word} is system-dependent).
}

\section{Performance considerations}{
  If you are doing a lot of regular expression matching, including on
  very long strings, you will want to consider the options used.
  Generally \code{perl = TRUE} will be faster than the default regular
  expression engine, and \code{fixed = TRUE} faster still (especially
  when each pattern is matched only a few times).

  If you are working with texts with non-ASCII characters, which can be
  easily turned into ASCII (e.g.  by substituting fancy quotes), doing so is
  likely to improve performance.

  If you are working in a single-byte locale (though not common since \R 4.2)
  and have marked UTF-8 strings that are representable in that locale,
  convert them first as just one UTF-8 string will force all the matching to
  be done in Unicode, which attracts a penalty of around \eqn{3\times{}}{3x}
  for the default POSIX 1003.2 mode.

  While \code{useBytes = TRUE} will improve performance further, because the
  strings will not be checked before matching and the actual matching will
  be faster, it can produce unexpected results so is best avoided.  With
  \code{fixed = TRUE} and \code{useBytes = FALSE}, optimizations are in
  place that take advantage of byte-based matching working for such patterns
  in UTF-8.  With \code{useBytes = TRUE}, character ranges, wildcards,
  and other regular expression patterns may produce unexpected results.

  PCRE-based matching by default used to put additional effort into
  \sQuote{studying} the compiled pattern when \code{x}/\code{text} has
  length 10 or more.  That study may use the PCRE \abbr{JIT} compiler on
  platforms where it is available (see \code{\link{pcre_config}}). As
  from PCRE2 (PCRE version >= 10.00 as reported by
  \code{\link{extSoftVersion}}), there is no study phase, but the
  patterns are optimized automatically when possible, and PCRE \abbr{JIT} is
  used when enabled.  The details are controlled by
  \code{\link{options}} \code{PCRE_study} and \code{PCRE_use_JIT}.
  (Some timing comparisons can be seen by running file
  \file{tests/PCRE.R} in the \R sources (and perhaps installed).)
  People working with PCRE and very long strings can adjust the maximum
  size of the \abbr{JIT} stack by setting environment variable
  \env{R_PCRE_JIT_STACK_MAXSIZE} before \abbr{JIT} is used to a value between
  \code{1} and \code{1000} in MB: the default is \code{64}. When \abbr{JIT} is
  not used with PCRE version < 10.30 (that is with PCRE1 and old
  versions of PCRE2), it might also be wise to set the option
  \code{PCRE_limit_recursion}.
}

\source{
  The C code for POSIX-style regular expression matching has changed
  over the years.  As from \R 2.10.0 (Oct 2009) the TRE library of Ville
  Laurikari (\url{https://github.com/laurikari/tre}) is used.  The POSIX
  standard does give some room for interpretation, especially in the
  handling of invalid regular expressions and the collation of character
  ranges, so the results will have changed slightly over the years.

  For Perl-style matching PCRE2 or PCRE (\url{https://www.pcre.org}) is
  used: again the results may depend (slightly) on the version of PCRE
  in use.
}

\note{
  Aspects will be platform-dependent as well as locale-dependent: for
  example the implementation of character classes (except
  \code{[:digit:]} and \code{[:xdigit:]}).  One can expect results to be
  consistent for ASCII inputs and when working in UTF-8 mode (when most
  platforms will use Unicode character tables, although those are
  updated frequently and subject to some degree of interpretation -- is
  a circled capital letter alphabetic or a symbol?).  However, results
  in 8-bit encodings can differ considerably between platforms, modes
  and from the UTF-8 versions.
}
\section{Warning}{
  An all too common mis-usage is to pass unnamed arguments which are then
  matched to one or more of \code{ignore.case}, \code{perl}, \code{value},
  \code{fixed}, \code{useBytes}  and \code{invert}.  So it is good
  practice to name all  the arguments (especially as
  \code{x}/\code{text} is not the first argument).
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole (\code{grep})
}

% the `aka' below is for ESS
\seealso{
  \link{regular expression} (aka \code{\link{regexp}}) for the details
  of the pattern specification.

  \code{\link{regmatches}} for extracting matched substrings based on
  the results of \code{regexpr}, \code{gregexpr} and \code{regexec}.

  \code{\link{glob2rx}} to turn wildcard matches into regular expressions.

  \code{\link{agrep}} for approximate matching.

  \code{\link{charmatch}}, \code{\link{pmatch}} for partial matching,
  \code{\link{match}} for matching to whole strings,
  \code{\link{startsWith}} for matching of initial parts of strings.

  \code{\link{tolower}}, \code{\link{toupper}} and \code{\link{chartr}}
  for character translations.

  \code{\link{apropos}} uses regexps and has more examples.

  \code{\link{grepRaw}} for matching raw vectors.

  Options \code{PCRE_limit_recursion}, \code{PCRE_study} and
  \code{PCRE_use_JIT}.

  \code{\link{extSoftVersion}} for the versions of regex and PCRE
  libraries in use, \code{\link{pcre_config}} for more details for
  PCRE.
}
\examples{
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
   cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\\" must be escaped, i.e., 'doubled'
gsub("([ab])", "\\\\1_\\\\1_", "abc and ABC")

txt <- c("The", "licenses", "for", "most", "software", "are",
  "designed", "to", "take", "away", "your", "freedom",
  "to", "share", "and", "change", "it.",
  "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
  "is", "intended", "to", "guarantee", "your", "freedom", "to",
  "share", "and", "change", "free", "software", "--",
  "to", "make", "sure", "the", "software", "is",
  "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt) ) # indices
stopifnot( txt[i] == grep("[gu]", txt, value = TRUE) )

## Note that for some implementations character ranges are
## locale-dependent (but not currently).  Then [b-e] in locales such as
## en_US may include B as the collation order is aAbBcCdDe ...
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution
## In caseless matching, ranges include both cases:
a <- grep("[b-e]", txt, value = TRUE)
b <- grep("[b-e]", txt, ignore.case = TRUE, value = TRUE)
setdiff(b, a)

txt[gsub("g","#", txt) !=
    gsub("g","#", txt, ignore.case = TRUE)] # the "G" words

regexpr("en", txt)

gregexpr("e", txt)

## Using grepl() for filtering
## Find functions with argument names matching "warn":
findArgs <- function(env, pattern) {
  nms <- ls(envir = as.environment(env))
  nms <- nms[is.na(match(nms, c("F","T")))] # <-- work around "checking hack"
  aa <- sapply(nms, function(.) { o <- get(.)
               if(is.function(o)) names(formals(o)) })
  iw <- sapply(aa, function(a) any(grepl(pattern, a, ignore.case=TRUE)))
  aa[iw]
}
findArgs("package:base", "warn")

## trim trailing white space
str <- "Now is the time      "
sub(" +$", "", str)  ## spaces only
## what is considered 'white space' depends on the locale.
sub("[[:space:]]+$", "", str) ## white space, POSIX-style
## what PCRE considered white space changed in version 8.34: see ?regex
sub("\\\\s+$", "", str, perl = TRUE) ## PCRE-style white space

## capitalizing
txt <- "a test of capitalizing"
gsub("(\\\\w)(\\\\w*)", "\\\\U\\\\1\\\\L\\\\2", txt, perl=TRUE)
gsub("\\\\b(\\\\w)",    "\\\\U\\\\1",       txt, perl=TRUE)

txt2 <- "useRs may fly into JFK or laGuardia"
gsub("(\\\\w)(\\\\w*)(\\\\w)", "\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3", txt2, perl=TRUE)
 sub("(\\\\w)(\\\\w*)(\\\\w)", "\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3", txt2, perl=TRUE)

## named capture
notables <- c("  Ben Franklin and Jefferson Davis",
              "\tMillard Fillmore")
# name groups 'first' and 'last'
name.rex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
(parsed <- regexpr(name.rex, notables, perl = TRUE))
gregexpr(name.rex, notables, perl = TRUE)[[2]]
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parse.one(notables, parsed)

## Decompose a URL into its components.
## Example by LT (http://www.cs.uiowa.edu/~luke/R/regexp.html).
x <- "http://stat.umn.edu:80/xyz"
m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
m
regmatches(x, m)
## Element 3 is the protocol, 4 is the host, 6 is the port, and 7
## is the path.  We can use this to make a function for extracting the
## parts of a URL:
URL_parts <- function(x) {
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
                     lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
}
URL_parts(x)

## gregexec() may match multiple times within a single string.
pattern <- "([[:alpha:]]+)([[:digit:]]+)"
s <- "Test: A1 BC23 DEF456"
m <- gregexec(pattern, s)
m
regmatches(s, m)

## Before gregexec() was implemented, one could emulate it by running
## regexec() on the regmatches obtained via gregexpr().  E.g.:
lapply(regmatches(s, gregexpr(pattern, s)),
       function(e) regmatches(e, regexec(pattern, e)))
}
\keyword{character}
\keyword{utilities}
