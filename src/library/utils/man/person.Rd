% File src/library/utils/man/person.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{person}
\title{Persons}
\alias{person}
\alias{as.person}
\alias{as.person.default}
\alias{[.person}
\alias{$.person}
\alias{as.character.person}
\alias{c.person}
\alias{format.person}
\alias{print.person}
\alias{toBibtex.person}
\description{
  A class and utility methods for holding information about persons
  like name and email address.
}
\usage{
person(given = NULL, family = NULL, middle = NULL,
       email = NULL, role = NULL, comment = NULL,
       first = NULL, last = NULL)

as.person(x)
\method{as.person}{default}(x)

\method{format}{person}(x,
       include = c("given", "family", "email", "role", "comment"),
       braces = list(given = "", family = "", email = c("<", ">"),
                     role = c("[", "]"), comment = c("(", ")")),
       collapse = list(given = " ", family = " ", email = ", ",
                       role = ", ", comment = ", "),
       ...,
       style = c("text", "R", "md")
)

\method{toBibtex}{person}(object, escape = FALSE, ...)
}
\arguments{
  \item{given}{a character vector with the \emph{given} names,
    or a list thereof.}
  \item{family}{a character string with the \emph{family} name,
    or a list thereof.}
  \item{middle}{a character string with the collapsed middle name(s).
    Deprecated, see \bold{Details}.}
  \item{email}{a character string (or vector) giving an e-mail address
    (each),
    or a list thereof.}
  \item{role}{a character vector specifying the role(s) of the person
    (see \bold{Details}),
    or a list thereof.}
  \item{comment}{a character string (or vector) providing comments,
    or a list thereof.}
  \item{first}{a character string giving the first name.
    Deprecated, see \bold{Details}.}
  \item{last}{a character string giving the last name.
    Deprecated, see \bold{Details}.}
  \item{x}{an object for the \code{as.person} generic;
    a character string for the \code{as.person} default method;
    an object of class \code{"person"} otherwise.}
  \item{include}{a character vector giving the fields to be included
    when formatting.}
  \item{braces}{a list of characters (see \bold{Details}).}
  \item{collapse}{a list of characters (see \bold{Details}).}
  \item{\dots}{currently not used.}
  \item{style}{a character string specifying the print style, with 
    \code{"R"} yielding formatting as R code and \code{"md"} yielding
    Markdown (with email addresses and \I{ORCID iD}s hyperlinked).}
  \item{object}{an \R object inhering from class \code{"person"}.}
  \item{escape}{a logical indicating whether non-ASCII characters should
    be translated to LaTeX escape sequences.}
}
\value{
  \code{person()} and \code{as.person()} return objects of class
  \code{"person"}.
}
\details{
  Objects of class \code{"person"} can hold information about an
  arbitrary positive number of persons.  These can be obtained by one
  call to \code{person()} with list arguments, or by first creating
  objects representing single persons and combining these via
  \code{c()}.

  The \code{format()} method collapses information about persons into
  character vectors (one string for each person): the fields in
  \code{include} are selected, each collapsed to a string using the
  respective element of \code{collapse} and subsequently
  \dQuote{embraced} using the respective element of \code{braces}, and
  finally collapsed into one string separated by white space.  If
  \code{braces} and/or \code{collapse} do not specify characters for all
  fields, the defaults shown in the usage are imputed.
  If \code{collapse} is \code{FALSE} or \code{NA} the corresponding
  field is not collapsed but only the first element is used.
  The \code{print()} method calls the \code{format()} method and prints
  the result, the \code{toBibtex()} method creates a suitable BibTeX
  representation.

  Person objects can be subscripted by fields (using \code{$}) or by
  position (using \code{[}).

  \code{as.person()} is a generic function.  Its default method tries to
  reverse the default person formatting, and can also handle formatted
  person entries collapsed by comma or \code{"and"} (with appropriate
  white space).

  Personal names are rather tricky, e.g.,
  \url{https://en.wikipedia.org/wiki/Personal_name}.

  The current implementation (starting from R 2.12.0) of the
  \code{"person"} class uses the notions of \emph{given} (including
  middle names) and \emph{family} names, as specified by \code{given}
  and \code{family} respectively.  Earlier versions used a scheme based
  on first, middle and last names, as appropriate for most of Western
  culture where the given name precedes the family name, but not
  universal, as some other cultures place it after the family name, or
  use no family name.  To smooth the transition to the new scheme,
  arguments \code{first}, \code{middle} and \code{last} are still
  supported, but their use is deprecated and they must not be given in
  combination with the corresponding new style arguments.  For persons
  which are not natural persons (e.g., institutions, companies, etc.) it
  is appropriate to use \code{given} (but not \code{family}) for the
  name, e.g., \code{person("R Core Team", role = "aut")}.

  The new scheme also adds the possibility of specifying \emph{roles}
  based on a subset of the MARC \I{Code List for Relators} 
  (\url{https://www.loc.gov/marc/relators/relaterm.html}).
  When giving the roles of persons in the context of authoring \R
  packages, the following usage is suggested.
  \describe{
    \item{\code{"aut"}}{(Author) Use for full authors who have made
      substantial contributions to the package and should show up in the
      package citation.}
    \item{\code{"com"}}{(Compiler) Use for persons who collected code
      (potentially in other languages) but did not make further
      substantial contributions to the package.}
    \item{\code{"cph"}}{(Copyright holder) Use for all copyright
      holders.  This is a legal concept so should use the legal name of
      an institution or corporate body.  Note that authors which are
      \sQuote{natural persons} are by default copyright holders and so
      do not need to be given this role.}
    \item{\code{"cre"}}{(Creator) Use for the package maintainer.}
    \item{\code{"ctb"}}{(Contributor) Use for authors who have made
      smaller contributions (such as code patches etc.) but should not
      show up in the package citation.}
    \item{\code{"ctr"}}{(Contractor) Use for authors who have been
      contracted to write (parts of) the package and hence do not own
      intellectual property.}
    \item{\code{"dtc"}}{(Data contributor) Use for persons who
      contributed data sets for the package.}
    \item{\code{"fnd"}}{(\I{Funder}) Use for persons or organizations that
      furnished financial support for the development of the package.}
    \item{\code{"rev"}}{(Reviewer) Use for persons or organizations
      responsible for reviewing (parts of) the package.}
    \item{\code{"ths"}}{(Thesis advisor) If the package is part of a
      thesis, use for the thesis advisor.}
    \item{\code{"trl"}}{(Translator) If the R code is a translation from
      another language (typically S), use for the translator to R.}
  }

  In the old scheme, person objects were used for single persons, and a
  separate \code{"personList"} class with corresponding creator
  \code{\link{personList}()} for collections of these.  The new scheme employs
  a single class for information about an arbitrary positive number of
  persons, eliminating the need for the \code{personList} mechanism.

  The \code{comment} field can be used for \dQuote{arbitrary} additional
  information about persons.  Elements named \code{"ORCID"} will be
  taken to give \abbr{ORCID} identifiers (see \url{https://orcid.org/} for more
  information), and be displayed as the corresponding \abbr{URI}s by the
  \code{print()} and \code{format()} methods (see \bold{Examples}
  below).  Similarly, elements named \code{"ROR"} will be taken to give
  \abbr{ROR} identifiers (see \url{https://ror.org/}).

  Where more than one entity is given a \code{"cph"} role, the
  \code{comment} field should be used to delimit who owns the copyright
  to what parts of the package.
}
\seealso{
  \code{\link{citation}}
}
\keyword{misc}
\examples{
## Create a person object directly ...
p1 <- person("Karl", "Pearson", email = "pearson@stats.heaven")

## ... or convert a string.
p2 <- as.person("Ronald Aylmer Fisher")

## Combining and subsetting.
p <- c(p1, p2)
p[1]
p[-1]

## Extracting fields.
p$family
p$email
p[1]$email

## Specifying package authors, example from "boot":
## AC is the first author [aut] who wrote the S original.
## BR is the second author [aut], who translated the code to R [trl],
## and maintains the package [cre].
b <- c(person("Angelo", "Canty", role = "aut", comment =
         "S original, <http://statwww.epfl.ch/davison/BMA/library.html>"),
       person(c("Brian", "D."), "Ripley", role = c("aut", "trl", "cre"),
              comment = "R port", email = "ripley@stats.ox.ac.uk")
     )
b

## Formatting.
format(b)
format(b, include = c("family", "given", "role"),
   braces = list(family = c("", ","), role = c("(Role(s): ", ")")))

## Conversion to BibTeX author field.
paste(format(b, include = c("given", "family")), collapse = " and ")
toBibtex(b)

## ORCID identifiers.
(p3 <- person("Achim", "Zeileis",
              comment = c(ORCID = "0000-0003-0918-3766")))

## ROR identifiers.
(p4 <- person("R Core Team",
              comment = c(ROR = "02zz1nj61")))

## Example for a contributor whose real name is unknown and cannot be
## determined:
person(given = "@zeileis", role = "ctb", comment = "GitHub user")
}
