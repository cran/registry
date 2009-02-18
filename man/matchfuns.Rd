\name{matchfuns}
\alias{matchfuns}
\alias{match_ignorecase}
\alias{match_exact}
\alias{match_partial}
\alias{match_partial_ignorecase}
\alias{match_regexp}
\title{Matching functions}
\description{
  Functions used for lookups of search keys.
}
\usage{
match_ignorecase(lookup, entry, \dots)
match_exact(lookup, entry, \dots)
match_partial(lookup, entry, \dots)
match_partial_ignorecase(lookup, entry, \dots)
match_regexp(lookup, entry, \dots)
}
\arguments{
  \item{lookup}{Search value to look up (for some key field).}
  \item{entry}{Vector of key values where \code{lookup} is sought.}
  \item{\dots}{For \code{match_ignorecase} and \code{match_exact}:
    currently not used. For \code{match_partial} and
    \code{match_partial_ignorecase}: additional arguments passed to
    \code{\link[base]{pmatch}}. For \code{match_regexp}: additional
    arguments passed to \code{\link[base]{grep}}.}
}
\details{
  These are matching functions to be specified for key fields,
  controlling how search values are looked up in the registry.
}
\author{David Meyer \email{David.Meyer@R-project.org}}

\seealso{\code{\link{regobj}}}

\examples{
## use exact matching
R <- registry(stop_if_missing = FALSE)
R$set_field("Key", type = "character", is_key = TRUE, index_FUN = match_exact)
R$set_field("Value", type = "numeric")
R$set_entry("the key", 1)

R[["the key"]]
R[["the"]]

## use partial matching
R <- registry()
R$set_field("Key", type = "character", is_key = TRUE, index_FUN = match_partial)
R$set_field("Value", type = "numeric")
R$set_entry("the key", 1)

R[["the"]]

## use regular expressions
R <- registry()
R$set_field("Key", type = "character", is_key = TRUE, index_FUN = match_regexp)
R$set_field("Value", type = "numeric")
R$set_entry("the key", 1)
R$set_entry("key", 2)

R[["k.*"]]
R["k.*"]

}
\keyword{data}