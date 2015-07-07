\name{regobj}
\alias{regobj}
\alias{summary.registry}
\alias{print.registry}
\alias{[.registry}
\alias{[[.registry}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Registry object}
\description{
  Registry object.
}
\usage{
\special{regobj$get_field(name)}
\special{regobj$get_fields()}
\special{regobj$get_field_names()}
\special{regobj$set_field(name,
                 type = NA, alternatives = NA, default = NA,
                 is_mandatory = FALSE, is_modifiable = TRUE,
                 is_key = FALSE, validity_FUN = NULL,
                 index_FUN = match_ignorecase, \dots)}

\special{regobj$has_entry(key)}
\special{regobj$get_entry(\dots)}
\special{regobj$get_entries(\dots)}
\special{regobj$grep_entries(pattern, \dots)}
\special{regobj$get_entry_names()}
\special{regobj$set_entry(\dots)}
\special{regobj$modify_entry(\dots)}
\special{regobj$delete_entry(\dots)}
\special{regobj$n_of_entries(name)}
\special{regobj$get_field_entries(field, unlist = TRUE)}

\special{regobj$get_permissions()}
\special{regobj$restrict_permissions(set_entries = TRUE,
         modify_entries = TRUE, delete_entries = TRUE, set_fields = TRUE)}
\special{regobj$seal_entries()}
\special{regobj$get_sealed_field_names()}


\method{print}{registry}(x, \dots)
\method{summary}{registry}(object, \dots)

\method{[[}{registry}(x, \dots)
\method{[}{registry}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{character string representing the name of an
    entry (case-insensitive).}
  \item{pattern}{regular expression to be matched to all fields of class
    \code{"character"} in all entries.}
  \item{type}{character vector specifying accepted classes
    for this field. If \code{NA} (default), any class will be accepted.
    If \code{type} is not a character vector, the
    class will be inferred from the argument given.}
  \item{alternatives}{vector of alternatives accepted for this field.}
  \item{default}{optional default value for the field.}
  \item{is_mandatory}{logical specifying whether new entries are required
    to have a value for this field.}
  \item{is_modifiable}{logical specifying whether entries can be changed
    with respect to that field.}
  \item{is_key}{logical indicating whether the field is (part of) an
    index.}
  \item{validity_FUN}{optional function or character string with the name of a
    function that checks the validity of a field entry. Such a function
    gets the value to be investigated as argument, and should stop with an
    error message if the value is not correct.}
  \item{index_FUN}{vectorized predicate function matching an
    index value to a vector (of existing field entries). See \link{matchfuns}.}
  \item{x, object}{a registry object.}
  \item{\dots}{for \code{regobj$set_entry} and \code{regobj$modify_entry}:
    named list of fields to be modified in or added to the registry,
    including the index field(s) (see details).
    For \code{grep_entries}: additional parameters passed to
    \code{\link[base]{grep}}.
    For \code{set_field}: additional parameters passed to the specified
    match function.
    For \code{get_entry}, \code{get_entries}
    and the indexing functions: key values for the entry (entries)
    to be looked up.}
}
\details{
  \code{regobj} represents a registry object returned by
  \code{\link{registry}} whose elements can be processed using
    the following accessor functions:
  
  \code{get_field_names()} returns a character
  vector with all field names. \code{get_field()} returns the information
  for a specific field as a list with components named as described
  above. \code{get_fields()} returns a list with all field
  entries. \code{set_field()} is used to create new fields in the
  repository (the default value will be set in all
  entries).

  \code{get_entry_names()} returns a character vector with (the first
  alias of) all entries. \code{entry_exists()} is a predicate checking
  if an entry with the specified alias exists in the
  registry. \code{get_entry()} returns the first specified entry
  if at least one exists (and, by
  default, gives an error if it does not). \code{get_entries()} is used to
  query more than one entry matching the index
  (named argument list) exactly. \code{grep_entries()} returns those entries
  where the regular expression in \code{pattern} matches \emph{any}
  character field in an entry. By default, all values are
  returned. \code{delete_entry} removes an existing entry from the
  registry.
  
  \code{set_entry}, \code{delete_entry} and \code{modify_entry}
  require a named list
  of arguments used as field entries.
  At least the index fields are required.

  \code{set_entry}
  will check for all other mandatory fields. If specified in the field
  meta data, each field entry and the entry as a whole is checked for
  validity. Note that it is possible to specify a vector of values for
  an index field, treated as alternative keys for this entry.

  It is possible to \emph{restrict} permissions (for setting, getting, deleting
  and modifying entries) using \code{restrict_permissions}. Further, a
  user can \emph{seal} the current registry state (fields, entries) so
  that \emph{existing} structure and information becomes
  immutable. Additional fields and entries can be changed according to the
  permissions set. Permissions and sealing are useful for exported
  registry objects to control the users' capabilities of
  modifying/extending them.
}
\author{David Meyer \email{David.Meyer@R-project.org}}

\seealso{\code{\link{dist}}, \link{matchfuns}}
\examples{
regobj <- registry()
regobj$set_field("X", type = TRUE)
regobj$set_field("Y", type = "character")
regobj$set_field("index", type = "character", is_key = TRUE,
            index_FUN = match_partial_ignorecase)
regobj$set_field("index2", type = "integer", is_key = TRUE)
regobj$set_entry(X = TRUE, Y = "bla", index = "test", index2 = 1L)
regobj$set_entry(X = FALSE, Y = "foo", index = c("test", "bar"), index2 = 2L)
regobj$get_entries("test")
regobj[["test", 1]]
regobj["test"]
regobj[["test"]]
}
\keyword{data}% __ONLY ONE__ keyword per line
