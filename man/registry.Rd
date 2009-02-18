\name{registry}
\alias{registry}
\title{Registry creator}
\description{
  Function to create a registry object.
}
\usage{
registry(entry_class = NULL, registry_class = NULL,
         validity_FUN = NULL, stop_if_missing = FALSE)
}

\arguments{
  \item{entry_class}{character string indicating a class the
   returned registry object will additionally inherit from
    (optional). Used for dispatching user-specified print and summary methods.}
  \item{registry_class}{character string indicating a class the
    registry entries will additionally inherit from (optional).
    Used for dispatching user-specified print and summary methods.}
  \item{validity_FUN}{a function accepting a new registry entry as argument for
    checking its validity and possibly aborting with an error
    message. The entry will be provided by the calling function as a
    list with named components (fields).}
  \item{stop_if_missing}{logical indicating whether the registry lookup
    functions should abort or just return \code{NULL} in case of no
    match.}
}
\details{
  This is a generating function that will return a registry object whose
  components are accessor functions for the contained data. These are
  documented separately (\code{\link{regobj}}).
}
\author{David Meyer \email{David.Meyer@R-project.org}}

\seealso{\code{\link{regobj}}}

\examples{
R <- registry()

R$set_field("X", type = TRUE)
R$set_field("Y", type = "character")
R$set_field("index", type = "character", is_key = TRUE,
            index_FUN = match_partial_ignorecase)
R$set_field("index2", type = "integer", is_key = TRUE)

R$set_entry(X = TRUE, Y = "bla", index = "test", index2 = 1L)
R$set_entry(X = FALSE, Y = "foo", index = c("test", "bar"), index2 = 2L)

R$get_entries("test")
R[["test", 1]]
R["test"]
R[["test"]]
}
\keyword{data}