% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coarsening_functions.R
\name{generate_anonymisation_key}
\alias{generate_anonymisation_key}
\title{Generate anonymisation key for holding identifiers}
\usage{
generate_anonymisation_key(ids, prefix = "", n_start)
}
\arguments{
\item{ids}{Character vector with existing holding identifiers to generate
replacement identifiers for}

\item{prefix}{Character string, to form the basis of replacement holding
identifiers. An integer will be appended to form this new identifier.}

\item{n_start}{Number from which to start numbering the replacement identifiers}
}
\value{
A named character vector, with original holding identifiers as names
and new (non-identifiable) identifiers as values. Replacement identifiers
consist of \code{prefix} followed by an integer. Integers have consecutive values,
starting from \code{n_start}, and are allocated to holdings in a random order.
}
\description{
\code{generate_anonymisation_key()} creates an anonymisation key that links
existing holding identifiers with new, non-identifiable, identifiers
(randomly allocated integers or prefix-integer combinations).
}
\keyword{internal}
