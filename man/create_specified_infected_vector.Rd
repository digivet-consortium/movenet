% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transmission_modelling.R
\name{create_specified_infected_vector}
\alias{create_specified_infected_vector}
\title{Create a vector of holdings infected at t=0 to plug into SimInf models, using
holding identifiers}
\usage{
create_specified_infected_vector(infected_holdings, key)
}
\arguments{
\item{infected_holdings}{A character vector with (original) identifiers of
infected holdings.}

\item{key}{The \code{key} provided by \code{data2contactmatrix()}, to identify the node
numbers of the infected holdings.}
}
\value{
A numeric vector, indicating which nodes are infected at t=0, to
plug into the \code{\link[siminf4movenet:SEIRcm]{siminf4movenet::SEIRcm()}} model or another SimInf model.
}
\description{
Create a vector of holdings infected at t=0 to plug into SimInf models, using
holding identifiers
}
\seealso{
\itemize{
\item \code{\link[siminf4movenet:SEIRcm]{siminf4movenet::SEIRcm()}} for the SEIRcm model developed for movenet.
\item \code{\link[SimInf:SimInf]{SimInf::SimInf}} for the underlying modelling package.
}

Other transmission modelling-related functions: 
\code{\link{SEIRcm_ASF}()},
\code{\link{create_random_infected_vector}()},
\code{\link{data2contactmatrix}()},
\code{\link{get_cumulative_infected}()}
}
\concept{transmission modelling-related functions}
