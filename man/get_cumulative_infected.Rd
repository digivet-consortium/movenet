% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transmission_modelling.R
\name{get_cumulative_infected}
\alias{get_cumulative_infected}
\title{Get the cumulative infected (I+R components) from SimInf model trajectories}
\usage{
get_cumulative_infected(trajectories, n_simulations, n_nodes)
}
\arguments{
\item{trajectories}{SimInf model trajectories.}

\item{n_simulations}{Number of simulations that the model was run for.}

\item{n_nodes}{Number of nodes/holdings included in the model.}
}
\description{
Get the cumulative infected (I+R components) from SimInf model trajectories
}
\seealso{
\itemize{
\item \code{\link[siminf4movenet:SEIRcm]{siminf4movenet::SEIRcm()}} for the SEIRcm model developed for movenet.
\item \code{\link[SimInf:SimInf]{SimInf::SimInf}} for the underlying modelling package.
}

Other transmission modelling-related functions: 
\code{\link{SEIRcm_ASF}()},
\code{\link{create_random_infected_vector}()},
\code{\link{create_specified_infected_vector}()},
\code{\link{data2contactmatrix}()}
}
\concept{transmission modelling-related functions}
