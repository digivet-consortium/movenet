% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coarsening_functions.R
\name{round_weights}
\alias{round_weights}
\title{Round numeric movement data}
\usage{
round_weights(data, unit, column = movenetenv$options$movedata_cols$weight)
}
\arguments{
\item{data}{A movenet-format movement tibble.}

\item{unit}{A positive number. The data in the selected column are rounded to
the nearest multiple of this number. \code{unit} is additionally set as minimum
possible value for the column.}

\item{column}{Name of a single numeric column to round. By default this is
the weight column (as specified in the loaded movement configurations).}
}
\value{
A movement tibble like \code{data}, but with rounding applied to the selected
numeric column.
}
\description{
\code{round_weights()} rounds a selected numeric column in a movenet-format
movement tibble. By default, the weight column is selected.
}
\details{
Requires that the appropriate movement config file is loaded, to correctly
identify the \code{weight} column in \code{data}.

The data in the selected column are modified by rounding to multiples of
\code{unit}.
Additionally, any data points \verb{< unit} are set to \code{unit}, so that this
becomes the minimum possible value in the column. This is to capture that any
livestock movement, no matter how small, has an inherent risk that is
conceptually closer to that of \code{unit}, than that of no movement at all.
}
\seealso{
\code{\link[plyr:round_any]{plyr::round_any()}}, which this function wraps.
}
