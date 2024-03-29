% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cms.R
\name{get_longest_run}
\alias{get_longest_run}
\title{Length of the longest run of TRUEs}
\usage{
get_longest_run(x)
}
\arguments{
\item{x}{Boolean vector}
}
\value{
an integer value of the length of the longest run of TRUE values
}
\description{
Identifies the length of the longest run of TRUEs in Boolean vector \code{x}.
used to be \code{maxCMS}
}
\examples{
x <- rbinom(100, size = 1, prob = 1 / 3)
get_runs(x == 1) # expected value for longest match is 3
get_longest_run(x == 1)
}
