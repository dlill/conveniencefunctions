% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{duplicated_includeFirst}
\alias{duplicated_includeFirst}
\title{ALL indices of a vector with duplicates}
\usage{
duplicated_includeFirst(x)
}
\arguments{
\item{x}{vector}
}
\value{
like duplicated but include the first elements which have duplicate values
}
\description{
ALL indices of a vector with duplicates
}
\examples{
x <- c(rep(1:10), c(1,3,4,2,1,1,2))
duplicated_includeFirst(x)
}
\author{
Daniel Lill (daniel.lill@physik.uni-freiburg.de)
}
