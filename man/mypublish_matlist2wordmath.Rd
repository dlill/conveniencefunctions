% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypublish.R
\name{mypublish_matlist2wordmath}
\alias{mypublish_matlist2wordmath}
\title{Print a list of matrices as matrices in word math environemt}
\usage{
mypublish_matlist2wordmath(x, filename)
}
\arguments{
\item{x}{a list of matrices}

\item{filename}{filename without ending}
}
\description{
Print a list of matrices as matrices in word math environemt
}
\examples{
\dontrun{
x <- list(r_0 = matrix(1:9, ncol = 3), r_1 = matrix(0,ncol = 3, nrow = 3), r_opt_1 = matrix(2:10, nrow = 3))
mypublish_matlist2wordmath(x, "matrices")
}
}
