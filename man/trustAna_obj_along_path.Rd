% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dModAugmentation.R
\name{trustAna_obj_along_path}
\alias{trustAna_obj_along_path}
\title{evaluate objfun along an argpath of a fit}
\usage{
trustAna_obj_along_path(est, hypothesis = 1, fit)
}
\arguments{
\item{est}{a dMod.frame}

\item{hypothesis}{1}

\item{fit}{a fit with blather = T, has to have the same pars as est$obj}
}
\value{
list of objlists
}
\description{
evaluate objfun along an argpath of a fit
}
