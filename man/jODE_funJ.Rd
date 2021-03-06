% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jODE.R
\name{jODE_funJ}
\alias{jODE_funJ}
\title{Generate Julia source code of prediction function}
\usage{
jODE_funJ(est_mat, fixed_mat, trafo, odes, obs, err)
}
\arguments{
\item{est_mat, fixed_mat}{output of \link{jODE_prepare_matrices}}

\item{trafo, odes, obs, err}{symbolic definitions of functions. 
* Trafo is a general trafo valid for all conditions (as in P_indiv)
* odes need to be in eqnvec format}
}
\value{
character vector of the functions p, p2, f, f2, g, e, prd_condition (prd for a signle condition)
}
\description{
Generate Julia source code of prediction function
}
