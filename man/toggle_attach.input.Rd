% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dModframetools.R
\name{toggle_attach.input}
\alias{toggle_attach.input}
\title{Toggle attach.input in a dMod.frame}
\usage{
toggle_attach.input(est1, att_in = NULL)
}
\arguments{
\item{est1}{a dMod.frame}

\item{att_in}{If supllied, overwrites the toggle}
}
\value{
no return, as the dMod.frame is modified inside this function. The function is called for its side effect.
}
\description{
Sometimes it is wanted to turn on "attach.input" in a prediction function to look at internal states
}
