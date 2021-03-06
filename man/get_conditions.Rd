% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d2d2dMod.R
\name{get_conditions}
\alias{get_conditions}
\title{Read CONDITIONS}
\usage{
get_conditions(
  def_content,
  states = get_states(def_content),
  derived = get_derived(def_content),
  substitutions = get_substitutions(def_content)
)
}
\arguments{
\item{def_content}{The def_content data.frame returned by read_def_content}

\item{states}{This has to be supplied from get_states(model_def)}

\item{derived}{derived get_derived(def_content) - data.frame with mandatory columns c("name", "eqn")}

\item{substitutions}{eqnvec containing the substitutions}
}
\value{
eqnvec with ONE CONDITION in the dMod-sense
}
\description{
The conditions section returns one condition in the dMod-sense. It basically returns a trafo with the parameters fixed/changed by this condition
}
